package com.search.robots.database.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.web.withdrawals.WithdrawalsAudit;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.Bill;
import com.search.robots.database.entity.Config;
import com.search.robots.database.entity.User;
import com.search.robots.database.entity.Withdrawals;
import com.search.robots.database.enums.BillTypeEnum;
import com.search.robots.database.enums.WithdrawalStatus;
import com.search.robots.database.mapper.WithdrawalsMapper;
import com.search.robots.database.service.BillService;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.UserService;
import com.search.robots.database.service.WithdrawalsService;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.TimeHelper;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.telegram.telegrambots.meta.api.methods.ParseMode;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Service
@RequiredArgsConstructor
public class WithdrawalsServiceImpl extends ServiceImpl<WithdrawalsMapper, Withdrawals> implements WithdrawalsService {

    private final UserService userService;
    private final BillService billService;
    private final ConfigService configService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void create(User user, BigDecimal amount) {

        BigDecimal balance = user.getBalance();
        Bill bill = Bill.buildAdvPaymentBill(user, balance, amount, BillTypeEnum.WITHDRAWAL);
        Withdrawals withdrawals = Withdrawals.buildDefault(user, amount);
        withdrawals.setWithdrawalNo(bill.getBillNo());

        Config config = this.configService.queryConfig();
        BigDecimal serviceFee = config.getWithdrawalServiceFee();

        balance = balance.subtract(withdrawals.getWithdrawalAmount());
        // 剩余金额不足1，则减少提现余额
        if (DecimalHelper.compare(balance, serviceFee)) {
            withdrawals.setActualAmount(withdrawals.getActualAmount().subtract(serviceFee));
        } else {
            balance = balance.subtract(serviceFee);
        }
        user.setBalance(balance);

        this.baseMapper.insert(withdrawals);
        this.userService.updateById(user);
        this.billService.save(bill);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean audit(WithdrawalsAudit audit) {
        WithdrawalStatus status = WithdrawalStatus.of(audit.getStatus());
        if (Objects.equals(status, WithdrawalStatus.FAILED)
                || Objects.equals(status, WithdrawalStatus.SUCCESS)) {
            Withdrawals withdrawals = this.baseMapper.selectById(audit.getId());
            withdrawals.setStatus(status);

            Bill bill = this.billService.getOne(
                    Wrappers.<Bill>lambdaQuery()
                            .eq(Bill::getBillNo, withdrawals.getWithdrawalNo())
                            .last("limit 1")
            );
            User user = this.userService.getById(withdrawals.getUserId());

            if (Objects.equals(status, WithdrawalStatus.FAILED) ) {
                Config config = this.configService.queryConfig();

                BigDecimal total = user.getBalance().add(withdrawals.getWithdrawalAmount())
                        .add(config.getWithdrawalServiceFee());
                user.setBalance(total);

                this.userService.updateById(user);
                AsyncSender.async(
                        SendMessage.builder()
                                .parseMode(ParseMode.MARKDOWN)
                                .text("您的提现被拒绝")
                                .chatId(user.getUserId())
                                .build()
                );
            } else {
                String first = StrUtil.format(
                        Constants.RECHARGE_USER_MESSAGE_TEXT,
                        user.getUserId(), user.getNickname(),
                        DecimalHelper.decimalParse(bill.getBeforeAmount()),
                        bill.getBillNo(),
                        TimeHelper.format(LocalDateTime.now()),
                        bill.getType().getDesc(),
                        DecimalHelper.decimalParse(bill.getBeforeAmount())
                );
                AsyncSender.async(
                        SendMessage.builder()
                                .parseMode(ParseMode.MARKDOWN)
                                .text(first)
                                .chatId(user.getUserId())
                                .build()
                );
            }
            this.baseMapper.updateById(withdrawals);
            return true;
        }
        return false;
    }

    @Override
    public Page<Withdrawals> withdrawalsPage(int current, int size, String username) {
        return this.baseMapper.selectPage(
                Page.of(current, size),
                Wrappers.<Withdrawals>lambdaQuery()
                        .like(StrUtil.isNotBlank(username), Withdrawals::getUsername, username)
        );
    }
}
