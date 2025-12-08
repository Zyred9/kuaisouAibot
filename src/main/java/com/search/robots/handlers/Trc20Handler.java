package com.search.robots.handlers;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.search.robots.beans.view.caffeine.Task;
import com.search.robots.beans.view.trc20.TransferBean;
import com.search.robots.config.BotProperties;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.*;
import com.search.robots.database.enums.BillTypeEnum;
import com.search.robots.database.service.*;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.TimeHelper;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.Update;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.concurrent.ConcurrentHashMap;

/**
 *
 *
 * @author admin
 * @since 2025/11/15 18:20
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class Trc20Handler extends AbstractHandler {

    @Override
    public boolean support(Update update) {return false;}
    @Override
    protected BotApiMethod<?> execute(Update update) { return null; }

    private final UserService userService;
    private final BillService billService;
    private final BotProperties properties;
    private final OkHttpClient okHttpClient;
    private final ConfigService configService;
    private final AddressService addressService;
    private final ListenReceiveService receiveService;
    private final ListenReceiveService listenReceiveService;
    private final RewardRecordService rewardRecordService;

    private static final Map<String, Long> LISTEN_START_CACHE = new ConcurrentHashMap<>();

    public boolean processorListenAddress(Task expire) {
        String address = expire.getAddress();

        long now = System.currentTimeMillis();
        long start = LISTEN_START_CACHE.computeIfAbsent(address, k -> now);
        if (now - start > 30 * 60 * 1000L) {
            LISTEN_START_CACHE.remove(address);
            return false;
        }

        Config config = this.configService.queryConfig();
        Address addressEntity = this.addressService.getById(address);
        List<TransferBean> transferBeans = this.doQuery(address, addressEntity.getPrevTimestamp());

        List<ListenReceive> listenReceives = new ArrayList<>(transferBeans.size());
        User user = this.userService.select(addressEntity.getUserId());
        List<Bill> bills = new ArrayList<>(transferBeans.size());

        for (TransferBean transferBean : transferBeans) {
            BigDecimal amount = transferBean.parseValue();
            BigDecimal balance = user.getBalance();

            user.setBalance(balance.add(amount));

            Integer preferentialRate = config.getPreferentialRate();
            BigDecimal oneHundred = BigDecimal.TEN.multiply(BigDecimal.TEN);
            BigDecimal rate = BigDecimal.valueOf(preferentialRate)
                    .divide(oneHundred, 2, RoundingMode.HALF_UP);

            BigDecimal giftAmount = amount.multiply(rate);
            bills.add(Bill.buildAdvPaymentBill(user, balance, amount, BillTypeEnum.RECHARGE));

            if (!DecimalHelper.compare(giftAmount, BigDecimal.ZERO)) {
                balance = user.getBalance();
                user.setBalance(user.getBalance().add(giftAmount));
                
                // 记录B用户的充值加赠到奖励表
                this.rewardRecordService.recordRechargeGiftReward(user, giftAmount);
                
                // 更新B用户的待入账金额
                BigDecimal userTotalAward = user.getTotalAward();
                user.setTotalAward(userTotalAward.add(giftAmount));
                
                // 记录到账单表
                bills.add(Bill.buildRewardBill(user, balance, giftAmount, BillTypeEnum.RECHARGE_GIFT));
            }

            // 处理广告代理返佣
            if (Objects.nonNull(user.getAdsParentId()) && Objects.nonNull(config.getAdCommissionRate()) && config.getAdCommissionRate() > 0) {
                User adsParent = this.userService.select(user.getAdsParentId());
                if (Objects.nonNull(adsParent)) {
                    // 从配置中获取返佣比例
                    BigDecimal commissionRate = BigDecimal.valueOf(config.getAdCommissionRate())
                            .divide(oneHundred, 2, RoundingMode.HALF_UP);
                    BigDecimal commissionAmount = amount.multiply(commissionRate);
                    
                    // 更新广告上级的待入账金额
                    BigDecimal adsParentBalance = adsParent.getBalance();
                    adsParent.setBalance(adsParentBalance.add(commissionAmount));
                    BigDecimal adsParentTotalAward = adsParent.getTotalAward();
                    adsParent.setTotalAward(adsParentTotalAward.add(commissionAmount));
                    this.userService.update(adsParent);
                    
                    // 记录广告代理返佣到奖励表
                    this.rewardRecordService.recordAdCommissionReward(adsParent, user, commissionAmount);
                    
                    // 创建账单记录（使用buildRewardBill构建奖励账单）
                    Bill commissionBill = Bill.buildRewardBill(adsParent, adsParentBalance, commissionAmount, BillTypeEnum.AWARD);
                    bills.add(commissionBill);
                    
                    log.info("[广告代理返佣] 充值用户：{}，充值金额：{}，广告上级：{}，返佣比例：{}%，返佣金额：{}", 
                            user.getUserId(), amount, adsParent.getUserId(), config.getAdCommissionRate(), commissionAmount);
                }
            }
            
            listenReceives.add(ListenReceive.buildPushAddress(address, transferBean));
            log.info("[交易监听] 推送用户：{}，地址：{}, 交易信息：{}", user.getUserId(), address, transferBeans.size());
        }

        this.buildMessage(user, bills);

        if (CollUtil.isNotEmpty(listenReceives)) {
            this.userService.update(user);
            this.receiveService.saveBatch(listenReceives);
        }
        if (CollUtil.isNotEmpty(bills)) {
            this.billService.saveBatch(bills);
        }

        this.addressService.update(
                Wrappers.lambdaUpdate(Address.class)
                        .set(Address::getPrevTimestamp, System.currentTimeMillis())
                        .eq(Address::getAddress, address)
        );
        return CollUtil.isEmpty(listenReceives);
    }

    private void buildMessage(User user, List<Bill> bills) {
        for (Bill bill : bills) {
            String first = StrUtil.format(
                    Constants.RECHARGE_USER_MESSAGE_TEXT,
                    user.getUserId(), user.getNickname(),
                    DecimalHelper.decimalParse(bill.getBeforeAmount()),
                    bill.getBillNo(),
                    TimeHelper.format(LocalDateTime.now()),
                    BillTypeEnum.RECHARGE.getDesc(),
                    DecimalHelper.decimalParse(bill.getBeforeAmount())
            );
            AsyncSender.async(markdown(user.getUserId(), first));
        }
    }


    @SneakyThrows
    public List<TransferBean> doQuery(String address, Long prevTimestamp) {
        prevTimestamp = Objects.isNull(prevTimestamp) ? System.currentTimeMillis() : prevTimestamp - (40 * 1000);
        JSONObject jsonObject = doHttpQuery(address, prevTimestamp);
        if (jsonObject == null) return Collections.emptyList();
        JSONArray tokenTransfers = (JSONArray) jsonObject.get("token_transfers");
        List<TransferBean> transferBeans = JSONUtil.toList(tokenTransfers, TransferBean.class);
        if (CollUtil.isEmpty(transferBeans)) {
            return Collections.emptyList();
        }

        transferBeans.sort(Comparator.comparing(TransferBean::getBlockTs));

        List<String> transactionIds = transferBeans.stream().map(TransferBean::getTransactionId).toList();
        List<ListenReceive> pushedList = this.listenReceiveService.list(
                Wrappers.<ListenReceive>lambdaQuery()
                        .select(ListenReceive::getTransactionId)
                        .eq(ListenReceive::getAddress, address)
                        .in(ListenReceive::getTransactionId, transactionIds)
        );
        Set<String> tIds = pushedList.stream().map(ListenReceive::getTransactionId).collect(Collectors.toSet());
        // 已经推送过的
        transferBeans.removeIf(transferBean -> CollUtil.contains(tIds, transferBean.getTransactionId()));
        return transferBeans;
    }

    @SuppressWarnings("all")
    private JSONObject doHttpQuery(String address, Long prevTimestamp) throws IOException {
        String url = StrUtil.format(Constants.TRANSFER_QUERY, address, prevTimestamp);
        Request request = new Request.Builder().url(url).build();
        Response response = this.okHttpClient.newCall(request).execute();
        if (!response.isSuccessful() || Objects.isNull(response.body())) {
            return null;
        }

        String body = response.body().string();
        if (this.properties.isLogs()) {
            log.info("[查询结果] 地址：{}，结果：{}", address, body);
        }
        return JSONUtil.parseObj(body);
    }

}
