package com.search.robots.database.entity;


import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.config.Constants;
import com.search.robots.database.enums.AdvertisingGradeEnum;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.StrHelper;
import com.search.robots.helper.TimeHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 *
 *
 * @author zyred
 * @since 2025/10/9 15:36
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_user")
public class User {

    @TableId(type = IdType.INPUT)
    private Long userId;
    private String username;
    private String nickname;
    private LocalDateTime registerTime;

    /** 邀请码 **/
    private String inviteCode;
    /** trc20 地址 **/
    private String trAddr;
    /** 广告等级 **/
    private AdvertisingGradeEnum grade;

    /** 总余额 **/
    private BigDecimal balance;
    /** 待入账的金额 **/
    private BigDecimal underwayAmount;
    /** 💰总提现金额：0.0000 **/
    private BigDecimal withdrawAmount;
    /** 💰总奖励金额：0.0000 $ **/
    private BigDecimal awardAmount;
    /** 总充值金额 **/
    private BigDecimal totalRechargeAmount;

    /** 上级ID **/
    private Long parentId;
    /** 广告上级, 会给人消息 **/
    private Long adsParentId;

    /** 今日待入账奖励 **/
    private BigDecimal todayAward;
    /** 总共待入账奖励 **/
    private BigDecimal totalAward;
    /** 累计入账奖励 **/
    private BigDecimal accumulativeTotalAward;


    public static User buildDefault(org.telegram.telegrambots.meta.api.objects.User user) {
        return new User()
                .setUserId(user.getId())
                .setUsername(user.getUserName())
                .setNickname(user.getFirstName())
                .setRegisterTime(LocalDateTime.now())
                .setGrade(AdvertisingGradeEnum.E)
                .setBalance(BigDecimal.ZERO)
                .setInviteCode(StrHelper.generateInviteCode())
                .setUnderwayAmount(BigDecimal.ZERO)
                .setWithdrawAmount(BigDecimal.ZERO)
                .setAwardAmount(BigDecimal.ZERO);
    }


    public String buildText () {
        return StrUtil.format(Constants.SELF_MESSAGE_TEXT,
                this.username, this.userId, this.grade.getDesc(),
                DecimalHelper.standard(this.balance),
                DecimalHelper.standard(this.underwayAmount),
                DecimalHelper.standard(this.withdrawAmount),
                DecimalHelper.standard(this.awardAmount));
    }

    public String buildWalletText (List<Bill> bills) {
        return StrUtil.format(Constants.SELF_WALLET_TEXT,
                this.username,
                this.userId,
                DecimalHelper.standardSymbol(this.balance),
                DecimalHelper.standardSymbol(this.withdrawAmount),
                StrUtil.isBlank(this.trAddr) ? "请绑定" : "||" + this.trAddr + "||",
                Bill.buildBillText(bills)
        );
    }


    public static String buildNextDetailText (Page<User> users) {
        List<User> records = users.getRecords();
        int size = records.size();

        StringBuilder sb = new StringBuilder();
        for (int i = 0, idx = 1; i < size; i++, idx++) {
            User user = records.get(i);
            sb.append(user.buildSelfNextDetailText(idx));
        }
        return sb.toString();
    }

    public String buildSelfNextDetailText (int index) {
        return StrUtil.format(Constants.NEXT_DEFAULT_USER_TEXT, index,
                StrHelper.specialResult(this.nickname),
                StrHelper.specialResult(this.username),
                this.userId,
                DecimalHelper.decimalParse(this.totalRechargeAmount),
                TimeHelper.format(this.registerTime)
        );
    }
}
