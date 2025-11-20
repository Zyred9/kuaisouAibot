package com.search.robots.database.entity;

import cn.hutool.core.lang.UUID;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.search.robots.database.enums.WithdrawalStatus;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 *      提现
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_withdrawals")
public class Withdrawals {

    @TableId(type = IdType.AUTO)
    private Long id;
    /** 用户ID **/
    private Long userId;
    /** 昵称 **/
    private String nickname;
    /** 用户名 **/
    private String username;
    /** 提现单号 **/
    private String withdrawalNo;
    /** 提现类型：Tron **/
    private String withdrawalType;
    /** 提现币种 **/
    private String currency;
    /** 提现金额 **/
    private BigDecimal withdrawalAmount;
    /** 实际到账金额 **/
    private BigDecimal actualAmount;
    /** 提现地址 **/
    private String withdrawalAddress;
    /** 提现时间 **/
    private LocalDateTime transactionDate;
    /** 提现状态 **/
    private WithdrawalStatus status;
    /** 拒绝原因 **/
    private String reason;


    public static Withdrawals buildDefault(User user, BigDecimal amount) {
        return new Withdrawals()
                .setWithdrawalNo(UUID.fastUUID().toString(true))
                .setUserId(user.getUserId())
                .setUsername(user.getUsername())
                .setNickname(user.getNickname())
                .setWithdrawalAddress(user.getTrAddr())
                .setWithdrawalType("Tron")
                .setCurrency("USDT")
                .setWithdrawalAmount(amount)
                .setActualAmount(amount)
                .setTransactionDate(LocalDateTime.now())
                .setStatus(WithdrawalStatus.PROCESSING);
    }
}
