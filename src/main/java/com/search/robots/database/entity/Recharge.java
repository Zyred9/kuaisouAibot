package com.search.robots.database.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.search.robots.beans.view.trc20.TransferBean;
import com.search.robots.database.enums.Currency;
import com.search.robots.database.enums.RechargeStatus;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.TimeHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 *      充值
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_recharge")
public class Recharge {

    @TableId(type = IdType.AUTO)
    private Long id;
    /** 转账ID **/
    private String transactionId;
    /** 订单号，唯一标识每一笔充值记录 **/
    private Long userId;
    /** 充值金额 **/
    private String amount;
    /** 下单时间 **/
    private LocalDateTime createTime;
    /** 充值小数点 **/
    private BigDecimal pointer;
    /** 充值金额，支持高精度数字表示 **/
    private Currency currency;
    /** 充值货币类型，如 USDT **/
    private String paymentMethod;
    /** 付款方式，如匿名、银行卡等 **/
    private String transactionDate;
    /** 充值的进度状态，例：0.处理中, 1.已到账、等 **/
    private RechargeStatus status;


    public static Recharge build (TransferBean bean, Long userId) {
        return new Recharge()
                .setUserId(userId)
                .setTransactionId(bean.getTransactionId())
                .setAmount(DecimalHelper.decimalParse(bean.parseValue()))
                .setCurrency(Currency.of(bean.getTokenInfo().getTokenAbbr()))
                .setPaymentMethod("Tron")
                .setTransactionDate(TimeHelper.format(LocalDateTime.now()))
                .setStatus(RechargeStatus.SUCCESS);
    }

    public static Recharge build (Long userId, BigDecimal pointer, BigDecimal rechargeAmount) {
        return new Recharge()
                .setUserId(userId)
                .setAmount(DecimalHelper.decimalParse(rechargeAmount))
                .setPointer(pointer)
                .setPaymentMethod("Tron")
                .setTransactionDate(TimeHelper.format(LocalDateTime.now()))
                .setStatus(RechargeStatus.PROCESS);
    }
}
