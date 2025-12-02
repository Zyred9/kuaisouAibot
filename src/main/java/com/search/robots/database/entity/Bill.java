package com.search.robots.database.entity;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.search.robots.config.Constants;
import com.search.robots.database.enums.BillTypeEnum;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.StrHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import com.search.robots.helper.TimeHelper;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 热搜词每日记录实体类
 * 对应数据库表 t_hot_search_daily
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_bill")
public class Bill {

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;

    private Long userId;
    private String username;
    private String nickname;

    /** 账单编号 **/
    private String billNo;
    /** 账单类型 **/
    private BillTypeEnum type;
    /** 变动前金额 **/
    private BigDecimal beforeAmount;
    /** 金额 **/
    private BigDecimal amount;
    /** 描述 **/
    private String description;
    /** 创建时间 **/
    private LocalDateTime createTime;

    /**
     * 构建广告扣费账单记录（如顶部链接/底部按钮购买等）
     */
    public static Bill buildAdvPaymentBill(User user, BigDecimal before,
                                           BigDecimal need, BillTypeEnum type) {
        return new Bill()
                .setUserId(user.getUserId())
                .setUsername(user.getUsername())
                .setNickname(user.getNickname())
                .setBillNo(TimeHelper.build())
                .setType(type)
                .setDescription("")
                .setBeforeAmount(before)
                .setAmount(need.negate())
                .setCreateTime(LocalDateTime.now());
    }

    /**
     * 构建奖励账单记录（如广告代理返佣等）
     */
    public static Bill buildRewardBill(User user, BigDecimal before,
                                       BigDecimal amount, BillTypeEnum type) {
        return new Bill()
                .setUserId(user.getUserId())
                .setUsername(user.getUsername())
                .setNickname(user.getNickname())
                .setBillNo(TimeHelper.build())
                .setType(type)
                .setDescription("")
                .setBeforeAmount(before)
                .setAmount(amount) // 奖励是正数，不需要negate
                .setCreateTime(LocalDateTime.now());
    }

    public static String buildBillText (List<Bill> bills) {
        if (CollUtil.isEmpty(bills)){
            return "暂无记录";
        }

        StringBuilder sb = new StringBuilder();
        for (Bill bill : bills) {
            sb.append(bill.buildTextLine()).append("\n");
        }
        return sb.toString();
    }


    // 20251102010226|奖励|0.015|审批通过✅
    public String buildTextLine() {
        return StrUtil.format(Constants.SELF_BILL_LINE_TEXT,
                this.billNo, this.type.getMessage(),
                DecimalHelper.decimalParse(this.amount),
                StrHelper.specialResult("审批通过✅")
        );
    }
}