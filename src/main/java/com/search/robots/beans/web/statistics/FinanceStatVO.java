package com.search.robots.beans.web.statistics;

import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * 财务收支统计数据
 * 
 * @author admin
 * @since 2025/01/17
 */
@Data
@Accessors(chain = true)
public class FinanceStatVO {
    
    /** 充值统计 */
    private RechargeStatistics rechargeStats;
    
    /** 提现统计 */
    private WithdrawalStatistics withdrawalStats;
    
    /** 充提差额分析 */
    private BalanceAnalysis balanceAnalysis;
    
    @Data
    @Accessors(chain = true)
    public static class RechargeStatistics {
        /** 总充值金额 */
        private BigDecimal totalAmount;
        
        /** 总充值笔数 */
        private Long totalCount;
        
        /** 今日充值金额 */
        private BigDecimal todayAmount;
        
        /** 今日充值笔数 */
        private Long todayCount;
        
        /** 充值用户数 */
        private Long rechargeUserCount;
        
        /** 平均充值金额 */
        private BigDecimal avgAmount;
        
        /** 每日充值趋势 */
        private List<DailyAmountItem> dailyTrend;
    }
    
    @Data
    @Accessors(chain = true)
    public static class WithdrawalStatistics {
        /** 总提现金额 */
        private BigDecimal totalAmount;
        
        /** 总提现笔数 */
        private Long totalCount;
        
        /** 今日提现金额 */
        private BigDecimal todayAmount;
        
        /** 今日提现笔数 */
        private Long todayCount;
        
        /** 待审批数量 */
        private Long pendingCount;
        
        /** 待审批金额 */
        private BigDecimal pendingAmount;
        
        /** 成功率 */
        private BigDecimal successRate;
        
        /** 平均提现金额 */
        private BigDecimal avgAmount;
        
        /** 每日提现趋势 */
        private List<DailyAmountItem> dailyTrend;
    }
    
    @Data
    @Accessors(chain = true)
    public static class BalanceAnalysis {
        /** 净流入金额(充值-提现) */
        private BigDecimal netInflow;
        
        /** 充提比 */
        private BigDecimal rechargeWithdrawRatio;
        
        /** 资金留存率 */
        private BigDecimal retentionRate;
    }
    
    @Data
    @Accessors(chain = true)
    public static class DailyAmountItem {
        /** 日期 */
        private String date;
        /** 金额 */
        private BigDecimal amount;
        /** 笔数 */
        private Long count;
    }
}
