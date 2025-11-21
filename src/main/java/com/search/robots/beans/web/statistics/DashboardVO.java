package com.search.robots.beans.web.statistics;

import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * Dashboard核心指标数据
 * 
 * @author zyred
 * @since 2025/01/17
 */
@Data
@Accessors(chain = true)
public class DashboardVO {
    
    /** 用户总数 */
    private Long totalUsers;
    
    /** 今日新增用户 */
    private Long todayNewUsers;
    
    /** 7天活跃用户 */
    private Long activeUsers7d;
    
    /** 30天活跃用户 */
    private Long activeUsers30d;
    
    /** 总余额 */
    private BigDecimal totalBalance;
    
    /** 今日搜索次数 */
    private Long todaySearchCount;
    
    /** 收录资源总数 */
    private Long totalIncluded;
    
    /** 总充值金额 */
    private BigDecimal totalRecharge;
    
    /** 今日充值金额 */
    private BigDecimal todayRecharge;
    
    /** 总提现金额 */
    private BigDecimal totalWithdrawal;
    
    /** 待审批提现数量 */
    private Long pendingWithdrawals;
    
    /** 待审批提现金额 */
    private BigDecimal pendingWithdrawalAmount;
    
    /** 平台总收益 */
    private BigDecimal totalRevenue;
    
    /** 今日收益 */
    private BigDecimal todayRevenue;
}
