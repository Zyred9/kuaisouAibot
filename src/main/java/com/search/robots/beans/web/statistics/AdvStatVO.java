package com.search.robots.beans.web.statistics;

import com.search.robots.database.enums.adv.AdvTypeEnum;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * 广告投放效果统计数据
 * 
 * @author zyred
 * @since 2025/01/17
 */
@Data
@Accessors(chain = true)
public class AdvStatVO {
    
    /** 广告总览 */
    private AdvOverview overview;
    
    /** 广告类型统计 */
    private Map<AdvTypeEnum, AdvTypeStatistics> typeStats;
    
    /** 广告效果分析 */
    private AdvEffectAnalysis effectAnalysis;
    
    /** 广告收入统计 */
    private AdvRevenueStatistics revenueStats;
    
    @Data
    @Accessors(chain = true)
    public static class AdvOverview {
        /** 广告总数 */
        private Long totalCount;
        
        /** 活跃广告数 */
        private Long activeCount;
        
        /** 已结束广告数 */
        private Long endedCount;
        
        /** 待审批广告数 */
        private Long pendingCount;
        
        /** 广告总收入 */
        private BigDecimal totalRevenue;
    }
    
    @Data
    @Accessors(chain = true)
    public static class AdvTypeStatistics {
        /** 广告类型 */
        private AdvTypeEnum advType;
        
        /** 数量 */
        private Long count;
        
        /** 收入 */
        private BigDecimal revenue;
        
        /** 展示次数 */
        private Long showCount;
    }
    
    @Data
    @Accessors(chain = true)
    public static class AdvEffectAnalysis {
        /** 广告展示总量 */
        private Long totalShowCount;
        
        /** 平均展示次数 */
        private Long avgShowCount;
        
        /** 热门关键词TOP10 */
        private List<KeywordRank> topKeywords;
        
        /** 7天内即将到期广告数量 */
        private Long expiringSoonCount;
    }
    
    @Data
    @Accessors(chain = true)
    public static class AdvRevenueStatistics {
        /** 总收入 */
        private BigDecimal totalRevenue;
        
        /** 今日收入 */
        private BigDecimal todayRevenue;
        
        /** 本周收入 */
        private BigDecimal weekRevenue;
        
        /** 本月收入 */
        private BigDecimal monthRevenue;
        
        /** 按类型收入分布 */
        private Map<AdvTypeEnum, BigDecimal> revenueByType;
        
        /** 每日收入趋势 */
        private List<DailyRevenueItem> dailyTrend;
    }
    
    @Data
    @Accessors(chain = true)
    public static class KeywordRank {
        /** 关键词 */
        private String keyword;
        
        /** 购买次数 */
        private Long purchaseCount;
    }
    
    @Data
    @Accessors(chain = true)
    public static class DailyRevenueItem {
        /** 日期 */
        private String date;
        
        /** 收入 */
        private BigDecimal revenue;
    }
}
