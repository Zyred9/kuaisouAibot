package com.search.robots.beans.web.statistics;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;
import java.util.Map;

/**
 * 用户增长趋势数据
 * 
 * @author admin
 * @since 2025/01/17
 */
@Data
@Accessors(chain = true)
public class UserGrowthVO {
    
    /** 每日新增用户数 - 日期:数量 */
    private List<DailyStatItem> dailyNewUsers;
    
    /** 用户等级分布 */
    private Map<String, Long> gradeDistribution;
    
    /** 总用户数 */
    private Long totalUsers;
    
    /** 本周新增 */
    private Long weekNewUsers;
    
    /** 本月新增 */
    private Long monthNewUsers;
    
    @Data
    @Accessors(chain = true)
    public static class DailyStatItem {
        /** 日期 */
        private String date;
        /** 数量 */
        private Long count;
    }
}
