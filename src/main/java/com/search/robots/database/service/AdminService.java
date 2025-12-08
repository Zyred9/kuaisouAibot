package com.search.robots.database.service;

import com.search.robots.beans.web.statistics.AdvStatVO;
import com.search.robots.beans.web.statistics.DashboardVO;
import com.search.robots.beans.web.statistics.FinanceStatVO;
import com.search.robots.beans.web.statistics.UserGrowthVO;

/**
 * 后台统计Service接口
 * 
 * @author admin
 * @since 2025/01/17
 */
public interface AdminService {
    
    /**
     * 获取Dashboard核心指标数据
     * 
     * @return Dashboard数据
     */
    DashboardVO getDashboard();
    
    /**
     * 获取用户增长趋势统计
     * 
     * @param days 统计天数(默认30天)
     * @return 用户增长数据
     */
    UserGrowthVO getUserGrowth(Integer days);
    
    /**
     * 获取财务收支统计
     * 
     * @param days 统计天数(默认30天)
     * @return 财务统计数据
     */
    FinanceStatVO getFinanceStats(Integer days);
    
    /**
     * 获取广告投放效果统计
     * 
     * @param days 统计天数(默认30天)
     * @return 广告统计数据
     */
    AdvStatVO getAdvStats(Integer days);
}
