package com.search.robots.database.controller;

import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.web.statistics.AdvStatVO;
import com.search.robots.beans.web.statistics.DashboardVO;
import com.search.robots.beans.web.statistics.FinanceStatVO;
import com.search.robots.beans.web.statistics.UserGrowthVO;
import com.search.robots.database.service.AdminService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;


/**
 * 后台统计控制器
 * 
 * @author admin
 * @since 2025/01/17
 */
@Slf4j
@RestController
@RequestMapping("/admin")
@RequiredArgsConstructor
public class AdminController {

    private final AdminService adminService;

    /**
     * 获取Dashboard核心指标数据
     * 
     * @return Dashboard数据
     */
    @GetMapping("/dashboard")
    public Result<DashboardVO> getDashboard() {
        try {
            DashboardVO dashboard = adminService.getDashboard();
            return Result.success(dashboard);
        } catch (Exception e) {
            log.error("获取Dashboard数据失败", e);
            return Result.error("获取Dashboard数据失败: " + e.getMessage());
        }
    }

    /**
     * 获取用户增长趋势统计
     * 
     * @param days 统计天数(默认30天)
     * @return 用户增长数据
     */
    @GetMapping("/user/growth")
    public Result<UserGrowthVO> getUserGrowth(@RequestParam(defaultValue = "30") Integer days) {
        try {
            UserGrowthVO growth = adminService.getUserGrowth(days);
            return Result.success(growth);
        } catch (Exception e) {
            log.error("获取用户增长统计失败", e);
            return Result.error("获取用户增长统计失败: " + e.getMessage());
        }
    }

    /**
     * 获取财务收支统计
     * 
     * @param days 统计天数(默认30天)
     * @return 财务统计数据
     */
    @GetMapping("/finance/stats")
    public Result<FinanceStatVO> getFinanceStats(@RequestParam(defaultValue = "30") Integer days) {
        try {
            FinanceStatVO stats = adminService.getFinanceStats(days);
            return Result.success(stats);
        } catch (Exception e) {
            log.error("获取财务统计失败", e);
            return Result.error("获取财务统计失败: " + e.getMessage());
        }
    }

    /**
     * 获取广告投放效果统计
     * 
     * @param days 统计天数(默认30天)
     * @return 广告统计数据
     */
    @GetMapping("/adv/stats")
    public Result<AdvStatVO> getAdvStats(@RequestParam(defaultValue = "30") Integer days) {
        try {
            AdvStatVO stats = adminService.getAdvStats(days);
            return Result.success(stats);
        } catch (Exception e) {
            log.error("获取广告统计失败", e);
            return Result.error("获取广告统计失败: " + e.getMessage());
        }
    }
}
