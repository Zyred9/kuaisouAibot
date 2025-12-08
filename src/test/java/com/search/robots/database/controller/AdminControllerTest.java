package com.search.robots.database.controller;

import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.web.statistics.*;
import com.search.robots.database.service.AdminService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.*;

/**
 * AdminController单元测试
 * 
 * @author admin
 * @since 2025/01/17
 */
@DisplayName("后台统计接口测试")
class AdminControllerTest {
    
    @Mock
    private AdminService adminService;
    
    @InjectMocks
    private AdminController adminController;
    
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }
    
    @Test
    @DisplayName("测试获取Dashboard数据 - 成功")
    void testGetDashboard_Success() {
        // 准备测试数据
        DashboardVO mockDashboard = new DashboardVO()
                .setTotalUsers(1000L)
                .setTodayNewUsers(50L)
                .setTotalBalance(new BigDecimal("10000.00"))
                .setTotalRecharge(new BigDecimal("50000.00"))
                .setTotalWithdrawal(new BigDecimal("30000.00"));
        
        when(adminService.getDashboard()).thenReturn(mockDashboard);
        
        // 执行测试
        Result<DashboardVO> result = adminController.getDashboard();
        
        // 验证结果
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertEquals(200, result.getCode());
        assertNotNull(result.getData());
        assertEquals(1000L, result.getData().getTotalUsers());
        assertEquals(50L, result.getData().getTodayNewUsers());
        
        // 验证方法调用
        verify(adminService, times(1)).getDashboard();
    }
    
    @Test
    @DisplayName("测试获取Dashboard数据 - 异常处理")
    void testGetDashboard_Exception() {
        // 模拟异常
        when(adminService.getDashboard()).thenThrow(new RuntimeException("数据库连接失败"));
        
        // 执行测试
        Result<DashboardVO> result = adminController.getDashboard();
        
        // 验证结果
        assertNotNull(result);
        assertFalse(result.isSuccess());
        assertEquals(500, result.getCode());
        assertTrue(result.getMessage().contains("失败"));
    }
    
    @Test
    @DisplayName("测试获取用户增长统计 - 默认30天")
    void testGetUserGrowth_Default() {
        // 准备测试数据
        UserGrowthVO mockGrowth = new UserGrowthVO()
                .setTotalUsers(1000L)
                .setWeekNewUsers(70L)
                .setMonthNewUsers(300L)
                .setDailyNewUsers(new ArrayList<>())
                .setGradeDistribution(new HashMap<>());
        
        when(adminService.getUserGrowth(30)).thenReturn(mockGrowth);
        
        // 执行测试
        Result<UserGrowthVO> result = adminController.getUserGrowth(30);
        
        // 验证结果
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertEquals(1000L, result.getData().getTotalUsers());
        assertEquals(70L, result.getData().getWeekNewUsers());
        
        verify(adminService, times(1)).getUserGrowth(30);
    }
    
    @Test
    @DisplayName("测试获取用户增长统计 - 自定义天数")
    void testGetUserGrowth_CustomDays() {
        // 准备测试数据
        UserGrowthVO mockGrowth = new UserGrowthVO()
                .setTotalUsers(1000L)
                .setDailyNewUsers(new ArrayList<>())
                .setGradeDistribution(new HashMap<>());
        
        when(adminService.getUserGrowth(7)).thenReturn(mockGrowth);
        
        // 执行测试
        Result<UserGrowthVO> result = adminController.getUserGrowth(7);
        
        // 验证结果
        assertNotNull(result);
        assertTrue(result.isSuccess());
        
        verify(adminService, times(1)).getUserGrowth(7);
    }
    
    @Test
    @DisplayName("测试获取财务统计 - 成功")
    void testGetFinanceStats_Success() {
        // 准备测试数据
        FinanceStatVO.RechargeStatistics rechargeStats = new FinanceStatVO.RechargeStatistics()
                .setTotalAmount(new BigDecimal("100000.00"))
                .setTotalCount(500L);
        
        FinanceStatVO.WithdrawalStatistics withdrawalStats = new FinanceStatVO.WithdrawalStatistics()
                .setTotalAmount(new BigDecimal("60000.00"))
                .setTotalCount(300L);
        
        FinanceStatVO.BalanceAnalysis balanceAnalysis = new FinanceStatVO.BalanceAnalysis()
                .setNetInflow(new BigDecimal("40000.00"))
                .setRechargeWithdrawRatio(new BigDecimal("1.67"));
        
        FinanceStatVO mockStats = new FinanceStatVO()
                .setRechargeStats(rechargeStats)
                .setWithdrawalStats(withdrawalStats)
                .setBalanceAnalysis(balanceAnalysis);
        
        when(adminService.getFinanceStats(30)).thenReturn(mockStats);
        
        // 执行测试
        Result<FinanceStatVO> result = adminController.getFinanceStats(30);
        
        // 验证结果
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getData().getRechargeStats());
        assertNotNull(result.getData().getWithdrawalStats());
        assertNotNull(result.getData().getBalanceAnalysis());
        
        verify(adminService, times(1)).getFinanceStats(30);
    }
    
    @Test
    @DisplayName("测试获取广告统计 - 成功")
    void testGetAdvStats_Success() {
        // 准备测试数据
        AdvStatVO.AdvOverview overview = new AdvStatVO.AdvOverview()
                .setTotalCount(200L)
                .setActiveCount(150L)
                .setEndedCount(40L)
                .setPendingCount(10L)
                .setTotalRevenue(new BigDecimal("50000.00"));
        
        AdvStatVO mockStats = new AdvStatVO()
                .setOverview(overview)
                .setTypeStats(new HashMap<>())
                .setEffectAnalysis(new AdvStatVO.AdvEffectAnalysis())
                .setRevenueStats(new AdvStatVO.AdvRevenueStatistics());
        
        when(adminService.getAdvStats(30)).thenReturn(mockStats);
        
        // 执行测试
        Result<AdvStatVO> result = adminController.getAdvStats(30);
        
        // 验证结果
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getData().getOverview());
        assertEquals(200L, result.getData().getOverview().getTotalCount());
        assertEquals(150L, result.getData().getOverview().getActiveCount());
        
        verify(adminService, times(1)).getAdvStats(30);
    }
    
    @Test
    @DisplayName("测试获取财务统计 - 异常处理")
    void testGetFinanceStats_Exception() {
        // 模拟异常
        when(adminService.getFinanceStats(anyInt())).thenThrow(new RuntimeException("查询失败"));
        
        // 执行测试
        Result<FinanceStatVO> result = adminController.getFinanceStats(30);
        
        // 验证结果
        assertNotNull(result);
        assertFalse(result.isSuccess());
        assertEquals(500, result.getCode());
    }
    
    @Test
    @DisplayName("测试获取广告统计 - 异常处理")
    void testGetAdvStats_Exception() {
        // 模拟异常
        when(adminService.getAdvStats(anyInt())).thenThrow(new RuntimeException("查询失败"));
        
        // 执行测试
        Result<AdvStatVO> result = adminController.getAdvStats(30);
        
        // 验证结果
        assertNotNull(result);
        assertFalse(result.isSuccess());
        assertEquals(500, result.getCode());
    }
}
