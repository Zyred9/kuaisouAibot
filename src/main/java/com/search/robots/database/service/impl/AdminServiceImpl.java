package com.search.robots.database.service.impl;

import cn.hutool.core.date.DateUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.search.robots.beans.web.statistics.*;
import com.search.robots.database.entity.*;
import com.search.robots.database.enums.AdvertisingGradeEnum;
import com.search.robots.database.enums.WithdrawalStatus;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import com.search.robots.database.mapper.*;
import com.search.robots.database.service.AdminService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 后台统计Service实现类
 * 
 * @author admin
 * @since 2025/01/17
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AdminServiceImpl implements AdminService {
    
    private final UserMapper userMapper;
    private final RechargeMapper rechargeMapper;
    private final WithdrawalsMapper withdrawalsMapper;
    private final AdvUserMapper advUserMapper;
    private final IncludedMapper includedMapper;
    private final HotSearchMapper hotSearchMapper;
    
    @Override
    public DashboardVO getDashboard() {
        DashboardVO dashboard = new DashboardVO();
        
        // 用户统计
        Long totalUsers = userMapper.selectCount(null);
        dashboard.setTotalUsers(totalUsers);
        
        // 今日新增用户
        LocalDateTime todayStart = LocalDateTime.of(LocalDate.now(), LocalTime.MIN);
        Long todayNewUsers = userMapper.selectCount(
                Wrappers.<User>lambdaQuery()
                        .ge(User::getRegisterTime, todayStart)
        );
        dashboard.setTodayNewUsers(todayNewUsers);
        
        // 7天活跃用户（这里简化为7天内注册的用户）
        LocalDateTime sevenDaysAgo = LocalDateTime.now().minusDays(7);
        Long activeUsers7d = userMapper.selectCount(
                Wrappers.<User>lambdaQuery()
                        .ge(User::getRegisterTime, sevenDaysAgo)
        );
        dashboard.setActiveUsers7d(activeUsers7d);
        
        // 30天活跃用户
        LocalDateTime thirtyDaysAgo = LocalDateTime.now().minusDays(30);
        Long activeUsers30d = userMapper.selectCount(
                Wrappers.<User>lambdaQuery()
                        .ge(User::getRegisterTime, thirtyDaysAgo)
        );
        dashboard.setActiveUsers30d(activeUsers30d);
        
        // 总余额
        List<User> allUsers = userMapper.selectList(
                Wrappers.<User>lambdaQuery().select(User::getBalance)
        );
        BigDecimal totalBalance = allUsers.stream()
                .map(User::getBalance)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        dashboard.setTotalBalance(totalBalance);
        
        // 收录资源总数
        Long totalIncluded = includedMapper.selectCount(null);
        dashboard.setTotalIncluded(totalIncluded);
        
        // 今日搜索次数
        Long todaySearchCount = hotSearchMapper.selectCount(
                Wrappers.<HotSearch>lambdaQuery()
                        .eq(HotSearch::getSearchDay, LocalDate.now())
        );
        dashboard.setTodaySearchCount(todaySearchCount);
        
        // 充值统计
        List<Recharge> allRecharges = rechargeMapper.selectList(null);
        BigDecimal totalRecharge = allRecharges.stream()
                .map(r -> new BigDecimal(r.getAmount()))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        dashboard.setTotalRecharge(totalRecharge);
        
        // 今日充值
        List<Recharge> todayRecharges = rechargeMapper.selectList(
                Wrappers.<Recharge>lambdaQuery()
                        .like(Recharge::getTransactionDate, LocalDate.now().toString())
        );
        BigDecimal todayRecharge = todayRecharges.stream()
                .map(r -> new BigDecimal(r.getAmount()))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        dashboard.setTodayRecharge(todayRecharge);
        
        // 提现统计
        List<Withdrawals> allWithdrawals = withdrawalsMapper.selectList(
                Wrappers.<Withdrawals>lambdaQuery()
                        .eq(Withdrawals::getStatus, WithdrawalStatus.SUCCESS)
        );
        BigDecimal totalWithdrawal = allWithdrawals.stream()
                .map(Withdrawals::getWithdrawalAmount)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        dashboard.setTotalWithdrawal(totalWithdrawal);
        
        // 待审批提现
        List<Withdrawals> pendingWithdrawals = withdrawalsMapper.selectList(
                Wrappers.<Withdrawals>lambdaQuery()
                        .eq(Withdrawals::getStatus, WithdrawalStatus.PROCESSING)
        );
        dashboard.setPendingWithdrawals((long) pendingWithdrawals.size());
        BigDecimal pendingAmount = pendingWithdrawals.stream()
                .map(Withdrawals::getWithdrawalAmount)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        dashboard.setPendingWithdrawalAmount(pendingAmount);
        
        // 平台收益（广告收入）
        List<AdvUser> allAdvs = advUserMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .select(AdvUser::getPriceMonth)
        );
        BigDecimal totalRevenue = allAdvs.stream()
                .map(AdvUser::getPriceMonth)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        dashboard.setTotalRevenue(totalRevenue);
        
        // 今日收益
        List<AdvUser> todayAdvs = advUserMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .select(AdvUser::getPriceMonth)
                        .ge(AdvUser::getCreatedAt, todayStart)
        );
        BigDecimal todayRevenue = todayAdvs.stream()
                .map(AdvUser::getPriceMonth)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        dashboard.setTodayRevenue(todayRevenue);
        
        return dashboard;
    }
    
    @Override
    public UserGrowthVO getUserGrowth(Integer days) {
        if (days == null || days <= 0) {
            days = 30;
        }
        
        UserGrowthVO growth = new UserGrowthVO();
        
        // 总用户数
        Long totalUsers = userMapper.selectCount(null);
        growth.setTotalUsers(totalUsers);
        
        // 每日新增用户统计
        List<UserGrowthVO.DailyStatItem> dailyNewUsers = new ArrayList<>();
        LocalDate endDate = LocalDate.now();
        LocalDate startDate = endDate.minusDays(days - 1);
        
        for (LocalDate date = startDate; !date.isAfter(endDate); date = date.plusDays(1)) {
            LocalDateTime dayStart = date.atStartOfDay();
            LocalDateTime dayEnd = date.atTime(LocalTime.MAX);
            
            Long count = userMapper.selectCount(
                    Wrappers.<User>lambdaQuery()
                            .ge(User::getRegisterTime, dayStart)
                            .le(User::getRegisterTime, dayEnd)
            );
            
            UserGrowthVO.DailyStatItem item = new UserGrowthVO.DailyStatItem();
            item.setDate(date.toString());
            item.setCount(count);
            dailyNewUsers.add(item);
        }
        growth.setDailyNewUsers(dailyNewUsers);
        
        // 本周新增
        LocalDateTime weekStart = LocalDateTime.now().minusDays(7);
        Long weekNewUsers = userMapper.selectCount(
                Wrappers.<User>lambdaQuery()
                        .ge(User::getRegisterTime, weekStart)
        );
        growth.setWeekNewUsers(weekNewUsers);
        
        // 本月新增
        LocalDateTime monthStart = LocalDateTime.of(LocalDate.now().withDayOfMonth(1), LocalTime.MIN);
        Long monthNewUsers = userMapper.selectCount(
                Wrappers.<User>lambdaQuery()
                        .ge(User::getRegisterTime, monthStart)
        );
        growth.setMonthNewUsers(monthNewUsers);
        
        // 用户等级分布
        List<User> allUsers = userMapper.selectList(
                Wrappers.<User>lambdaQuery().select(User::getGrade)
        );
        Map<String, Long> gradeDistribution = allUsers.stream()
                .filter(u -> u.getGrade() != null)
                .collect(Collectors.groupingBy(
                        u -> u.getGrade().name(),
                        Collectors.counting()
                ));
        
        // 确保所有等级都有值
        for (AdvertisingGradeEnum grade : AdvertisingGradeEnum.values()) {
            gradeDistribution.putIfAbsent(grade.name(), 0L);
        }
        growth.setGradeDistribution(gradeDistribution);
        
        return growth;
    }
    
    @Override
    public FinanceStatVO getFinanceStats(Integer days) {
        if (days == null || days <= 0) {
            days = 30;
        }
        
        FinanceStatVO financeStats = new FinanceStatVO();
        
        // 充值统计
        FinanceStatVO.RechargeStatistics rechargeStats = buildRechargeStats(days);
        financeStats.setRechargeStats(rechargeStats);
        
        // 提现统计
        FinanceStatVO.WithdrawalStatistics withdrawalStats = buildWithdrawalStats(days);
        financeStats.setWithdrawalStats(withdrawalStats);
        
        // 充提差额分析
        FinanceStatVO.BalanceAnalysis balanceAnalysis = new FinanceStatVO.BalanceAnalysis();
        BigDecimal netInflow = rechargeStats.getTotalAmount().subtract(withdrawalStats.getTotalAmount());
        balanceAnalysis.setNetInflow(netInflow);
        
        // 充提比
        if (withdrawalStats.getTotalAmount().compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal ratio = rechargeStats.getTotalAmount()
                    .divide(withdrawalStats.getTotalAmount(), 2, RoundingMode.HALF_UP);
            balanceAnalysis.setRechargeWithdrawRatio(ratio);
        } else {
            balanceAnalysis.setRechargeWithdrawRatio(BigDecimal.ZERO);
        }
        
        // 资金留存率
        if (rechargeStats.getTotalAmount().compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal retentionRate = netInflow
                    .divide(rechargeStats.getTotalAmount(), 4, RoundingMode.HALF_UP)
                    .multiply(new BigDecimal("100"));
            balanceAnalysis.setRetentionRate(retentionRate);
        } else {
            balanceAnalysis.setRetentionRate(BigDecimal.ZERO);
        }
        
        financeStats.setBalanceAnalysis(balanceAnalysis);
        
        return financeStats;
    }
    
    private FinanceStatVO.RechargeStatistics buildRechargeStats(Integer days) {
        FinanceStatVO.RechargeStatistics stats = new FinanceStatVO.RechargeStatistics();
        
        // 所有充值记录
        List<Recharge> allRecharges = rechargeMapper.selectList(null);
        
        BigDecimal totalAmount = allRecharges.stream()
                .map(r -> new BigDecimal(r.getAmount()))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setTotalAmount(totalAmount);
        stats.setTotalCount((long) allRecharges.size());
        
        // 今日充值
        String today = LocalDate.now().toString();
        List<Recharge> todayRecharges = allRecharges.stream()
                .filter(r -> r.getTransactionDate() != null && r.getTransactionDate().startsWith(today))
                .collect(Collectors.toList());
        
        BigDecimal todayAmount = todayRecharges.stream()
                .map(r -> new BigDecimal(r.getAmount()))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setTodayAmount(todayAmount);
        stats.setTodayCount((long) todayRecharges.size());
        
        // 充值用户数（去重）
        long rechargeUserCount = allRecharges.stream()
                .map(Recharge::getUserId)
                .filter(Objects::nonNull)
                .distinct()
                .count();
        stats.setRechargeUserCount(rechargeUserCount);
        
        // 平均充值金额
        if (stats.getTotalCount() > 0) {
            BigDecimal avgAmount = totalAmount.divide(
                    new BigDecimal(stats.getTotalCount()), 2, RoundingMode.HALF_UP
            );
            stats.setAvgAmount(avgAmount);
        } else {
            stats.setAvgAmount(BigDecimal.ZERO);
        }
        
        // 每日充值趋势
        List<FinanceStatVO.DailyAmountItem> dailyTrend = new ArrayList<>();
        LocalDate endDate = LocalDate.now();
        LocalDate startDate = endDate.minusDays(days - 1);
        
        for (LocalDate date = startDate; !date.isAfter(endDate); date = date.plusDays(1)) {
            String dateStr = date.toString();
            List<Recharge> dayRecharges = allRecharges.stream()
                    .filter(r -> r.getTransactionDate() != null && r.getTransactionDate().startsWith(dateStr))
                    .collect(Collectors.toList());
            
            BigDecimal dayAmount = dayRecharges.stream()
                    .map(r -> new BigDecimal(r.getAmount()))
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            
            FinanceStatVO.DailyAmountItem item = new FinanceStatVO.DailyAmountItem();
            item.setDate(dateStr);
            item.setAmount(dayAmount);
            item.setCount((long) dayRecharges.size());
            dailyTrend.add(item);
        }
        stats.setDailyTrend(dailyTrend);
        
        return stats;
    }
    
    private FinanceStatVO.WithdrawalStatistics buildWithdrawalStats(Integer days) {
        FinanceStatVO.WithdrawalStatistics stats = new FinanceStatVO.WithdrawalStatistics();
        
        // 所有提现记录
        List<Withdrawals> allWithdrawals = withdrawalsMapper.selectList(null);
        
        // 成功的提现
        List<Withdrawals> successWithdrawals = allWithdrawals.stream()
                .filter(w -> w.getStatus() == WithdrawalStatus.SUCCESS)
                .collect(Collectors.toList());
        
        BigDecimal totalAmount = successWithdrawals.stream()
                .map(Withdrawals::getWithdrawalAmount)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setTotalAmount(totalAmount);
        stats.setTotalCount((long) successWithdrawals.size());
        
        // 今日提现
        LocalDate today = LocalDate.now();
        List<Withdrawals> todayWithdrawals = successWithdrawals.stream()
                .filter(w -> w.getTransactionDate() != null && w.getTransactionDate().toLocalDate().equals(today))
                .collect(Collectors.toList());
        
        BigDecimal todayAmount = todayWithdrawals.stream()
                .map(Withdrawals::getWithdrawalAmount)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setTodayAmount(todayAmount);
        stats.setTodayCount((long) todayWithdrawals.size());
        
        // 待审批
        List<Withdrawals> pendingWithdrawals = allWithdrawals.stream()
                .filter(w -> w.getStatus() == WithdrawalStatus.PROCESSING)
                .collect(Collectors.toList());
        
        stats.setPendingCount((long) pendingWithdrawals.size());
        BigDecimal pendingAmount = pendingWithdrawals.stream()
                .map(Withdrawals::getWithdrawalAmount)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setPendingAmount(pendingAmount);
        
        // 成功率
        if (!allWithdrawals.isEmpty()) {
            BigDecimal successRate = new BigDecimal(successWithdrawals.size())
                    .divide(new BigDecimal(allWithdrawals.size()), 4, RoundingMode.HALF_UP)
                    .multiply(new BigDecimal("100"));
            stats.setSuccessRate(successRate);
        } else {
            stats.setSuccessRate(BigDecimal.ZERO);
        }
        
        // 平均提现金额
        if (stats.getTotalCount() > 0) {
            BigDecimal avgAmount = totalAmount.divide(
                    new BigDecimal(stats.getTotalCount()), 2, RoundingMode.HALF_UP
            );
            stats.setAvgAmount(avgAmount);
        } else {
            stats.setAvgAmount(BigDecimal.ZERO);
        }
        
        // 每日提现趋势
        List<FinanceStatVO.DailyAmountItem> dailyTrend = new ArrayList<>();
        LocalDate endDate = LocalDate.now();
        LocalDate startDate = endDate.minusDays(days - 1);
        
        for (LocalDate date = startDate; !date.isAfter(endDate); date = date.plusDays(1)) {
            LocalDate finalDate = date;
            List<Withdrawals> dayWithdrawals = successWithdrawals.stream()
                    .filter(w -> w.getTransactionDate() != null && w.getTransactionDate().toLocalDate().equals(finalDate))
                    .collect(Collectors.toList());
            
            BigDecimal dayAmount = dayWithdrawals.stream()
                    .map(Withdrawals::getWithdrawalAmount)
                    .filter(Objects::nonNull)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            
            FinanceStatVO.DailyAmountItem item = new FinanceStatVO.DailyAmountItem();
            item.setDate(date.toString());
            item.setAmount(dayAmount);
            item.setCount((long) dayWithdrawals.size());
            dailyTrend.add(item);
        }
        stats.setDailyTrend(dailyTrend);
        
        return stats;
    }
    
    @Override
    public AdvStatVO getAdvStats(Integer days) {
        if (days == null || days <= 0) {
            days = 30;
        }
        
        AdvStatVO advStats = new AdvStatVO();
        
        // 广告总览
        AdvStatVO.AdvOverview overview = buildAdvOverview();
        advStats.setOverview(overview);
        
        // 广告类型统计
        Map<AdvTypeEnum, AdvStatVO.AdvTypeStatistics> typeStats = buildAdvTypeStats();
        advStats.setTypeStats(typeStats);
        
        // 广告效果分析
        AdvStatVO.AdvEffectAnalysis effectAnalysis = buildAdvEffectAnalysis();
        advStats.setEffectAnalysis(effectAnalysis);
        
        // 广告收入统计
        AdvStatVO.AdvRevenueStatistics revenueStats = buildAdvRevenueStats(days);
        advStats.setRevenueStats(revenueStats);
        
        return advStats;
    }
    
    private AdvStatVO.AdvOverview buildAdvOverview() {
        AdvStatVO.AdvOverview overview = new AdvStatVO.AdvOverview();
        
        Long totalCount = advUserMapper.selectCount(null);
        overview.setTotalCount(totalCount);
        
        Long activeCount = advUserMapper.selectCount(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
        );
        overview.setActiveCount(activeCount);
        
        Long endedCount = advUserMapper.selectCount(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getAdvStatus, AdvStatus.THE_END)
        );
        overview.setEndedCount(endedCount);
        
        Long pendingCount = advUserMapper.selectCount(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getAdvStatus, AdvStatus.UNDER_APPROVAL)
        );
        overview.setPendingCount(pendingCount);
        
        List<AdvUser> allAdvs = advUserMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery().select(AdvUser::getPriceMonth)
        );
        BigDecimal totalRevenue = allAdvs.stream()
                .map(AdvUser::getPriceMonth)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        overview.setTotalRevenue(totalRevenue);
        
        return overview;
    }
    
    private Map<AdvTypeEnum, AdvStatVO.AdvTypeStatistics> buildAdvTypeStats() {
        Map<AdvTypeEnum, AdvStatVO.AdvTypeStatistics> typeStatsMap = new HashMap<>();
        
        List<AdvUser> allAdvs = advUserMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .select(AdvUser::getAdvType, AdvUser::getPriceMonth, AdvUser::getShowCount)
        );
        
        Map<AdvTypeEnum, List<AdvUser>> grouped = allAdvs.stream()
                .filter(a -> a.getAdvType() != null)
                .collect(Collectors.groupingBy(AdvUser::getAdvType));
        
        for (Map.Entry<AdvTypeEnum, List<AdvUser>> entry : grouped.entrySet()) {
            AdvTypeEnum type = entry.getKey();
            List<AdvUser> advs = entry.getValue();
            
            AdvStatVO.AdvTypeStatistics stats = new AdvStatVO.AdvTypeStatistics();
            stats.setAdvType(type);
            stats.setCount((long) advs.size());
            
            BigDecimal revenue = advs.stream()
                    .map(AdvUser::getPriceMonth)
                    .filter(Objects::nonNull)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            stats.setRevenue(revenue);
            
            Long showCount = advs.stream()
                    .map(AdvUser::getShowCount)
                    .filter(Objects::nonNull)
                    .reduce(0L, Long::sum);
            stats.setShowCount(showCount);
            
            typeStatsMap.put(type, stats);
        }
        
        // 确保所有类型都有统计
        for (AdvTypeEnum type : AdvTypeEnum.values()) {
            typeStatsMap.putIfAbsent(type, new AdvStatVO.AdvTypeStatistics()
                    .setAdvType(type)
                    .setCount(0L)
                    .setRevenue(BigDecimal.ZERO)
                    .setShowCount(0L));
        }
        
        return typeStatsMap;
    }
    
    private AdvStatVO.AdvEffectAnalysis buildAdvEffectAnalysis() {
        AdvStatVO.AdvEffectAnalysis analysis = new AdvStatVO.AdvEffectAnalysis();
        
        List<AdvUser> allAdvs = advUserMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .select(AdvUser::getShowCount, AdvUser::getKeyword, AdvUser::getExpirationTime)
        );
        
        // 总展示次数
        Long totalShowCount = allAdvs.stream()
                .map(AdvUser::getShowCount)
                .filter(Objects::nonNull)
                .reduce(0L, Long::sum);
        analysis.setTotalShowCount(totalShowCount);
        
        // 平均展示次数
        if (!allAdvs.isEmpty()) {
            Long avgShowCount = totalShowCount / allAdvs.size();
            analysis.setAvgShowCount(avgShowCount);
        } else {
            analysis.setAvgShowCount(0L);
        }
        
        // 热门关键词TOP10
        Map<String, Long> keywordCount = allAdvs.stream()
                .filter(a -> a.getKeyword() != null && !a.getKeyword().isEmpty())
                .collect(Collectors.groupingBy(AdvUser::getKeyword, Collectors.counting()));
        
        List<AdvStatVO.KeywordRank> topKeywords = keywordCount.entrySet().stream()
                .sorted(Map.Entry.<String, Long>comparingByValue().reversed())
                .limit(10)
                .map(e -> new AdvStatVO.KeywordRank()
                        .setKeyword(e.getKey())
                        .setPurchaseCount(e.getValue()))
                .collect(Collectors.toList());
        analysis.setTopKeywords(topKeywords);
        
        // 7天内即将到期的广告
        LocalDateTime sevenDaysLater = LocalDateTime.now().plusDays(7);
        Long expiringSoonCount = allAdvs.stream()
                .filter(a -> a.getExpirationTime() != null 
                        && a.getExpirationTime().isBefore(sevenDaysLater)
                        && a.getExpirationTime().isAfter(LocalDateTime.now()))
                .count();
        analysis.setExpiringSoonCount(expiringSoonCount);
        
        return analysis;
    }
    
    private AdvStatVO.AdvRevenueStatistics buildAdvRevenueStats(Integer days) {
        AdvStatVO.AdvRevenueStatistics stats = new AdvStatVO.AdvRevenueStatistics();
        
        List<AdvUser> allAdvs = advUserMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .select(AdvUser::getPriceMonth, AdvUser::getAdvType, AdvUser::getCreatedAt)
        );
        
        // 总收入
        BigDecimal totalRevenue = allAdvs.stream()
                .map(AdvUser::getPriceMonth)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setTotalRevenue(totalRevenue);
        
        // 今日收入
        LocalDateTime todayStart = LocalDateTime.of(LocalDate.now(), LocalTime.MIN);
        BigDecimal todayRevenue = allAdvs.stream()
                .filter(a -> a.getCreatedAt() != null && a.getCreatedAt().isAfter(todayStart))
                .map(AdvUser::getPriceMonth)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setTodayRevenue(todayRevenue);
        
        // 本周收入
        LocalDateTime weekStart = LocalDateTime.now().minusDays(7);
        BigDecimal weekRevenue = allAdvs.stream()
                .filter(a -> a.getCreatedAt() != null && a.getCreatedAt().isAfter(weekStart))
                .map(AdvUser::getPriceMonth)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setWeekRevenue(weekRevenue);
        
        // 本月收入
        LocalDateTime monthStart = LocalDateTime.of(LocalDate.now().withDayOfMonth(1), LocalTime.MIN);
        BigDecimal monthRevenue = allAdvs.stream()
                .filter(a -> a.getCreatedAt() != null && a.getCreatedAt().isAfter(monthStart))
                .map(AdvUser::getPriceMonth)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        stats.setMonthRevenue(monthRevenue);
        
        // 按类型收入分布
        Map<AdvTypeEnum, BigDecimal> revenueByType = allAdvs.stream()
                .filter(a -> a.getAdvType() != null && a.getPriceMonth() != null)
                .collect(Collectors.groupingBy(
                        AdvUser::getAdvType,
                        Collectors.reducing(BigDecimal.ZERO, AdvUser::getPriceMonth, BigDecimal::add)
                ));
        stats.setRevenueByType(revenueByType);
        
        // 每日收入趋势
        List<AdvStatVO.DailyRevenueItem> dailyTrend = new ArrayList<>();
        LocalDate endDate = LocalDate.now();
        LocalDate startDate = endDate.minusDays(days - 1);
        
        for (LocalDate date = startDate; !date.isAfter(endDate); date = date.plusDays(1)) {
            LocalDateTime dayStart = date.atStartOfDay();
            LocalDateTime dayEnd = date.atTime(LocalTime.MAX);
            
            BigDecimal dayRevenue = allAdvs.stream()
                    .filter(a -> a.getCreatedAt() != null 
                            && a.getCreatedAt().isAfter(dayStart)
                            && a.getCreatedAt().isBefore(dayEnd))
                    .map(AdvUser::getPriceMonth)
                    .filter(Objects::nonNull)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            
            AdvStatVO.DailyRevenueItem item = new AdvStatVO.DailyRevenueItem();
            item.setDate(date.toString());
            item.setRevenue(dayRevenue);
            dailyTrend.add(item);
        }
        stats.setDailyTrend(dailyTrend);
        
        return stats;
    }
}
