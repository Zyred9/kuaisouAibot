package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.HotSearch;
import com.search.robots.database.enums.SearchPeriodEnum;
import com.search.robots.database.mapper.HotSearchMapper;
import com.search.robots.database.service.HotSearchService;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Service
public class HotSearchServiceImpl extends ServiceImpl<HotSearchMapper, HotSearch> implements HotSearchService {

    @Override
    public void search(String keyword) {
        List<HotSearch> searches = this.baseMapper.selectList(
                Wrappers.<HotSearch>lambdaQuery()
                        .eq(HotSearch::getKeyword, keyword)
                        .eq(HotSearch::getSearchDay, LocalDate.now())
        );

        if (CollUtil.isEmpty(searches)) {
            this.baseMapper.insert(HotSearch.buildSearch(keyword));
            return;
        }

        // 只有一个
        if (searches.size() == 1) {
            HotSearch hotSearch = searches.get(0);
            hotSearch.setSearchCount(hotSearch.getSearchCount() + 1);
            this.baseMapper.updateById(hotSearch);
            return;
        }

        // 有多个的情况
        int sum = searches.stream().map(HotSearch::getSearchCount)
                .mapToInt(Integer::intValue).sum();
        HotSearch update = searches.remove(0);
        update.setSearchCount(sum);
        this.baseMapper.updateById(update);

        Set<Long> ids = searches.stream().map(HotSearch::getId)
                .collect(Collectors.toSet());
        this.baseMapper.deleteBatchIds(ids);
    }

    @Override
    public List<HotSearch> keywords(SearchPeriodEnum hit) {
        // hit: 1.当天 2.昨天 3.近三天 4.仅七天 5.当月
        final int LIMIT_COUNT = 40;
        LocalDate endDate = LocalDate.now();
        LocalDate startDate;
        switch (hit) {
            case DAILY: // 当天
                startDate = endDate;
                break;
            case YESTERDAY: // 昨天
                startDate = endDate.minusDays(1);
                endDate = startDate; // 昨天，开始和结束都是昨天
                break;
            case LAST_3_DAYS: // 近三天 (D-2 到 D)
                startDate = endDate.minusDays(2);
                break;
            case LAST_7_DAYS: // 近七天 (D-6 到 D)
                startDate = endDate.minusDays(6);
                break;
            case MONTHLY: // 当月 (本月第一天到今天)
                startDate = endDate.with(TemporalAdjusters.firstDayOfMonth());
                break;
            default:
                // 如果传入的 hit 参数无效，返回空列表或抛出异常
                return Collections.emptyList();
        }
        List<HotSearch> search = this.baseMapper.keywords(startDate, endDate, LIMIT_COUNT);
        return CollUtil.isEmpty(search) ? Collections.emptyList() : search;
    }
}
