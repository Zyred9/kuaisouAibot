package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.HotSearch;
import com.search.robots.database.enums.SearchPeriodEnum;

import java.util.List;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface HotSearchService extends IService<HotSearch> {

    void search(String keyword);

    List<HotSearch> keywords (SearchPeriodEnum hit);

    String hottest();
}
