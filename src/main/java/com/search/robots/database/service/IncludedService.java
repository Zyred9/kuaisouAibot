package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.Included;
import com.search.robots.database.enums.Included.IncludedSearchTypeEnum;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface IncludedService extends IService<Included> {

    String buildSelfIndexText();

    Page<Included> selectPage(int current, Long userId, IncludedSearchTypeEnum searchType);
}
