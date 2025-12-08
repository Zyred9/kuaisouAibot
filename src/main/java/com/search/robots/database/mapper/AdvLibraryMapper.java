package com.search.robots.database.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.search.robots.database.entity.AdvLibrary;

/**
 * 广告库Mapper
 * <pre>
 * 提供基础CRUD功能
 * 复杂关联查询已迁移至Service层实现,避免XML依赖
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
public interface AdvLibraryMapper extends BaseMapper<AdvLibrary> {
}

