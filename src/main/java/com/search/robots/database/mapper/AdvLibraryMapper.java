package com.search.robots.database.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.search.robots.database.entity.AdvLibrary;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 广告库Mapper
 * <pre>
 * 提供基础CRUD及关联查询功能
 * 支持一次性查询关键词及价格配置,避免N+1查询
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
public interface AdvLibraryMapper extends BaseMapper<AdvLibrary> {

    /**
     * 根据关键词查询广告库及其所有价格配置(一次性关联查询)
     * <pre>
     * 使用collection映射一次性获取完整数据:
     * - 广告库基础信息
     * - priceList填充所有启用的价格配置
     * 
     * 查询效率: O(1) - 单次SQL查询
     * </pre>
     *
     * @param keyword 关键词
     * @return 广告库实体(priceList已填充),不存在返回null
     */
    AdvLibrary selectByKeywordWithPrices(@Param("keyword") String keyword);

    /**
     * 批量查询多个关键词及其价格配置(一次性关联查询)
     * <pre>
     * 支持批量查询场景:
     * - 热门关键词列表
     * - 搜索结果聚合
     * 
     * 查询效率: O(1) - 单次SQL IN查询
     * </pre>
     *
     * @param keywords 关键词列表
     * @return 广告库实体列表(每个priceList已填充)
     */
    List<AdvLibrary> selectByKeywordsWithPrices(@Param("keywords") List<String> keywords);

    /**
     * 查询热门关键词及其价格配置(一次性关联查询)
     * <pre>
     * 按展现次数排序,返回Top N热门关键词
     * 同时填充priceList
     * </pre>
     *
     * @param limit 返回数量限制
     * @return 广告库实体列表(每个priceList已填充)
     */
    List<AdvLibrary> selectHotKeywordsWithPrices(@Param("limit") int limit);

}

