package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.AdvLibrary;
import com.search.robots.database.entity.AdvPrice;

import java.util.List;

/**
 * 广告库Service
 * <pre>
 * 提供关键词广告的查询和管理功能
 * 支持一次性关联查询,避免N+1查询问题
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
public interface AdvLibraryService extends IService<AdvLibrary> {

    /**
     * 根据关键词查询广告库(仅基础信息)
     * <pre>
     * 仅返回广告库实体,不包含价格配置
     * 适用于只需要基础信息的场景
     * </pre>
     */
    AdvLibrary getByKeyword(String keyword);

    /**
     * 根据关键词查询广告库及其所有价格配置(推荐使用)
     * <pre>
     * 一次性查询返回:
     * - 广告库基础信息
     * - priceList填充所有启用的价格配置
     *
     * 查询效率: 单次SQL,无N+1问题
     *
     * 使用示例:
     * AdvLibrary library = advLibraryService.getByKeywordWithPrices("Java");
     * if (Objects.nonNull(library)) {
     *     List<AdvPrice> prices = library.getPriceList();
     *     // 业务逻辑...
     * }
     * </pre>
     *
     * @param keyword 关键词
     * @param data
     * @return 广告库实体(priceList已填充), 不存在返回null
     */
    AdvLibrary getByKeywordWithPrices(String keyword, String data);

    /**
     * 批量查询多个关键词及其价格配置
     * <pre>
     * 支持批量查询场景:
     * - 热门关键词列表展示
     * - 搜索结果聚合
     * 
     * 查询效率: 单次SQL IN查询
     * </pre>
     *
     * @param keywords 关键词列表
     * @return 广告库实体列表(每个priceList已填充)
     */
    List<AdvLibrary> getByKeywordsWithPrices(List<String> keywords);

    /**
     * 获取热门关键词列表(仅基础信息)
     */
    List<AdvLibrary> getHotKeywords(int limit);

    /**
     * 获取热门关键词及其价格配置(推荐使用)
     * <pre>
     * 一次性查询返回:
     * - Top N热门关键词
     * - 每个关键词的priceList填充价格配置
     * 
     * 适用于首页推荐、热门榜单等场景
     * </pre>
     *
     * @param limit 返回数量限制(最大100)
     * @return 广告库实体列表(每个priceList已填充)
     */
    List<AdvLibrary> getHotKeywordsWithPrices(int limit);

    /**
     * 根据广告库ID查询价格配置列表
     * <pre>
     * 仅返回价格配置,不包含广告库信息
     * 适用于已有library对象,只需补充价格信息的场景
     * </pre>
     *
     * @param libraryId 广告库ID
     * @return 价格配置列表
     */
    List<AdvPrice> getPriceListByLibraryId(Long libraryId);
}
