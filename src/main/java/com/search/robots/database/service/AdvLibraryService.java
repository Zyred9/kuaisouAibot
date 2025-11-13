package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.AdvLibrary;

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

    AdvLibrary buildDefault(String keyword, String data);
    AdvLibrary getByKeywordWithPrices(String keyword, String data);
    AdvLibrary getByIdWithPrices(Long libraryId);

}
