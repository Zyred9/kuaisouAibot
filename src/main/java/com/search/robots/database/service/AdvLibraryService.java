package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.AdvLibrary;
import com.search.robots.database.enums.adv.AdvTypeEnum;

import java.util.List;

/**
 * 广告库Service
 * <pre>
 * 提供关键词广告的查询和管理功能
 * 支持一次性关联查询,避免N+1查询问题
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
public interface AdvLibraryService extends IService<AdvLibrary> {

    AdvLibrary buildDefault(String keyword, String data);
    AdvLibrary getByKeywordWithPrices(String keyword, String data);
    AdvLibrary getByIdWithPrices(Long libraryId);


    /**
     * 处理客户端搜索对关键词的 7 天搜索记录处理
     *
     * @param keyword 关键词
     */
    void search(String keyword);

    /**
     * 新增广告库
     *
     * @param advLibrary 广告库实体
     */
    void addAdvLibrary(AdvLibrary advLibrary);

    /**
     * 编辑广告库
     *
     * @param advLibrary 广告库实体
     */
    void updateAdvLibrary(AdvLibrary advLibrary);

    /**
     * 删除广告库
     *
     * @param id 广告库ID
     */
    void removeAdvLibrary(Long id);

    List<AdvLibrary> selectLibraries(AdvTypeEnum data, int limit);
}
