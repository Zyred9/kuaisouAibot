package com.search.robots.database.service;


import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.database.enums.content.SortEnum;
import com.search.robots.database.enums.content.SourceTypeEnum;
import org.springframework.data.domain.Page;

import java.util.List;

/**
 * Elasticsearch 搜索服务接口
 *
 * @author zyred
 * @since 2025/11/9 20:15
 */
public interface SearchService {

    /**
     * 根据关键词和类型搜索资源
     *
     * @param text    搜索关键词(匹配sourceName字段)
     * @param type    资源类型(null表示不限类型)
     * @param current 当前页码(从0开始)
     * @param chatIds 目标群组id
     * @param filter  过滤被标记的
     * @return 分页搜索结果(按collectTime降序)
     */
    Page<SearchBean> search(String text, SourceTypeEnum type, int current, SortEnum sort, List<Long> chatIds, Boolean filter);

    long countSource(List<Long> chatIds);
}
