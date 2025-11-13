package com.search.robots.database.service.impl;


import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.database.enums.content.SortEnum;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.database.mapper.SearchRepository;
import com.search.robots.database.service.SearchService;
import lombok.RequiredArgsConstructor;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Elasticsearch 搜索服务实现类
 *
 * @author zyred
 * @since 2025/11/9 20:16
 */
@Component
@RequiredArgsConstructor
public class SearchServiceImpl implements SearchService {

    private static final int PAGE_SIZE = 12;

    private final ElasticsearchRestTemplate elasticsearchTemplate;

    @Override
    public Page<SearchBean> search(String text, SourceTypeEnum type, int current, SortEnum sort) {
        if (StrUtil.isBlank(text)) {
            return Page.empty();
        }

        // 构建布尔查询
        BoolQueryBuilder boolQuery = QueryBuilders.boolQuery();
        
        // 智能分词匹配策略:
        // 1. should子句: 短语精确匹配(权重x3) + 分词匹配(权重x1)
        // 2. minimum_should_match: 至少满足一个条件
        BoolQueryBuilder shouldQuery = QueryBuilders.boolQuery()
                .should(QueryBuilders.matchPhraseQuery("sourceName", text).boost(3.0f))  // 短语匹配优先
                .should(QueryBuilders.matchQuery("sourceName", text).boost(1.0f))        // 允许分词匹配
                .minimumShouldMatch(1);
        
        boolQuery.must(shouldQuery);
        
        // 可选过滤: 按类型筛选
        if (Objects.nonNull(type)) {
            boolQuery.must(QueryBuilders.matchQuery("type", type.name()));
        }

        // 构建分页对象(只按分数降序排序,暂时去掉时间排序)
        Sort orders;
        if (Objects.nonNull(sort) && !Objects.equals(sort, SortEnum.EMPTY)) {
            orders = Sort.by(Sort.Direction.DESC, "_score")
                    .and(Sort.by(Sort.Direction.DESC, sort.getFields()));
        } else {
            orders = Sort.by(Sort.Direction.DESC, "_score")
                    .and(Sort.by(Sort.Direction.DESC, "collectTime"));
        }

        PageRequest pageRequest = PageRequest.of(current, PAGE_SIZE, orders);

        // 构建查询对象
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(boolQuery)
                .withPageable(pageRequest)
                .build();

        // 执行查询
        SearchHits<SearchBean> searchHits = elasticsearchTemplate.search(searchQuery, SearchBean.class);
        
        // 提取结果列表
        List<SearchBean> content = searchHits.getSearchHits().stream()
                .map(SearchHit::getContent)
                .collect(Collectors.toList());

        // 返回分页结果
        return new PageImpl<>(content, pageRequest, searchHits.getTotalHits());
    }
}
