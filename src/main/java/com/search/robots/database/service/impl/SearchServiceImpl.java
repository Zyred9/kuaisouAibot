package com.search.robots.database.service.impl;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.database.enums.content.SortEnum;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.database.mapper.SearchRepository;
import com.search.robots.database.service.SearchService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Elasticsearch 搜索服务实现类
 *
 * @author zyred
 * @since 2025/11/9 20:16
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class SearchServiceImpl implements SearchService {

    private static final int PAGE_SIZE = 12;

    private final ElasticsearchRestTemplate elasticsearchTemplate;
    private final SearchRepository searchRepository;

    @Override
    public Page<SearchBean> search(String text, SourceTypeEnum type, int current, SortEnum sort, List<Long> chatIds, Boolean filter) {
        if (StrUtil.isBlank(text)) {
            return Page.empty();
        }

        BoolQueryBuilder boolQuery = QueryBuilders.boolQuery();

        boolQuery.must(QueryBuilders.boolQuery()
                .should(QueryBuilders.matchPhraseQuery("sourceName", text).boost(3.0f))
                .should(QueryBuilders.matchQuery("sourceName", text).boost(1.0f))
                .minimumShouldMatch(1));
        if (Objects.nonNull(type)) {
            boolQuery.must(QueryBuilders.matchQuery("type", type.name()));
        }
        if (CollUtil.isNotEmpty(chatIds)) {
            boolQuery.must(QueryBuilders.termsQuery("chatId", chatIds));
        }
        if (Boolean.TRUE.equals(filter)) {
            boolQuery.must(QueryBuilders.termsQuery("marked", false));
        }

        Sort orders;
        if (Objects.nonNull(sort) && !Objects.equals(sort, SortEnum.EMPTY)) {
            orders = Sort.by(Sort.Direction.DESC, "_score")
                    .and(Sort.by(Sort.Direction.DESC, sort.getFields()));
        } else {
            orders = Sort.by(Sort.Direction.DESC, "_score")
                    .and(Sort.by(Sort.Direction.DESC, "collectTime"));
        }

        PageRequest pageRequest = PageRequest.of(current, PAGE_SIZE, orders);
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(boolQuery)
                .withPageable(pageRequest)
                .build();

        if (Objects.nonNull(searchQuery.getQuery())) {
            log.info("\n===\n{}\n===", StrUtil.removeAll(searchQuery.getQuery().toString(), '\n', ' '));
        }

        SearchHits<SearchBean> searchHits = elasticsearchTemplate.search(searchQuery, SearchBean.class);
        List<SearchHit<SearchBean>> searchList = searchHits.getSearchHits();
        List<SearchBean> content = new ArrayList<>(searchList.size());
        for (SearchHit<SearchBean> hit : searchList) {
            content.add(hit.getContent());
        }
        return new PageImpl<>(content, pageRequest, searchHits.getTotalHits());
    }

    @Override
    public long countSource(List<Long> chatIds) {
        if (CollUtil.isEmpty(chatIds)) {
            return 0L;
        }
        BoolQueryBuilder boolQuery = QueryBuilders.boolQuery()
                .must(QueryBuilders.termsQuery("chatId", chatIds));
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(boolQuery)
                .build();
        SearchHits<SearchBean> searchHits = elasticsearchTemplate.search(searchQuery, SearchBean.class);
        return searchHits.getTotalHits();
    }

    @Override
    public void save(SearchBean bean) {
        if (Objects.isNull(bean) || Objects.isNull(bean.getType()) || StrUtil.isBlank(bean.getSourceName())) {
            return;
        }
        try {
            this.searchRepository.save(bean);
        } catch (Exception ex) {
            log.error("[ES保存搜索资源失败] bean: {}, 错误信息: {}", bean, ex.getMessage(), ex);
        }
    }
}
