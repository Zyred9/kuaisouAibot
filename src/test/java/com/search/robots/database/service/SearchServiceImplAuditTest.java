package com.search.robots.database.service;

import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.database.enums.content.SortEnum;
import com.search.robots.database.mapper.SearchRepository;
import com.search.robots.database.service.impl.SearchServiceImpl;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.data.domain.Page;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@DisplayName("SearchServiceImpl 审核通过筛选测试")
class SearchServiceImplAuditTest {

    @Test
    void testSearchIncludesApprovedFilter() {
        ElasticsearchRestTemplate template = mock(ElasticsearchRestTemplate.class);
        SearchRepository repo = mock(SearchRepository.class);
        SearchServiceImpl service = new SearchServiceImpl(template, repo);

        SearchHits<SearchBean> hits = mock(SearchHits.class);
        when(hits.getSearchHits()).thenReturn(Collections.emptyList());
        when(hits.getTotalHits()).thenReturn(0L);
        when(template.search(any(NativeSearchQuery.class), eq(SearchBean.class))).thenReturn(hits);

        Page<SearchBean> page = service.search("测试", null, 0, SortEnum.EMPTY, Collections.emptyList(), false);
        assertNotNull(page);

        ArgumentCaptor<NativeSearchQuery> captor = ArgumentCaptor.forClass(NativeSearchQuery.class);
        verify(template, times(1)).search(captor.capture(), eq(SearchBean.class));
        String q = captor.getValue().getQuery().toString();
        assertTrue(q.contains("auditStatus"));
        assertTrue(q.contains("APPROVED"));
    }

    @Test
    void testCountSourceIncludesApprovedFilter() {
        ElasticsearchRestTemplate template = mock(ElasticsearchRestTemplate.class);
        SearchRepository repo = mock(SearchRepository.class);
        SearchServiceImpl service = new SearchServiceImpl(template, repo);

        SearchHits<SearchBean> hits = mock(SearchHits.class);
        when(hits.getTotalHits()).thenReturn(10L);
        when(template.search(any(NativeSearchQuery.class), eq(SearchBean.class))).thenReturn(hits);

        long total = service.countSource(java.util.Arrays.asList(1L, 2L));
        assertEquals(10L, total);

        ArgumentCaptor<NativeSearchQuery> captor = ArgumentCaptor.forClass(NativeSearchQuery.class);
        verify(template, times(1)).search(captor.capture(), eq(SearchBean.class));
        String q = captor.getValue().getQuery().toString();
        assertTrue(q.contains("chatId"));
        assertTrue(q.contains("auditStatus"));
        assertTrue(q.contains("APPROVED"));
    }
}

