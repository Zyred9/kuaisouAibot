package com.search.robots.database.service.impl;


import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.database.service.SearchService;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;

/**
 *
 *
 * @author zyred
 * @since 2025/11/9 20:16
 */
@Component
public class SearchServiceImpl implements SearchService {

    @Override
    public Page<SearchBean> search(String text) {
        return Page.empty();
    }
}
