package com.search.robots.database.service;


import com.search.robots.beans.view.vo.search.SearchBean;
import org.springframework.data.domain.Page;

/**
 *
 *
 * @author zyred
 * @since 2025/11/9 20:15
 */
public interface SearchService {

    Page<SearchBean> search(String text);

}
