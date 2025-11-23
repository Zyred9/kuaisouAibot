package com.search.robots.beans.view;


import com.search.robots.beans.view.vo.search.SearchBean;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 *
 *
 * @author zyred
 * @since 2025/11/13 15:45
 */
@Setter
@Getter
@Accessors(chain = true)
public class AsyncBean {


    /** 热搜词的关键词 **/
    private String searchKeyword;
    /** 直接搜索展现的关键词 **/
    private Set<Long> directIds;
    /** 关联搜索展现的关键词 **/
    private Set<Long> relatedIds;

    /** 曝光的数据 **/
    private Set<Long> exposureChatIds;

    public static AsyncBean kw (String kw) {
        return new AsyncBean()
                .setSearchKeyword(kw);
    }

    /**
     * 关键词直接展示
     */
    public static AsyncBean directIncr(Set<Long> userAdvIds) {
        return new AsyncBean()
                .setDirectIds(userAdvIds);
    }

    /**
     * 关键词直接展示
     */
    public static AsyncBean relatedIncr(Set<Long> userAdvIds) {
        return new AsyncBean()
                .setRelatedIds(userAdvIds);
    }

    /**
     * 添加群组曝光数据
     *
     * @param search       搜索结果
     * @return                  任务对象
     */
    public static AsyncBean exposure(Page<SearchBean> search) {
        Set<Long> chatIds = search.toSet().stream()
                .map(SearchBean::getChatId).collect(Collectors.toSet());
        return new AsyncBean().setExposureChatIds(chatIds);
    }
}
