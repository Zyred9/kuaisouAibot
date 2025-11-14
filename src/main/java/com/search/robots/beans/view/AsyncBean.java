package com.search.robots.beans.view;


import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.Set;

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
}
