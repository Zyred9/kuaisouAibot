package com.search.robots.database.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 热搜词每日记录实体类
 * 对应数据库表 t_hot_search_daily
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_hot_search")
public class HotSearch {

    public static final String HOT_SEARCH_KEY = "search:keyword:hot_search";

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    private String keyword;
    private Integer searchCount;
    private LocalDate searchDay;
    private LocalDateTime createTime;


    public static HotSearch buildSearch (String keyword) {
        return new HotSearch()
                .setKeyword(keyword)
                .setSearchCount(1)
                .setSearchDay(LocalDate.now())
                .setCreateTime(LocalDateTime.now());
    }

}