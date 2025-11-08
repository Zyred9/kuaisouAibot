package com.search.robots.database.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.search.robots.database.entity.HotSearch;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.time.LocalDate;
import java.util.List;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@SuppressWarnings("all")
public interface HotSearchMapper extends BaseMapper<HotSearch> {

    @Select("""
        SELECT
            keyword,
            SUM(search_count) as search_count
        FROM
            t_hot_search
        WHERE
            search_day BETWEEN #{startDate} AND #{endDate}
        GROUP BY
            keyword
        ORDER BY
            search_count DESC
        LIMIT #{limit}
    """)
    List<HotSearch> keywords(
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate,
            @Param("limit") Integer limit
    );

}
