package com.search.robots.beans.view.vo;


import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.springframework.cglib.core.Local;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.stream.Collectors;


/**
 * 广告展现统计VO
 * <pre>
 * 记录每日的展现数据:
 * - directShow: 直接搜索展现次数
 * - relatedShow: 关联搜索展现次数
 * - uniqueUser: 独立用户数
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
@Getter
@Setter
@Accessors(chain = true)
public class AdvShow {

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE;

    /** 统计日期 **/
    private String date;
    
    /** 直接搜索展现次数 **/
    private Long directShow;
    
    /** 关联搜索展现次数 **/
    private Long relatedShow;
    
    /** 独立访客数 **/
    private Long uniqueUser;
    
    /** 总展现次数(计算字段) **/
    public Long getTotalShow() {
        return (directShow == null ? 0L : directShow) + (relatedShow == null ? 0L : relatedShow);
    }
    
    
    public static AdvShow buildDefault (boolean direct) {
        AdvShow advShow = new AdvShow()
                .setDate(LocalDate.now().toString())
                .setUniqueUser(0L);
        if (direct) {
            advShow.setDirectShow(1L);
        } else {
            advShow.setRelatedShow(1L);
        }
        return advShow;
    }


    /**
     * 保证只有7天的数据
     *
     * @param advShows  所有的数据
     * @return          7天的数据
     */
    public static List<AdvShow> filterLastSevenDays(List<AdvShow> advShows) {
        if (advShows == null || advShows.isEmpty()) {
            return advShows;
        }
        LocalDate today = LocalDate.now();
        LocalDate sevenDaysAgo = today.minusDays(6);
        return advShows.stream()
                .filter(show -> {
                    try {
                        LocalDate showDate = LocalDate.parse(show.getDate(), DATE_FORMATTER);
                        boolean isAfterOrEqualSevenDaysAgo = !showDate.isBefore(sevenDaysAgo);
                        boolean isBeforeOrEqualToday = !showDate.isAfter(today);
                        return isAfterOrEqualSevenDaysAgo && isBeforeOrEqualToday;
                    } catch (Exception e) {
                        return false;
                    }
                })
                .collect(Collectors.toList());
    }

}
