package com.search.robots.database.enums.adv;


import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;
import java.util.Objects;

/**
 * 广告位置枚举
 * <pre>
 * 定义广告展示的具体位置:
 * - RANK_1~RANK_10: 榜单排名位置(1-10位)
 * - DIRECT_PAGE: 关键词直接搜索专页
 * - RELATED_PAGE: 关键词关联搜索专页
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
@Getter
@AllArgsConstructor
public enum AdvPositionEnum {

    RANK_1(1, "榜单第1位", "🥇"),
    RANK_2(2, "榜单第2位", "🥈"),
    RANK_3(3, "榜单第3位", "🥉"),
    RANK_4(4, "榜单第4位", "🏅"),
    RANK_5(5, "榜单第5位", "🎖"),
    RANK_6(6, "榜单第6位", "🎖"),
    RANK_7(7, "榜单第7位", "🎖"),

    DIRECT_PAGE(101, "关键词直接搜索专页", "直接:"),
    RELATED_PAGE(102, "关键词关联搜索专页", "关联:")
    ;

    @EnumValue
    private final int code;
    private final String desc;
    private final String icon;

    public static AdvPositionEnum of (int code) {
        return Arrays.stream(AdvPositionEnum.values())
                .filter(a -> Objects.equals(code, a.getCode()))
                .findFirst().orElse(null);
    }

    /**
     * 判断是否为榜单排名位置
     */
    public boolean isRankPosition() {
        return code >= 1 && code <= 10;
    }

    /**
     * 判断是否为专页位置
     */
    public boolean isPagePosition() {
        return code >= 101;
    }


}
