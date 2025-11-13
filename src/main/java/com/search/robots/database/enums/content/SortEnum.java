package com.search.robots.database.enums.content;


import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;

/**
 *
 *
 * @author zyred
 * @since 2025/11/10 18:38
 */
@Getter
@AllArgsConstructor
public enum SortEnum {

    EMPTY("0", "综合", "♻️", null),
    TIMES("1", "时长", "\uD83D\uDD58", "times"),
    VIEWS("2", "浏览量", "\uD83C\uDFA6", "views"),
    LATEST("3", "最新", "\uD83C\uDD95", "collectTime"),
    ;


    @EnumValue
    private final String code;
    private final String desc;
    private final String icon;
    private final String fields;

    private static final SortEnum[] KEYBOARDS = new SortEnum[]{TIMES, VIEWS, LATEST};
    public static SortEnum[] keyboards (){
        return KEYBOARDS;
    }

    public static SortEnum of(String code) {
        return Arrays.stream(SortEnum.values())
                .filter(a -> StrUtil.equals(a.code, code))
                .findFirst()
                .orElse(null);
    }
}
