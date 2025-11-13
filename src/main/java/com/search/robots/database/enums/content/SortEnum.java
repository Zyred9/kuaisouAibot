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

    TIMES("0", "时长", "\uD83D\uDD58"),
    VIEWS("1", "浏览量", "\uD83C\uDFA6"),
    LATEST("2", "最新", "\uD83C\uDD95"),
    EMPTY("100", "综合", "♻️"),
    ;


    @EnumValue
    private final String code;
    private final String desc;
    private final String icon;

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
