package com.search.robots.database.enums.Included;


import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * 搜索优先级
 *
 * @author zyred
 * @since 2025/11/2 15:39
 */
@Getter
@AllArgsConstructor
public enum IncludedSearchPriorityEnum {

    CHANNEL_FIRST(1, "频道优先", "five#priority_customize#1#"),
    GROUP_FIRST(2, "群组优先", "five#priority_customize#2#"),
    VIDEO_FIRST(3, "视频优先", "five#priority_customize#3#"),
    AUDIO_FIRST(4, "音频优先", "five#priority_customize#4#"),
    PHOTO_FIRST(5, "图片优先", "five#priority_customize#5#"),
    FILE_FIRST(6, "文件优先", "five#priority_customize#6#"),

    CHANNEL_TEXT_FIRST(7, "频道文本优先", "five#priority_customize#7#"),
    GROUP_TEXT_FIRST(8, "群组文本优先", "five#priority_customize#8#"),

    BOT_FIRST(9, "机器人优先", "five#priority_customize#9#"),
    PRIVACY_FIRST(10, "私聊优先", "five#priority_customize#10#"),
    ;

    @EnumValue
    private final int code;
    private final String desc;
    private final String call;

    public static IncludedSearchPriorityEnum of (int code) {
        return Arrays.stream(IncludedSearchPriorityEnum.values())
                .filter(a -> Objects.equals(code, a.getCode()))
                .findFirst().orElse(null);
    }

    public static List<IncludedSearchPriorityEnum> vals () {
        return Arrays.stream(IncludedSearchPriorityEnum.values()).toList();
    }
}
