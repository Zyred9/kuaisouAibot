package com.search.robots.database.enums.Included;


import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 收录的类型
 *
 * @author admin
 * @since 2025/11/2 15:39
 */
@Getter
@AllArgsConstructor
public enum IncludedSearchTypeEnum {

    ALL(2, "全部", "two#total_group_channel#2#"),
    CHANNEL(1, "频道", "two#total_group_channel#1#"),
    GROUP(0, "群组", "two#total_group_channel#0#"),
    ;

    private final int code;
    private final String desc;
    private final String call;

    public static IncludedSearchTypeEnum of (int code) {
        return code == GROUP.code ? GROUP : (code == CHANNEL.code) ? CHANNEL : ALL;
    }


}
