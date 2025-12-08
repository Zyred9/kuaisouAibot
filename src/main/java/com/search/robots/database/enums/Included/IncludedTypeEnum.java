package com.search.robots.database.enums.Included;


import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Objects;
import java.util.Set;

/**
 * 收录的类型
 *
 * @author admin
 * @since 2025/11/2 15:39
 */
@Getter
@AllArgsConstructor
public enum IncludedTypeEnum {

    GROUP(0, "群组"),
    CHANNEL(1, "频道");

    @EnumValue
    private final int code;
    private final String desc;

    public static IncludedTypeEnum of (IncludedSearchTypeEnum searchType) {
        if (Objects.equals(searchType, IncludedSearchTypeEnum.ALL)) {
            return null;
        }
        return searchType.getCode() == GROUP.code ? GROUP : searchType.getCode() == CHANNEL.code ? CHANNEL : null;
    }

    public static IncludedTypeEnum of (String type) {
        Set<String> group = Set.of("group", "supergroup");
        Set<String> channel = Set.of("channel");

        if (CollUtil.contains(group, type)) {
            return IncludedTypeEnum.GROUP;
        }
        if (CollUtil.contains(channel, type)) {
            return IncludedTypeEnum.CHANNEL;
        }
        return null;
    }
}
