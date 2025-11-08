package com.search.robots.database.enums.Included;


import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 *
 *
 * @author zyred
 * @since 2025/11/2 15:47
 */
@Getter
@AllArgsConstructor
public enum PrivacyTypeEnum {

    PUBLIC(0, "公开群组", "公开"),
    PRIVATE(1, "私密群组", "私密"),
    ;

    @EnumValue
    private final int code;
    private final String desc;
    private final String btn;
}