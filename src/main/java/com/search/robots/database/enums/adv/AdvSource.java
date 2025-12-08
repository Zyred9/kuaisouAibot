package com.search.robots.database.enums.adv;


import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 *
 *
 * @author admin
 * @since 2025/11/7 21:58
 */
@Getter
@AllArgsConstructor
public enum AdvSource {

    BUY(0, "购买"),
    ;

    @EnumValue
    private final int code;
    private final String desc;
}
