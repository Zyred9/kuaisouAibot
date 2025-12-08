package com.search.robots.database.enums;


import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 *
 *
 * @author admin
 * @since 2025/11/1 20:55
 */
@Getter
@AllArgsConstructor
public enum AdvertisingGradeEnum {

    A(0, "A级代理"),
    B(1, "B级代理"),
    C(2, "C级代理"),
    D(3, "D级代理"),
    E(4, "E级代理")
    ;

    @EnumValue
    private final int code;
    private final String desc;

}
