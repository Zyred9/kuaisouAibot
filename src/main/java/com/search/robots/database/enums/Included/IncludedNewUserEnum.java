package com.search.robots.database.enums.Included;


import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 收录的类型
 *
 * @author zyred
 * @since 2025/11/2 15:39
 */
@Getter
@AllArgsConstructor
public enum IncludedNewUserEnum {

    ONE_DAY(1, "1天一次 ", "four#new_user#1#"),
    THREE_DAY(3, "3天一次", "four#new_user#3#"),
    SEVEN_DAY(7, "7天一次", "four#new_user#7#"),
    ;

    @EnumValue
    private final int code;
    private final String desc;
    private final String call;


    public static IncludedNewUserEnum of (int code) {
        return code == ONE_DAY.code ? ONE_DAY : (code == THREE_DAY.code) ? THREE_DAY : SEVEN_DAY;
    }
}
