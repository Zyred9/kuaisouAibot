package com.search.robots.database.enums.adv;


import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 *
 *
 * @author zyred
 * @since 2025/11/7 21:58
 */
@Getter
@AllArgsConstructor
public enum AdvStatus {

    UNDER_APPROVAL(0, "审批中"),
    UN_START(1, "未开始"),
    PROMOTION_ING(2, "推广中"),
    PAUSE_ING(3, "暂停中"),
    THE_END(4, "已结束")
    ;

    @EnumValue
    private final int code;
    private final String desc;
}
