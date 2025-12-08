package com.search.robots.database.enums.adv;


import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.EnumValue;
import com.search.robots.helper.StrHelper;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;


/**
 *
 *
 * @author admin
 * @since 2025/11/7 21:58
 */
@Getter
@AllArgsConstructor
public enum AdvStatus {

    UN_START(1, "未开始❌", "未开始"),
    UNDER_APPROVAL(2, "审批中⏳", "审批中"),
    APPROVAL_PASS(3, "审批通过✅", "审批通过"),
    PROMOTION_ING(4, "推广中⏳", "推广中"),
    PAUSE_ING(5, "暂停中\uD83D\uDFE0", "暂停中"),
    THE_END(6, "已结束", "已结束")
    ;

    @EnumValue
    private final int code;
    private final String desc;
    private final String button;

    public static AdvStatus of(String status) {
        return Arrays.stream(AdvStatus.values())
                .filter(a -> StrUtil.equals(String.valueOf(a.code), status))
                .findFirst().orElse(null);
    }


    public String buildName (String status) {
        boolean equals = StrUtil.equals(String.valueOf(this.code), status);
        return StrHelper.hit(this.button, equals);
    }
}
