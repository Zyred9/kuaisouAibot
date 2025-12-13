package com.search.robots.database.enums.caffeine;


import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.concurrent.TimeUnit;

/**
 *
 *
 * @author admin
 * @since 2025/11/15 18:10
 */
@Getter
@AllArgsConstructor
public enum TaskNode {

    RECHARGE(0, 30, TimeUnit.SECONDS, "充值"),
    EVERY_ADV(1, -1, TimeUnit.DAYS, "每日拉新广告"),

    REUSE_RECHARGE(2, 30, TimeUnit.SECONDS, "地址复用充值"),
    ;

    private final int code;
    private final int loop;
    private final TimeUnit timeUnit;
    private final String desc;
}
