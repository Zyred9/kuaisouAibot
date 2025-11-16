package com.search.robots.database.enums.caffeine;


import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.concurrent.TimeUnit;

/**
 *
 *
 * @author zyred
 * @since 2025/11/15 18:10
 */
@Getter
@AllArgsConstructor
public enum TaskNode {

    RECHARGE(0, 30, TimeUnit.SECONDS, "充值")
    ;

    private final int code;
    private final int loop;
    private final TimeUnit timeUnit;
    private final String desc;
}
