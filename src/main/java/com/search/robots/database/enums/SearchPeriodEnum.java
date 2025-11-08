package com.search.robots.database.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;

/**
 * 热搜词统计查询的周期类型枚举。
 * 包含了周期的中文描述、对应的 hit 值，以及计算起始日期的方法。
 */
@Getter
@AllArgsConstructor
public enum SearchPeriodEnum {

    DAILY(1, "当天", "one#period#1"),
    YESTERDAY(2, "昨天", "one#period#2"),
    LAST_3_DAYS(3, "近三天", "one#period#3"),
    LAST_7_DAYS(4, "近七天", "one#period#4"),
    MONTHLY(5, "当月", "one#period#5");

    private final int code;
    private final String desc;
    private final String call;

    public static SearchPeriodEnum fromHit(int hit) {
        for (SearchPeriodEnum type : values()) {
            if (type.code == hit) {
                return type;
            }
        }
        return null;
    }
    
    public LocalDate getStartDate(LocalDate endDate) {
        return switch (this) {
            case DAILY, YESTERDAY ->
                // 昨天：开始和结束日期相同，将在 Service 层处理
                    endDate.minusDays(this == YESTERDAY ? 1 : 0);
            case LAST_3_DAYS ->
                // D-2 到 D (共 3 天)
                    endDate.minusDays(2);
            case LAST_7_DAYS ->
                // D-6 到 D (共 7 天)
                    endDate.minusDays(6);
            case MONTHLY ->
                // 本月第一天
                    endDate.with(TemporalAdjusters.firstDayOfMonth());
            default -> throw new IllegalStateException("未处理的周期类型: " + this);
        };
    }
}