package com.search.robots.beans.view.caffeine;


import com.search.robots.database.enums.Included.IncludedNewUserEnum;
import com.search.robots.database.enums.caffeine.TaskNode;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.concurrent.TimeUnit;

/**
 *
 *
 * @author admin
 * @since 2025/11/15 18:08
 */
@Setter
@Getter
@Accessors(chain = true)
public class Task {

    /** 任务类型 **/
    private TaskNode node;
    /*** 充值地址 **/
    private String address;

    /// ------------------------------------
    /** chatId **/
    private Long chatId;
    /** 天数 **/
    private IncludedNewUserEnum days;
    /** 剩下时间单位 **/
    private TimeUnit unit;
    /** 剩余分钟数 **/
    private Integer minutes;
    /// ------------------------------------

    public static Task buildRecharge (String address) {
        return new Task()
                .setNode(TaskNode.RECHARGE)
                .setAddress(address);
    }


    public static Task buildEveryAdv (Long chatId, IncludedNewUserEnum days) {
        return buildEveryAdv(chatId, days, null, null);
    }
    public static Task buildEveryAdv (Long chatId, IncludedNewUserEnum days, Integer minutes) {
        return buildEveryAdv(chatId, days, TimeUnit.MINUTES, minutes);
    }
    public static Task buildEveryAdv (Long chatId, IncludedNewUserEnum days, TimeUnit unit, Integer minutes) {
        return new Task()
                .setNode(TaskNode.EVERY_ADV)
                .setChatId(chatId)
                .setDays(days)
                .setUnit(unit)
                .setMinutes(minutes);

    }
}
