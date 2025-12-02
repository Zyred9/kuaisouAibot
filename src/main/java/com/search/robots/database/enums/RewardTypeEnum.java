package com.search.robots.database.enums;

import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 奖励类型枚举
 *
 * @author zyred
 * @since 2025/11/25
 */
@Getter
@AllArgsConstructor
public enum RewardTypeEnum {

    DIRECT_NEW_USER(1, "直接拉新奖励"),
    NEXT_NEW_USER(2, "下级拉新奖励"),
    PRIVATE_CHAT(3, "私聊奖励"),
    GROUP_SEARCH(4, "群搜索奖励"),
    AD_COMMISSION(5, "广告代理奖励"),
    RECHARGE_GIFT(6, "充值加赠奖励"),
    ;

    @EnumValue
    private final int code;
    private final String desc;

    public static RewardTypeEnum of(int code) {
        for (RewardTypeEnum value : values()) {
            if (value.code == code) {
                return value;
            }
        }
        return null;
    }
}
