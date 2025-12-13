package com.search.robots.database.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Getter
@AllArgsConstructor
public enum Dialogue {


    // 等待输入地址
    INPUT_ADDRESS(0),
    INPUT_WITHDRAWAL_AMOUNT(1),
    INPUT_TARGETED_SEARCH(2),

    // 等待输入广告标题
    INPUT_ADV_TITLE(3),
    // 等待输入广告链接
    INPUT_ADV_LINK(4),

    // 等待输入充值金额
    INPUT_RECHARGE_AMOUNT(5)
    ;


    private final Integer code;

    public static Dialogue getByCode(Integer code) {
        for (Dialogue dialogue : values()) {
            if (dialogue.getCode().equals(code)) {
                return dialogue;
            }
        }
        return null;
    }
}
