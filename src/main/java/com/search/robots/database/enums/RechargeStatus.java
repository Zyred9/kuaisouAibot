package com.search.robots.database.enums;

import com.baomidou.mybatisplus.annotation.EnumValue;
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
public enum RechargeStatus {

    // 充值的进度状态，例：0.处理中, 1.已到账、等
    PROCESS(0, "处理中"),
    SUCCESS(1, "已到账"),
    TIMEOUT(2, "超时"),
    ;

    @EnumValue
    private final Integer code;
    private final String desc;
}
