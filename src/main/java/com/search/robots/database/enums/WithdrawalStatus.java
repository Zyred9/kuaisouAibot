package com.search.robots.database.enums;

import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;
import java.util.Objects;

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
public enum WithdrawalStatus {

    PROCESSING(0, "⌛️等待财务处理"),
    SUCCESS(1, "✅提现成功"),
    FAILED(2, "❌提现失败"),
    CANCEL(3, "❌取消提现");

    @EnumValue
    private final Integer code;
    private final String name;


    public static WithdrawalStatus of (Integer code) {
        return Arrays.stream(WithdrawalStatus.values())
                .filter(status -> Objects.equals(status.getCode(), code))
                .findFirst().orElse(null);
    }
}
