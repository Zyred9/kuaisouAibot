package com.search.robots.database.enums;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;
import java.util.Locale;

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
public enum Currency {

    USDT(0, "USDT", 1),
    TRX(1, "TRX", 10),
    CNY(2, "CNY", 10),
    ERROR(3, "ERROR", 0),
    ;

    @EnumValue
    private final Integer code;
    private final String desc;
    private final Integer min;


    public boolean isUsdt () {
        return this.equals(USDT);
    }

    public boolean isTrx() {
        return this.equals(TRX);
    }

    public boolean isCny() {
        return this.equals(CNY);
    }

    public boolean isError() {
        return this.equals(ERROR);
    }


    public static Currency[] currencies () {
        return new Currency[] {USDT, TRX, CNY};
    }

    public static Currency of(String name) {
        Currency[] currencies = currencies();
        for (Currency currency : currencies) {
            if (StrUtil.equals(currency.name(), name)) {
                return currency;
            }
        }
        return null;
    }

    public static Currency ofCode(String code) {
        Currency[] currencies = currencies();
        for (Currency currency : currencies) {
            if (StrUtil.equals(String.valueOf(currency.getCode()), code)) {
                return currency;
            }
        }
        return null;
    }

    public static Currency ofCurrency (String unit) {
        if (StrUtil.endWithAny(unit, "u", "usdt", "U", "USDT")) {
            return USDT;
        }
        else if (StrUtil.endWithAny(unit, "t", "trx", "T", "TRX")) {
            return TRX;
        }
        else if (StrUtil.endWithAny(unit, "c", "cny", "C", "CNY")) {
            return CNY;
        }
        else {
            return USDT;
        }
    }

    public static BigDecimal ofAmount (String amountStr) {
        String text = StrUtil.removeAll(amountStr, 'u', 's', 'd', 't', 'U', 'S', 'D', 'T', 't', 'r', 'x', 'T', 'R', 'X', 'c', 'n', 'y', 'C', 'N', 'Y');
        return new BigDecimal(text);
    }

    public static Currency ofQueryCondition(String condition) {
        if (StrUtil.equals(condition, "all") || StrUtil.isEmpty(condition)) {
            return null;
        }
        return of(condition.toUpperCase(Locale.ROOT));
    }
}
