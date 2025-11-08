package com.search.robots.helper;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;
import java.util.Objects;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;
/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public class DecimalHelper {


    public static String standard (BigDecimal source) {
        if (Objects.isNull(source)) {
            return "0.0000";
        }
        DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.ENGLISH);
        symbols.setDecimalSeparator('.');
        DecimalFormat df = new DecimalFormat("0.0000", symbols);
        return df.format(source);
    }

    public static String standardSymbol(BigDecimal source) {
        if (Objects.isNull(source)) {
            return "0\\.0000";
        }
        String standard = standard(source);
        return standard.replace(".", "\\.");
    }

    /**
     * 比较两个 BigDecimal 值的大小。
     *
     * @param source 待比较的源 BigDecimal。
     * @param target 用于比较的目标 BigDecimal。
     * @return 如果 source 小于 target，则返回 true；否则（source 大于或等于 target），返回 false。
     */
    public static boolean compare(BigDecimal source, BigDecimal target) {
        return source.compareTo(target) < 0;
    }

    public static String decimalParse (BigDecimal bigDecimal) {
        if (Objects.isNull(bigDecimal)) {
            return "";
        }
        return bigDecimal.stripTrailingZeros().toPlainString();
    }


}
