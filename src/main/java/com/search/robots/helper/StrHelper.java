package com.search.robots.helper;

import cn.hutool.core.util.StrUtil;
import com.search.robots.database.enums.Included.IncludedSearchTypeEnum;
import com.search.robots.database.enums.SearchPeriodEnum;

import java.security.SecureRandom;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public class StrHelper {

    public static int collGet(List<String> coll, int idx, int defaultVal) {
        try {
            return Integer.parseInt(coll.get(idx));
        } catch (Exception ex) {
            return defaultVal;
        }
    }

    public static String collGet(List<String> coll, int idx, String defaultVal) {
        try {
            return coll.get(idx);
        } catch (Exception ex) {
            return defaultVal;
        }
    }

    public static String hit (String name, boolean hit) {
        if (hit) {
            return "✅" + name;
        }
        return name;
    }

    public static String hit (String name, IncludedSearchTypeEnum condition, IncludedSearchTypeEnum hit) {
        if (Objects.equals(condition, hit)) {
            return name + "✅";
        }
        return name;
    }

    public static String hit (String name, SearchPeriodEnum condition, SearchPeriodEnum hit) {
        if (Objects.equals(condition, hit)) {
            return name + "✅";
        }
        return name;
    }

    public static String buildName (Object ... values) {
        return StrUtil.join("#", values);
    }

    public static String buildSymbolName (String symbol, Object ... values) {
        return StrUtil.join(symbol, values);
    }

    public static String nickname (String first, String last) {
        if (StrUtil.isAllBlank(first, last)) {
            return "";
        }
        String name = first;
        if (StrUtil.isAllNotBlank(first, last)) {
            name = first + " " + last;
        }
        return specialChar(name);
    }


    public static String specialChar(String input) {
        String specialChar = "([_*|\\[\\]()~`>#+\\-={}.!\\\\])";
        return input.replaceAll(specialChar, "\\\\$0");
    }

    public static String specialResult(String input) {
        if (StrUtil.isBlank(input)) {
            return "";
        }
        String specialChar = "([_*|\\[\\]()~`>#+\\-={}.!\\\\])";
        return input.replaceAll(specialChar, "\\\\$0");
    }

    public static String specialLong(Long value) {
        return specialChar(String.valueOf(value));
    }

    public static String getKey(Long userId) {
        return userId + "";
    }


    private static final SecureRandom RANDOM = new SecureRandom();
    private static final String CHARACTERS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    public static String generateInviteCode() {
        StringBuilder sb = new StringBuilder(7);
        for (int i = 0; i < 7; i++) {
            int randomIndex = RANDOM.nextInt(CHARACTERS.length());
            sb.append(CHARACTERS.charAt(randomIndex));
        }
        return sb.toString();
    }
}
