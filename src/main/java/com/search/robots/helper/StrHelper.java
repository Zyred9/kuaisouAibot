package com.search.robots.helper;

import cn.hutool.core.util.StrUtil;
import com.search.robots.database.enums.Included.IncludedSearchTypeEnum;
import com.search.robots.database.enums.SearchPeriodEnum;

import java.security.SecureRandom;
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

    public static Integer parseTimeToSeconds(String timeStr) {
        if (StrUtil.isBlank(timeStr)) {
            return null;
        }

        try {
            String cleanTime = timeStr.trim();
            String[] parts = cleanTime.split(":");
            if (parts.length == 1) {
                return Integer.parseInt(parts[0]);
            } else if (parts.length == 2) {
                int minutes = Integer.parseInt(parts[0]);
                int seconds = Integer.parseInt(parts[1]);
                return minutes * 60 + seconds;
            } else if (parts.length == 3) {
                int hours = Integer.parseInt(parts[0]);
                int minutes = Integer.parseInt(parts[1]);
                int seconds = Integer.parseInt(parts[2]);
                return hours * 3600 + minutes * 60 + seconds;
            }
            return null;
        } catch (NumberFormatException e) {
            return null;
        }
    }

    public static String formatSecondsToTime(Integer seconds) {
        if (Objects.isNull(seconds) || seconds < 0) {
            return "";
        }

        int hours = seconds / 3600;
        int minutes = (seconds % 3600) / 60;
        int secs = seconds % 60;

        if (hours > 0) {
            return String.format("%02d:%02d:%02d", hours, minutes, secs);
        } else {
            return String.format("%02d:%02d", minutes, secs);
        }
    }

    // 编码为十六进制
    public static String encode(String text) {
        StringBuilder hex = new StringBuilder();
        for (byte b : text.getBytes()) {
            hex.append(String.format("%02x", b));
        }
        return hex.toString();
    }

    // 解码
    public static String decode(String hex) {
        byte[] bytes = new byte[hex.length() / 2];
        for (int i = 0; i < bytes.length; i++) {
            int index = i * 2;
            bytes[i] = (byte) Integer.parseInt(hex.substring(index, index + 2), 16);
        }
        return new String(bytes);
    }

    /**
     * 解析 callback_data 中的数值部分
     * 
     * @param callbackData 格式: "300_600" (展示次数_价格)
     * @return int数组 [展示次数, 价格]，解析失败返回null
     * 
     * @example
     * parseCallbackNumbers("300_600") -> [300, 600]
     * parseCallbackNumbers("1200_1800") -> [1200, 1800]
     * parseCallbackNumbers("invalid") -> null
     */
    public static int[] parseCallbackNumbers(String callbackData) {
        if (StrUtil.isBlank(callbackData)) {
            return null;
        }

        try {
            String[] parts = callbackData.split("_");
            if (parts.length != 2) {
                return null;
            }

            int showNumber = Integer.parseInt(parts[0].trim());  // 展示次数
            int amount = Integer.parseInt(parts[1].trim());      // 价格

            return new int[]{showNumber, amount};
        } catch (NumberFormatException e) {
            return null;
        }
    }

}
