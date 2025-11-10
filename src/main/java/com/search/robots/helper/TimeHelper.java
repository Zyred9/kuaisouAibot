package com.search.robots.helper;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public class TimeHelper {

    /**
     * 判断目标时间是否距离当前时间超过指定天数
     *
     * @param targetTime 目标时间
     * @param days       判断天数（例如 5 表示超过 5 天）
     * @return true 表示目标时间距离现在超过指定天数
     */
    public static boolean isMoreThanDays(LocalDateTime targetTime, long days) {
        if (targetTime == null) {
            return false;
        }
        long diff = ChronoUnit.DAYS.between(LocalDateTime.now(), targetTime);
        return diff > days;
    }

    public static List<LocalDate> ofSevenDays () {
        List<LocalDate> days = new ArrayList<>(7);
        for (int i = 0; i < 7; i++) {
            days.add(LocalDate.now().minusDays(i));
        }
        return days;
    }

    public static String formatV2 (LocalDateTime time) {
        if (Objects.isNull(time)) {
            return null;
        }
        return StrHelper.specialResult(TimeHelper.format(time));
    }

    public static String formatV2_ (LocalDateTime time) {
        if (Objects.isNull(time)) {
            return null;
        }
        return StrHelper.specialResult(TimeHelper.format(time, "MM-dd HH:mm:ss"));
    }



    public static String format(LocalDateTime time, String pattener) {
        if (Objects.isNull(time)) {
            return null;
        }
        return time.format(DateTimeFormatter.ofPattern(pattener));
    }

    public static String format(LocalDateTime time) {
        if (Objects.isNull(time)) {
            return null;
        }
        return format(time, "yyyy-MM-dd HH:mm:ss");
    }

    public static long getTimestamp(LocalDateTime time) {
        LocalDateTime now = LocalDateTime.now();
        ZoneOffset hongKongOffset = ZoneOffset.of("+08:00");
        return now.toEpochSecond(hongKongOffset);
    }

    // 将 1分钟，1小时，10小时，1天，2天 转换为 分钟
    public static Integer convertMinutes(String timeStr) {
        Pattern pattern = Pattern.compile("(\\d+)([天小时分钟秒月]+)");
        Matcher matcher = pattern.matcher(timeStr);

        if (matcher.find()) {
            int amount = Integer.parseInt(matcher.group(1));
            String unit = matcher.group(2);

            switch (unit) {
                case "秒", "分钟":
                    return amount;
                case "小时":
                    return amount * 60;
                case "天":
                    return amount * 24 * 60;
                case "月":
                    return amount * 30 * 24 * 60;
            }
        }
        return null;
    }


    /**
     * 获取解封时间
     *
     * @param minutes       分钟
     * @return              解封时间
     */
    public static long getTelegramTime (Integer minutes) {
        return System.currentTimeMillis() / 1000 + (minutes * 60);
    }

    // 将分钟转换为 1分钟，1小时，10小时，1天，2天
    public static String convertTime(Integer minutes) {
        if (Objects.isNull(minutes)) {
            return null;
        }
        if (minutes < 60) {
            return minutes + "分钟";
        } else if (minutes < 60 * 24) {
            return minutes / 60 + "小时";
        } else if (minutes < 60 * 24 * 30) {
            return minutes / 60 / 24 + "天";
        } else {
            return minutes / 60 / 24 / 30 + "月";
        }
    }

}
