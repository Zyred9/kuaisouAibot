package com.search.robots.helper;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.search.robots.config.SelfException;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;

/**
 * 断言工具类
 * <pre>
 * 提供各种数据类型的断言判断
 * 断言失败时抛出 SelfException 异常
 * 
 * 使用示例:
 * Assert.notNull(user, "用户不存在");
 * Assert.isTrue(age > 18, "年龄必须大于18岁");
 * Assert.notBlank(username, "用户名不能为空");
 * Assert.notEmpty(list, "列表不能为空");
 * </pre>
 *
 * @author zyred
 * @since 2025/11/9 16:51
 */
public class Assert {

    // ==================== 对象判断 ====================

    /**
     * 断言对象不为null
     *
     * @param obj     待检查对象
     * @param message 异常信息
     * @throws SelfException 对象为null时抛出
     */
    public static void notNull(Object obj, String message) {
        if (Objects.isNull(obj)) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言对象不为null（自定义错误码）
     *
     * @param obj     待检查对象
     * @param code    错误码
     * @param message 异常信息
     * @throws SelfException 对象为null时抛出
     */
    public static void notNull(Object obj, int code, String message) {
        if (Objects.isNull(obj)) {
            throw new SelfException(code, message);
        }
    }

    /**
     * 断言对象为null
     *
     * @param obj     待检查对象
     * @param message 异常信息
     * @throws SelfException 对象不为null时抛出
     */
    public static void isNull(Object obj, String message) {
        if (Objects.isNull(obj)) {
            throw new SelfException(message);
        }
    }

    // ==================== 布尔判断 ====================

    /**
     * 断言表达式为true
     *
     * @param expression 布尔表达式
     * @param message    异常信息
     * @throws SelfException 表达式为false时抛出
     */
    public static void isTrue(boolean expression, String message) {
        if (!expression) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言表达式为true（自定义错误码）
     *
     * @param expression 布尔表达式
     * @param code       错误码
     * @param message    异常信息
     * @throws SelfException 表达式为false时抛出
     */
    public static void isTrue(boolean expression, int code, String message) {
        if (!expression) {
            throw new SelfException(code, message);
        }
    }

    /**
     * 断言表达式为false
     *
     * @param expression 布尔表达式
     * @param message    异常信息
     * @throws SelfException 表达式为true时抛出
     */
    public static void isFalse(boolean expression, String message) {
        if (expression) {
            throw new SelfException(message);
        }
    }

    // ==================== 字符串判断 ====================

    /**
     * 断言字符串不为空（不为null且不为空字符串）
     *
     * @param str     待检查字符串
     * @param message 异常信息
     * @throws SelfException 字符串为空时抛出
     */
    public static void notEmpty(String str, String message) {
        if (StrUtil.isEmpty(str)) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言字符串不为空白（不为null且不为空白字符串）
     *
     * @param str     待检查字符串
     * @param message 异常信息
     * @throws SelfException 字符串为空白时抛出
     */
    public static void notBlank(String str, String message) {
        if (StrUtil.isBlank(str)) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言字符串不为空白（自定义错误码）
     *
     * @param str     待检查字符串
     * @param code    错误码
     * @param message 异常信息
     * @throws SelfException 字符串为空白时抛出
     */
    public static void notBlank(String str, int code, String message) {
        if (StrUtil.isBlank(str)) {
            throw new SelfException(code, message);
        }
    }

    // ==================== 集合判断 ====================

    /**
     * 断言集合不为空
     *
     * @param collection 待检查集合
     * @param message    异常信息
     * @throws SelfException 集合为空时抛出
     */
    public static void notEmpty(Collection<?> collection, String message) {
        if (CollUtil.isEmpty(collection)) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言集合不为空（自定义错误码）
     *
     * @param collection 待检查集合
     * @param code       错误码
     * @param message    异常信息
     * @throws SelfException 集合为空时抛出
     */
    public static void notEmpty(Collection<?> collection, int code, String message) {
        if (CollUtil.isEmpty(collection)) {
            throw new SelfException(code, message);
        }
    }

    /**
     * 断言Map不为空
     *
     * @param map     待检查Map
     * @param message 异常信息
     * @throws SelfException Map为空时抛出
     */
    public static void notEmpty(Map<?, ?> map, String message) {
        if (CollUtil.isEmpty(map)) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言数组不为空
     *
     * @param array   待检查数组
     * @param message 异常信息
     * @throws SelfException 数组为空时抛出
     */
    public static void notEmpty(Object[] array, String message) {
        if (Objects.isNull(array) || array.length == 0) {
            throw new SelfException(message);
        }
    }

    // ==================== 数值判断 ====================

    /**
     * 断言整数大于0
     *
     * @param value   待检查整数
     * @param message 异常信息
     * @throws SelfException 整数小于等于0时抛出
     */
    public static void isPositive(Integer value, String message) {
        if (Objects.isNull(value) || value <= 0) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言长整数大于0
     *
     * @param value   待检查长整数
     * @param message 异常信息
     * @throws SelfException 长整数小于等于0时抛出
     */
    public static void isPositive(Long value, String message) {
        if (Objects.isNull(value) || value <= 0L) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言整数大于等于0
     *
     * @param value   待检查整数
     * @param message 异常信息
     * @throws SelfException 整数小于0时抛出
     */
    public static void isNotNegative(Integer value, String message) {
        if (Objects.isNull(value) || value < 0) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言长整数大于等于0
     *
     * @param value   待检查长整数
     * @param message 异常信息
     * @throws SelfException 长整数小于0时抛出
     */
    public static void isNotNegative(Long value, String message) {
        if (Objects.isNull(value) || value < 0L) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言整数在指定范围内（包含边界）
     *
     * @param value   待检查整数
     * @param min     最小值
     * @param max     最大值
     * @param message 异常信息
     * @throws SelfException 整数不在范围内时抛出
     */
    public static void inRange(Integer value, int min, int max, String message) {
        if (Objects.isNull(value) || value < min || value > max) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言长整数在指定范围内（包含边界）
     *
     * @param value   待检查长整数
     * @param min     最小值
     * @param max     最大值
     * @param message 异常信息
     * @throws SelfException 长整数不在范围内时抛出
     */
    public static void inRange(Long value, long min, long max, String message) {
        if (Objects.isNull(value) || value < min || value > max) {
            throw new SelfException(message);
        }
    }

    // ==================== 相等性判断 ====================

    /**
     * 断言两个对象相等
     *
     * @param obj1    对象1
     * @param obj2    对象2
     * @param message 异常信息
     * @throws SelfException 对象不相等时抛出
     */
    public static void equals(Object obj1, Object obj2, String message) {
        if (!Objects.equals(obj1, obj2)) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言两个对象不相等
     *
     * @param obj1    对象1
     * @param obj2    对象2
     * @param message 异常信息
     * @throws SelfException 对象相等时抛出
     */
    public static void notEquals(Object obj1, Object obj2, String message) {
        if (Objects.equals(obj1, obj2)) {
            throw new SelfException(message);
        }
    }

    // ==================== 字符串长度判断 ====================

    /**
     * 断言字符串长度在指定范围内
     *
     * @param str        待检查字符串
     * @param minLength  最小长度
     * @param maxLength  最大长度
     * @param message    异常信息
     * @throws SelfException 字符串长度不在范围内时抛出
     */
    public static void lengthInRange(String str, int minLength, int maxLength, String message) {
        if (StrUtil.isBlank(str) || str.length() < minLength || str.length() > maxLength) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言字符串最小长度
     *
     * @param str        待检查字符串
     * @param minLength  最小长度
     * @param message    异常信息
     * @throws SelfException 字符串长度小于最小长度时抛出
     */
    public static void minLength(String str, int minLength, String message) {
        if (StrUtil.isBlank(str) || str.length() < minLength) {
            throw new SelfException(message);
        }
    }

    /**
     * 断言字符串最大长度
     *
     * @param str        待检查字符串
     * @param maxLength  最大长度
     * @param message    异常信息
     * @throws SelfException 字符串长度大于最大长度时抛出
     */
    public static void maxLength(String str, int maxLength, String message) {
        if (StrUtil.isNotBlank(str) && str.length() > maxLength) {
            throw new SelfException(message);
        }
    }
}
