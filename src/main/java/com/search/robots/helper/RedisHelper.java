package com.search.robots.helper;

import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.TimeUnit;

/**
 * Redis 工具类
 *
 * @author zyred
 * @since 1.0
 */
@Component
public class RedisHelper {

    private static StringRedisTemplate template;

    public RedisHelper(StringRedisTemplate template) {
        RedisHelper.template = template;
    }


    // ==================== Key 通用操作 ====================

    /**
     * 判断 key 是否存在
     *
     * @param key 键
     * @return 是否存在
     */
    public static Boolean hasKey(String key) {
        return RedisHelper.template.hasKey(key);
    }

    /**
     * 删除 key
     *
     * @param key 键
     */
    public static void delete(String key) {
        RedisHelper.template.delete(key);
    }

    /**
     * 批量删除 key
     *
     * @param keys 键集合
     */
    public static void delete(Collection<String> keys) {
        RedisHelper.template.delete(keys);
    }

    /**
     * 设置过期时间
     *
     * @param key     键
     * @param timeout 过期时间
     * @param unit    时间单位
     * @return 是否成功
     */
    public static Boolean expire(String key, long timeout, TimeUnit unit) {
        return RedisHelper.template.expire(key, timeout, unit);
    }

    /**
     * 获取 key 的剩余过期时间（秒）
     *
     * @param key 键
     * @return 过期时间（秒），-1 表示永久有效
     */
    public static Long getExpire(String key) {
        return RedisHelper.template.getExpire(key);
    }


    // ==================== String 操作 ====================

    /**
     * 设置值
     *
     * @param key   键
     * @param value 值
     */
    public static void set(String key, String value) {
        RedisHelper.template.opsForValue().set(key, value);
    }

    /**
     * 设置值并指定过期时间
     *
     * @param key     键
     * @param value   值
     * @param timeout 过期时间
     * @param unit    时间单位
     */
    public static void setEx(String key, String value, long timeout, TimeUnit unit) {
        RedisHelper.template.opsForValue().set(key, value, timeout, unit);
    }

    /**
     * 只有在 key 不存在时才设置值
     *
     * @param key   键
     * @param value 值
     * @return 是否成功（已存在返回 false，不存在返回 true）
     */
    public static Boolean setIfAbsent(String key, String value) {
        return RedisHelper.template.opsForValue().setIfAbsent(key, value);
    }

    /**
     * 获取值
     *
     * @param key 键
     * @return 值
     */
    public static String get(String key) {
        return RedisHelper.template.opsForValue().get(key);
    }

    /**
     * 批量获取
     *
     * @param keys 键集合
     * @return 值列表
     */
    public static List<String> multiGet(Collection<String> keys) {
        return RedisHelper.template.opsForValue().multiGet(keys);
    }

    /**
     * 自增（步长为 1）
     *
     * @param key 键
     * @return 自增后的值
     */
    public static Long incr(String key) {
        return RedisHelper.template.opsForValue().increment(key, 1L);
    }

    /**
     * 自增（指定步长）
     *
     * @param key       键
     * @param increment 步长
     * @return 自增后的值
     */
    public static Long incrBy(String key, long increment) {
        return RedisHelper.template.opsForValue().increment(key, increment);
    }

    /**
     * 自减（步长为 1）
     *
     * @param key 键
     * @return 自减后的值
     */
    public static Long decr(String key) {
        return RedisHelper.template.opsForValue().decrement(key, 1L);
    }

    /**
     * 自减（指定步长）
     *
     * @param key       键
     * @param decrement 步长
     * @return 自减后的值
     */
    public static Long decrBy(String key, long decrement) {
        return RedisHelper.template.opsForValue().decrement(key, decrement);
    }


    // ==================== Hash 操作 ====================

    /**
     * 设置 Hash 字段值
     *
     * @param key   键
     * @param field 字段
     * @param value 值
     */
    public static void hPut(String key, String field, String value) {
        RedisHelper.template.opsForHash().put(key, field, value);
    }

    /**
     * 批量设置 Hash 字段值
     *
     * @param key  键
     * @param map 字段-值映射
     */
    public static void hPutAll(String key, Map<String, String> map) {
        RedisHelper.template.opsForHash().putAll(key, map);
    }

    /**
     * 获取 Hash 字段值
     *
     * @param key   键
     * @param field 字段
     * @return 值
     */
    public static String hGet(String key, String field) {
        Object value = RedisHelper.template.opsForHash().get(key, field);
        return Objects.nonNull(value) ? value.toString() : null;
    }

    /**
     * 获取 Hash 所有字段和值
     *
     * @param key 键
     * @return 字段-值映射
     */
    public static Map<Object, Object> hGetAll(String key) {
        return RedisHelper.template.opsForHash().entries(key);
    }

    /**
     * 判断 Hash 字段是否存在
     *
     * @param key   键
     * @param field 字段
     * @return 是否存在
     */
    public static Boolean hExists(String key, String field) {
        return RedisHelper.template.opsForHash().hasKey(key, field);
    }

    /**
     * 删除 Hash 字段
     *
     * @param key    键
     * @param fields 字段（可变参数）
     * @return 删除的字段数量
     */
    public static Long hDelete(String key, Object... fields) {
        return RedisHelper.template.opsForHash().delete(key, fields);
    }

    /**
     * 获取 Hash 字段数量
     *
     * @param key 键
     * @return 字段数量
     */
    public static Long hSize(String key) {
        return RedisHelper.template.opsForHash().size(key);
    }

    /**
     * 获取 Hash 所有字段名
     *
     * @param key 键
     * @return 字段名集合
     */
    public static Set<Object> hKeys(String key) {
        return RedisHelper.template.opsForHash().keys(key);
    }

    /**
     * 获取 Hash 所有值
     *
     * @param key 键
     * @return 值集合
     */
    public static List<Object> hValues(String key) {
        return RedisHelper.template.opsForHash().values(key);
    }

    /**
     * Hash 字段值自增
     *
     * @param key       键
     * @param field     字段
     * @param increment 增量
     * @return 自增后的值
     */
    public static Long hIncrBy(String key, String field, long increment) {
        return RedisHelper.template.opsForHash().increment(key, field, increment);
    }


    // ==================== Set 操作 ====================

    /**
     * 添加元素到 Set
     *
     * @param key    键
     * @param values 值（可变参数）
     * @return 添加成功的元素数量
     */
    public static Long sAdd(String key, String... values) {
        return RedisHelper.template.opsForSet().add(key, values);
    }

    /**
     * 移除 Set 元素
     *
     * @param key    键
     * @param values 值（可变参数）
     * @return 移除成功的元素数量
     */
    public static Long sRemove(String key, Object... values) {
        return RedisHelper.template.opsForSet().remove(key, values);
    }

    /**
     * 判断元素是否在 Set 中
     *
     * @param key   键
     * @param value 值
     * @return 是否存在
     */
    public static Boolean sIsMember(String key, Object value) {
        return RedisHelper.template.opsForSet().isMember(key, value);
    }

    /**
     * 获取 Set 所有元素
     *
     * @param key 键
     * @return 元素集合
     */
    public static Set<String> sMembers(String key) {
        return RedisHelper.template.opsForSet().members(key);
    }

    /**
     * 获取 Set 大小
     *
     * @param key 键
     * @return 元素数量
     */
    public static Long sSize(String key) {
        return RedisHelper.template.opsForSet().size(key);
    }

    /**
     * 随机获取 Set 中一个元素
     *
     * @param key 键
     * @return 随机元素
     */
    public static String sRandomMember(String key) {
        return RedisHelper.template.opsForSet().randomMember(key);
    }

    /**
     * 随机获取 Set 中多个元素
     *
     * @param key   键
     * @param count 数量
     * @return 随机元素列表
     */
    public static List<String> sRandomMembers(String key, long count) {
        return RedisHelper.template.opsForSet().randomMembers(key, count);
    }

    /**
     * 移除并返回 Set 中一个随机元素
     *
     * @param key 键
     * @return 被移除的元素
     */
    public static String sPop(String key) {
        return RedisHelper.template.opsForSet().pop(key);
    }

    /**
     * 获取两个 Set 的交集
     *
     * @param key      键
     * @param otherKey 另一个键
     * @return 交集元素集合
     */
    public static Set<String> sIntersect(String key, String otherKey) {
        return RedisHelper.template.opsForSet().intersect(key, otherKey);
    }

    /**
     * 获取两个 Set 的并集
     *
     * @param key      键
     * @param otherKey 另一个键
     * @return 并集元素集合
     */
    public static Set<String> sUnion(String key, String otherKey) {
        return RedisHelper.template.opsForSet().union(key, otherKey);
    }

    /**
     * 获取两个 Set 的差集
     *
     * @param key      键
     * @param otherKey 另一个键
     * @return 差集元素集合
     */
    public static Set<String> sDifference(String key, String otherKey) {
        return RedisHelper.template.opsForSet().difference(key, otherKey);
    }

    // ==================== List 操作 ====================

    /**
     * 从列表左侧（头部）添加元素
     *
     * @param key   键
     * @param value 值
     * @return 操作后列表长度
     */
    public static Long lLeftPush(String key, String value) {
        return RedisHelper.template.opsForList().leftPush(key, value);
    }

    /**
     * 批量从列表左侧（头部）添加元素
     *
     * @param key    键
     * @param values 值（可变参数）
     * @return 操作后列表长度
     */
    public static Long lLeftPushAll(String key, String... values) {
        return RedisHelper.template.opsForList().leftPushAll(key, values);
    }

    /**
     * 批量从列表左侧（头部）添加元素
     *
     * @param key    键
     * @param values 值集合
     * @return 操作后列表长度
     */
    public static Long lLeftPushAll(String key, Collection<String> values) {
        return RedisHelper.template.opsForList().leftPushAll(key, values);
    }

    /**
     * 从列表右侧（尾部）添加元素
     *
     * @param key   键
     * @param value 值
     * @return 操作后列表长度
     */
    public static Long lRightPush(String key, String value) {
        return RedisHelper.template.opsForList().rightPush(key, value);
    }

    /**
     * 批量从列表右侧（尾部）添加元素
     *
     * @param key    键
     * @param values 值（可变参数）
     * @return 操作后列表长度
     */
    public static Long lRightPushAll(String key, String... values) {
        return RedisHelper.template.opsForList().rightPushAll(key, values);
    }

    /**
     * 批量从列表右侧（尾部）添加元素
     *
     * @param key    键
     * @param values 值集合
     * @return 操作后列表长度
     */
    public static Long lRightPushAll(String key, Collection<String> values) {
        return RedisHelper.template.opsForList().rightPushAll(key, values);
    }

    /**
     * 从列表左侧（头部）弹出元素
     *
     * @param key 键
     * @return 弹出的元素
     */
    public static String lLeftPop(String key) {
        return RedisHelper.template.opsForList().leftPop(key);
    }

    /**
     * 从列表右侧（尾部）弹出元素
     *
     * @param key 键
     * @return 弹出的元素
     */
    public static String lRightPop(String key) {
        return RedisHelper.template.opsForList().rightPop(key);
    }

    /**
     * 获取列表指定范围的元素
     *
     * @param key   键
     * @param start 开始位置（0 是开始位置）
     * @param end   结束位置（-1 返回所有）
     * @return 元素列表
     */
    public static List<String> lRange(String key, long start, long end) {
        return RedisHelper.template.opsForList().range(key, start, end);
    }

    /**
     * 获取列表指定索引的元素
     *
     * @param key   键
     * @param index 索引
     * @return 元素值
     */
    public static String lIndex(String key, long index) {
        return RedisHelper.template.opsForList().index(key, index);
    }

    /**
     * 获取列表长度
     *
     * @param key 键
     * @return 列表长度
     */
    public static Long lSize(String key) {
        return RedisHelper.template.opsForList().size(key);
    }

    /**
     * 通过索引设置列表元素的值
     *
     * @param key   键
     * @param index 索引
     * @param value 值
     */
    public static void lSet(String key, long index, String value) {
        RedisHelper.template.opsForList().set(key, index, value);
    }

    /**
     * 移除列表中指定值的元素
     *
     * @param key   键
     * @param count 移除数量（0：全部，正数：从头开始，负数：从尾开始）
     * @param value 值
     * @return 移除的元素数量
     */
    public static Long lRemove(String key, long count, String value) {
        return RedisHelper.template.opsForList().remove(key, count, value);
    }

    /**
     * 裁剪列表（保留指定区间的元素）
     *
     * @param key   键
     * @param start 开始位置
     * @param end   结束位置
     */
    public static void lTrim(String key, long start, long end) {
        RedisHelper.template.opsForList().trim(key, start, end);
    }
}
