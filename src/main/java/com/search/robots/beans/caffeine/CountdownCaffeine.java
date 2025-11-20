package com.search.robots.beans.caffeine;

import cn.hutool.core.lang.UUID;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.Expiry;
import com.github.benmanes.caffeine.cache.Scheduler;
import com.search.robots.beans.view.caffeine.Task;
import com.search.robots.database.enums.caffeine.TaskNode;
import lombok.SneakyThrows;
import org.checkerframework.checker.index.qual.NonNegative;
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Component
public class CountdownCaffeine extends Thread {

    private static Cache<String, Task> DESTORY_CACHE;

    public CountdownCaffeine(ExpireListener listener) {
        DESTORY_CACHE = Caffeine.newBuilder()
                .initialCapacity(100)
                .expireAfter(new DynamicExpire())
                .removalListener(listener)
                .scheduler(Scheduler.systemScheduler())
                .maximumSize(10_000_000)
                .build();
        super.start();
    }

    public static void set (Task task) {
        String key = UUID.fastUUID().toString(true);
        DESTORY_CACHE.put(key, task);
    }

    @Override
    @SneakyThrows
    public void run() {
        while (!Thread.interrupted()) {
            TimeUnit.SECONDS.sleep(1);
            DESTORY_CACHE.cleanUp();
        }
    }

    private static class DynamicExpire implements Expiry<String, Task> {

        @Override
        public long expireAfterCreate(String messageId, Task expire, long currentTime) {
            if (Objects.equals(expire.getNode(), TaskNode.RECHARGE)) {
                return expire.getNode().getTimeUnit().toNanos(expire.getNode().getLoop());
            }
            if (Objects.isNull(expire.getUnit())) {
                return expire.getNode().getTimeUnit().toNanos(expire.getDays().getCode());
            } else {
                return expire.getUnit().toNanos(expire.getMinutes());
            }
        }

        @Override
        public long expireAfterUpdate(String messageId, Task expire, long currentTime, @NonNegative long currentDuration) {
            return currentDuration;
        }

        @Override
        public long expireAfterRead(String messageId, Task expire, long currentTime, @NonNegative long currentDuration) {
            return currentDuration;
        }
    }
}
