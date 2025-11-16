package com.search.robots.beans.caffeine;

import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.RemovalListener;
import com.search.robots.beans.view.caffeine.Task;
import com.search.robots.database.enums.caffeine.TaskNode;
import com.search.robots.handlers.AbstractHandler;
import com.search.robots.handlers.Trc20Handler;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.Update;

import java.util.Objects;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class ExpireListener extends AbstractHandler implements RemovalListener<String, Task> {

    private final Trc20Handler trc20Handler;

    @Override
    public boolean support(Update update) {return false;}
    @Override
    protected BotApiMethod<?> execute(Update update) {return null;}

    @Override
    public void onRemoval(@Nullable String key, @Nullable Task expire, RemovalCause cause) {
        if (Objects.isNull(expire) || !RemovalCause.EXPIRED.equals(cause)) {
            return;
        }

        boolean reset = false;
        try {
            TaskNode node = expire.getNode();
            if (Objects.equals(TaskNode.RECHARGE, node)) {
                reset = this.trc20Handler.processorListenAddress(expire);
            }
        } catch (Exception ex) {
            log.error("[caffeine监听] 错误原因：{}", ex.getMessage(), ex);
        } finally {
            if (reset) {
                CountdownCaffeine.set(expire);
            }
        }
    }
}
