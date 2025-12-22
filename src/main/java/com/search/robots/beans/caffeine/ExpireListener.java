package com.search.robots.beans.caffeine;

import cn.hutool.core.util.StrUtil;
import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.RemovalListener;
import com.search.robots.beans.view.caffeine.Task;
import com.search.robots.database.entity.Config;
import com.search.robots.database.entity.Included;
import com.search.robots.database.enums.caffeine.TaskNode;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.IncludedService;
import com.search.robots.handlers.AbstractHandler;
import com.search.robots.handlers.Trc20Handler;
import com.search.robots.helper.KeyboardHelper;
import com.search.robots.sender.AsyncSender;
import com.search.robots.sender.SyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.methods.pinnedmessages.PinChatMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;

import java.time.LocalDateTime;
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
    private final ConfigService configService;
    private final IncludedService includedService;

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

            if (Objects.equals(TaskNode.REUSE_RECHARGE, node)) {
                reset = this.trc20Handler.processorReuseListenAddress(expire);
            }

            if (Objects.equals(TaskNode.EVERY_ADV, node)) {
                this.processorEveryAdv(expire.getChatId());
            }
        } catch (Exception ex) {
            log.error("[caffeine监听] 错误原因：{}", ex.getMessage(), ex);
        } finally {
            if (reset) {
                CountdownCaffeine.set(expire);
            }
        }
    }

    /**
     * 处理每天的广告
     *
     * @param expire    过期得对象
     * @return          结果
     */
    public void processorEveryAdv(Long chatId) {
        if (Objects.isNull(chatId)) {
            return;
        }

        Config config = this.configService.queryConfig();
        InlineKeyboardMarkup keyboard = KeyboardHelper.keyboard(config.getHelpfulPopularizeKeyboard());
        Message send = null;
        if (StrUtil.isAllNotBlank(config.getHelpfulPopularizeMarkdown(),
                config.getHelpfulPopularizeFileId())) {
            send = SyncSender.send(photoMarkdownV2(chatId, config.getHelpfulPopularizeFileId(),
                    config.getHelpfulPopularizeMarkdown(), keyboard));
        }
        if (StrUtil.isEmpty(config.getHelpfulPopularizeFileId())
                && StrUtil.isNotEmpty(config.getHelpfulPopularizeMarkdown())) {
            send = SyncSender.send(markdownV2(chatId, config.getHelpfulPopularizeMarkdown(), keyboard));
        }
        if (Objects.isNull(send)) {
            return;
        }

        Included included = this.includedService.get(chatId);
        if (Objects.nonNull(included)) {
            included.buildEveryAdv();
        }
        included.setSendNewTime(LocalDateTime.now());
        this.includedService.updateSelf(included);
        AsyncSender.async(
                PinChatMessage.builder()
                        .chatId(chatId)
                        .messageId(send.getMessageId())
                        .build()
        );
    }
}
