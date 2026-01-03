package com.search.robots;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.search.robots.beans.initializer.InitializerHandler;
import com.search.robots.config.BotProperties;
import com.search.robots.config.MultiThreadUpdateConsumer;
import com.search.robots.handlers.AbstractHandler;
import com.search.robots.helper.RedisHelper;
import com.search.robots.helper.StrHelper;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.longpolling.BotSession;
import org.telegram.telegrambots.longpolling.interfaces.LongPollingUpdateConsumer;
import org.telegram.telegrambots.longpolling.starter.AfterBotRegistration;
import org.telegram.telegrambots.longpolling.starter.SpringLongPollingBot;
import org.telegram.telegrambots.meta.api.methods.GetMe;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.PhotoSize;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.User;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;
import org.telegram.telegrambots.meta.generics.TelegramClient;

import javax.annotation.Resource;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import com.search.robots.config.Constants;


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
public class SearchRobot implements SpringLongPollingBot, MultiThreadUpdateConsumer {

    public static boolean processor = true;

    @Resource private BotProperties properties;
    @Resource private TelegramClient telegramClient;
    @Resource private InitializerHandler initializerHandler;

    @Override
    public void consume(Update update) {
        BotApiMethod<?> message = null;
        if (this.properties.isLogs()) {
            log.info(JSONUtil.toJsonStr(update));
        }
        try {
            if (processor) {
                message = AbstractHandler.doExecute(update, this.properties.isLogs());
            } else {
                if (update.hasMessage() && update.getMessage().hasText()) {
                    if (StrUtil.equals(update.getMessage().getText(), Constants.RESTART_PROCESSOR)) {
                        processor = !processor;
                        RedisHelper.set(StrHelper.getProcessor(), String.valueOf(processor));
                    }
                }
            }
            if (Objects.nonNull(message)) {
                this.telegramClient.execute(message);
            }
        } catch (TelegramApiException e) {
            log.error("【同步消息异常】异常消息：{} \n  进入消息{} \n  响应消息：{} \n ",
                    e.getMessage(), JSONUtil.toJsonStr(update), JSONUtil.toJsonStr(message));
        }
    }


    @Override
    public String getBotToken() {
        return this.properties.getToken();
    }

    @Override
    public LongPollingUpdateConsumer getUpdatesConsumer() {
        return this;
    }


    @SneakyThrows
    @SuppressWarnings("all")
    @AfterBotRegistration
    public void afterRegistration(BotSession botSession) {
        try {
            String val = RedisHelper.get(StrHelper.getProcessor());
            if (StrUtil.isEmpty(val)) {
                processor = true;
                RedisHelper.set(StrHelper.getProcessor(), String.valueOf(processor));
            } else {
                processor = Boolean.parseBoolean(val);
            }
            User user = this.telegramClient.execute(GetMe.builder().build());
            this.initializerHandler.init(user);
            log.info("[机器人状态] {}", botSession.isRunning());
        } catch (Exception ex) {
            log.error("Bot初始化异常，但不影响Web服务启动", ex);
            // 不要调用 System.exit(0)，否则会导致整个应用退出，Web服务无法启动
            // 如果需要关闭Bot但保持Web服务运行，可以在这里设置标志位
            processor = false;
        }
    }

}
