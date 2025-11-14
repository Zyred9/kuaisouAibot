package com.search.robots;

import cn.hutool.json.JSONUtil;
import com.search.robots.beans.initializer.InitializerHandler;
import com.search.robots.config.BotProperties;
import com.search.robots.config.MultiThreadUpdateConsumer;
import com.search.robots.handlers.AbstractHandler;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.longpolling.BotSession;
import org.telegram.telegrambots.longpolling.interfaces.LongPollingUpdateConsumer;
import org.telegram.telegrambots.longpolling.starter.AfterBotRegistration;
import org.telegram.telegrambots.longpolling.starter.SpringLongPollingBot;
import org.telegram.telegrambots.meta.api.methods.GetMe;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.User;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;
import org.telegram.telegrambots.meta.generics.TelegramClient;

import javax.annotation.Resource;
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
public class SearchRobot implements SpringLongPollingBot, MultiThreadUpdateConsumer {

    @Resource private BotProperties properties;
    @Resource private TelegramClient telegramClient;
    @Resource private InitializerHandler initializerHandler;

    @Override
    public void consume(Update update) {
        BotApiMethod<?> message = null;
        try {
            message = AbstractHandler.doExecute(update, this.properties.isLogs());
            if (Objects.nonNull(message)) {
                this.telegramClient.execute(message);
            }
        } catch (TelegramApiException e) {
            log.error("【同步消息异常】消息内容：{} \n 异常消息：{}",
                    JSONUtil.toJsonStr(message), e.getMessage(), e);
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
            User user = this.telegramClient.execute(GetMe.builder().build());
            this.initializerHandler.init(user);
            this.properties.setBotUsername(user.getUserName());
            log.info("[机器人状态] {}", botSession.isRunning());
        } catch (Exception ex) {
            log.error("初始化异常", ex);
        }
    }

}
