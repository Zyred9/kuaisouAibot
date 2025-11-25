package com.search.robots.handlers;


import cn.hutool.core.util.StrUtil;
import com.search.robots.database.entity.Config;
import com.search.robots.database.entity.User;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.UserService;
import com.search.robots.helper.KeyboardHelper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.AnswerInlineQuery;
import org.telegram.telegrambots.meta.api.methods.ParseMode;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.inlinequery.InlineQuery;
import org.telegram.telegrambots.meta.api.objects.inlinequery.inputmessagecontent.InputTextMessageContent;
import org.telegram.telegrambots.meta.api.objects.inlinequery.result.InlineQueryResult;
import org.telegram.telegrambots.meta.api.objects.inlinequery.result.InlineQueryResultArticle;
import org.telegram.telegrambots.meta.api.objects.inlinequery.result.cached.InlineQueryResultCachedPhoto;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 *
 *
 * @author zyred
 * @since 2025/11/24 18:49
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class InlineQueryHandler extends AbstractHandler{


    private final UserService userService;
    private final ConfigService configService;

    @Override
    public boolean support(Update update) {
        return Objects.nonNull(update.getInlineQuery());
    }

    @Override
    protected BotApiMethod<?> execute(Update update) {
        InlineQuery inlineQuery = update.getInlineQuery();
        Long userId = inlineQuery.getFrom().getId();
        User user = this.userService.select(userId);

        Config config = this.configService.queryConfig();
        List<InlineQueryResult> results = new ArrayList<>();
        AnswerInlineQuery.AnswerInlineQueryBuilder<?, ?> builder = AnswerInlineQuery.builder();

        // 1. 邀请用户（文本卡片，带图片消息）
        results.add(InlineQueryResultArticle.builder()
                .inputMessageContent(
                        InputTextMessageContent.builder()
                                .messageText(config.getHelpfulPopularizeMarkdown())
                                .parseMode(ParseMode.MARKDOWN)
                                .disableWebPagePreview(true)
                                .build()
                )
                .title("邀请用户")
                .replyMarkup(KeyboardHelper.keyboard(config.getHelpfulPopularizeKeyboard()))
                .id(inlineQuery.getId() + "_invite_user")
                .description("点击分享你的专属链接给用户")
                .build());

        // 2. 邀请广告主（文本卡片）
        results.add(InlineQueryResultArticle.builder()
                .inputMessageContent(
                        InputTextMessageContent.builder()
                                .messageText(config.getInlineModelAdvMarkdown())  // 广告文案内容
                                .parseMode(ParseMode.MARKDOWN)
                                .disableWebPagePreview(true)
                                .build()
                )
                .title("邀请广告主")
                .replyMarkup(KeyboardHelper.keyboard(config.getInlineModelAdvKeyboard()))  // 广告文案按钮
                .id(inlineQuery.getId() + "_invite_advertiser")  // 唯一ID
                .description("点击分享你的专属链接给广告主")
                .build()
        );

        // 3. 邀请频道主（文本卡片）
        results.add(InlineQueryResultArticle.builder()
                .inputMessageContent(
                        InputTextMessageContent.builder()
                                .messageText(config.getInlineModelChannelMarkdown())  // 提交记录文本
                                .parseMode(ParseMode.MARKDOWN)
                                .disableWebPagePreview(true)
                                .build()
                )
                .title("邀请频道主")
                .replyMarkup(KeyboardHelper.keyboard(config.getInlineModelChannelKeyboard()))  // 提交记录按钮
                .id(inlineQuery.getId() + "_invite_channel_owner")  // 唯一ID
                .description("点击邀请频道主")
                .build()
        );

        return builder.results(results)
                .cacheTime(0)
                .inlineQueryId(inlineQuery.getId())
                .build();
    }
}
