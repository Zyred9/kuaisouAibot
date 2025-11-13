package com.search.robots.handlers;


import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.config.BotProperties;
import com.search.robots.database.enums.content.SortEnum;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.database.service.SearchService;
import com.search.robots.helper.KeyboardHelper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.CallbackQuery;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;

import java.util.List;

/**
 *
 *
 * @author zyred
 * @since 2025/11/12 20:46
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class SearchHandler extends AbstractHandler {

    private final BotProperties properties;
    private final SearchService searchService;

    @Override
    public boolean support(Update update) {
        return false;
    }

    @Override
    protected BotApiMethod<?> execute(Update update) {
        return null;
    }

    public BotApiMethod<?> processorSearch(CallbackQuery callbackQuery,
                                           Message message, List<String> command) {

        Integer messageDate = message.getDate();
        long currentTimestamp = System.currentTimeMillis() / 1000;
        long messageAge = currentTimestamp - messageDate;
        if (messageAge > 600) {
            return answerAlert(callbackQuery, "⚠️消息已过期，请重新发送关键词");
        }

        // search#0#0#false#100#安卓
        // "search", type.getCode(), current, filter, sort.getCode(), keyword
        SourceTypeEnum sourceType = SourceTypeEnum.fromCode(command.get(1));
        int current = Integer.parseInt(command.get(2));
        Boolean filter = Boolean.valueOf(command.get(3));
        SortEnum sort = SortEnum.of(command.get(4));


        StringBuilder sb = new StringBuilder();
        Page<SearchBean> search = this.searchService.search(command.get(5), sourceType, current);
        if (!search.isEmpty()) {
            search.forEach(a -> sb.append(a.buildLineText()));
        } else {
            sb.append("关键词暂未收录\n");
        }

        InlineKeyboardMarkup markup = KeyboardHelper.buildSearchResultKeyboard(
                command.get(1), current, filter, sort, this.properties.getBotUsername(), search, command.get(5)
        );
        return editMarkdownV2(message, sb.toString(), markup);
    }

}
