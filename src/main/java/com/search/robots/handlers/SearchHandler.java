package com.search.robots.handlers;


import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.config.BotProperties;
import com.search.robots.database.enums.content.SortEnum;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.database.service.AdvUserService;
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
    private final AdvUserService advUserService;

    @Override
    public boolean support(Update update) {
        return false;
    }

    @Override
    protected BotApiMethod<?> execute(Update update) {
        return null;
    }

    /**
     * 输入的搜索
     *
     * @param message       消息体
     * @return              结果
     */
    public BotApiMethod<?> processorDefaultSearch(Message message) {
        StringBuilder sb = new StringBuilder();
        String advText = this.advUserService.buildCurrent(message.getText());
        if (StrUtil.isNotEmpty(advText)) {
            sb.append(advText).append("~\n");
        }
        Page<SearchBean> searchResult = this.searchService.search(message.getText(), null, 0, null);
        if (!searchResult.isEmpty()) {
            searchResult.forEach(a -> sb.append(a.buildLineText()));
        } else {
            sb.append("关键词暂未收录\n");
        }
        InlineKeyboardMarkup markup = KeyboardHelper.buildSearchResultKeyboard(
                "", 0, false, SortEnum.EMPTY,
                this.properties.getBotUsername(), searchResult,
                message.getText()
        );
        return markdownV2(message, sb.toString(), markup);
    }


    /**
     * 键盘点击回调
     *
     * @param callbackQuery     回调
     * @param message           消息
     * @param command           命令列表
     * @return                  编辑结果
     */
    public BotApiMethod<?> processorCallbackSearch(CallbackQuery callbackQuery,
                                                   Message message, List<String> command) {

        Integer messageDate = message.getDate();
        long currentTimestamp = System.currentTimeMillis() / 1000;
        long messageAge = currentTimestamp - messageDate;
        if (messageAge > 600) {
            return answerAlert(callbackQuery, "⚠️消息已过期，请重新发送关键词");
        }

        SourceTypeEnum sourceType = SourceTypeEnum.fromCode(command.get(1));
        int current = Integer.parseInt(command.get(2));
        Boolean filter = Boolean.valueOf(command.get(3));
        SortEnum sort = SortEnum.of(command.get(4));
        String keyword = command.get(5);


        StringBuilder sb = new StringBuilder();
        Page<SearchBean> search = this.searchService.search(keyword, sourceType, current, sort);
        if (!search.isEmpty()) {
            search.forEach(a -> sb.append(a.buildLineText()));
        } else {
            sb.append("关键词暂未收录\n");
        }

        InlineKeyboardMarkup markup = KeyboardHelper.buildSearchResultKeyboard(
                command.get(1), current, filter, sort, this.properties.getBotUsername(), search, keyword
        );
        return editMarkdownV2(message, sb.toString(), markup);
    }


}
