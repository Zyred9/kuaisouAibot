package com.search.robots.handlers;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.view.AsyncBean;
import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.config.BotProperties;
import com.search.robots.database.entity.AdvUser;
import com.search.robots.database.enums.content.SortEnum;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.database.service.AdvUserService;
import com.search.robots.database.service.HotSearchService;
import com.search.robots.database.service.SearchService;
import com.search.robots.helper.KeyboardHelper;
import com.search.robots.helper.StrHelper;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
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
    private final HotSearchService hotSearchService;

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
    public BotApiMethod<?> processorChatSearch(Message message, Boolean filter, List<Long> chatIds) {
        return this.doSearch(message, "", message.getText(), null,
                0, SortEnum.EMPTY, filter, true, chatIds);
    }

    /**
     * 输入的搜索
     *
     * @param message       消息体
     * @return              结果
     */
    public BotApiMethod<?> processorDefaultSearch(Message message) {
        return this.doSearch(message, "", message.getText(), null,
                0, SortEnum.EMPTY, Boolean.FALSE, true, null);
    }

    /**
     * start 链接
     *
     * @param message   消息体
     * @return          新消息
     */
    public BotApiMethod<?> processorStartSearch(Message message, String decode, boolean send) {
        return this.doSearch(message, "", decode, null,
                0, SortEnum.EMPTY, Boolean.FALSE, send, null);
    }


    /**
     * 键盘点击回调
     *
     * @param message           消息
     * @param command           命令列表
     * @return                  编辑结果
     */
    public BotApiMethod<?> processorCallbackSearch(Message message, List<String> command) {
        SourceTypeEnum sourceType = SourceTypeEnum.fromCode(command.get(1));
        int current = Integer.parseInt(command.get(2));
        Boolean filter = Boolean.valueOf(command.get(3));
        SortEnum sort = SortEnum.of(command.get(4));
        String keyword = command.get(5);
        return this.doSearch(message, command.get(1), keyword, sourceType,
                current, sort, filter, false, null);
    }



    private BotApiMethod<?> doSearch(Message message, String hitType, String keyword, SourceTypeEnum sourceType,
                                     int current, SortEnum sort, Boolean filter, boolean send, List<Long> chatIds) {

        StringBuilder sb = new StringBuilder();

        // 定向搜索（HTML）
        if (chatIds != null) {
            sb.append("\uD83D\uDD14 关键词：<b>").append(keyword).append("</b>\n");
            if (CollUtil.isNotEmpty(chatIds)) {
                long count = this.searchService.countSource(chatIds);
                sb.append("\uD83D\uDD14 \"当前为定向搜索：结果来自\"")
                  .append(chatIds.size()).append("个频道/群组，共")
                  .append(count).append("个资源。").append("\n");
            }
        }

        // 头部的广告或者关键词
        String advText = this.advUserService.buildCurrent(message.getText());
        if (StrUtil.isNotEmpty(advText)) {
            sb.append(advText).append("\n");
        }

        boolean hasButton = false;
        // 数据的查询
        Page<SearchBean> search = this.searchService.search(keyword, sourceType, current, sort, chatIds, filter);
        if (!search.isEmpty()) {
            search.forEach(a -> sb.append(a.buildLineHtmlText(keyword)));
            if (current != 0) {
                sb.append("\uD83D\uDC47点击筛选类型，当前【第").append(current + 1).append("页】");
            } else {
                if (Boolean.TRUE.equals(filter)) {
                    sb.append("<b>已过滤因色情被系统限制访问的链接</b>");
                } else {
                    String hottest = this.hotSearchService.hottest();
                    if (StrUtil.isNotBlank(hottest)) {
                        sb.append(hottest);
                    }
                }
            }
            AsyncTaskHandler.async(AsyncBean.kw(keyword));
            AsyncTaskHandler.async(AsyncBean.exposure(search));
            hasButton = true;
        } else {
            sb.append("关键词暂未收录\n");
        }
        // 底部按钮的处理
        AdvUser buttonAdv = this.advUserService.buttonAdv();
        InlineKeyboardMarkup markup = KeyboardHelper.buildSearchResultKeyboard(
                hitType, current, filter, sort, this.properties.getBotUsername(), search, keyword, buttonAdv, hasButton
        );

        return send ? html(message, sb.toString(), markup) : editHtml(message, sb.toString(), markup);
    }


}
