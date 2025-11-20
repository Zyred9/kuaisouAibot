package com.search.robots.handlers;

import com.search.robots.database.entity.Included;
import com.search.robots.database.service.IncludedService;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.message.Message;

import java.util.Collections;
import java.util.List;


/**
 * <p>
 * 普通群
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class PublicChatHandler extends AbstractHandler {

    private final SearchHandler searchHandler;
    private final IncludedService includedService;

    @Override
    public boolean support(Update update) {
        return update.hasMessage()
                && update.getMessage().hasText()
                && (update.getMessage().getChat().isGroupChat()
                || update.getMessage().getChat().isSuperGroupChat());
    }


    @Override
    protected BotApiMethod<?> execute(Update update) {
        Message message = update.getMessage();
        Included chat = this.includedService.get(message.getChatId());
        if (Boolean.FALSE.equals(chat.getOpenSearch())) {
            return null;
        }

        List<Long> searchChat = Collections.emptyList();
        Boolean openGlobalSearch = chat.getOpenGlobalSearch();
        if (Boolean.FALSE.equals(openGlobalSearch)) {
            searchChat = chat.getTargetedSearchIndexIds();
        }

        // openFilterMinors
        BotApiMethod<?> result = this.searchHandler.processorChatSearch(message, chat.getOpenFilterMinors(), searchChat);
        if (Boolean.TRUE.equals(chat.getOpenPrivacySearch())) {
            AsyncSender.async(delete(message));
        }
        return result;
    }
}
