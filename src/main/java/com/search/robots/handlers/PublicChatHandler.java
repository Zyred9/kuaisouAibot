package com.search.robots.handlers;

import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.database.entity.Included;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.database.service.IncludedService;
import com.search.robots.database.service.SearchService;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.message.Message;

import java.util.Collections;
import java.util.List;
import java.util.Objects;


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
    private final SearchService searchService;

    @Override
    public boolean support(Update update) {
        return update.hasMessage()
                && (update.getMessage().getChat().isGroupChat()
                || update.getMessage().getChat().isSuperGroupChat());
    }


    @Override
    protected BotApiMethod<?> execute(Update update) {
        Message message = update.getMessage();
        if (message.getFrom().getIsBot()) {
            return null;
        }
        Included chat = this.includedService.get(message.getChatId());

        // 如果message是text类型，则判断群组是否开启搜索功能
        // 如果是其他类型，判断是否开启内容搜集，如果有收集则需要保存到 es 中
        if (message.hasText() && Boolean.TRUE.equals(chat.getOpenSearch())) {
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

        // 开启监听
        if (Boolean.TRUE.equals(chat.getOpenListen())) {
            SearchBean bean = this.buildSearchBean(message, chat);
            if (Objects.nonNull(bean)) {
                this.searchService.save(bean);
            }
        }

        return null;
    }

    private SearchBean buildSearchBean(Message message, Included chat) {
        if (message.hasText()) {
            return null;
        }

        SourceTypeEnum type = null;
        Integer times = null;

        if (message.hasVideo() && Objects.nonNull(message.getVideo())) {
            type = SourceTypeEnum.VIDEO;
            times = message.getVideo().getDuration();
        } else if (message.hasAnimation() && Objects.nonNull(message.getAnimation())) {
            type = SourceTypeEnum.VIDEO;
            times = message.getAnimation().getDuration();
        } else if ((message.hasAudio() && Objects.nonNull(message.getAudio()))
                || (message.hasVoice() && Objects.nonNull(message.getVoice()))) {
            type = SourceTypeEnum.AUDIO;
            if (message.getAudio() != null) {
                times = message.getAudio().getDuration();
            } else if (message.getVoice() != null) {
                times = message.getVoice().getDuration();
            }
        } else if (message.hasPhoto() && Objects.nonNull(message.getPhoto())) {
            type = SourceTypeEnum.PHOTO;
        } else if (message.hasDocument() && Objects.nonNull(message.getDocument())) {
            type = SourceTypeEnum.FILE;
        }

        if (Objects.isNull(type)) {
            return null;
        }

        String caption = StrUtil.emptyToDefault(message.getCaption(), "");
        String indexTitle = StrUtil.emptyToDefault(chat.getIndexTitle(), "");
        String sourceName = StrUtil.isNotBlank(caption) ? caption : indexTitle;

        String channelUsername = chat.getIndexUsername();
        String channelUrl = "";
        if (StrUtil.isNotBlank(channelUsername)) {
            channelUrl = "https://t.me/" + channelUsername;
        }
        String sourceUrl = channelUrl;
        if (StrUtil.isNotBlank(channelUrl) && Objects.nonNull(message.getMessageId())) {
            sourceUrl = channelUrl + "/" + message.getMessageId();
        }

        long collectTime = Objects.nonNull(message.getDate())
                ? message.getDate() * 1000L
                : System.currentTimeMillis();

        SearchBean bean = new SearchBean()
                .setType(type)
                .setSourceName(sourceName)
                .setSourceUrl(sourceUrl)
                .setChannelName(indexTitle)
                .setChannelUsername(channelUsername)
                .setChannelUrl(channelUrl)
                .setSubscribers(Objects.nonNull(chat.getNumber()) ? String.valueOf(chat.getNumber()) : null)
                .setChatId(chat.getId())
                .setMessageId(Objects.nonNull(message.getMessageId()) ? message.getMessageId().longValue() : null)
                .setCollectTime(collectTime)
                .setTimes(times)
                .setViews(0)
                .setMarked(Boolean.FALSE);

        return bean;
    }
}
