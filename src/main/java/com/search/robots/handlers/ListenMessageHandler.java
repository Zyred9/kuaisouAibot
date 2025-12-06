package com.search.robots.handlers;

import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.config.BotProperties;
import com.search.robots.database.entity.Config;
import com.search.robots.database.entity.Included;
import com.search.robots.database.entity.User;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.IncludedService;
import com.search.robots.database.service.RewardRecordService;
import com.search.robots.database.service.SearchService;
import com.search.robots.database.service.UserService;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.message.Message;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Objects;


/**
 * <p>
 *  监听群组、频道的各种消息
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class ListenMessageHandler extends AbstractHandler {

    private final UserService userService;
    private final BotProperties properties;
    private final ConfigService configService;
    private final SearchHandler searchHandler;
    private final SearchService searchService;
    private final IncludedService includedService;
    private final RewardRecordService rewardRecordService;

    @Override
    public boolean support(Update update) {
        return update.hasMessage()
                && (!Objects.equals(this.properties.getNotifyChatId(), update.getMessage().getChatId()))
                && (update.getMessage().getChat().isGroupChat()
                || update.getMessage().getChat().isSuperGroupChat()
                || update.getMessage().getChat().isChannelChat());
    }


    @Override
    protected BotApiMethod<?> execute(Update update) {
        Message message = update.getMessage();
        // 过滤机器人消息
        if (message.getFrom() != null && message.getFrom().getIsBot()) {
            return null;
        }
        
        Included chat = this.includedService.get(message.getChatId());
        // 如果群组/频道未收录，不处理
        if (chat == null) {
            return null;
        }

        // 如果message是text类型，则判断群组是否开启搜索功能
        if (message.hasText() && Boolean.TRUE.equals(chat.getOpenSearch())) {
            List<Long> searchChat = Collections.emptyList();
            Boolean openGlobalSearch = chat.getOpenGlobalSearch();
            if (Boolean.FALSE.equals(openGlobalSearch)) {
                searchChat = chat.getTargetedSearchIndexIds();
            }
            // openFilterMinors
            BotApiMethod<?> result = this.searchHandler.processorChatSearch(message, chat.getOpenFilterMinors(), searchChat);
            
            // 群搜索奖励：给群主发放奖励
            this.processGroupSearchReward(chat, message.getFrom().getId());
            
            if (Boolean.TRUE.equals(chat.getOpenPrivacySearch())) {
                AsyncSender.async(delete(message));
            }
            return result;
        }

        // 开启监听：监听所有非文本消息（视频、图片、音频、文件等）
        if (Boolean.TRUE.equals(chat.getOpenListen())) {
            SearchBean bean = this.buildSearchBean(message, chat);
            if (Objects.nonNull(bean)) {
                this.searchService.save(bean);
            }
        }

        return null;
    }

    /**
     * 将消息组装成SearchBean
     * 收录规则：
     * - 视频：必须有caption，否则不收录
     * - 动画：必须有caption，否则不收录
     * - 音频：使用音频文件名作为sourceName
     * - 语音：不收录
     * - 图片：必须有caption，否则不收录
     * - 文件：使用文件名作为sourceName
     * 支持：公开群组、私密群组、公开频道、私密频道
     */
    private SearchBean buildSearchBean(Message message, Included chat) {
        // 只监听非文本消息
        if (message.hasText()) {
            return null;
        }

        SourceTypeEnum type = null;
        Integer times = null;
        String sourceName = null;
        String caption = StrUtil.emptyToDefault(message.getCaption(), "");

        // 判断消息类型并构建资源名称
        if (message.hasVideo() && Objects.nonNull(message.getVideo())) {
            // 视频：必须有caption
            if (StrUtil.isBlank(caption)) {
                return null;
            }
            type = SourceTypeEnum.VIDEO;
            times = message.getVideo().getDuration();
            sourceName = caption;
            
        } else if (message.hasAnimation() && Objects.nonNull(message.getAnimation())) {
            // 动画：必须有caption
            if (StrUtil.isBlank(caption)) {
                return null;
            }
            type = SourceTypeEnum.VIDEO;
            times = message.getAnimation().getDuration();
            sourceName = caption;
            
        } else if (message.hasAudio() && Objects.nonNull(message.getAudio())) {
            // 音频：使用音频文件名
            String fileName = message.getAudio().getFileName();
            if (StrUtil.isBlank(fileName)) {
                // 如果没有文件名，尝试使用title或performer
                String title = message.getAudio().getTitle();
                String performer = message.getAudio().getPerformer();
                if (StrUtil.isNotBlank(title)) {
                    fileName = StrUtil.isNotBlank(performer) ? performer + "-" + title : title;
                } else {
                    return null;
                }
            }
            type = SourceTypeEnum.AUDIO;
            times = message.getAudio().getDuration();
            sourceName = fileName;
            
        } else if (message.hasVoice() && Objects.nonNull(message.getVoice())) {
            // 语音：不收录
            return null;
            
        } else if (message.hasPhoto() && !message.getPhoto().isEmpty()) {
            // 图片：必须有caption
            if (StrUtil.isBlank(caption)) {
                return null;
            }
            type = SourceTypeEnum.PHOTO;
            sourceName = caption;
            
        } else if (message.hasDocument() && Objects.nonNull(message.getDocument())) {
            // 文件：使用文件名
            String fileName = message.getDocument().getFileName();
            if (StrUtil.isBlank(fileName)) {
                return null;
            }
            type = SourceTypeEnum.FILE;
            sourceName = fileName;
        }

        // 如果不是支持的媒体类型或没有资源名称，不处理
        if (Objects.isNull(type) || StrUtil.isBlank(sourceName)) {
            return null;
        }

        // 构建链接URL
        String channelUsername = chat.getIndexUsername();
        String channelUrl = "";
        String sourceUrl = "";
        
        // 公开群组/频道：有username
        if (StrUtil.isNotBlank(channelUsername)) {
            channelUrl = "https://t.me/" + channelUsername;
            if (Objects.nonNull(message.getMessageId())) {
                sourceUrl = channelUrl + "/" + message.getMessageId();
            } else {
                sourceUrl = channelUrl;
            }
        } else {
            // 私密群组/频道：使用chatId构建链接
            // 私密群组链接格式：https://t.me/c/{chatId}/{messageId}
            Long chatId = chat.getId();
            if (chatId != null && chatId < 0) {
                // 移除负号，私密群组ID是负数
                String privateChatId = String.valueOf(Math.abs(chatId));
                // 如果是-100开头的超级群组ID，需要去掉-100前缀
                if (privateChatId.length() > 10) {
                    privateChatId = privateChatId.substring(3);
                }
                channelUrl = "https://t.me/c/" + privateChatId;
                if (Objects.nonNull(message.getMessageId())) {
                    sourceUrl = channelUrl + "/" + message.getMessageId();
                } else {
                    sourceUrl = channelUrl;
                }
            }
        }

        // 收集时间
        long collectTime = Objects.nonNull(message.getDate())
                ? message.getDate() * 1000L
                : System.currentTimeMillis();

        // 获取群组/频道标题
        String indexTitle = StrUtil.emptyToDefault(chat.getIndexTitle(), "");

        // 组装SearchBean
        return new SearchBean()
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
    }

    /**
     * 处理群搜索奖励
     */
    private void processGroupSearchReward(Included chat, Long searcherUserId) {
        // 如果群组没有所有者，不处理
        if (Objects.isNull(chat.getUserId())) {
            return;
        }
        
        Config config = this.configService.queryConfig();
        // 如果没有配置奖励或奖励为0，不处理
        if (Objects.isNull(config.getGroupSearchReward()) || config.getGroupSearchReward().compareTo(BigDecimal.ZERO) <= 0) {
            return;
        }
        
        // 获取群主信息
        User groupOwner = this.userService.select(chat.getUserId());
        if (Objects.isNull(groupOwner)) {
            return;
        }
        
        // 构建群组链接
        String chatLink = "";
        if (StrUtil.isNotBlank(chat.getIndexUsername())) {
            chatLink = "https://t.me/" + chat.getIndexUsername();
        }
        
        // 记录群搜索奖励
        this.rewardRecordService.recordGroupSearchReward(
            groupOwner, 
            chat.getId(), 
            chat.getIndexTitle(), 
            chatLink, 
            config.getGroupSearchReward()
        );
        
        // 更新群主的待入账金额
        User updateOwner = new User();
        updateOwner.setUserId(groupOwner.getUserId());
        updateOwner.setTotalAward(groupOwner.getTotalAward().add(config.getGroupSearchReward()));
        this.userService.update(updateOwner);
    }
}
