package com.search.robots.handlers;

import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.caffeine.ExpireListener;
import com.search.robots.beans.chat.ChatQueryHandler;
import com.search.robots.config.BotProperties;
import com.search.robots.database.entity.Config;
import com.search.robots.database.entity.Included;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.IncludedService;
import com.search.robots.helper.CollectHelper;
import com.search.robots.helper.KeyboardHelper;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.methods.pinnedmessages.PinChatMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.User;
import org.telegram.telegrambots.meta.api.objects.chat.Chat;
import org.telegram.telegrambots.meta.api.objects.chat.ChatFullInfo;
import org.telegram.telegrambots.meta.api.objects.chatmember.*;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;

import java.util.Objects;

/**
 * <p>
 *     进群消息
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class BotJoinChatHandler extends AbstractHandler {


    private final BotProperties properties;
    private final CollectHelper collectHelper;
    private final ConfigService configService;
    private final ExpireListener expireListener;
    private final IncludedService includedService;
    private final ChatQueryHandler chatQueryHandler;


    @Override
    public boolean support(Update update) {
        return Objects.nonNull(update.getMyChatMember());
    }

    @Override
    protected BotApiMethod<?> execute(Update update) {
        ChatMemberUpdated myChatMember = update.getMyChatMember();
        ChatMember newMember = myChatMember.getNewChatMember();

        // 不是机器人
        User user = newMember.getUser();
        if (!user.getIsBot()) {
            return null;
        }
        // 不是当前机器人
        String botUsername = this.properties.getBotUsernameSelf();
        if (!StrUtil.equals(botUsername, user.getUserName())) {
            return null;
        }

        Chat chat = myChatMember.getChat();
        ChatMember oldMember = myChatMember.getOldChatMember();
        User from = myChatMember.getFrom();

        // 邀请进群
        if (StrUtil.equals(oldMember.getStatus(), ChatMemberLeft.STATUS)
                && StrUtil.equals(newMember.getStatus(), ChatMemberMember.STATUS)) {

            Config config = this.configService.queryConfig();
            AsyncSender.async(markdown(
                    chat.getId(), config.getJoinSendMessage()
            ));
            AsyncSender.async(ok(chat.getId(), "请将我提升为管理员，并授予我发言权限吧～这样我才能更好地为您工作哦 \uD83D\uDE0A"));
        }

        // 升级成管理员
        if (StrUtil.equals(newMember.getStatus(), ChatMemberAdministrator.STATUS)) {
            Config config = this.configService.queryConfig();

            // 查询该群组是否已经存在，或者是别人的附属群组
            Included included = this.includedService.get(chat.getId());
            ChatFullInfo chatInfo = this.chatQueryHandler.getChat(chat.getId());
            Integer count = this.chatQueryHandler.getChatMemberCount(chat.getId());

            Included parentIncluded = Included.buildBean(chatInfo, from, true, count);
            if (Objects.isNull(included)) {
                this.includedService.save(parentIncluded);
            } else {
                this.includedService.updateSelf(parentIncluded);
            }
            included = parentIncluded;

            String text = included.buildDetailIncludedText(this.properties.groupStart(), config);
            InlineKeyboardMarkup markup = KeyboardHelper.buildIncludedDetailKeyboard(included);
            AsyncSender.async(markdownV2(chat.getId(), text, markup));

            String url = "";
            if (StrUtil.isNotEmpty(chatInfo.getUserName())) {
                url = "https://t.me/" + chatInfo.getUserName();
            }
            this.collectHelper.history(url, chatInfo.getInviteLink());
            this.expireListener.processorEveryAdv(chat.getId());
        }
        return null;
    }


}
