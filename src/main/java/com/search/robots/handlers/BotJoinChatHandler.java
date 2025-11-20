package com.search.robots.handlers;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.search.robots.beans.chat.ChatQueryHandler;
import com.search.robots.config.BotProperties;
import com.search.robots.database.entity.Config;
import com.search.robots.database.entity.Included;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.IncludedService;
import com.search.robots.helper.KeyboardHelper;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Call;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.User;
import org.telegram.telegrambots.meta.api.objects.chat.Chat;
import org.telegram.telegrambots.meta.api.objects.chat.ChatFullInfo;
import org.telegram.telegrambots.meta.api.objects.chatmember.*;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;

import java.io.IOException;
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
    private final OkHttpClient okHttpClient;
    private final ConfigService configService;
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
        String botUsername = this.properties.getBotUsername();
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
        if (StrUtil.equals(oldMember.getStatus(), ChatMemberMember.STATUS)
                && StrUtil.equals(newMember.getStatus(), ChatMemberAdministrator.STATUS)) {
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
            this.history(url, chatInfo.getInviteLink());
        }
        return null;
    }


    private void history (String url, String inviteLink) {
        log.info("[获取历史聊天记录] 开始 {}， {}", url, inviteLink);
        String fullUrl = this.properties.getHistoryUrl()
                + "?link=" + url
                + "&inviteLink=" + inviteLink
                + "&count=10000";

        Call call = this.okHttpClient.newCall(
                new Request.Builder()
                        .get()
                        .url(fullUrl)
                        .build()
        );
        try (Response execute = call.execute()) {
            if (execute.isSuccessful() && Objects.nonNull(execute.body())) {
                String body = execute.body().string();
                log.info("[获取历史聊天记录] 结果 {}， {}, 结果：{}", url, inviteLink, body);
            } else {
                log.info("[获取历史聊天记录] 失败 {}， {}，错误码：{}", url, inviteLink, execute.code());
            }
        } catch (IOException e) {
            log.error("[获取历史聊天记录] 异常 {}， {}", url, inviteLink, e);
        }
    }
}
