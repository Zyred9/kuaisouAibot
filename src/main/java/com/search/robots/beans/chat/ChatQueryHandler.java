package com.search.robots.beans.chat;

import cn.hutool.core.util.StrUtil;
import lombok.SneakyThrows;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.groupadministration.GetChat;
import org.telegram.telegrambots.meta.api.methods.groupadministration.GetChatAdministrators;
import org.telegram.telegrambots.meta.api.methods.groupadministration.GetChatMember;
import org.telegram.telegrambots.meta.api.methods.groupadministration.GetChatMemberCount;
import org.telegram.telegrambots.meta.api.objects.chat.ChatFullInfo;
import org.telegram.telegrambots.meta.api.objects.chatmember.ChatMember;
import org.telegram.telegrambots.meta.generics.TelegramClient;

import javax.annotation.Resource;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Component
public class ChatQueryHandler {

    @Resource private TelegramClient telegramClient;

    @SneakyThrows
    public boolean isChatAdmin (Long chatId, Long userId) {
        List<ChatMember> members = this.telegramClient.execute(
                GetChatAdministrators.builder()
                        .chatId(chatId)
                        .build()
        );
        for (ChatMember member : members) {
            if (Objects.equals(member.getUser().getId(), userId)) {
                return true;
            }
        }
        return false;
    }

    @SneakyThrows
    public String getGroupInviteLink(Long groupId) {
        ChatFullInfo fullInfo = this.getGroupInfo(groupId);
        if (Objects.isNull(fullInfo)) {
            return null;
        }
        return fullInfo.getInviteLink();
    }

    @SneakyThrows
    public ChatFullInfo getGroupInfo(Long groupId) {
        ChatFullInfo fullInfo = telegramClient.execute(
                GetChat.builder().chatId(groupId).build()
        );
        if (Objects.isNull(fullInfo)) {
            return null;
        }
        return fullInfo;
    }

    /**
     * 通过用户名（@xxx 或 xxx）查询群组/频道信息
     */
    @SneakyThrows
    public ChatFullInfo getGroupInfo(String username) {
        if (StrUtil.isBlank(username)) {
            return null;
        }
        String handle = username.startsWith("@") ? username : ("@" + username);
        ChatFullInfo fullInfo = telegramClient.execute(
                GetChat.builder().chatId(handle).build()
        );
        if (Objects.isNull(fullInfo)) {
            return null;
        }
        return fullInfo;
    }

    /**
     * 根据输入（ID 或 用户名）查询群组信息
     * 支持："-100xxx"、"12345"、"@username"、"username"
     */
    @SneakyThrows
    public ChatFullInfo findGroupByIdOrUsername(String input) {
        if (StrUtil.isBlank(input)) {
            return null;
        }
        String val = input.trim();
        // 数字ID形式
        try {
            Long id = Long.parseLong(val);
            return getGroupInfo(id);
        } catch (NumberFormatException ignore) {
            // 既不是纯数字也不是明显的用户名，尝试按用户名查询
            return getGroupInfo(val);
        }
    }

    @SneakyThrows
    public boolean checkUserInGroup(Long groupId, Long id) {
        ChatMember chatMember = this.telegramClient.execute(GetChatMember.builder()
                .chatId(groupId)
                .userId(id)
                .build());
        String status = chatMember.getStatus();
        return !status.equals("left") && !status.equals("kicked");
    }

    @SneakyThrows
    public ChatFullInfo getChat (Long userId){
        return this.telegramClient.execute(GetChat.builder().chatId(userId).build());
    }

    @SneakyThrows
    public Integer getChatMemberCount(Long chatId) {
        return this.telegramClient.execute(GetChatMemberCount.builder().chatId(chatId).build());
    }
}
