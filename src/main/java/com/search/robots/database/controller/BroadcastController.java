package com.search.robots.database.controller;


import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.web.broadcast.BroadcastRequest;
import com.search.robots.config.SelfException;
import com.search.robots.database.entity.User;
import com.search.robots.database.service.UserService;
import com.search.robots.sender.AsyncSender;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.telegram.telegrambots.meta.api.methods.ParseMode;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.methods.send.SendPhoto;
import org.telegram.telegrambots.meta.api.objects.InputFile;
import org.telegram.telegrambots.meta.api.methods.send.SendVideo;

import java.util.List;

/**
 *
 *
 * @author admin
 * @since 2025/11/24 18:02
 */
@RestController
@RequestMapping("/broadcast")
public class BroadcastController {

    @Setter(onMethod_ = @Autowired)
    private UserService userService;


    @PostMapping("/lunch")
    public Result<String> lunchBroadcast (@RequestBody @Validated BroadcastRequest request) {
        boolean hasImage = StrUtil.isNotBlank(request.getImageFileId());
        boolean hasVideo = StrUtil.isNotBlank(request.getVideoFileId());
        boolean hasText = StrUtil.isNotBlank(request.getMarkdownContent());
        if ((hasImage && hasVideo) || (!hasImage && !hasVideo && !hasText)) {
            throw new SelfException("参数错误");
        }

        List<User> users = this.userService.list(
                Wrappers.<User>lambdaQuery()
                        .select(User::getUserId)
        );

        if (hasText && !hasImage && !hasVideo) {
            for (User user : users) {
                AsyncSender.async(SendMessage.builder()
                        .chatId(user.getUserId())
                        .disableWebPagePreview(true)
                        .parseMode(ParseMode.MARKDOWN)
                        .text(request.getMarkdownContent())
                        .build());
            }
        }

        if (hasText && hasImage) {
            for (User user : users) {
                AsyncSender.async(SendPhoto.builder()
                        .chatId(user.getUserId())
                        .photo(new InputFile(request.getImageFileId()))
                        .parseMode(ParseMode.MARKDOWN)
                        .caption(request.getMarkdownContent())
                        .build());
            }
        }

        if (!hasText && hasImage) {
            for (User user : users) {
                AsyncSender.async(SendPhoto.builder()
                        .chatId(user.getUserId())
                        .photo(new InputFile(request.getImageFileId()))
                        .build());
            }
        }

        if (hasText && hasVideo) {
            for (User user : users) {
                AsyncSender.async(SendVideo.builder()
                        .chatId(user.getUserId())
                        .video(new InputFile(request.getVideoFileId()))
                        .parseMode(ParseMode.MARKDOWN)
                        .caption(request.getMarkdownContent())
                        .build());
            }
        }

        if (!hasText && hasVideo) {
            for (User user : users) {
                AsyncSender.async(SendVideo.builder()
                        .chatId(user.getUserId())
                        .video(new InputFile(request.getVideoFileId()))
                        .build());
            }
        }

        // 计算预计耗时  一秒20个用户
        int sec = users.size() / 20;
        return Result.success(StrUtil.format("{}个用户，预计{}内完成广播", users.size(), sec));
    }




}
