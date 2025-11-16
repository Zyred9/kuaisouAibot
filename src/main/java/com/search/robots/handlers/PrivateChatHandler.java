package com.search.robots.handlers;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.cache.CommonCache;
import com.search.robots.beans.chat.ChatQueryHandler;
import com.search.robots.beans.view.DialogueCtx;
import com.search.robots.config.BotProperties;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.*;
import com.search.robots.database.enums.Dialogue;
import com.search.robots.database.enums.SearchPeriodEnum;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.service.*;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.KeyboardHelper;
import com.search.robots.helper.StrHelper;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.methods.commands.SetMyCommands;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.chat.ChatFullInfo;
import org.telegram.telegrambots.meta.api.objects.commands.BotCommand;
import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

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
    private final SearchService searchService;
    private final SearchHandler searchHandler;
    private final ConfigService configService;
    private final AdvUserService advUserService;
    private final IncludedService includedService;
    private final HotSearchService hotSearchService;
    private final ChatQueryHandler chatQueryHandler;
    private final AdvLibraryService advLibraryService;

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

        if (StrUtil.equals(text, "/start")) {
            return this.processorStart(message);
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
                String decode = StrHelper.decode(encode);
                return this.searchHandler.processorStartSearch(message, decode, true);
            }
            if (StrUtil.equals(commands.get(1), "ad_null")) {
                Config config = this.configService.queryConfig();
                InlineKeyboardMarkup markup = KeyboardHelper.buildAdvertisingKeyboard();
                return markdown(message, config.getAdvertisingMarkdown(), markup);
            }
            this.processorStartWith(message, commands);
        }

        if (StrUtil.equals(text, "语法")) {
            return markdownV2(message, Constants.MARKDOWN_V2_TEXT);
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

            BotApiMethod<?> result = null;
            DialogueCtx dialogueCtx = CommonCache.getDialogue(message.getFrom().getId());
            User user = this.userService.user(message.getFrom());

            // 输入了地址
            if (Objects.equals(dialogueCtx.getDialogue(), Dialogue.INPUT_ADDRESS)) {

                user.setTrAddr(message.getText());
                this.userService.updateById(user);

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
                }
                // TODO 金额正常的情况
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
                Included included = this.includedService.getById(includedId);

                List<Included> children = new ArrayList<>();
                for (ChatFullInfo info : results) {
                    Integer count = this.chatQueryHandler.getChatMemberCount(info.getId());
                    children.add(Included.buildBean(info, message.getFrom(), false, count));
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
                    this.includedService.updateById(updateEntity);
                }

                List<Included> includeList = Collections.emptyList();
                if (CollUtil.isNotEmpty(included.getTargetedSearchIndexIds())) {
                    includeList = this.includedService
                            .listByIds(included.getTargetedSearchIndexIds());
                }

                Config config = this.configService.queryConfig();

                InlineKeyboardMarkup markup = KeyboardHelper.buildTargetedSearchKeyboard(included, includeList);
                result = markdown(message, included.buildDetailIncludedText(this.properties.groupStart(), config), markup);
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


        // -------------------- 处理搜索 --------------------//
        return this.searchHandler.processorDefaultSearch(message);
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
                String.valueOf(totalShowCount)
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

        User newOldUser = this.userService.getById(message.getFrom().getId());
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

        if (insert) {
            this.userService.save(newOldUser);
        } else {
            this.userService.updateById(newOldUser);
        }

        // 如果是广告，需要给上级发送
        if (isAdv) {
            String masterText = StrUtil.format(Constants.USER_ADV_NEXT_INVITE_TEXT,
                    newOldUser.getUserId(), newOldUser.getUsername(),
                    newOldUser.getNickname(), newOldUser.getUsername(),
                    newOldUser.getUsername(), newOldUser.getUsername()
            );
            AsyncSender.async(markdown(master.getUserId(), masterText));

            Config config = this.configService.queryConfig();
            String format = StrUtil.format(Constants.MY_LEADER_TEXT, config.getPreferentialRate(), master.getUsername());
            AsyncSender.async(markdown(message.getFrom().getId(), format));
        }

        // 子节点邀请
        if (isChild) {
            AsyncSender.async(this.processorStart(message));
        }
        // todo 发送携带按钮的搜索

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

        this.userService.user(message.getFrom());
        Config config = this.configService.queryConfig();
        return markdownReply(message, config.getStartMessage(), KeyboardHelper.buildStartKeyboard());
    }

}
