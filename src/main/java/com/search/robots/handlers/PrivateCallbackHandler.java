package com.search.robots.handlers;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.cache.CommonCache;
import com.search.robots.beans.view.DialogueCtx;
import com.search.robots.beans.view.vo.AdvButton;
import com.search.robots.config.BotProperties;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.*;
import com.search.robots.database.enums.BillTypeEnum;
import com.search.robots.database.enums.Dialogue;
import com.search.robots.database.enums.Included.IncludedNewUserEnum;
import com.search.robots.database.enums.Included.IncludedSearchPriorityEnum;
import com.search.robots.database.enums.Included.IncludedSearchTypeEnum;
import com.search.robots.database.enums.SearchPeriodEnum;
import com.search.robots.database.enums.adv.AdvPositionEnum;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import com.search.robots.database.service.*;
import com.search.robots.helper.*;
import com.search.robots.sender.AsyncSender;
import com.search.robots.sender.SyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.CallbackQuery;
import org.telegram.telegrambots.meta.api.objects.PhotoSize;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.InlineKeyboardRow;

import java.io.File;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

/**
 * <p>
 *      私聊点击回调
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class PrivateCallbackHandler extends AbstractHandler {

    private final BillService billService;
    private final UserService userService;
    private final BotProperties properties;
    private final SearchHandler searchHandler;
    private final ConfigService configService;
    private final AdvUserService advUserService;
    private final AdvPriceService advPriceService;
    private final IncludedService includedService;
    private final HotSearchService hotSearchService;
    private final AddressService addressService;
    private final AdvLibraryService advLibraryService;
    private final ExhibitionService exhibitionService;
    private final PrivateChatHandler privateChatHandler;

    @Override
    public boolean support(Update update) {
        return Objects.nonNull(update.getCallbackQuery())
                && Objects.nonNull(update.getCallbackQuery().getMessage())
                && (update.getCallbackQuery().getMessage().isUserMessage()
                || update.getCallbackQuery().getMessage().isSuperGroupMessage()
                || update.getCallbackQuery().getMessage().isGroupMessage());
    }

    @Override
    protected BotApiMethod<?> execute(Update update) {
        CallbackQuery callbackQuery = update.getCallbackQuery();
        String text = callbackQuery.getData();
        Message message = (Message) callbackQuery.getMessage();
        List<String> command = StrUtil.split(text, "#");

        if (this.properties.isLogs()) {
            log.info("[回调] {}", text);
        }

        Integer messageDate = message.getDate();
        long currentTimestamp = System.currentTimeMillis() / 1000;
        long messageAge = currentTimestamp - messageDate;
        if (messageAge > 600) {
            return answerAlert(callbackQuery, "⚠️消息已过期，请重新发送关键词");
        }


        // 查询
        if (StrUtil.equals(command.get(0), "search")) {
            return this.searchHandler.processorCallbackSearch(message, command);
        }

        // 第一层
        if (StrUtil.equals(command.get(0), "one")) {
            return this.processorLevelOne(callbackQuery, message, command);
        }
        // 第二层
        if (StrUtil.equals(command.get(0), "two")) {
            return this.processorLevelTwo(callbackQuery, message, command);
        }
        // 第三层
        if (StrUtil.equals(command.get(0), "three")) {
            return this.processorLevelThree(callbackQuery, message, command);
        }
        if (StrUtil.equals(command.get(0), "four")) {
            return this.processorLevelFour(message, command);
        }

        // five
        if (StrUtil.equals(command.get(0), "five")) {
            if (StrUtil.equals(command.get(1), "priority_customize")) {

                int hit = Integer.parseInt(command.get(2));
                Long chatId = Long.parseLong(command.get(3));
                IncludedSearchPriorityEnum priorityEnum = IncludedSearchPriorityEnum.of(hit);
                Included included = this.includedService.getById(chatId);

                List<IncludedSearchPriorityEnum> chooses = included.getPriorities();
                if (CollUtil.isEmpty(included.getPriorities())) {
                    chooses = new ArrayList<>();
                }

                chooses.add(priorityEnum);
                included.setPriorities(chooses);
                this.includedService.updateById(included);
                InlineKeyboardMarkup markup = KeyboardHelper.buildSearchPriorityCustomizeKeyboard(included);
                return editKeyboard(message, markup);
            }
        }

        // six
        if (StrUtil.equals(command.get(0), "six")) {
            if (StrUtil.equals(command.get(1), "search_result_customize")) {
                long chatId = Long.parseLong(command.get(3));
                Included included = this.includedService.getById(chatId);
                // 全局搜索
                InlineKeyboardMarkup markup = null;
                if (StrUtil.equals(command.get(2), "global_search")) {
                    included.setOpenGlobalSearch(Boolean.TRUE);
                    markup = KeyboardHelper.buildSearchResultCustomizeKeyboard(included);
                }

                else if (StrUtil.equals(command.get(2), "targeted_search")) {
                    included.setOpenGlobalSearch(Boolean.FALSE);

                    List<Included> includeList = Collections.emptyList();
                    if (CollUtil.isNotEmpty(included.getTargetedSearchIndexIds())) {
                        includeList = this.includedService
                                .listByIds(included.getTargetedSearchIndexIds());
                    }
                    markup = KeyboardHelper.buildTargetedSearchKeyboard(included, includeList);
                }

                this.includedService.updateById(included);
                return editKeyboard(message, markup);
            }
        }

        // seven
        if (StrUtil.equals(command.get(0), "seven")) {
            if (StrUtil.equals(command.get(1), "targeted_search")) {
                if (StrUtil.equals(command.get(2), "enable")) {
                    long parentId = Long.parseLong(command.get(3));
                    long childId = Long.parseLong(command.get(4));
                    Included parent = this.includedService.getById(parentId);
                    Included child = this.includedService.getById(childId);

                    child.setChildTargetedSearch(!child.getChildTargetedSearch());
                    this.includedService.updateById(child);

                    List<Included> includeList = Collections.emptyList();
                    List<Long> ids = parent.getTargetedSearchIndexIds();
                    if (CollUtil.isNotEmpty(ids)) {
                        includeList = this.includedService.listByIds(ids);
                    }

                    InlineKeyboardMarkup  markup = KeyboardHelper.buildTargetedSearchKeyboard(parent, includeList);
                    return editKeyboard(message, markup);
                }
                // 添加
                if (StrUtil.equals(command.get(2), "add")) {
                    long includedId = Long.parseLong(command.get(3));
                    CommonCache.putDialogue(callbackQuery.getFrom().getId(),
                            new DialogueCtx(Dialogue.INPUT_TARGETED_SEARCH, includedId));
                    Config config = this.configService.queryConfig();
                    return editMarkdown(message, config.getAddTargetedSearch());
                }
            }
        }
        return null;
    }

    private BotApiMethod<?> processorLevelFour(Message message, List<String> command) {
        // ads_toggle 拉新广告
        if (StrUtil.equals(command.get(1), "new_user")) {
            int newUserCode = Integer.parseInt(command.get(2));
            IncludedNewUserEnum newUserEnum = IncludedNewUserEnum.of(newUserCode);

            long chatId = Long.parseLong(command.get(3));
            Included included = this.includedService.getById(chatId);
            included.setNewUsers(newUserEnum);
            this.includedService.updateById(included);

            InlineKeyboardMarkup markup = KeyboardHelper.buildNewUserKeyboard(included);
            return editKeyboard(message, markup);
        }
        // 隐私设置privacy_setting#search_result_customize
        if (StrUtil.equals(command.get(1), "privacy_setting")) {
            long chatId = Long.parseLong(command.get(3));
            Included included = this.includedService.getById(chatId);
            // 隐私设置
            if (StrUtil.equals(command.get(2), "open_privacy_search")) {
                included.setOpenPrivacySearch(!included.getOpenPrivacySearch());
                this.includedService.updateById(included);

                InlineKeyboardMarkup markup = KeyboardHelper.buildSearchCustomizeKeyboard(included);
                return editKeyboard(message, markup);
            }
            // 未成年设置
            if (StrUtil.equals(command.get(2), "open_filter_minors")) {
                included.setOpenFilterMinors(!included.getOpenFilterMinors());
                this.includedService.updateById(included);
                InlineKeyboardMarkup markup = KeyboardHelper.buildSearchCustomizeKeyboard(included);
                return editKeyboard(message, markup);
            }
            // ️优先级自定义
            if (StrUtil.equals(command.get(2), "priority_customize")) {
                InlineKeyboardMarkup markup = KeyboardHelper.buildSearchPriorityCustomizeKeyboard(included);
                return editKeyboard(message, markup);
            }
            // ️搜索结果自定义
            if (StrUtil.equals(command.get(2), "search_result_customize")) {
                InlineKeyboardMarkup markup = KeyboardHelper.buildSearchResultCustomizeKeyboard(included);
                return editKeyboard(message, markup);
            }
        }
        return null;
    }

    private BotApiMethod<?> processorLevelThree(CallbackQuery callbackQuery, Message message, List<String> command) {
        Config config = this.configService.queryConfig();

        // 群组详情设置
        if (StrUtil.equals(command.get(1), "details_setting")) {
            long chatId = Long.parseLong(command.get(3));
            Included included = this.includedService.getById(chatId);
            if (Objects.isNull(included)) {
                return null;
            }
            // 更新基础数据
            if (StrUtil.equals(command.get(2), "update_basic")) {
                AsyncSender.async(
                        editMarkdown(message, "正在同步中，请稍等....")
                );
                ThreadHelper.execute(() -> {
                    ThreadHelper.sleep(2);
                    InlineKeyboardMarkup markup = KeyboardHelper.buildIncludedDetailKeyboard(included);
                    AsyncSender.async(editMarkdown(message, included.buildDetailIncludedText(this.properties.groupStart(), config), markup));
                });
                return null;
            }
            if (StrUtil.equals(command.get(2), "listen_toggle")) {
                included.setOpenListen(!included.getOpenListen());
            }
            // 查看曝光数据
            if (StrUtil.equals(command.get(2), "exposure_data")) {
                String chatExposureKeyboard = config.getChatExposureKeyboard();
                InlineKeyboardMarkup markup = KeyboardHelper.keyboard(chatExposureKeyboard);

                String includedText = included.buildDetailExhibition();
                List<LocalDate> localDates = TimeHelper.ofSevenDays();
                String exhibitionText = this.exhibitionService.querySevenExhibition(localDates, chatId);

                String format = StrUtil.format(Constants.QUERY_EXPOSURE_DATA_TEXT, includedText, exhibitionText);
                return markdownV2(message, format, markup);
            }
            // 开启搜索/关闭
            if (StrUtil.equals(command.get(2), "search_toggle")) {
                included.setOpenSearch(!included.getOpenSearch());
            }
            // ads_toggle 拉新广告
            if (StrUtil.equals(command.get(2), "ads_toggle")) {
                InlineKeyboardMarkup markup = KeyboardHelper.buildNewUserKeyboard(included);
                return editKeyboard(message, markup);
            }
            // search_customize 自定义搜索
            if (StrUtil.equals(command.get(2), "search_customize")) {
                InlineKeyboardMarkup markup = KeyboardHelper.buildSearchCustomizeKeyboard(included);
                return editKeyboard(message, markup);
            }

            this.includedService.updateById(included);
            String detailIncludedText = included.buildDetailIncludedText(this.properties.groupStart(), config);
            InlineKeyboardMarkup markup = KeyboardHelper.buildIncludedDetailKeyboard(included);
            return editMarkdown(message, detailIncludedText, markup);
        }

        // 顶部链接和底部按钮展示次数购买
        if (StrUtil.equals(command.get(1), "do_payment_button")) {
            String prev = command.get(2);
            BillTypeEnum bt = BillTypeEnum.ofData(prev);
            AdvTypeEnum advTypeEnum = AdvTypeEnum.ofData(prev);
            AdvPositionEnum rank = AdvPositionEnum.RANK_1;

            List<String> priceList = StrUtil.split(command.get(3), "_");
            long showCount = Long.parseLong(priceList.get(0));
            java.math.BigDecimal need = new java.math.BigDecimal(priceList.get(1));

            User user = this.userService.user(callbackQuery.getFrom());

            AdvButton button = new AdvButton(need, showCount);
            AdvUser advUser = AdvUser.buildAdvUserDefault(user, button, advTypeEnum, rank);
            this.advUserService.save(advUser);

            if (this.processorUserRealBalance(message, user, need, bt)) {
                return null;
            }
            InlineKeyboardMarkup markup = KeyboardHelper.buildAdvUserDetailKeyboard(advUser);
            return editMarkdownV2(message, advUser.getAdvText(), markup);
        }

        // 关键词购买支付
        if (StrUtil.equals(command.get(1), "do_payment_keyword")) {
            String prev = command.get(2);
            BillTypeEnum bt = BillTypeEnum.ofData(prev);
            long priceId = Long.parseLong(command.get(3));
            User user = this.userService.user(callbackQuery.getFrom());

            AdvPrice price = this.advPriceService.getById(priceId);
            boolean b = this.processorUserRealBalance(message, user, price.getMonthlyPrice(), bt);
            if (b) {
                return null;
            }

            AdvLibrary library = this.advLibraryService.getById(price.getLibraryId());
            price.setIsSold(Boolean.TRUE);
            this.advPriceService.updateById(price);

            AdvUser advUser = AdvUser.buildKeywordAdvUserDefault(user, library, price);
            this.advUserService.save(advUser);

            InlineKeyboardMarkup markup = KeyboardHelper.buildAdvUserDetailKeyboard(advUser);
            return editMarkdownV2(message, advUser.getAdvText(), markup);
        }

        // 我的广告
        if (StrUtil.equals(command.get(1), "my_adv")) {
            if (StrUtil.equalsAny(command.get(2), "edit_title", "edit_link")) {
                long userAdvId = Long.parseLong(command.get(4));
                boolean title = StrUtil.equals(command.get(2), "edit_title");
                if (title) {
                    DialogueCtx ctx = new DialogueCtx(Dialogue.INPUT_ADV_TITLE, userAdvId, command.get(3));
                    CommonCache.putDialogue(callbackQuery.getFrom().getId(), ctx);
                    return reply(message, "请回复新的标题（特殊表情自动去除）:");
                }
                DialogueCtx ctx = new DialogueCtx(Dialogue.INPUT_ADV_LINK, userAdvId);
                CommonCache.putDialogue(callbackQuery.getFrom().getId(), ctx);
                return reply(message, "请回复新的链接:");
            }
        }

        if (StrUtil.equals(command.get(1), "my_adv")) {
            // 优先续订
            if (StrUtil.equals(command.get(2), "priority_renewal")) {
                long advUserId = Long.parseLong(command.get(2));
                AdvUser advUser = this.advUserService.getById(advUserId);
                // 判断今天和目标时间是否大于5天
                if (TimeHelper.isMoreThanDays(advUser.getExpirationTime(), 5)) {
                    return answerAlert(callbackQuery, "关键词广告可在到期前 5 天内续订，目前尚未进入续订时间，请稍后再试。");
                }
                // TODO 无法知道优先续订的流程
            }

            // 开始推广
            if (StrUtil.equals(command.get(2), "start_promotion")) {
                long advUserId = Long.parseLong(command.get(3));
                AdvUser advUser = this.advUserService.getById(advUserId);

                if (StrUtil.isBlank(advUser.getAdvContent()) || StrUtil.isBlank(advUser.getAdvUrl())) {
                    return answerAlert(callbackQuery, "请先补充广告内的配置");
                }

                String key = AdvUser.KEYWORD_ADV_USER + advUser.getKeyword();
                Set<String> ids = RedisHelper.sMembers(key);
                if (Objects.equals(advUser.getAdvStatus(), AdvStatus.PROMOTION_ING)) {
                    advUser.setAdvStatus(AdvStatus.PAUSE_ING);
                    if (CollUtil.isNotEmpty(ids)) {
                        RedisHelper.sRemove(key, String.valueOf(advUser.getId()));
                    }
                } else {
                    advUser.setAdvStatus(AdvStatus.PROMOTION_ING);
                    RedisHelper.sAdd(key, String.valueOf(advUser.getId()));
                }

                this.advUserService.updateById(advUser);
                InlineKeyboardMarkup markup = KeyboardHelper.buildAdvUserDetailKeyboard(advUser);
                return editMarkdownV2(message, advUser.getAdvText(), markup);
            }
        }

        return null;
    }

    private boolean processorUserRealBalance(Message message, User user, BigDecimal need, BillTypeEnum bt) {
        BigDecimal balance = user.getBalance();

        if (balance == null || balance.compareTo(need) < 0) {
            InlineKeyboardMarkup kb = KeyboardHelper.buildAdvPaymentLackKeyboard();
            AsyncSender.async(editMarkdown(message, "余额不足，请充值❗️", kb));
            return true;
        }

        user.setBalance(balance.subtract(need));
        this.userService.updateById(user);

        Bill bill = Bill.buildAdvPaymentBill(user, need, bt);
        this.billService.save(bill);
        return false;
    }

    private BotApiMethod<?> processorLevelTwo(CallbackQuery callbackQuery, Message message, List<String> command) {
        // 处理已售出按钮点击
        if (StrUtil.equals(command.get(1), "sold")) {
            Long priceId = Long.parseLong(command.get(3));
            AdvPrice price = this.advPriceService.getById(priceId);
            if (Objects.isNull(price)) {
                return answerAlert(callbackQuery, "广告位信息不存在!");
            }
            
            // 通过 priceId 查询购买该广告位的用户记录
            AdvUser advUser = this.advUserService.getOne(
                    Wrappers.<AdvUser>lambdaQuery()
                            .eq(AdvUser::getPriceId, priceId)
                            .orderByDesc(AdvUser::getCreatedAt)
                            .last("LIMIT 1")
            );
            
            if (Objects.isNull(advUser)) {
                return answerAlert(callbackQuery, "未找到该广告购买记录!");
            }
            
            InlineKeyboardMarkup markup;
            if (Objects.equals(callbackQuery.getFrom().getId(), advUser.getUserId())
                    && !Objects.equals(advUser.getAdvStatus(), AdvStatus.THE_END)) {
                markup = KeyboardHelper.buildAdvUserDetailKeyboard(advUser);
            } else {
                markup = KeyboardHelper
                        .buildKeywordSoldKeyboard(command.get(2), advUser.getLibraryId());
            }

            return editMarkdownV2(message, advUser.getAdvText(), markup);
        }
        
        // 处理购买按钮点击
        if (StrUtil.equals(command.get(1), "to_buy")) {
            String data = command.get(2);
            Long priceId = Long.parseLong(command.get(3));
            AdvPrice price = this.advPriceService.getById(priceId);
            if (Objects.isNull(price)) {
                return null;
            }
            if (Boolean.TRUE.equals(price.getIsSold())) {
                return answerAlert(callbackQuery, "该广告位已被购买,请选择其他位置!");
            }
            AdvLibrary library = this.advLibraryService.getById(price.getLibraryId());
            String toBuyText = price.buildToBuyText(library);
            InlineKeyboardMarkup keyboard = KeyboardHelper.buildToBuyKeywordKeyboard(priceId, library.getId(), data);
            return editMarkdown(message, toBuyText, keyboard);
        }
        
        // 回到我的
        if (StrUtil.equals(command.get(1), "self")) {
            // 删除缓存
            CommonCache.removeDialogue(message.getFrom().getId());
            User user = this.userService.user(message.getFrom());
            InlineKeyboardMarkup markup = KeyboardHelper.buildSelfKeyboard();

            if (command.size() == 2) {
                return editMarkdown(message, user.buildText(), markup);
            } else {
                AsyncSender.async(delete(message));
                ThreadHelper.sleepMs(50);
                return markdown(message, user.buildText(), markup);
            }
        }
        // 更新地址
        if (StrUtil.equals(command.get(1), "update_addr")) {
            CommonCache.putDialogue(callbackQuery.getFrom().getId(), new DialogueCtx(Dialogue.INPUT_ADDRESS));
            return reply(message, "请向我发送新的TRC20地址：");
        }
        // 点击提现
        if (StrUtil.equals(command.get(1), "withdrawal")) {
            CommonCache.putDialogue(callbackQuery.getFrom().getId(), new DialogueCtx(Dialogue.INPUT_WITHDRAWAL_AMOUNT));
            return reply(message, "请向我发送提现金额：");
        }
        // 群组频道#全部/群组/频道的点击
        if (StrUtil.equals(command.get(1), "total_group_channel")) {
            int hit = Integer.parseInt(command.get(2));
            int current = Integer.parseInt(command.get(3));
            IncludedSearchTypeEnum searchType = IncludedSearchTypeEnum.of(hit);
            Page<Included> includedPage = this.includedService.selectPage(current,
                    callbackQuery.getFrom().getId(), searchType);
            String selfIndexText = this.includedService.buildSelfIndexText();
            InlineKeyboardMarkup markup = KeyboardHelper.buildGroupChannelKeyboard(searchType, includedPage);
            return editMarkdown(message, selfIndexText, markup);
        }
        // 群组详情
        if (StrUtil.equals(command.get(1), "included_detail")) {
            Long includedId = Long.parseLong(command.get(2));
            Included included = this.includedService.getById(includedId);
            if (Objects.isNull(included)) {
                return null;
            }
            Config config = this.configService.queryConfig();

            String detailIncludedText = included.buildDetailIncludedText(this.properties.groupStart(), config);
            InlineKeyboardMarkup markup = KeyboardHelper.buildIncludedDetailKeyboard(included);
            return editMarkdown(message, detailIncludedText, markup);
        }
        // 推广报表
        if (StrUtil.equals(command.get(1), "get_spread_statement")) {
            String spreadStatementText = this.userService.getSpreadStatement(
                    message.getFrom().getId());
            InlineKeyboardMarkup markup = KeyboardHelper.buildSingleBackKeyboard("one#invite");

            AsyncSender.async(delete(message));
            return markdownV2(message, spreadStatementText, markup);
        }
        // 获取推广参考文案
        if (StrUtil.equals(command.get(1), "get_spread_text")) {
            Config config = configService.queryConfig();
            if (StrUtil.isBlank(config.getHelpfulPopularizeFileId())) {
                return null;
            }
            InlineKeyboardMarkup keyboard = KeyboardHelper.keyboard(config.getHelpfulPopularizeKeyboard());

            AsyncSender.async(
                    photoMarkdownV2(message, config.getHelpfulPopularizeFileId(),
                            config.getHelpfulPopularizeMarkdown(), keyboard)
            );
            return null;
        }
        // 广告代理下级明细
        if (StrUtil.equals(command.get(1), "get_spread_next")) {
            int current = 1;
            if (command.size() == 3) {
                current = Integer.parseInt(command.get(2));
            }

            AsyncSender.async(delete(message));
            User user = this.userService.user(callbackQuery.getFrom());
            Page<User> children = this.userService.selectChildAdsUsers(callbackQuery.getFrom().getId(), current);

            String buildNextDetailText = User.buildNextDetailText(children);

            String format = StrUtil.format(Constants.NEXT_DETAIL_TEXT, user.getGrade().getDesc(),
                    children.getTotal(), StrHelper.specialResult(user.getUsername()), user.getInviteCode(),
                    buildNextDetailText);
            InlineKeyboardMarkup markup = KeyboardHelper.buildNextDetailKeyboard(children, current);
            return markdownV2(message, format, markup);
        }
        // 广告顶部链接  广告底部按钮
        if (StrUtil.equalsAny(command.get(1), "top_link", "bottom_button")) {
            boolean top = StrUtil.equals(command.get(1), "top_link");

            List<String> priceList = StrUtil.split(command.get(2), "_");
            int showCount = Integer.parseInt(priceList.get(0));
            int price = Integer.parseInt(priceList.get(1));

            String title;
            if (top) {
                title = "顶部链接";
            } else {
                title = "底部按钮链接";
            }
            String format = StrUtil.format(Constants.TO_BUY_TEXT, title, showCount, price);
            return editMarkdown(message, format, KeyboardHelper.buildAdvDetailKeyboard(command.get(1), command.get(2)));
        }
        // 充值
        if (StrUtil.equals(command.get(1), "adv_recharge")) {
            // 1) 获取配置，并进行空值与异常保护
            Config config = this.configService.queryConfig();
            Address address = this.addressService.getByUserId(callbackQuery.getFrom().getId());

            String tipMarkdown = config.getRechargeTipMarkdown();
            InlineKeyboardMarkup backKb = KeyboardHelper.buildSingleBackKeyboard("one#self_adv_center_new");

            if (StrUtil.isBlank(address.getQrImageId())) {
                File file = QRCodeGenerator.buildQrCode(
                        address.getAddress(),
                        callbackQuery.getFrom().getId(),
                        this.properties.getQrCodePath()
                );
                Message send = SyncSender.send(photoMarkdown(message, file, tipMarkdown, backKb));
                PhotoSize photoSize = send.getPhoto().stream().
                        max(Comparator.comparingInt(PhotoSize::getFileSize)).orElse(null);

                if (Objects.nonNull(photoSize)) {
                    address.setQrImageId(photoSize.getFileId());
                    this.addressService.updateById(address);
                }
            } else {
                AsyncSender.async(
                        photoMarkdown(message, address.getQrImageId(), tipMarkdown, backKb)
                );
            }
            return null;
        }
        // 我的广告
        if (StrUtil.equals(command.get(1), "self_adv")) {
            String statisticsText = this.advUserService.advStatistics(callbackQuery.getFrom().getId());
            String position = StrHelper.collGet(command, 2, "0");
            String status = StrHelper.collGet(command, 3, "0");
            int current = StrHelper.collGet(command, 4, 1);

            AdvTypeEnum advType = AdvTypeEnum.of(position);
            AdvStatus advStatus = AdvStatus.of(status);

            Page<AdvUser> userPage = this.advUserService.selfPage(current, callbackQuery.getFrom().getId(), advType, advStatus);
            InlineKeyboardMarkup markup = KeyboardHelper.buildSelfAdvKeyboard(position, status, userPage);
            return editMarkdown(message, statisticsText, markup);
        }
        // 专页购买
        if (StrUtil.equals(command.get(1), "special_page")) {
            Config config = configService.queryConfig();
            return ok(message, StrUtil.format("关键词专页购买帮助 @{}", config.getCustomUsername()));
        }

        // 用户广告详情
        if (StrUtil.equals(command.get(1), "user_adv_detail")) {
            Long advUserId = Long.parseLong(command.get(2));
            // 通过 priceId 查询购买该广告位的用户记录
            AdvUser advUser = this.advUserService.getById(advUserId);
            if (Objects.isNull(advUser)) {
                return answerAlert(callbackQuery, "未找到该广告购买记录!");
            }
            InlineKeyboardMarkup markup = KeyboardHelper.buildAdvUserDetailKeyboard(advUser);
            return editMarkdownV2(message, advUser.getAdvText(), markup);
        }
        return null;
    }

    private BotApiMethod<?> processorLevelOne(CallbackQuery callbackQuery, Message message, List<String> command) {
        // 广告投放
        if (StrUtil.equals(command.get(1), "advertising")) {
            Config config = this.configService.queryConfig();
            InlineKeyboardMarkup markup = KeyboardHelper.buildAdvertisingKeyboard();
            return editMarkdown(message, config.getAdvertisingMarkdown(), markup);
        }
        // 热搜词汇周期点击
        if (StrUtil.equals(command.get(1), "period")) {
            SearchPeriodEnum hit = SearchPeriodEnum.fromHit(Integer.parseInt(command.get(2)));
            List<HotSearch> keywords = this.hotSearchService.keywords(hit);
            InlineKeyboardMarkup markup = KeyboardHelper.buildHotSearchKeyboard(keywords, hit);
            return editKeyboard(message, markup);
        }
        // 我的钱包
        if (StrUtil.equals(command.get(1), "wallet")) {
            List<Bill> bills = this.billService.userBills(callbackQuery.getFrom().getId());
            User user = this.userService.user(callbackQuery.getFrom());
            String walletText = user.buildWalletText(bills);
            InlineKeyboardMarkup markup = KeyboardHelper.buildBindingTrcAddrKeyboard();
            return editMarkdownV2(message, walletText, markup);
        }
        // 邀请赚钱
        if (StrUtil.equals(command.get(1), "invite")) {
            User user = this.userService.user(callbackQuery.getFrom());
            Config config = this.configService.queryConfig();
            String inviteText = config.buildInviteText(this.properties.getBotUsername(), user.getInviteCode());
            String uri = this.properties.groupStart();

            InlineKeyboardMarkup markup = KeyboardHelper.buildInviteKeyboard(uri);
            AsyncSender.async(delete(message));
            ThreadHelper.sleepMs(50);
            AsyncSender.async(photoMarkdownV2(message, config.getInviteImageId(), inviteText, markup));
            return null;
        }
        // 群组频道
        if (StrUtil.equals(command.get(1), "group_channel")) {
            Page<Included> includedPage = this.includedService.selectPage(1,
                    callbackQuery.getFrom().getId(), IncludedSearchTypeEnum.ALL);
            String selfIndexText = this.includedService.buildSelfIndexText();
            InlineKeyboardMarkup markup = KeyboardHelper.buildGroupChannelKeyboard(IncludedSearchTypeEnum.ALL, includedPage);
            return editMarkdown(message, selfIndexText, markup);
        }
        // 提交记录
        if (StrUtil.equals(command.get(1), "commit_record")) {
            Config config = configService.queryConfig();
            if (StrUtil.isBlank(config.getCommitRecordMarkdown())) {
                return null;
            }
            InlineKeyboardMarkup keyboard = KeyboardHelper.keyboard(config.getCommitRecordKeyboard());
            List<InlineKeyboardRow> ks;
            if (CollUtil.isEmpty(keyboard.getKeyboard())) {
                ks = new ArrayList<>();
            } else {
                ks = new ArrayList<>(keyboard.getKeyboard());
            }
            ks.add(KeyboardHelper.row(KeyboardHelper.buttonText("⬅️返回", "two#self")));
            keyboard.setKeyboard(ks);
            return markdown(message, config.getCommitRecordMarkdown(), keyboard);
        }
        // 个人广告中心
        if (StrUtil.equalsAny(command.get(1), "self_adv_center", "self_adv_center_new")){
            boolean edit = StrUtil.equals(command.get(1), "self_adv_center");
            User user = this.userService.user(callbackQuery.getFrom());
            String text2 = StrUtil.format(Constants.ADV_CENTER_TEXT,
                    user.getNickname(),
                    user.getUserId(),
                    DecimalHelper.standard(user.getBalance()),
                    DecimalHelper.standard(user.getTotalRechargeAmount()),
                    DecimalHelper.standard(user.getAwardAmount())
            );
            Config config = this.configService.queryConfig();
            InlineKeyboardMarkup markup = KeyboardHelper.buildAdvCenterKeyboard(
                    config.getCommunityName()
            );
            if (edit) {
                return editMarkdown(message, text2, markup);
            }
            return markdown(message, text2, markup);
        }

        // 顶部链接 & 底部链接
        if (StrUtil.equalsAny(command.get(1), "top_link", "bottom_button")){
            boolean top = StrUtil.equals(command.get(1), "top_link");

            Config config = this.configService.queryConfig();
            InlineKeyboardMarkup markup;
            String messageText;
            if (top) {
                messageText = """
                            \uD83D\uDCE2 *顶部链接*
                            此广告将会展示在搜索结果的顶部，根据展现次数计费。
                            *可选套餐如下：*""";
                markup = KeyboardHelper.keyboard(config.getTopLinkPackage());
            } else {
                messageText = """
                            \uD83D\uDCE2 *底部按钮*
                            此广告将会展示在搜索结果的底部按钮，根据展现次数计费。
                            *可选套餐如下：*""";
                markup = KeyboardHelper.keyboard(config.getBottomButtonPackage());
            }
            return editMarkdown(message, messageText, markup);
        }
        // 品牌专页 关键词专页 关键词排行
        if (StrUtil.equalsAny(command.get(1), "brand_page", "keyword_page", "keyword_rank")) {

            Config config = this.configService.queryConfig();
            if (StrUtil.isBlank(config.getBrandPageText())
                    || StrUtil.isBlank(config.getKeywordPageText())
                    || StrUtil.isBlank(config.getKeywordRankText())) {
                return null;
            }

            String resultText = "";
            if (StrUtil.equals(command.get(1), "brand_page")) {
                resultText = config.getBrandPageText();
            }
            if (StrUtil.equals(command.get(1), "keyword_page")) {
                resultText = config.getKeywordPageText();
                CommonCache.putData(callbackQuery.getFrom().getId(), "keyword_page");
            }
            if (StrUtil.equals(command.get(1), "keyword_rank")) {
                resultText = config.getKeywordRankText();
                CommonCache.putData(callbackQuery.getFrom().getId(), "keyword_rank");
            }

            if (StrUtil.isBlank(resultText)) {
                return null;
            }

            InlineKeyboardMarkup markup = KeyboardHelper.buildBrandPageKeyboard();
            return editMarkdown(message, resultText, markup);
        }

        // 关键词
        if (StrUtil.equals(command.get(1), "query_keyword")) {
            long libraryId = Long.parseLong(command.get(3));
            AdvLibrary library = this.advLibraryService.getByIdWithPrices(libraryId);
            return this.privateChatHandler.processorQueryKeyword(message, library, command.get(2));
        }
        return null;
    }
}
