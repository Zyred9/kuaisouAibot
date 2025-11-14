package com.search.robots.helper;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.view.ButtonTransfer;
import com.search.robots.beans.view.KeyboardTransfer;
import com.search.robots.beans.view.vo.search.SearchBean;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.*;
import com.search.robots.database.enums.Included.IncludedNewUserEnum;
import com.search.robots.database.enums.Included.IncludedSearchPriorityEnum;
import com.search.robots.database.enums.Included.IncludedSearchTypeEnum;
import com.search.robots.database.enums.SearchPeriodEnum;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.database.enums.content.SortEnum;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.*;

import java.util.*;

/**
 * <p>
 * 键盘助手类
 * <pre>
 * 提供各种场景下的键盘按钮配置
 * 支持广告购买、关键词查询等功能按钮
 * </pre>
 *
 * @author admin
 * @since v 0.0.1
 */
public class KeyboardHelper {


    /**
     *
     * @param hitType       资源类型
     * @param current       当前分页
     * @param filter        是否过滤18
     * @param sort          排序
     * @param bot           机器人名字
     * @param beans         查询结果
     * @return              按钮
     */
    public static InlineKeyboardMarkup buildSearchResultKeyboard(String hitType, int current, Boolean filter, SortEnum sort,
                                                                 String bot, org.springframework.data.domain.Page<SearchBean> beans,
                                                                 String keyword) {
        List<InlineKeyboardRow> rows = new ArrayList<>(2);

        // 第一排
        InlineKeyboardRow sourceRow = new InlineKeyboardRow();
        for (SourceTypeEnum type : SourceTypeEnum.keyboards()) {
            String name = type.getIcon();
            if (StrUtil.equals(hitType, type.getCode())) {
                name = SourceTypeEnum.FLUSH.getIcon();
            }
            sourceRow.add(
                buttonText(name, StrHelper.buildName("search", type.getCode(),
                        current, filter, sort.getCode(), keyword))
            );
        }
        rows.add(sourceRow);

        // 有浏览量、时长、最新的
        InlineKeyboardRow viewsRow = new InlineKeyboardRow();
        if (CollUtil.contains(SourceTypeEnum.views(), hitType)) {
            for (SortEnum value : SortEnum.keyboards()) {
                String name = value.getIcon();
                if (Objects.equals(sort, value)) {
                    name = SortEnum.EMPTY.getIcon();
                }
                viewsRow.add(
                        buttonText(name, StrHelper.buildName("search", hitType,
                                current, filter, value.getCode(), keyword))
                );
            }
        }
        rows.add(viewsRow);

        InlineKeyboardRow optionRow = new InlineKeyboardRow();
        // 购买广告
        optionRow.add(buttonUrl("购买广告", StrUtil.format(Constants.START_AD_CENTER, bot)));
        optionRow.add(buttonText("\uD83D\uDD1E过滤", StrHelper.buildName("search", hitType, current, !filter, sort.getCode(), keyword)));

        if (beans.hasPrevious()) {
            optionRow.add(buttonText("⬅️上一页", StrHelper.buildName("search", hitType, (current - 1), filter, sort.getCode(), keyword)));
        }
        if (beans.hasNext()) {
            optionRow.add(buttonText("➡️下一页", StrHelper.buildName("search", hitType, (current + 1), filter, sort.getCode(), keyword)));
        }
        rows.add(optionRow);
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildAuditAfterKeyboard(AdvUser advUser) {
        return InlineKeyboardMarkup.builder().keyboardRow(
                row(buttonText(advUser.buildButtonName(), "two#user_adv_detail#" + advUser.getId()))
        ).build();
    }

    public static InlineKeyboardMarkup buildSelfAdvKeyboard (String position, String status, Page<AdvUser> page) {
        String buttonPrefix = "two#self_adv";
        List<InlineKeyboardRow> rows = new ArrayList<>();
        rows.add(row(
                buttonText(StrHelper.hit("全部", StrUtil.equals(position, "0")), StrHelper.buildName(buttonPrefix, "0", status, page.getCurrent())),
                buttonText(AdvTypeEnum.BUY_KEYWORD_RANK.buildName(position), StrHelper.buildName(buttonPrefix, "3", status, page.getCurrent())),
                buttonText(AdvTypeEnum.BUY_KEYWORD_PAGE_RANK.buildName(position), StrHelper.buildName(buttonPrefix, "4", status, page.getCurrent())),
                buttonText(AdvTypeEnum.BUY_BRAND_PAGE_RANK.buildName(position), StrHelper.buildName(buttonPrefix, "5", status, page.getCurrent())),
                buttonText(AdvTypeEnum.BUY_TOP_LINK.buildName(position), StrHelper.buildName(buttonPrefix, "1", status, page.getCurrent())),
                buttonText(AdvTypeEnum.BUY_BOTTOM_BUTTON.buildName(position), StrHelper.buildName(buttonPrefix, "2", status, page.getCurrent()))
        ));
        rows.add(row(
                buttonText(StrHelper.hit("全部", StrUtil.equals(status, "0")), StrHelper.buildName(buttonPrefix, position, "0", page.getCurrent())),
                buttonText(AdvStatus.UN_START.buildName(status), StrHelper.buildName(buttonPrefix, position, "1", page.getCurrent())),
                buttonText(AdvStatus.PROMOTION_ING.buildName(status), StrHelper.buildName(buttonPrefix, position, "4", page.getCurrent())),
                buttonText(AdvStatus.PAUSE_ING.buildName(status), StrHelper.buildName(buttonPrefix, position, "5", page.getCurrent())),
                buttonText(AdvStatus.THE_END.buildName(status), StrHelper.buildName(buttonPrefix, position, "6", page.getCurrent())),
                buttonText(AdvStatus.UNDER_APPROVAL.buildName(status), StrHelper.buildName(buttonPrefix, position, "2", page.getCurrent()))
        ));

        for (AdvUser record : page.getRecords()) {
            String callbackData = StrHelper.buildName("two#user_adv_detail", record.getId());
            rows.add(row(buttonText(record.buildButtonName(), callbackData)));
        }
        if (page.hasNext() || page.hasPrevious()) {
            InlineKeyboardRow pageRow = new InlineKeyboardRow();
            if (page.hasPrevious()) {
                String prev =  StrHelper.buildName(buttonPrefix, position, status, (page.getCurrent() + 1));
                pageRow.add(buttonText("上一页", prev));
            }
            if (page.hasNext()) {
                String next =  StrHelper.buildName(buttonPrefix, position, status, (page.getCurrent() - 1));
                pageRow.add(buttonText("下一页", next));
            }
            rows.add(pageRow);
        }
        rows.add(row(buttonText("⬅️返回", "one#self_adv_center")));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildPymentKeywordKeyboard (Long userAdvId, String prev, Long libraryId) {
        List<InlineKeyboardRow> rows = new ArrayList<>(6);
        rows.add(row(buttonText("\uD83D\uDD04优先续订", "three#my_adv#priority_renewal#" + userAdvId)));
        rows.add(row(buttonText("\uD83D\uDFE2开始推广", "three#my_adv#start_promotion#" + userAdvId)));
        rows.add(row(
                buttonText("✏️修改广告标题", "three#my_adv#edit_title#" + prev + "#" + userAdvId),
                buttonText("✏️修改广告链接", "three#my_adv#edit_link#" + prev + "#" + userAdvId)
        ));
        if (Objects.nonNull(libraryId)) {
            rows.add(row(buttonText("⬅️返回关键词列表", "one#query_keyword#" + prev + "#" + libraryId)));
        }
        rows.add(row(buttonText("⬅️返回我的广告", "two#self_adv#0#0#1")));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }


    public static InlineKeyboardMarkup buildAdvUserDetailKeyboard (AdvUser advUser) {
        String prev = AdvTypeEnum.dataOf(advUser.getAdvType());
        String name = Objects.equals(advUser.getAdvStatus(), AdvStatus.PROMOTION_ING) ?
                "\uD83D\uDFE0暂停推广" : "\uD83D\uDFE2开始推广";
        InlineKeyboardButton button = buttonText(name, "three#my_adv#start_promotion#" + advUser.getId());

        return InlineKeyboardMarkup.builder().keyboard(List.of(
                row(buttonText("\uD83D\uDD04优先续订", "three#my_adv#priority_renewal#" + advUser.getId())),
                row(button),
                row(buttonText("✏️修改广告标题", "three#my_adv#edit_title#" + prev + "#" + advUser.getId()),
                        buttonText("✏️修改广告链接", "three#my_adv#edit_link#" + prev + "#" + advUser.getId())),
                row(buttonText("⬅️返回关键词列表", "one#query_keyword#" + prev + "#" + advUser.getLibraryId())),
                row(buttonText("⬅️返回我的广告", "two#self_adv#0#0#1"))
        )).build();
    }


    public static InlineKeyboardMarkup buildKeywordSoldKeyboard (String prev, Long libraryId) {
        return InlineKeyboardMarkup.builder().keyboard(List.of(
                row(buttonText("⬅️返回关键词列表", "one#query_keyword#" + prev + "#" + libraryId)),
                row(buttonText("⬅️返回我的广告", "two#self_adv#0#0#1"))
        )).build();
    }

    public static InlineKeyboardMarkup buildToBuyKeywordKeyboard(Long priceId, Long libraryId, String prev) {
        List<InlineKeyboardRow> rows = new ArrayList<>(2);
        rows.add(
                row(buttonText("⬅️返回", "one#query_keyword#" + prev + "#" + libraryId),
                buttonText("✅确认支付", "three#do_payment_keyword#" + prev + "#" + priceId))
        );
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildKeywordQueryKeyboard(List<AdvPrice> priceList, String data) {
        if (CollUtil.isEmpty(priceList)) {
            priceList = Collections.emptyList();
        }
        List<InlineKeyboardRow> rows = new ArrayList<>();
        List<AdvPrice> rankPrices = priceList.stream()
                .filter(price -> price.getAdvPosition().isRankPosition())
                .sorted((p1, p2) -> p2.getMonthlyPrice().compareTo(p1.getMonthlyPrice()))
                .toList();
        List<AdvPrice> pagePrices =  priceList.stream()
                .filter(price -> price.getAdvPosition().isPagePosition())
                .sorted(Comparator.comparingInt(p -> p.getAdvPosition().getCode()))
                .toList();

        rows.add(row(buttonText("👇关键词排行", "ignore")));
        rankPrices.forEach(price -> {
            String icon = price.getAdvPosition().getIcon();
            String formattedPrice = DecimalHelper.decimalParse(price.getMonthlyPrice());
            String priceText = icon + formattedPrice + "$/月";

            boolean isSold = Boolean.TRUE.equals(price.getIsSold());
            String callback = isSold
                    ? "two#sold#" + data + "#" + price.getId()
                    : "two#to_buy#" + data + "#" + price.getId();
            rows.add(row(
                    buttonText(priceText, callback),
                    buttonText(isSold ? "已被购买" : "购买", callback)
            ));
        });


        rows.add(row(buttonText("👇关键词专页", "ignore")));
        pagePrices.forEach(price -> {
            String formattedPrice = DecimalHelper
                    .decimalParse(price.getMonthlyPrice());
            String icon = price.getAdvPosition().getIcon();
            String priceText = icon + formattedPrice + "$/月";

            boolean isSold = Boolean.TRUE.equals(price.getIsSold());
            String callback = "two#special_page";
            rows.add(row(
                    buttonText(priceText, callback),
                    buttonText(isSold ? "已被购买" : "购买", callback)
            ));
        });

        rows.add(row(
                buttonText("🔍相关热搜词", "one#hotsearch"),
                buttonText("⬅️返回", "one#keyword_rank")
        ));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildBrandPageKeyboard() {
        return InlineKeyboardMarkup.builder()
                .keyboardRow(row(
                        buttonText("⬅️返回", "one#advertising"),
                        buttonText("\uD83D\uDD25热搜", "one#hotsearch")
                )).build();
    }

    public static InlineKeyboardMarkup buildAdvPaymentLackKeyboard() {
        return InlineKeyboardMarkup.builder()
                .keyboardRow(row(
                        buttonText("⬅️返回", "two#adv_recharge"),
                        buttonText("\uD83D\uDCB0充值", "three#adv_recharge")
                )).build();
    }


    public static InlineKeyboardMarkup buildAdvDetailKeyboard(String prev, String price) {
        List<InlineKeyboardRow> rows = new ArrayList<>(2);
        rows.add(
                row(buttonText("⬅️返回", "one#" + prev),
                buttonText("✅确认支付", "three#do_payment_button#" + prev + "#" + price))
        );
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildAdvCenterKeyboard(String communityName) {
        List<InlineKeyboardRow> rows = new ArrayList<>(6);
        rows.add(row(buttonText("\uD83D\uDC64我的广告", "two#self_adv#0#0#1")));
        rows.add(row(buttonText("\uD83D\uDCB0充值", "two#adv_recharge")));
        rows.add(row(buttonUrl("\uD83D\uDC65交流群", "https://t.me/" + communityName)));
        rows.add(row(buttonText("⬅️返回", "one#advertising")));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildAdvertisingKeyboard() {
        List<InlineKeyboardRow> rows = new ArrayList<>(8);
        rows.add(row(buttonText("\uD83E\uDD47关键词排行", "one#keyword_rank")));
        rows.add(row(buttonText("\uD83E\uDD47关键词专页", "one#keyword_page")));
        rows.add(row(buttonText("\uD83D\uDCCC品牌专页", "one#brand_page")));
        rows.add(row(buttonText("\uD83C\uDF10顶部链接", "one#top_link")));
        rows.add(row(buttonText("\uD83C\uDF10底部按钮", "one#bottom_button")));
        rows.add(row(buttonText("\uD83D\uDC64个人广告中心", "one#self_adv_center")));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }


    public static InlineKeyboardMarkup buildNextDetailKeyboard(Page<User> children, int current) {
        List<InlineKeyboardRow> rows = new ArrayList<>(11);
        if (children.hasNext() || children.hasPrevious()) {
            InlineKeyboardRow pageRow = new InlineKeyboardRow();

            String prefix = "two#get_spread_next#";
            if (children.hasPrevious()) {
                String prev = prefix + (children.getCurrent() - 1);
                pageRow.add(buttonText("上一页", prev));
            }
            if (children.hasNext()) {
                String next = prefix + (children.getCurrent() + 1);
                pageRow.add(buttonText("下一页", next));
            }
            rows.add(pageRow);
        }

        rows.add(row(buttonText("⬅️返回", "one#invite")));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }


    public static InlineKeyboardMarkup buildSingleBackKeyboard (String callback) {
        return InlineKeyboardMarkup.builder().keyboardRow(row(buttonText("⬅️返回", callback))).build();
    }


    public static InlineKeyboardMarkup buildTargetedSearchKeyboard(Included included, List<Included> children) {
        children = CollUtil.isEmpty(children) ? Collections.emptyList() : children;

        List<InlineKeyboardRow> rows = new ArrayList<>();
        for (Included child : children) {
            String name = (Boolean.TRUE.equals(child.getChildTargetedSearch()) ? "✅" : "")
                    + child.getPrivacyType().getBtn() + "|"
                    + child.getIndexTitle()
                    + "|资源数:"
                    + child.getSourceCount();
            rows.add(row(buttonText(name, "seven#targeted_search#enable#" + included.getId() + "#" + child.getId())));
        }
        rows.add(row(buttonText("➕添加定向搜索频道/群组", "seven#targeted_search#add#" + included.getId())));
        rows.add(row(buttonText("⬅️返回", "four#privacy_setting#search_result_customize#" + included.getId())));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildSearchResultCustomizeKeyboard(Included included) {
        List<InlineKeyboardRow> rows = new ArrayList<>();

        List<Long> ids = included.getTargetedSearchIndexIds();

        String gName = Boolean.TRUE.equals(included.getOpenGlobalSearch()) ? "✅全局搜索" : "\uD83D\uDD18全局搜索";
        String tName = Boolean.FALSE.equals(included.getOpenGlobalSearch()) ? (
                CollUtil.isNotEmpty(ids) ?
                StrUtil.format("✅定向搜索(已开启{}个目标，点击修改)", ids.size()) : "✅定向搜索"
        ) : "\uD83D\uDD18定向搜索";

        rows.add(row(buttonText(gName, "six#search_result_customize#global_search#" + included.getId())));
        rows.add(row(buttonText(tName, "six#search_result_customize#targeted_search#" + included.getId())));
        rows.add(row(buttonText("⬅️返回", "three#details_setting#search_customize#" + included.getId())));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildSearchPriorityCustomizeKeyboard(Included included) {
        List<IncludedSearchPriorityEnum> priorities = CollUtil.isEmpty(included.getPriorities())
                ? Collections.emptyList() : included.getPriorities();

        IncludedSearchPriorityEnum[] values = IncludedSearchPriorityEnum.values();
        List<InlineKeyboardRow> rows = new ArrayList<>(values.length);

        Set<IncludedSearchPriorityEnum> hits = new HashSet<>(priorities);
        for (IncludedSearchPriorityEnum value : values) {
            String name = StrHelper.hit(value.getDesc(), hits.contains(value));
            rows.add(row(buttonText(name, value.getCall() + included.getId())));
        }
        rows.add(row(buttonText("⬅️返回", "three#details_setting#search_customize#" + included.getId())));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildSearchCustomizeKeyboard(Included included) {
        List<InlineKeyboardRow> rows = new ArrayList<>(4);
        rows.add(row(buttonText(Boolean.TRUE.equals(included.getOpenPrivacySearch()) ? "✅已开启隐私搜索" : "❌已关闭隐私搜索", "four#privacy_setting#open_privacy_search#" + included.getId())));
        rows.add(row(buttonText(Boolean.TRUE.equals(included.getOpenFilterMinors()) ? "✅已过滤\uD83D\uDD1E资源" : "❌过滤\uD83D\uDD1E资源", "four#privacy_setting#open_filter_minors#" + included.getId())));
        rows.add(row(buttonText("⚙️优先级自定义", "four#privacy_setting#priority_customize#" + included.getId())));
        rows.add(row(buttonText("⚙️搜索结果自定义", "four#privacy_setting#search_result_customize#" + included.getId())));
        rows.add(row(buttonText("⬅️返回", "two#included_detail#" + included.getId())));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildNewUserKeyboard(Included included) {

        List<InlineKeyboardRow> rows = new ArrayList<>(11);
        for (IncludedNewUserEnum value : IncludedNewUserEnum.values()) {
            String name = "\uD83D\uDD32" + value.getDesc();
            if (Objects.equals(included.getNewUsers(), value)) {
                name = "✅" + value.getDesc();
            }
            rows.add(row(
                buttonText(name, value.getCall() + included.getId())
            ));
        }
        rows.add(row(buttonText("⬅️返回", "two#included_detail#" + included.getId())));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildIncludedDetailKeyboard(Included included) {
        String searchBtnName = Boolean.TRUE.equals(included.getOpenSearch()) ? "✅已开启搜索" : "❌已关闭搜索";
        String listenBtnName = Boolean.FALSE.equals(included.getOpenListen()) ? "❌已关闭消息监听" : "✅已开启消息监听";
        String adsBtnName = StrUtil.format("✅已开启拉新广告（{}）", included.getNewUsers().getDesc());

        List<InlineKeyboardRow> rows = new ArrayList<>();
        rows.add(row(buttonText("\uD83D\uDD04更新基础信息", "three#details_setting#update_basic#" + included.getId())));
        rows.add(row(buttonText("\uD83D\uDCCA查看曝光数据", "three#details_setting#exposure_data#" + included.getId())));

        if (Boolean.TRUE.equals(included.getOpenSearch())) {
            rows.add(row(
                    buttonText(searchBtnName, "three#details_setting#search_toggle#" + included.getId()),
                    buttonText("⚙️搜索自定义", "three#details_setting#search_customize#" + included.getId())
            ));
        } else {
            rows.add(row(
                    buttonText(searchBtnName, "three#details_setting#search_toggle#" + included.getId())
            ));
        }

        rows.add(row(buttonText(listenBtnName, "three#details_setting#listen_toggle#" + included.getId())));
        rows.add(row(buttonText(adsBtnName, "three#details_setting#ads_toggle#" + included.getId())));
        rows.add(row(buttonText("⬅️返回", "one#group_channel")));

        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }


    public static InlineKeyboardMarkup buildGroupChannelKeyboard (IncludedSearchTypeEnum hit, Page<Included> includePage) {
        List<InlineKeyboardRow> rows = new ArrayList<>(11);

        InlineKeyboardRow line = new InlineKeyboardRow();
        rows.add(line);

        for (IncludedSearchTypeEnum value : IncludedSearchTypeEnum.values()) {
            line.add(buttonText(StrHelper.hit(value.getDesc(), value, hit), value.getCall() + "1"));
        }

        List<Included> records = includePage.getRecords();
        for (Included record : records) {
            String subTitle = StrUtil.sub(record.getIndexTitle(), 0, 10);
            String desc = record.getIncludedStatus().getDesc();
            rows.add(row(
                    buttonText(subTitle + "|" + desc, "two#included_detail#" + record.getId())
            ));
        }

        if (includePage.hasNext() || includePage.hasPrevious()) {
            InlineKeyboardRow pageRow = new InlineKeyboardRow();
            if (includePage.hasPrevious()) {
                String prev = hit.getCall() + (includePage.getCurrent() - 1);
                pageRow.add(buttonText("上一页", prev));
            }
            if (includePage.hasNext()) {
                String next = hit.getCall() + (includePage.getCurrent() + 1);
                pageRow.add(buttonText("下一页", next));
            }
            rows.add(pageRow);
        }

        rows.add(row(buttonText("⬅️返回", "two#self")));
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }

    public static InlineKeyboardMarkup buildInviteKeyboard(String uri) {
        return InlineKeyboardMarkup.builder()
                .keyboard(List.of(
                        row(buttonText("\uD83D\uDE4C获取推广参考文案", "two#get_spread_text")),
                        row(buttonUrl("\uD83C\uDF81邀请机器人", uri)),
                        row(
                                buttonText("\uD83D\uDCB9推广报表", "two#get_spread_statement"),
                                buttonText("\uD83D\uDCC8广告代理下级明细", "two#get_spread_next")
                        ),
                        row(buttonText("⬅️返回", "two#self#delete"))
                )).build();
    }

    public static InlineKeyboardMarkup buildBindingTrcAddrSuccessKeyboard() {
        return InlineKeyboardMarkup.builder()
                .keyboard(List.of(
                        row(
                                buttonText("\uD83D\uDCB5提现", "two#withdrawal"),
                                buttonText("\uD83D\uDC64我的", "two#self")
                        )
                )).build();
    }

    public static InlineKeyboardMarkup buildBindingTrcAddrKeyboard() {
        return InlineKeyboardMarkup.builder()
                .keyboard(List.of(
                        row(
                                buttonText("✏️更新TRC20地址", "two#update_addr"),
                                buttonText("\uD83D\uDCB5提现", "two#withdrawal")
                        ),
                        row(buttonText("⬅️返回", "two#self"))
                )).build();
    }

    public static InlineKeyboardMarkup buildHotSearchKeyboard (List<HotSearch> hots, SearchPeriodEnum hit) {
        List<InlineKeyboardRow> rows = new ArrayList<>(11);

        InlineKeyboardRow line = new InlineKeyboardRow();
        rows.add(line);

        for (SearchPeriodEnum value : SearchPeriodEnum.values()) {
            line.add(buttonText(StrHelper.hit(value.getDesc(), value, hit), value.getCall()));
        }

        if (CollUtil.isNotEmpty(hots)) {
            InlineKeyboardRow row = new InlineKeyboardRow();
            rows.add(row);
            for (int i = 0, idx = 0, max = 4; i < hots.size(); i++) {
                HotSearch hotSearch = hots.get(i);
                if (idx == max) {
                    row = new InlineKeyboardRow();
                    rows.add(row);
                    idx = 0;
                }
                row.add(buttonText(hotSearch.getKeyword(), "one#keyword#" + hotSearch.getId()));
                idx ++;
            }
        }
        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }


    public static InlineKeyboardMarkup buildSelfKeyboard() {
        return InlineKeyboardMarkup.builder()
                .keyboard(List.of(
                        rowChosen("\uD83C\uDF81分享", "invite"),
                        row(
                                buttonText("\uD83D\uDCB5我的钱包", "one#wallet"),
                                buttonText("\uD83D\uDCB0邀请赚钱", "one#invite")
                        ),
                        row(
                                buttonText("⚙️群组频道", "one#group_channel"),
                                buttonText("\uD83D\uDD17提交记录", "one#commit_record")
                        )
                )).build();
    }


    public static InlineKeyboardMarkup buildPacketKeyboard(Boolean packet, int query) {
        return InlineKeyboardMarkup.builder()
                .keyboard(List.of(
                        row(
                                buttonText(packet == null ? "是" :  packet ? "✅是" : "是", ""),
                                buttonText(packet == null ? "否" : !packet ? "✅否" : "否", "")
                        ),
                        row(buttonText("\uD83D\uDD1C 下一步", "privacy#packet#confirm#"+query), cancelButton())
                )).build();
    }

    public static InlineKeyboardMarkup buildPageOfKeyboard(Page<Object> pages) {
        List<InlineKeyboardRow> rows = new ArrayList<>();
        InlineKeyboardRow row = new InlineKeyboardRow();
        rows.add(row);

        int idx = 0, max = 3;
        List<Object> records = pages.getRecords();
        for (Object addr : records) {
            if (idx == max) {
                row = new InlineKeyboardRow();
                rows.add(row);
                idx = 0;
            }
            row.add(buttonText("", ""));
            idx ++;
        }

        if (pages.hasNext() || pages.hasPrevious()) {
            InlineKeyboardRow pageRow = new InlineKeyboardRow();
            if (pages.hasPrevious()) {
                pageRow.add(buttonText("上一页", ""));
            }
            if (pages.hasNext()) {
                pageRow.add(buttonText("下一页", ""));
            }
            rows.add(pageRow);
        }

        InlineKeyboardRow back = new InlineKeyboardRow();
        back.add(buttonText("返回上一级", ""));
        back.add(cancelButton());
        rows.add(back);

        return InlineKeyboardMarkup.builder().keyboard(rows).build();
    }


    public static ReplyKeyboard buildStartKeyboard() {
        return ReplyKeyboardMarkup.builder()
                .resizeKeyboard(true)
                .keyboard(List.of(
                        new KeyboardRow(List.of(
                                KeyboardButton.builder().text("\uD83D\uDD0D热搜").build(),
                                KeyboardButton.builder().text("\uD83D\uDC64我的").build())
                        )
                )).build();
    }

    public static InlineKeyboardMarkup keyboard (String json) {
        InlineKeyboardMarkup markup = null;
        if (StrUtil.isNotBlank(json)) {
            KeyboardTransfer transfer = JSONUtil.toBean(json, KeyboardTransfer.class);
            if (Objects.nonNull(transfer)) {
                List<InlineKeyboardRow> rows = new ArrayList<>(transfer.getKeyboard().size());
                for (List<ButtonTransfer> buttonTransfers : transfer.getKeyboard()) {
                    InlineKeyboardRow row = new InlineKeyboardRow();
                    for (ButtonTransfer buttonTransfer : buttonTransfers) {
                        if (StrUtil.isNotBlank(buttonTransfer.getUrl())) {
                            row.add(InlineKeyboardButton.builder()
                                    .text(buttonTransfer.getText()).url(buttonTransfer.getUrl()).build());
                        }
                        if (StrUtil.isNotBlank(buttonTransfer.getCallback_data())) {
                            row.add(InlineKeyboardButton.builder()
                                    .text(buttonTransfer.getText()).callbackData(buttonTransfer.getCallback_data()).build());
                        }
                    }
                    rows.add(row);
                }
                markup = InlineKeyboardMarkup.builder().keyboard(rows).build();
            }
        } else {
            markup = InlineKeyboardMarkup.builder().build();
        }
        return markup;
    }

    /*
        设置广告#-1002344866985#15:30
        &内容内容内容内容内容内容内容内容内容内容内容内容内容内容内容内容内容内容内容内容内容
        &24小时客服#https://t.me/|24小时客服#https://t.me/$24小时客服#https://t.me/
    */

    public static void main(String[] args) {
        String key = "&交流群#https://t.me/DevelopBotAny668";
        String s = parseKeyboard(key);
        System.out.println(s);
    }

    public static String parseKeyboard(String keyboardCommand) {
        String keyboardJson = "";
        if (StrUtil.isNotBlank(keyboardCommand)) {
            // &24小时客服#https://t.me/|24小时客服#https://t.me/$24小时客服#https://t.me/
            List<String> keyboardLines = StrUtil.split(keyboardCommand, "$");
            List<InlineKeyboardRow> rows = new ArrayList<>(keyboardLines.size());
            for (String keyboardLine : keyboardLines) {
                // 24小时客服#https://t.me/|24小时客服#https://t.me/
                List<String> row = StrUtil.split(keyboardLine, "|");
                InlineKeyboardRow keyboardRow = new InlineKeyboardRow();
                for (String buttonLine : row) {
                    List<String> buttons = StrUtil.split(buttonLine, "#");
                    keyboardRow.add(InlineKeyboardButton.builder().text(buttons.get(0)).url(buttons.get(1)).build());
                }
                rows.add(keyboardRow);
            }
            InlineKeyboardMarkup markup = InlineKeyboardMarkup.builder()
                    .keyboard(rows)
                    .build();
            keyboardJson = JSONUtil.toJsonStr(markup);
        }
        return keyboardJson;
    }

    public static InlineKeyboardButton cancelButton () {
        return InlineKeyboardButton.builder().text("❌取消").callbackData("delete").build();
    }

    public static InlineKeyboardRow row (InlineKeyboardButton ... buttons) {
        InlineKeyboardRow row = new InlineKeyboardRow();
        row.addAll(Arrays.asList(buttons));
        return row;
    }

    public static InlineKeyboardRow rowChosen (String name, String defaultVal) {
        return new InlineKeyboardRow(InlineKeyboardButton.builder()
                .text(name)
                .switchInlineQueryChosenChat(SwitchInlineQueryChosenChat.builder()
                        .allowGroupChats(true)
                        .allowUserChats(true)
                        .allowChannelChats(true)
                        .build())
                .switchInlineQuery(defaultVal)
                .build());
    }

    public static InlineKeyboardButton chosenButton (String name, String defaultVal) {
        return InlineKeyboardButton.builder()
                .text(name)
                .switchInlineQueryChosenChat(SwitchInlineQueryChosenChat.builder()
                        .allowGroupChats(true)
                        .allowUserChats(true)
                        .allowChannelChats(true)
                        .build())
                .switchInlineQuery(defaultVal)
                .build();
    }

    public static InlineKeyboardRow row (String[] names, String[] callbacks) {
        InlineKeyboardRow row = new InlineKeyboardRow();
        for (int i = 0; i < names.length; i++) {
            row.add(buttonText(names[i], callbacks[i]));
        }
        return row;
    }

    public static InlineKeyboardButton buttonUrl (String name, String url) {
        return InlineKeyboardButton.builder().url(url).text(name).build();
    }

     public static InlineKeyboardButton buttonText (String name, String callback) {
        return InlineKeyboardButton.builder().text(name).callbackData(callback).build();
    }

    public static String[] arr (String ... k) {
        return k;
    }
}
