package com.search.robots.database.enums;


import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 *
 *
 * @author zyred
 * @since 2025/11/1 21:37
 */
@Getter
@AllArgsConstructor
public enum BillTypeEnum {

    // 账单类型：0-奖励，1-提现，2-购买顶部链接，3-购买底部按钮
    AWARD(0, "奖励"),
    WITHDRAWAL(1, "提现"),
    BUY_TOP_LINK(2, "购买顶部链接"),
    BUY_BOTTOM_BUTTON(3, "购买底部按钮"),
    BUY_KEYWORD_RANK(4, "购买关键词排行广告"),
    BUY_KEYWORD_PAGE_RANK(5, "购买关键词专页广告"),
    BUY_BRAND_PAGE_RANK(6, "购买品牌专页广告"),
    ;

    @EnumValue
    private final int code;
    private final String desc;


    public static BillTypeEnum ofData (String data) {
        BillTypeEnum bt = null;
        if (StrUtil.equals(data, "keyword_page")) {
            bt = BillTypeEnum.BUY_KEYWORD_PAGE_RANK;
        }
        else if (StrUtil.equals(data, "keyword_rank")) {
            bt = BillTypeEnum.BUY_KEYWORD_RANK;
        }
        if (StrUtil.equals(data, "top_link")) {
            bt = BillTypeEnum.BUY_TOP_LINK;
        }
        else if (StrUtil.equals(data, "bottom_button")) {
            bt = BillTypeEnum.BUY_BOTTOM_BUTTON;
        }
        return bt;
    }
}
