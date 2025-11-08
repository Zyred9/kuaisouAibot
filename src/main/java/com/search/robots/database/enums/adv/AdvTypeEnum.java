package com.search.robots.database.enums.adv;


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
public enum AdvTypeEnum {

    BUY_TOP_LINK(1, "顶部链接"),
    BUY_BOTTOM_BUTTON(2, "底部按钮"),
    BUY_KEYWORD_RANK(3, "关键词排行广告"),
    BUY_KEYWORD_PAGE_RANK(4, "关键词专页广告"),
    BUY_BRAND_PAGE_RANK(5, "品牌专页广告"),
    ;

    @EnumValue
    private final int code;
    private final String desc;

}
