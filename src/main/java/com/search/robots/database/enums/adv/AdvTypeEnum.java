package com.search.robots.database.enums.adv;


import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.EnumValue;
import com.search.robots.helper.StrHelper;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;
import java.util.Objects;

/**
 *
 *
 * @author zyred
 * @since 2025/11/1 21:37
 */
@Getter
@AllArgsConstructor
public enum AdvTypeEnum {

    BUY_TOP_LINK(1, "顶部链接", "按钮链接"),
    BUY_BOTTOM_BUTTON(2, "按钮链接", "按钮链接"),
    BUY_KEYWORD_RANK(3, "关键词排行", "关键词"),
    BUY_KEYWORD_PAGE_RANK(4, "关键词专页", "关键词"),
    BUY_BRAND_PAGE_RANK(5, "品牌专页", "品牌专页"),
    ;

    @EnumValue
    private final int code;
    private final String desc;
    private final String prefix;

    public static AdvTypeEnum ofData(String data) {
        if (StrUtil.equals(data, "keyword_rank")) {
            return BUY_KEYWORD_RANK;
        }
        if (StrUtil.equals(data, "keyword_page")) {
            return BUY_KEYWORD_PAGE_RANK;
        }
        return null;
    }

    public static String dataOf(AdvTypeEnum of) {
        if (Objects.equals(BUY_KEYWORD_RANK, of)) {
            return "keyword_rank";
        }
        if (Objects.equals(BUY_KEYWORD_PAGE_RANK, of)) {
            return "keyword_page";
        }
        return null;
    }

    public static AdvTypeEnum of(String position) {
        return Arrays.stream(AdvTypeEnum.values())
                .filter(a -> StrUtil.equals(String.valueOf(a.code), position))
                .findFirst().orElse(null);
    }


    public String buildName (String position) {
        boolean equals = StrUtil.equals(String.valueOf(this.code), position);
        return StrHelper.hit(this.desc, equals);
    }
}
