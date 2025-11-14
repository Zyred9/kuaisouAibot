package com.search.robots.beans.view.vo.adv;


import com.search.robots.database.enums.adv.AdvTypeEnum;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.List;

/**
 *
 *
 * @author zyred
 * @since 2025/11/14 16:05
 */
@Setter
@Getter
@NoArgsConstructor
@Accessors(chain = true)
public class AdvStatistics {

    /** 广告类型 **/
    private AdvTypeEnum advType;
    /** 总数量 **/
    private int total;
    /** 进行中的 **/
    private int doing;
    /** 结束的 **/
    private int stop;

    public AdvStatistics (AdvTypeEnum advType) {
        this.advType = advType;
        this.total = 0;
        this.doing = 0;
        this.stop = 0;
    }

    public static List<AdvStatistics> buildDefault () {
        return List.of(
                new AdvStatistics(AdvTypeEnum.BUY_KEYWORD_RANK),
                new AdvStatistics(AdvTypeEnum.BUY_KEYWORD_PAGE_RANK),
                new AdvStatistics(AdvTypeEnum.BUY_BRAND_PAGE_RANK),
                new AdvStatistics(AdvTypeEnum.BUY_TOP_LINK),
                new AdvStatistics(AdvTypeEnum.BUY_BOTTOM_BUTTON)
        );
    }

}
