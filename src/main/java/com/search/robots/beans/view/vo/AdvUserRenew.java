package com.search.robots.beans.view.vo;


import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 *
 *
 * @author zyred
 * @since 2025/11/8 16:48
 */
@Setter
@Getter
@Accessors(chain = true)
public class AdvUserRenew {

    /** 购买时间 **/
    private String time;
    /** 购买价格 **/
    private String price;

}
