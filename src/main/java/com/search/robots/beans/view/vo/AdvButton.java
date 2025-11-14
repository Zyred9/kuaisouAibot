package com.search.robots.beans.view.vo;


import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 *
 *
 * @author zyred
 * @since 2025/11/14 9:15
 */
@Setter
@Getter
@Accessors(chain = true)
public class AdvButton {

    /** 名字 **/
    private String name;
    /** 次数 **/
    private BigDecimal amount;
    /** 展示次数 **/
    private BigDecimal showNumber;

}
