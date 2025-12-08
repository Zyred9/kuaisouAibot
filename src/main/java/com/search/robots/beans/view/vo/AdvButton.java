package com.search.robots.beans.view.vo;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 *
 *
 * @author admin
 * @since 2025/11/14 9:15
 */
@Setter
@Getter
@NoArgsConstructor
@Accessors(chain = true)
public class AdvButton {

    /** 名字 **/
    private String name;
    /** 次数 **/
    private BigDecimal amount;
    /** 展示次数 **/
    private Long showNumber;


    public AdvButton(BigDecimal amount, Long showNumber) {
        this.amount = amount;
        this.showNumber = showNumber;
    }

}
