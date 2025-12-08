package com.search.robots.beans.web.withdrawals;


import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 *
 *
 * @author admin
 * @since 2025/11/9 16:38
 */
@Setter
@Getter
public class WithdrawalsAudit implements Serializable {

    /** 提现的 **/
    private Long id;
    /** 状态 **/
    private Integer status;

}

