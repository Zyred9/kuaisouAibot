package com.search.robots.beans.web.adv;


import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 *
 *
 * @author zyred
 * @since 2025/11/9 16:38
 */
@Setter
@Getter
public class AdvUserAudit implements Serializable {

    /** 用户的广告id **/
    private Long id;
    /** 审批状态  0：审批拒绝，1: 审批通过 **/
    private Integer advStatus;

}

