package com.search.robots.beans.view.vo.adv;


import com.search.robots.database.enums.adv.AdvStatus;
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
    private Long advUserId;
    /** 审批状态  0：审批拒绝，1: 审批通过 **/
    private Integer advStatus;

}

