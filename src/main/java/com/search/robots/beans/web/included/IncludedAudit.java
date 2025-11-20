package com.search.robots.beans.web.included;


import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 *
 *
 * @author zyred
 * @since 2025/11/19 12:05
 */
@Setter
@Getter
public class IncludedAudit implements Serializable {

    private Long includedId;
    private Integer auditCode;
    private String rejectReason;

}
