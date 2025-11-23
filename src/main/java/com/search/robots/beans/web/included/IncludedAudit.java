package com.search.robots.beans.web.included;


import lombok.Getter;
import lombok.Setter;
import org.jetbrains.annotations.NotNull;

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

    @NotNull("id为空")
    private Long includedId;
    @NotNull("审核码错误")
    private Integer auditCode;
    private String rejectReason;

}
