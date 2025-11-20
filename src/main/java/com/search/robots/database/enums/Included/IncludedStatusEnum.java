package com.search.robots.database.enums.Included;


import com.baomidou.mybatisplus.annotation.EnumValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Objects;

/**
 *  收录审批状态
 *
 * @author zyred
 * @since 2025/11/2 15:41
 */
@Getter
@AllArgsConstructor
public enum IncludedStatusEnum {

    DOING(0, "审批中⏳"),
    PASS(1, "审批通过✅"),
    REJECT(-1, "审批拒绝❌")
    ;

    @EnumValue
    private final int code;
    private final String desc;

    public static IncludedStatusEnum of(Integer auditCode) {
        return Objects.equals(auditCode, PASS.code) ? PASS : REJECT;
    }
}
