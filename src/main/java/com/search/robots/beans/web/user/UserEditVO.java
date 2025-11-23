package com.search.robots.beans.web.user;

import com.search.robots.database.enums.AdvertisingGradeEnum;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 用户编辑请求参数
 *
 * @author admin
 * @since 2025/11/21
 */
@Setter
@Getter
public class UserEditVO implements Serializable {

    /** 用户ID **/
    private Long userId;
    /** 广告等级 **/
    private AdvertisingGradeEnum grade;
    /** 用户余额 **/
    private BigDecimal balance;

}
