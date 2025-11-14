package com.search.robots.database.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;

/**
 * TRC20充值地址实体类
 * 
 * @author zyred
 * @since 2025/01/17
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_address")
public class Address {

    /** 主键ID **/
    @TableId(type = IdType.AUTO)
    private Long id;

    /** TRC20地址 **/
    private String address;

    /** 绑定的用户ID **/
    private Long userId;

    /** 二维码图片ID(可空) **/
    private String qrImageId;

    /** 创建时间 **/
    private LocalDateTime createTime;

    /** 分配时间 **/
    private LocalDateTime assignTime;

    /** 更新时间 **/
    private LocalDateTime updateTime;
}
