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

    /** TRC20 地址 **/
    @TableId(type = IdType.INPUT)
    private String address;
    /** 分配的用户ID **/
    private Long userId;
    /** 图片地址 **/
    private String imageId;
    /** 监听次数 **/
    private Integer listenCount;
    /** 上一次查询时间 **/
    private Long prevTimestamp;
}
