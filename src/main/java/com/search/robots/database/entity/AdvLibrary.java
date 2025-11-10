package com.search.robots.database.entity;


import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.JacksonTypeHandler;
import com.search.robots.beans.view.vo.AdvShow;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

/**
 * 广告库实体
 * <pre>
 * 存储关键词的基础信息和展现统计
 * 价格梯度已迁移至独立表 t_adv_price
 * 
 * priceList 为非持久化字段,通过关联查询填充
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName(value = "t_adv_library", autoResultMap = true)
public class AdvLibrary {

    public static final String ADV_LIBRARY_KEY = "search:keyword:library:";

    @TableId(type = IdType.AUTO)
    private Long id;
    
    /** 广告类型 **/
    private AdvTypeEnum advType;
    
    /** 关键词 **/
    private String keyword;
    
    /** 基础价格(最低价,用于快速筛选) **/
    private BigDecimal price;
    
    /** 总展现次数(累计) **/
    private Long showCount;
    
    /** 创建时间 **/
    private LocalDateTime createTime;
    
    /** 更新时间 **/
    private LocalDateTime updatedAt;
    
    /** 近7日展现轨迹(每日展现统计) **/
    @TableField(value = "show_7d", typeHandler = JacksonTypeHandler.class)
    private List<AdvShow> show7d;
    
    /** 价格配置列表(非持久化字段,关联查询时填充) **/
    @TableField(exist = false)
    private List<AdvPrice> priceList;

    public static AdvLibrary buildDefaultLibrary (String keyword, AdvTypeEnum type) {
        return new AdvLibrary()
                .setAdvType(type)
                .setKeyword(keyword)
                .setPrice(BigDecimal.ZERO)
                .setShowCount(0L)
                .setCreateTime(LocalDateTime.now())
                .setUpdatedAt(LocalDateTime.now())
                .setShow7d(Collections.emptyList());
    }
}
