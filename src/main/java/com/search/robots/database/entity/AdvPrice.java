package com.search.robots.database.entity;


import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.search.robots.config.Constants;
import com.search.robots.database.enums.adv.AdvPositionEnum;
import com.search.robots.helper.DecimalHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 广告价格实体
 * <pre>
 * 存储不同位置、来源的价格配置
 * 支持价格版本管理和历史追溯
 * 
 * 使用示例:
 * AdvPrice price = new AdvPrice()
 *     .setLibraryId(1L)
 *     .setAdvPosition(AdvPositionEnum.RANK_1)
 *     .setSource("direct")
 *     .setRanking(1)
 *     .setMonthlyPrice(new BigDecimal("999.00"))
 *     .setCurrency("CNY")
 *     .setStatus(1);
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_adv_price")
public class AdvPrice {

    @TableId(type = IdType.AUTO)
    private Long id;
    
    /** 关联广告库ID **/
    private Long libraryId;
    
    /** 广告位置枚举 **/
    private AdvPositionEnum advPosition;
    
    /** 来源类型(direct/related) **/
    private String source;
    
    /** 具体排名(仅榜单位置有效,1-10) **/
    private Integer ranking;
    
    /** 月费价格 **/
    private BigDecimal monthlyPrice;
    
    /** 货币单位(默认CNY) **/
    private String currency;
    
    /** 价格版本(用于历史追溯) **/
    private Integer version;
    
    /** 状态(1-启用,0-停用) **/
    private Integer status;
    
    /** 是否已售出(0-未售出,1-已售出) **/
    private Boolean isSold;
    
    /** 备注说明 **/
    private String remark;
    
    /** 创建时间 **/
    private LocalDateTime createdAt;
    
    /** 更新时间 **/
    private LocalDateTime updatedAt;

    public static AdvPrice buildDefault(int index, Integer posi, BigDecimal bigDecimal) {
        return new AdvPrice().setLibraryId(0L)
                .setId((long) index + 1).setSource("direct").setRanking(index)
                .setAdvPosition(AdvPositionEnum.of(posi))
                .setMonthlyPrice(bigDecimal).setStatus(1).setVersion(1).setIsSold(false)
                .setCreatedAt(LocalDateTime.now()).setUpdatedAt(LocalDateTime.now());
    }


    public String buildToBuyText(AdvLibrary library) {
        return StrUtil.format(Constants.TO_BUY_ADV_TEXT,
                library.getKeyword(), this.ranking,
                DecimalHelper.decimalParse(this.monthlyPrice));
    }
}
