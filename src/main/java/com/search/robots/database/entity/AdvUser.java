package com.search.robots.database.entity;


import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.JacksonTypeHandler;
import com.search.robots.beans.view.vo.AdvShow;
import com.search.robots.beans.view.vo.AdvUserRenew;
import com.search.robots.database.enums.adv.AdvPositionEnum;
import com.search.robots.database.enums.adv.AdvSource;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 用户广告购买记录实体
 * <pre>
 * 采用冗余设计和关联设计相结合:
 * - 冗余关键词、展现等信息,避免频繁JOIN
 * - priceId关联价格表,保证价格数据的准确性和可追溯性
 * - 支持自动续费和账单关联
 * 
 * 使用示例:
 * AdvUser userAdv = new AdvUser()
 *     .setUserId(123L)
 *     .setLibraryId(456L)
 *     .setPriceId(789L)
 *     .setKeyword("AI机器人")
 *     .setAdvType(AdvTypeEnum.BUY_KEYWORD_RANK)
 *     .setAdvPosition(AdvPositionEnum.RANK_1)
 *     .setRanking(1)
 *     .setAutoRenew(true);
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName(value = "t_user_adv", autoResultMap = true)
public class AdvUser {

    @TableId(type = IdType.AUTO)
    private Long id;
    
    /** 关联用户ID **/
    private Long userId;
    
    /** 关联广告库ID(冗余存储,便于追溯源数据) **/
    private Long libraryId;
    
    /** 关联价格ID(关联t_adv_price,购买时的价格配置) **/
    private Long priceId;

    /** 购买的价格 **/
    private BigDecimal price;
    
    /** 关键词快照(冗余,避免JOIN) **/
    private String keyword;
    
    /** 广告类型快照 **/
    private AdvTypeEnum advType;
    
    /** 广告位置枚举 **/
    private AdvPositionEnum advPosition;
    
    /** 具体排名(榜单位置时有效,1-10) **/
    private Integer ranking;
    
    /** 来源类型(direct/related) **/
    private String source;
    
    /** 月费价格快照 **/
    private BigDecimal priceMonth;
    
    /** 货币单位 **/
    private String currency;
    
    /** 购买时的展现量快照(用于展示购买决策依据) **/
    private Long showCountSnapshot;
    
    /** 广告状态 **/
    private AdvStatus advStatus;
    
    /** 生效时间 **/
    private LocalDateTime effectiveTime;
    
    /** 失效时间 **/
    private LocalDateTime expirationTime;
    
    /** 购买来源 **/
    private AdvSource advSource;
    
    /** 账单号(关联t_bill) **/
    private String billNo;
    
    /** 是否自动续费 **/
    private Boolean autoRenew;

    // 广告展示配置
    /** 广告文本 **/
    private String advContent;
    
    /** 广告链接 **/
    private String advUrl;
    
    /** 当前实时展现次数(运行期间累加) **/
    private Long showCount;
    
    /** 广告7天的展示(实时更新) **/
    @TableField(typeHandler = JacksonTypeHandler.class)
    private List<AdvShow> advShow;
    
    /** 购买时展现轨迹快照 **/
    @TableField(typeHandler = JacksonTypeHandler.class)
    private List<AdvShow> advShowSnapshot;

    /** 续订记录 **/
    @TableField(typeHandler = JacksonTypeHandler.class)
    private List<AdvUserRenew> userRenews;
    
    /** 创建时间 **/
    private LocalDateTime createdAt;
    
    /** 更新时间 **/
    private LocalDateTime updatedAt;

    /**
     * 构建默认的广告用户记录
     * <pre>
     * 用于用户购买广告时创建初始记录
     * 无法获取的字段使用默认值:
     * - advStatus: 审批中(0)
     * - autoRenew: false
     * - showCount: 0
     * - currency: CNY
     * </pre>
     *
     * @param user 用户信息
     * @param library 广告库信息
     * @param price 价格配置
     * @return 默认的AdvUser对象
     */
    public static AdvUser buildAdvUserDefault(User user, AdvLibrary library, AdvPrice price) {
        LocalDateTime now = LocalDateTime.now();
        
        return new AdvUser()
                .setUserId(user.getUserId())
                .setLibraryId(library.getId())
                .setPriceId(price.getId())
                .setPrice(price.getMonthlyPrice())
                .setKeyword(library.getKeyword())
                .setAdvType(library.getAdvType())
                .setAdvPosition(price.getAdvPosition())
                .setRanking(price.getRanking())
                .setSource(price.getSource())
                .setPriceMonth(price.getMonthlyPrice())
                .setCurrency(price.getCurrency())
                .setShowCountSnapshot(library.getShowCount())
                .setAdvStatus(AdvStatus.PENDING)
                .setEffectiveTime(now)
                .setExpirationTime(now.plusMonths(1))
                .setAdvSource(AdvSource.USER_BUY)
                .setBillNo(null)
                .setAutoRenew(false)
                .setAdvContent(null)
                .setAdvUrl(null)
                .setShowCount(0L)
                .setAdvShow(null)
                .setAdvShowSnapshot(library.getShow7d())
                .setUserRenews(null)
                .setCreatedAt(now)
                .setUpdatedAt(now);
    }

}
