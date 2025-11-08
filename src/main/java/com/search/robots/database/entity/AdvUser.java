package com.search.robots.database.entity;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.JacksonTypeHandler;
import com.search.robots.beans.view.vo.AdvShow;
import com.search.robots.beans.view.vo.AdvUserRenew;
import com.search.robots.config.Constants;
import com.search.robots.database.enums.adv.AdvPositionEnum;
import com.search.robots.database.enums.adv.AdvSource;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.StrHelper;
import com.search.robots.helper.TimeHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

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
    
    /** 广告状态 **/
    private AdvStatus advStatus;
    
    /** 生效时间 **/
    private LocalDateTime effectiveTime;
    
    /** 失效时间 **/
    private LocalDateTime expirationTime;
    
    /** 购买来源 **/
    private AdvSource advSource;

    /** 广告文本 **/
    private String advContent;
    
    /** 广告链接 **/
    private String advUrl;
    
    /** 当前实时展现次数(运行期间累加) **/
    private Long showCount;
    
    /** 广告7天的展示(实时更新) **/
    @TableField(typeHandler = JacksonTypeHandler.class)
    private List<AdvShow> advShow;
    
    /** 续订记录 **/
    @TableField(typeHandler = JacksonTypeHandler.class)
    private List<AdvUserRenew> userRenews;

    /** 创建时间 **/
    private LocalDateTime createdAt;


    public String getAdvContent() {
        return StrUtil.isBlank(this.advContent) ? "未配置❌" : this.advContent;
    }

    public String getAdvUrl() {
        return StrUtil.isBlank(this.advUrl) ? "未配置❌" : this.advUrl;
    }

    public static AdvUser buildAdvUserDefault(User user, AdvLibrary library, AdvPrice price) {
        LocalDateTime now = LocalDateTime.now();
        return new AdvUser()
                .setUserId(user.getUserId())
                .setLibraryId(library.getId())
                .setPriceId(price.getId())
                .setKeyword(library.getKeyword())
                .setAdvType(library.getAdvType())
                .setAdvPosition(price.getAdvPosition())
                .setRanking(price.getRanking())
                .setSource(price.getSource())
                .setPriceMonth(price.getMonthlyPrice())
                .setCurrency(price.getCurrency())
                .setAdvStatus(AdvStatus.UN_START)
                .setEffectiveTime(now)
                .setExpirationTime(now.plusMonths(1))
                .setAdvSource(AdvSource.BUY)
                .setAdvContent("")
                .setAdvUrl("")
                .setShowCount(0L)
                .setAdvShow(Collections.emptyList())
                .setUserRenews(Collections.emptyList())
                .setCreatedAt(now);
    }


    public String buildAdvUserPaymentText() {
        String renewText = buildRenewRecordsText(this);
        String showDetailText = buildShowDetailText(this);
        return StrUtil.format(Constants.KEYWORD_PAYMENT_TEXT,
                StrHelper.specialLong(this.getId()),
                StrHelper.specialResult(this.getAdvType().getDesc()),
                StrHelper.specialResult(this.getKeyword()),
                Objects.isNull(this.getRanking()) ? "无" : StrHelper.specialResult(String.valueOf(this.getRanking())),
                DecimalHelper.decimalParse(this.getPriceMonth()),
                renewText,
                StrHelper.specialResult(this.getAdvStatus().getDesc()),
                TimeHelper.format(this.getEffectiveTime()),
                TimeHelper.format(this.getExpirationTime()),
                StrHelper.specialResult(this.getAdvSource().getDesc()),
                StrHelper.specialResult(this.getAdvContent()),
                StrHelper.specialResult(this.getAdvUrl()),
                StrHelper.specialLong(this.getShowCount()),
                showDetailText
        );
    }

    private String buildRenewRecordsText(AdvUser advUser) {
        List<AdvUserRenew> renews = advUser.getUserRenews();
        if (CollUtil.isEmpty(renews)) {
            return "";
        }
        StringBuilder sb = new StringBuilder().append("续订记录：\n");
        for (AdvUserRenew renew : renews) {
            sb.append("   ")
                    .append(StrHelper.specialResult(renew.getTime()))
                    .append("：")
                    .append(StrHelper.specialResult(renew.getPrice()))
                    .append("$\n");
        }
        return "\n" + sb.toString().trim();
    }

    private String buildShowDetailText(AdvUser advUser) {
        List<AdvShow> advShows = advUser.getAdvShow();
        if (CollUtil.isEmpty(advShows)) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        for (AdvShow show : advShows) {
            sb.append("   ").append(StrHelper.specialResult(show.getDate()))
                    .append("：")
                    .append(StrHelper.specialLong(show.getTotalShow()))
                    .append("\n");
        }
        return "\n" + sb.toString().trim();
    }
}
