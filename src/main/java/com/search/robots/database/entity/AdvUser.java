package com.search.robots.database.entity;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.JacksonTypeHandler;
import com.search.robots.beans.view.vo.AdvButton;
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

@Setter
@Getter
@Accessors(chain = true)
@TableName(value = "t_user_adv", autoResultMap = true)
public class AdvUser {

    public final static String KEYWORD_ADV_USER = "search:keyword:user:";

    @TableId(type = IdType.AUTO)
    private Long id;
    
    /** 关联用户ID **/
    private Long userId;
    /*** 用户名 **/
    private String username;
    
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
    
    /** 具体排名(榜单位置时有效,1-7) **/
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

    /** 失效次数 */
    private Long expirationCount;
    
    /** 购买来源 **/
    private AdvSource advSource;

    /** 广告文本 **/
    private String advContent;
    /** 广告链接 **/
    private String advUrl;

    // 状态为审批的中，则需要展示
    /** 变更后广告文本 **/
    private String tempContent;
    /** 变更后广告链接 **/
    private String tempUrl;
    
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

    /**
     * 构建关键词用户广告
     * @return  用户广告
     */
    public static AdvUser buildKeywordAdvUserDefault(User user, AdvLibrary library, AdvPrice price) {
        LocalDateTime now = LocalDateTime.now();
        return new AdvUser()
                .setUserId(user.getUserId())
                .setUsername(user.getUsername())
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

    public static AdvUser buildAdvUserDefault (User user, AdvButton advButton, AdvTypeEnum advType, AdvPositionEnum rank) {
        LocalDateTime now = LocalDateTime.now();
        return new AdvUser()
                .setUserId(user.getUserId())
                .setUsername(user.getUsername())
                .setPriceId(null)
                .setKeyword("")
                .setAdvType(advType)
                .setAdvPosition(rank)
                .setRanking(0)
                .setSource("direct")
                .setPriceMonth(advButton.getAmount())
                .setCurrency("USDT")
                .setAdvStatus(AdvStatus.UN_START)
                .setEffectiveTime(now)
                .setExpirationTime(null)
                .setExpirationCount(advButton.getShowNumber())
                .setAdvSource(AdvSource.BUY)
                .setAdvContent("")
                .setAdvUrl("")
                .setTempContent("")
                .setTempUrl("")
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
                this.updateContentText(),
                StrHelper.specialLong(this.getShowCount()),
                showDetailText
        );
    }

    public String buildTopButtonPaymentText () {
        String renewText = buildRenewRecordsText(this);
        String showDetailText = buildShowDetailText(this);
        return StrUtil.format(Constants.TOP_BUTTON_PAYMENT_TEXT,
                StrHelper.specialLong(this.getId()),
                StrHelper.specialResult(this.getAdvType().getDesc()),
                StrHelper.specialLong(this.getExpirationCount()),
                DecimalHelper.decimalParse(this.getPriceMonth()),
                renewText,
                StrHelper.specialResult(this.getAdvStatus().getDesc()),
                TimeHelper.format(this.getEffectiveTime()),
                StrHelper.specialLong(this.getExpirationCount()),
                StrHelper.specialResult(this.getAdvSource().getDesc()),
                StrHelper.specialResult(this.getAdvContent()),
                StrHelper.specialResult(this.getAdvUrl()),
                this.updateContentText(),
                StrHelper.specialLong(this.getShowCount()),
                showDetailText
        );
    }

    private String updateContentText() {
        if (Objects.equals(this.advStatus, AdvStatus.UNDER_APPROVAL)) {
            String resultText = """
                    \n\\=\\=\\=变更
                    新广告文本：`{}`
                    新广告链接：`{}`""";
            return StrUtil.format(resultText, StrHelper.specialResult(this.tempContent), StrHelper.specialResult(this.tempUrl));
        }
        return "";
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

    public String buildButtonName() {
        boolean noSetting = StrUtil.isBlank(this.advContent);
        return this.advType.getPrefix() + ":" +
                StrHelper.buildSymbolName("|", keyword, noSetting ? "未配置" : this.advContent, this.advStatus.getDesc());
    }

    public String buildUpdateText(String customUsername) {
        boolean equals = Objects.equals(this.advStatus, AdvStatus.APPROVAL_PASS);
        String title = equals ? "✅广告审批通过" : "❌广告审拒绝";
        return StrUtil.format(Constants.ADV_USER_AUDIT_SUCCESS_TEXT,
                this.userId, this.username, this.id,
                this.getAdvContent(), this.getAdvUrl(),
                this.getTempContent(), this.getTempUrl(),
                title, customUsername
        );
    }
}
