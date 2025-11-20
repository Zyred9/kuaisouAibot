package com.search.robots.database.entity;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.JacksonTypeHandler;
import com.search.robots.config.Constants;
import com.search.robots.database.enums.Included.*;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.StrHelper;
import com.search.robots.helper.TimeHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.telegram.telegrambots.meta.api.objects.User;
import org.telegram.telegrambots.meta.api.objects.chat.ChatFullInfo;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 *
 *
 * @author zyred
 * @since 2025/11/2 15:37
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName(value = "t_included", autoResultMap = true)
public class Included {

    public static final String INCLUDED_PREFIX_KEY = "data:included:";

    /** 频道id/群组id **/
    @TableId(type = IdType.INPUT)
    private Long id;
    /** 群组/频道标题 **/
    private String indexTitle;
    /** 群组/频道名字 **/
    private String indexUsername;
    /** 是否是父类 **/
    private Boolean parent;

    private Long userId;
    private String username;
    private String nickname;

    /** 是否开启搜索 */
    private Boolean openSearch = true;
    /** 类型，群组还是频道 **/
    private IncludedTypeEnum includedType;
    /** 审核状态 **/
    private IncludedStatusEnum includedStatus;
    /** 群组私密状态 **/
    private PrivacyTypeEnum privacyType;
    /** 人数 **/
    public Integer number;
    /** 权重 **/
    private Integer weighted;
    /** 色情限制 **/
    private Boolean eroticismRestriction = false;
    /** 群组/频道创建时间 **/
    private LocalDateTime indexCreateTime;
    /** 提交时间 **/
    private LocalDateTime indexCommitTime;
    /** 消息监听 **/
    private Boolean openListen = true;
    /** 每日内容曝光上限 **/
    private Integer exposureLimit;
    /** 曝光加权：0% **/
    private BigDecimal exposureWeighting;
    /** 已记录资源数：29 **/
    private Integer sourceCount;
    /** 全局搜索资源数：0 **/
    private Integer globalSearchCount;
    /** 拉新广告 x天/次 */
    private IncludedNewUserEnum newUsers = IncludedNewUserEnum.THREE_DAY;

    /** 隐私搜索 **/
    private Boolean openPrivacySearch = false;
    /** 过滤未成年资源 **/
    private Boolean openFilterMinors = false;

    /** 优先级 **/
    @TableField(typeHandler = JacksonTypeHandler.class)
    private List<IncludedSearchPriorityEnum> priorities;

    /** 全局搜索 **/
    private Boolean openGlobalSearch = true;
    /** 定向搜索频道/群组 **/
    @TableField(typeHandler = JacksonTypeHandler.class)
    private List<Long> targetedSearchIndexIds;
    /** 子群是否启用定向搜索 **/
    private Boolean childTargetedSearch = true;
    private String auditReason;

    public static Included buildBean(ChatFullInfo info, User from, boolean parent, Integer count) {

        PrivacyTypeEnum pt;
        if (StrUtil.equals(info.getType(), "channel")) {
            pt = PrivacyTypeEnum.PUBLIC;
        } else {
            pt = Objects.isNull(info.getUserName()) ? PrivacyTypeEnum.PRIVATE : PrivacyTypeEnum.PUBLIC;
        }

        return new Included()
                .setId(info.getId())
                .setParent(parent)
                .setIndexTitle(info.getTitle())
                .setIndexUsername(info.getUserName())
                .setUserId(from.getId())
                .setUsername(from.getUserName())
                .setNickname(from.getFirstName())
                .setOpenSearch(true)
                .setIncludedType(IncludedTypeEnum.of(info.getType()))
                .setIncludedStatus(IncludedStatusEnum.DOING)
                .setPrivacyType(pt)
                .setNumber(count)
                .setWeighted(1)
                .setEroticismRestriction(false)
                .setIndexCreateTime(LocalDateTime.now())
                .setIndexCommitTime(LocalDateTime.now())
                .setOpenListen(Boolean.TRUE)
                .setExposureLimit(10000)
                .setExposureWeighting(BigDecimal.ZERO)
                .setSourceCount(0)
                .setGlobalSearchCount(0)
                .setNewUsers(IncludedNewUserEnum.THREE_DAY)
                .setOpenPrivacySearch(false)
                .setOpenFilterMinors(false)
                .setPriorities(IncludedSearchPriorityEnum.vals())
                .setOpenGlobalSearch(true)
                .setTargetedSearchIndexIds(Collections.emptyList())
                .setAuditReason("")
                .setChildTargetedSearch(false);
    }

    public List<Long> getTargetedSearchIndexIds() {
        return Objects.isNull(this.targetedSearchIndexIds)
                ? new ArrayList<>(1) : this.targetedSearchIndexIds;
    }


    public String buildDetailExhibition() {
        String includedText = buildDetailIncludedText("", new Config());
        List<String> split = StrUtil.split(includedText, "\n");
        List<String> sub = CollUtil.sub(split, 0, split.size() - 4);
        return StrUtil.join("\n", sub);
    }

    public String buildDetailIncludedText(String groupStart, Config config) {
        if (Objects.equals(this.privacyType, PrivacyTypeEnum.PUBLIC)) {
            return StrUtil.format(Constants.INDEX_DETAIL_TEXT,
                    this.id, this.privacyType.getDesc(),
                    StrHelper.specialResult(this.indexTitle),
                    StrHelper.specialResult(this.indexUsername),
                    this.number, this.weighted,
                    Boolean.FALSE.equals(this.eroticismRestriction) ? "没有限制" : "有限制",
                    TimeHelper.formatV2(this.indexCreateTime),
                    TimeHelper.formatV2(this.indexCommitTime),
                    Boolean.FALSE.equals(this.openListen) ? "未监听" : "监听",
                    this.exposureLimit,
                    DecimalHelper.decimalParse(this.exposureWeighting),
                    this.sourceCount,
                    this.globalSearchCount,
                    this.includedStatus.getDesc(), this.auditReason,

                    groupStart,
                    config.getTutorialUrl(),
                    config.getCommunityName()
            );
        }
        return StrUtil.format(Constants.PRIVACY_INDEX_DETAIL_TEXT,
                this.id, this.privacyType.getDesc(),
                StrHelper.specialResult(this.indexTitle),
                this.number, this.weighted,
                Boolean.FALSE.equals(this.eroticismRestriction) ? "没有限制" : "有限制",
                TimeHelper.formatV2(this.indexCommitTime),
                Boolean.FALSE.equals(this.openListen) ? "未监听" : "监听",
                this.exposureLimit,
                DecimalHelper.decimalParse(this.exposureWeighting),
                this.sourceCount,
                this.globalSearchCount,
                this.includedStatus.getDesc(), this.auditReason,

                groupStart,
                config.getTutorialUrl(),
                config.getCommunityName()
        );
    }

}
