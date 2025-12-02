package com.search.robots.database.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.search.robots.database.enums.RewardTypeEnum;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 奖励记录实体类
 *
 * @author zyred
 * @since 2025/11/25
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_reward_record")
public class RewardRecord {

    @TableId(type = IdType.AUTO)
    private Long id;

    /** 获得奖励的用户ID */
    private Long userId;

    /** 用户名 */
    private String username;

    /** 昵称 */
    private String nickname;

    /** 奖励金额 */
    private BigDecimal rewardAmount;

    /** 触发奖励的来源用户ID */
    private Long sourceUserId;

    /** 来源名称（群名/用户昵称） */
    private String sourceName;

    /** 来源链接（群链接等） */
    private String sourceLink;

    /** 搜索次数（仅群搜索奖励使用） */
    private Integer searchCount;

    /** 奖励类型 */
    private RewardTypeEnum rewardType;

    /** 奖励产生时间 */
    private LocalDateTime createTime;

    /**
     * 构建直接拉新奖励记录
     */
    public static RewardRecord buildDirectNewUserReward(User inviter, User newUser, BigDecimal rewardAmount) {
        return new RewardRecord()
                .setUserId(inviter.getUserId())
                .setUsername(inviter.getUsername())
                .setNickname(inviter.getNickname())
                .setRewardAmount(rewardAmount)
                .setSourceUserId(newUser.getUserId())
                .setRewardType(RewardTypeEnum.DIRECT_NEW_USER)
                .setCreateTime(LocalDateTime.now());
    }

    /**
     * 构建下级拉新奖励记录
     */
    public static RewardRecord buildNextNewUserReward(User parentUser, User newUser, BigDecimal rewardAmount) {
        return new RewardRecord()
                .setUserId(parentUser.getUserId())
                .setUsername(parentUser.getUsername())
                .setNickname(parentUser.getNickname())
                .setRewardAmount(rewardAmount)
                .setSourceUserId(newUser.getUserId())
                .setRewardType(RewardTypeEnum.NEXT_NEW_USER)
                .setCreateTime(LocalDateTime.now());
    }

    /**
     * 构建私聊奖励记录
     */
    public static RewardRecord buildPrivateChatReward(User parentUser, User chatUser, BigDecimal rewardAmount) {
        return new RewardRecord()
                .setUserId(parentUser.getUserId())
                .setUsername(parentUser.getUsername())
                .setNickname(parentUser.getNickname())
                .setRewardAmount(rewardAmount)
                .setSourceUserId(chatUser.getUserId())
                .setRewardType(RewardTypeEnum.PRIVATE_CHAT)
                .setCreateTime(LocalDateTime.now());
    }

    /**
     * 构建群搜索奖励记录
     */
    public static RewardRecord buildGroupSearchReward(User groupOwner, Long chatId, String chatTitle, String chatLink, BigDecimal rewardAmount) {
        return new RewardRecord()
                .setUserId(groupOwner.getUserId())
                .setUsername(groupOwner.getUsername())
                .setNickname(groupOwner.getNickname())
                .setRewardAmount(rewardAmount)
                .setSourceUserId(chatId)
                .setSourceName(chatTitle)
                .setSourceLink(chatLink)
                .setSearchCount(1)
                .setRewardType(RewardTypeEnum.GROUP_SEARCH)
                .setCreateTime(LocalDateTime.now());
    }

    /**
     * 构建广告代理返佣记录
     */
    public static RewardRecord buildAdCommissionReward(User adsParent, User chargeUser, BigDecimal rewardAmount) {
        return new RewardRecord()
                .setUserId(adsParent.getUserId())
                .setUsername(adsParent.getUsername())
                .setNickname(adsParent.getNickname())
                .setRewardAmount(rewardAmount)
                .setSourceUserId(chargeUser.getUserId())
                .setSourceName(chargeUser.getNickname())
                .setRewardType(RewardTypeEnum.AD_COMMISSION)
                .setCreateTime(LocalDateTime.now());
    }

    /**
     * 构建充值加赠奖励记录
     */
    public static RewardRecord buildRechargeGiftReward(User user, BigDecimal rewardAmount) {
        return new RewardRecord()
                .setUserId(user.getUserId())
                .setUsername(user.getUsername())
                .setNickname(user.getNickname())
                .setRewardAmount(rewardAmount)
                .setSourceUserId(user.getUserId()) // 来源用户就是自己
                .setRewardType(RewardTypeEnum.RECHARGE_GIFT)
                .setCreateTime(LocalDateTime.now());
    }
}
