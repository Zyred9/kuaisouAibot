package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.RewardRecord;
import com.search.robots.database.entity.User;
import com.search.robots.database.enums.RewardTypeEnum;

import java.math.BigDecimal;
import java.util.List;

/**
 * 奖励记录Service
 *
 * @author admin
 * @since 2025/11/25
 */
public interface RewardRecordService extends IService<RewardRecord> {

    /**
     * 记录直接拉新奖励
     *
     * @param inviter      邀请人
     * @param newUser      新用户
     * @param rewardAmount 奖励金额
     */
    void recordDirectNewUserReward(User inviter, User newUser, BigDecimal rewardAmount);

    /**
     * 记录下级拉新奖励
     *
     * @param parentUser   上级用户
     * @param newUser      新用户
     * @param rewardAmount 奖励金额
     */
    void recordNextNewUserReward(User parentUser, User newUser, BigDecimal rewardAmount);

    /**
     * 记录私聊奖励
     *
     * @param parentUser   上级用户
     * @param chatUser     私聊用户
     * @param rewardAmount 奖励金额
     */
    void recordPrivateChatReward(User parentUser, User chatUser, BigDecimal rewardAmount);

    /**
     * 记录群搜索奖励
     *
     * @param groupOwner     群主
     * @param chatId         群组ID
     * @param chatTitle      群组标题
     * @param chatLink       群组链接
     * @param rewardAmount   奖励金额
     */
    void recordGroupSearchReward(User groupOwner, Long chatId, String chatTitle, String chatLink, BigDecimal rewardAmount);

    /**
     * 记录广告代理返佣
     *
     * @param adsParent      广告上级
     * @param chargeUser     充值用户
     * @param rewardAmount   返佣金额
     */
    void recordAdCommissionReward(User adsParent, User chargeUser, BigDecimal rewardAmount);

    /**
     * 记录充值加赠奖励
     *
     * @param user           充值用户
     * @param rewardAmount   奖励金额
     */
    void recordRechargeGiftReward(User user, BigDecimal rewardAmount);

    /**
     * 查询最近N天的奖励记录
     * @param userId 用户ID
     * @param rewardType 奖励类型
     * @param days 天数
     * @return 奖励记录列表
     */
    List<RewardRecord> listRecentRewards(Long userId, RewardTypeEnum rewardType, int days);

    /**
     * 查询最近N天的所有奖励记录
     * @param userId 用户ID
     * @param days 天数
     * @return 奖励记录列表
     */
    List<RewardRecord> listAllRecentRewards(Long userId, int days);
}
