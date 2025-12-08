package com.search.robots.database.service.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.RewardRecord;
import com.search.robots.database.entity.User;
import com.search.robots.database.enums.RewardTypeEnum;
import com.search.robots.database.mapper.RewardRecordMapper;
import com.search.robots.database.service.RewardRecordService;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 奖励记录ServiceImpl
 *
 * @author admin
 * @since 2025/11/25
 */
@Service
public class RewardRecordServiceImpl extends ServiceImpl<RewardRecordMapper, RewardRecord> implements RewardRecordService {

    @Override
    public void recordDirectNewUserReward(User inviter, User newUser, BigDecimal rewardAmount) {
        // 创建直接拉新奖励记录
        RewardRecord rewardRecord = RewardRecord.buildDirectNewUserReward(inviter, newUser, rewardAmount);
        this.save(rewardRecord);
    }

    @Override
    public void recordNextNewUserReward(User parentUser, User newUser, BigDecimal rewardAmount) {
        // 创建下级拉新奖励记录
        RewardRecord rewardRecord = RewardRecord.buildNextNewUserReward(parentUser, newUser, rewardAmount);
        this.save(rewardRecord);
    }

    @Override
    public void recordPrivateChatReward(User parentUser, User chatUser, BigDecimal rewardAmount) {
        // 创建私聊奖励记录
        RewardRecord rewardRecord = RewardRecord.buildPrivateChatReward(parentUser, chatUser, rewardAmount);
        this.save(rewardRecord);
    }

    @Override
    public void recordGroupSearchReward(User groupOwner, Long chatId, String chatTitle, String chatLink, BigDecimal rewardAmount) {
        // 创建群搜索奖励记录
        RewardRecord rewardRecord = RewardRecord.buildGroupSearchReward(groupOwner, chatId, chatTitle, chatLink, rewardAmount);
        this.save(rewardRecord);
    }

    @Override
    public void recordAdCommissionReward(User adsParent, User chargeUser, BigDecimal rewardAmount) {
        // 创建广告代理返佣记录
        RewardRecord rewardRecord = RewardRecord.buildAdCommissionReward(adsParent, chargeUser, rewardAmount);
        this.save(rewardRecord);
    }

    @Override
    public void recordRechargeGiftReward(User user, BigDecimal rewardAmount) {
        // 创建充值加赠奖励记录
        RewardRecord rewardRecord = RewardRecord.buildRechargeGiftReward(user, rewardAmount);
        this.save(rewardRecord);
    }

    @Override
    public List<RewardRecord> listRecentRewards(Long userId, RewardTypeEnum rewardType, int days) {
        LocalDateTime startTime = LocalDateTime.now().minusDays(days);
        return this.baseMapper.selectList(
                Wrappers.<RewardRecord>lambdaQuery()
                        .eq(RewardRecord::getUserId, userId)
                        .eq(rewardType != null, RewardRecord::getRewardType, rewardType)
                        .ge(RewardRecord::getCreateTime, startTime)
                        .orderByDesc(RewardRecord::getCreateTime)
        );
    }

    @Override
    public List<RewardRecord> listAllRecentRewards(Long userId, int days) {
        LocalDateTime startTime = LocalDateTime.now().minusDays(days);
        return this.baseMapper.selectList(
                Wrappers.<RewardRecord>lambdaQuery()
                        .eq(RewardRecord::getUserId, userId)
                        .ge(RewardRecord::getCreateTime, startTime)
                        .orderByDesc(RewardRecord::getCreateTime)
        );
    }
}
