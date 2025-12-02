package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.RewardRecord;
import com.search.robots.database.entity.User;
import com.search.robots.database.enums.RewardTypeEnum;
import com.search.robots.database.mapper.UserMapper;
import com.search.robots.database.service.RewardRecordService;
import com.search.robots.database.service.UserService;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.RedisHelper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Service
@RequiredArgsConstructor
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {

    private final RewardRecordService rewardRecordService;

    @Override
    public User user(org.telegram.telegrambots.meta.api.objects.User from) {
        String cacheKey = User.USER_PREFIX_KEY + from.getId();

        String cachedUserJson = RedisHelper.get(cacheKey);
        if (StrUtil.isNotBlank(cachedUserJson)) {
            return JSONUtil.toBean(cachedUserJson, User.class);
        }
        User user = this.baseMapper.selectById(from.getId());
        if (Objects.nonNull(user)) {
            RedisHelper.set(cacheKey, JSONUtil.toJsonStr(user));
            return user;
        }
        user = User.buildDefault(from);
        if (!from.getIsBot()) {
            this.baseMapper.insert(user);
            RedisHelper.set(cacheKey, JSONUtil.toJsonStr(user));
        } else {
            RedisHelper.set(cacheKey, JSONUtil.toJsonStr(user));
        }
        return user;
    }

    @Override
    public User select(Long userId) {
        String cacheKey = User.USER_PREFIX_KEY + userId;

        String cachedUserJson = RedisHelper.get(cacheKey);
        if (StrUtil.isNotBlank(cachedUserJson)) {
            return JSONUtil.toBean(cachedUserJson, User.class);
        }
        User user = this.baseMapper.selectById(userId);
        if (Objects.nonNull(user)) {
            RedisHelper.set(cacheKey, JSONUtil.toJsonStr(user));
            return user;
        }
        return null;
    }

    @Override
    public void update(User user) {
        this.baseMapper.updateById(user);
        String cacheKey = User.USER_PREFIX_KEY + user.getUserId();
        RedisHelper.delete(cacheKey);
    }

    @Override
    public User selectByInviteCode(String inviteCode) {
        return this.baseMapper.selectOne(
                Wrappers.<User>lambdaQuery()
                        .eq(User::getInviteCode, inviteCode)
        );
    }

    @Override
    public String getSpreadStatement(Long userId) {
        User user = this.baseMapper.selectById(userId);
        // 查询我的下级数据
        List<User> users = this.baseMapper.selectList(
                Wrappers.<User>lambdaQuery()
                        .select(User::getUserId)
                        .eq(User::getParentId, userId)
        );
        List<Long> parentIds = users.stream().map(User::getUserId).toList();
        // 根据下级查询下下级数量
        Long grandsonCount = 0L;
        if (CollUtil.isNotEmpty(parentIds)) {
            grandsonCount = this.baseMapper.selectCount(
                    Wrappers.<User>lambdaQuery()
                            .in(User::getParentId, parentIds)
            );
        }
        // 查询广告的下级数量
        Long advCount = this.baseMapper.selectCount(
                Wrappers.<User>lambdaQuery()
                        .in(User::getAdsParentId, userId)
        );

        // 一次性查询最近3天的所有奖励记录
        List<RewardRecord> allRewards = rewardRecordService.listAllRecentRewards(userId, 3);
        
        // 按奖励类型分组
        Map<RewardTypeEnum, List<RewardRecord>> rewardsByType = allRewards.stream()
                .collect(Collectors.groupingBy(RewardRecord::getRewardType));
        
        // 格式化各种奖励文本
        String directNewUserRewardText = formatRewardText(rewardsByType.get(RewardTypeEnum.DIRECT_NEW_USER));
        String nextNewUserRewardText = formatRewardText(rewardsByType.get(RewardTypeEnum.NEXT_NEW_USER));
        String privateChatRewardText = formatRewardText(rewardsByType.get(RewardTypeEnum.PRIVATE_CHAT));
        String groupSearchRewardText = formatGroupSearchRewardText(rewardsByType.get(RewardTypeEnum.GROUP_SEARCH));
        String adCommissionRewardText = formatRewardText(rewardsByType.get(RewardTypeEnum.AD_COMMISSION));

        return StrUtil.format(
                Constants.SELF_PROMOTION_REPORT_TEXT,
                user.getGrade().getDesc(),
                DecimalHelper.decimalParse(user.getTodayAward()),
                DecimalHelper.decimalParse(user.getTotalAward()),
                DecimalHelper.decimalParse(user.getAccumulativeTotalAward()),
                users.size(), grandsonCount, advCount,
                directNewUserRewardText, nextNewUserRewardText, privateChatRewardText, groupSearchRewardText, adCommissionRewardText
        );
    }

    /**
     * 格式化奖励文本
     */
    private String formatRewardText(List<RewardRecord> rewards) {
        if (CollUtil.isEmpty(rewards)) {
            return "无";
        }

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        StringBuilder sb = new StringBuilder();
        for (RewardRecord reward : rewards) {
            String line = String.format("`%s %s %s\\$`",
                    reward.getCreateTime().format(formatter),
                    StrUtil.isNotBlank(reward.getNickname()) ? reward.getNickname() : reward.getUsername(),
                    DecimalHelper.decimalParse(reward.getRewardAmount())
            );
            sb.append(line).append("\n");
        }
        return sb.toString();
    }

    /**
     * 格式化群搜索奖励文本（按日期分组）
     */
    private String formatGroupSearchRewardText(List<RewardRecord> rewards) {
        if (CollUtil.isEmpty(rewards)) {
            return "无";
        }

        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        StringBuilder sb = new StringBuilder();

        // 按日期分组
        Map<LocalDate, List<RewardRecord>> rewardsByDate = rewards.stream()
                .collect(Collectors.groupingBy(
                        reward -> reward.getCreateTime().toLocalDate(),
                        LinkedHashMap::new,
                        Collectors.toList()
                ));

        // 按日期倒序排列
        List<Map.Entry<LocalDate, List<RewardRecord>>> sortedEntries = rewardsByDate.entrySet().stream()
                .sorted(Map.Entry.<LocalDate, List<RewardRecord>>comparingByKey().reversed())
                .toList();

        for (Map.Entry<LocalDate, List<RewardRecord>> entry : sortedEntries) {
            LocalDate date = entry.getKey();
            List<RewardRecord> dayRewards = entry.getValue();

            // 计算当日总奖励
            java.math.BigDecimal totalReward = dayRewards.stream()
                    .map(RewardRecord::getRewardAmount)
                    .reduce(java.math.BigDecimal.ZERO, java.math.BigDecimal::add);

            // 日期行
            sb.append(date.format(dateFormatter))
                    .append(" 预估奖励：")
                    .append(DecimalHelper.decimalParse(totalReward))
                    .append("$ 状态：待入账⏳\n");

            // 按群组统计搜索次数
            Map<Long, List<RewardRecord>> rewardsByChat = dayRewards.stream()
                    .collect(Collectors.groupingBy(RewardRecord::getSourceUserId));

            for (Map.Entry<Long, List<RewardRecord>> chatEntry : rewardsByChat.entrySet()) {
                List<RewardRecord> chatRewards = chatEntry.getValue();
                RewardRecord firstRecord = chatRewards.get(0);
                int searchCount = chatRewards.stream()
                        .mapToInt(r -> r.getSearchCount() != null ? r.getSearchCount() : 1)
                        .sum();

                // 群组行
                sb.append("          ");
                if (StrUtil.isNotBlank(firstRecord.getSourceLink())) {
                    sb.append("[").append(firstRecord.getSourceName()).append("](")
                            .append(firstRecord.getSourceLink()).append(")");
                } else {
                    sb.append(firstRecord.getSourceName());
                }
                sb.append(" \\-  ")
                        .append(searchCount)
                        .append(" 次有效群搜索\n");
            }
        }

        return sb.toString();
    }

    @Override
    public Page<User> selectChildAdsUsers(Long userId, int current) {
        return this.baseMapper.selectPage(
                Page.of(current, 10),
                Wrappers.<User>lambdaQuery()
                        .eq(User::getAdsParentId, userId)
                        .orderByDesc(User::getRegisterTime)
        );
    }



}
