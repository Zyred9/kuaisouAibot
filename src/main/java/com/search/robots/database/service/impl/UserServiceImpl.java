package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.User;
import com.search.robots.database.mapper.UserMapper;
import com.search.robots.database.service.UserService;
import com.search.robots.helper.DecimalHelper;
import com.search.robots.helper.RedisHelper;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Service
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {

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
        // todo 查询最近三天拉新记录
        // todo 查询最近三天拉新奖励
        // todo 查新最近三天下级私聊奖励

        return StrUtil.format(
                Constants.SELF_PROMOTION_REPORT_TEXT,
                user.getGrade().getDesc(),
                DecimalHelper.decimalParse(user.getTodayAward()),
                DecimalHelper.decimalParse(user.getTotalAward()),
                DecimalHelper.decimalParse(user.getAccumulativeTotalAward()),
                users.size(), grandsonCount, advCount,
                "无", "无", "无", "无"
        );
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
