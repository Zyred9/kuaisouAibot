package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.AdvUser;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.mapper.AdvUserMapper;
import com.search.robots.database.service.AdvUserService;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

/**
 * 用户广告购买记录ServiceImpl
 * <pre>
 * 实现广告购买、续费、查询等功能
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
@Service
public class AdvUserServiceImpl extends ServiceImpl<AdvUserMapper, AdvUser> implements AdvUserService {

    @Override
    public List<AdvUser> listByUserId(Long userId) {
        if (Objects.isNull(userId)) {
            return CollUtil.newArrayList();
        }
        return this.baseMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getUserId, userId)
                        .orderByDesc(AdvUser::getCreatedAt)
        );
    }

    @Override
    public List<AdvUser> listByUserIdAndStatus(Long userId, AdvStatus status) {
        if (Objects.isNull(userId) || Objects.isNull(status)) {
            return CollUtil.newArrayList();
        }
        return this.baseMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getUserId, userId)
                        .eq(AdvUser::getAdvStatus, status)
                        .orderByDesc(AdvUser::getCreatedAt)
        );
    }

    @Override
    public List<AdvUser> listAutoRenewAds() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime threeDaysLater = now.plusDays(3);
        
        return this.baseMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getAutoRenew, true)
                        .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
                        .between(AdvUser::getExpirationTime, now, threeDaysLater)
                        .orderByAsc(AdvUser::getExpirationTime)
        );
    }
}
