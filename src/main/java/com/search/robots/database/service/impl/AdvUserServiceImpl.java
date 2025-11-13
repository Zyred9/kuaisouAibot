package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.view.vo.adv.AdvUserAudit;
import com.search.robots.database.entity.AdvUser;
import com.search.robots.database.entity.Config;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import com.search.robots.database.mapper.AdvUserMapper;
import com.search.robots.database.service.AdvUserService;
import com.search.robots.database.service.ConfigService;
import com.search.robots.handlers.EmptyHandler;
import com.search.robots.helper.Assert;
import com.search.robots.helper.KeyboardHelper;
import com.search.robots.helper.RedisHelper;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

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
@RequiredArgsConstructor
public class AdvUserServiceImpl extends ServiceImpl<AdvUserMapper, AdvUser> implements AdvUserService {

    private final EmptyHandler emptyHandler;
    private final ConfigService configService;

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
                        .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
                        .between(AdvUser::getExpirationTime, now, threeDaysLater)
                        .orderByAsc(AdvUser::getExpirationTime)
        );
    }

    @Override
    public void auditAdvUser(AdvUserAudit audit) {
        AdvUser advUser = this.baseMapper.selectById(audit.getAdvUserId());
        Assert.isNull(advUser, "用户广告不存在");

        Config config = this.configService.queryConfig();
        advUser.setAdvStatus(
                Objects.equals(audit.getAdvStatus(), 1)
                        ? AdvStatus.APPROVAL_PASS : AdvStatus.UN_START
        );
        advUser.setAdvContent(advUser.getTempContent());
        advUser.setAdvUrl(advUser.getTempUrl());
        String updateText = advUser.buildUpdateText(config.getCustomUsername());

        advUser.setTempContent("");
        advUser.setTempUrl("");
        this.updateById(advUser);

        String key = AdvUser.KEYWORD_ADV_USER + advUser.getKeyword();
        Set<String> ids = RedisHelper.sMembers(key);
        if (CollUtil.isNotEmpty(ids)) {
            if (Objects.equals(advUser.getAdvStatus(), AdvStatus.APPROVAL_PASS)) {
                RedisHelper.sAdd(key, String.valueOf(advUser.getId()));
            } else {
                RedisHelper.sRemove(key, String.valueOf(advUser.getId()));
            }
        } else {
            if (Objects.equals(advUser.getAdvStatus(), AdvStatus.APPROVAL_PASS)) {
                RedisHelper.sAdd(key, String.valueOf(advUser.getId()));
            }
        }

        InlineKeyboardMarkup markup = KeyboardHelper.buildAuditAfterKeyboard(advUser);
        AsyncSender.async(
                this.emptyHandler.markdown(advUser.getUserId(), updateText, markup)
        );
    }

    @Override
    public Page<AdvUser> selfPage(int current, Long userId, AdvTypeEnum type, AdvStatus status) {
        return this.baseMapper.selectPage(
                Page.of(current, 10),
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getUserId, userId)
                        .eq(Objects.nonNull(type), AdvUser::getAdvType, type)
                        .eq(Objects.nonNull(status), AdvUser::getAdvStatus, status)
                        .orderByDesc(AdvUser::getCreatedAt)
        );
    }

    @Override
    public String buildCurrent(String keyword) {
        String key = AdvUser.KEYWORD_ADV_USER + keyword;
        Set<String> ids = RedisHelper.sMembers(key);
        if (CollUtil.isEmpty(ids)) {
            return null;
        }
        Set<Long> longIds = ids.stream().map(Long::parseLong)
                .collect(Collectors.toSet());
        List<AdvUser> advUsers = this.baseMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .in(AdvUser::getId, longIds)
                        .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
                        .orderByAsc(AdvUser::getRanking)
                        .last(" limit 7")
        );
        if (CollUtil.isEmpty(advUsers)) {
            return null;
        }

        StringBuilder sb = new StringBuilder();
        for (AdvUser advUser : advUsers) {
            sb.append(advUser.getAdvPosition().getIcon())
                    .append("[")
                    .append(advUser.getAdvContent())
                    .append("](")
                    .append(advUser.getAdvUrl())
                    .append(")")
                    .append("\n");
        }
        return sb.toString();
    }
}
