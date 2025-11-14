package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.view.AsyncBean;
import com.search.robots.beans.view.vo.AdvShow;
import com.search.robots.beans.view.vo.adv.AdvUserAudit;
import com.search.robots.database.entity.AdvUser;
import com.search.robots.database.entity.Config;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import com.search.robots.database.mapper.AdvUserMapper;
import com.search.robots.database.service.AdvUserService;
import com.search.robots.database.service.ConfigService;
import com.search.robots.handlers.AsyncTaskHandler;
import com.search.robots.handlers.EmptyHandler;
import com.search.robots.helper.Assert;
import com.search.robots.helper.KeyboardHelper;
import com.search.robots.helper.RedisHelper;
import com.search.robots.helper.StrHelper;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
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

        List<AdvUser> advUsers;
        if (CollUtil.isEmpty(ids)) {
            advUsers = this.randTopAdv();
        } else {
            Set<Long>  longIds = ids.stream().map(Long::parseLong).collect(Collectors.toSet());
            advUsers = this.baseMapper.selectList(
                    Wrappers.<AdvUser>lambdaQuery()
                            .in(AdvUser::getId, longIds)
                            .eq(AdvUser::getAdvType, AdvTypeEnum.BUY_KEYWORD_RANK)
                            .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
                            .orderByAsc(AdvUser::getRanking)
                            .last(" limit 7")
            );
            AsyncTaskHandler.async(AsyncBean.directIncr(longIds));
        }
        if (CollUtil.isEmpty(advUsers)) {
            advUsers = this.randTopAdv();
        }
        if (CollUtil.isEmpty(advUsers)) {
            return null;
        }

        StringBuilder sb = new StringBuilder();
        for (AdvUser advUser : advUsers) {
            if (Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_TOP_LINK)
                    || Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_BOTTOM_BUTTON)) {
                sb.append("\\[广告\\] ");
            }
            if (Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_KEYWORD_RANK)) {
                sb.append(advUser.getAdvPosition().getIcon());
            }
            sb.append("[")
                    .append(StrHelper.specialResult(advUser.getAdvContentText()))
                    .append("](")
                    .append(advUser.getAdvUrlText())
                    .append(")")
                    .append("\n");
        }
        return sb.toString();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void incr(Set<Long> directIds, boolean direct) {
        if (CollUtil.isEmpty(directIds)) {
            return;
        }

        String today = LocalDate.now().toString();

        List<AdvUser> advUsers = this.listByIds(directIds);
        List<AdvUser> updates = new ArrayList<>(advUsers.size());
        for (AdvUser advUser : advUsers) {
            AdvUser update = new AdvUser();
            update.setId(advUser.getId());

            // 每天统计展示
            List<AdvShow> advShows = advUser.getAdvShow();
            if (CollUtil.isNotEmpty(advShows)) {
                AdvShow advShow = advShows.stream()
                        .filter(a -> StrUtil.equals(a.getDate(), today))
                        .findFirst().orElse(null);
                if (Objects.nonNull(advShow)) {
                    if (direct) {
                        advShow.setDirectShow(advShow.getDirectShow() + 1);
                    } else {
                        advShow.setRelatedShow(advShow.getRelatedShow() + 1);
                    }
                } else {
                    advShows.add(AdvShow.buildDefault(direct));
                }

                // 保证只有7天的数据
                advShows = AdvShow.filterLastSevenDays(advShows);
            } else {
                advShows = new ArrayList<>();
                advShows.add(AdvShow.buildDefault(direct));
            }
            update.setAdvShow(advShows);

            // 总展示次数
            update.setShowCount(advUser.getShowCount() + 1);

            // 状态判断(顶部链接、按钮链接)
            if (Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_TOP_LINK)
                    || Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_BOTTOM_BUTTON)) {
                if (advUser.getShowCount() >= advUser.getExpirationCount()) {
                    update.setAdvStatus(AdvStatus.THE_END);
                }
            }
            // 关键词排行
            else if (Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_KEYWORD_RANK)) {
                // 比较两个时间
                boolean after = LocalDateTime.now().isAfter(advUser.getExpirationTime());
                if (after) {
                    update.setAdvStatus(AdvStatus.THE_END);
                }
            }
            updates.add(update);
        }
        this.updateBatchById(updates);
    }


    public List<AdvUser> randTopAdv () {
        List<AdvUser> advUsers = this.baseMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getAdvType, AdvTypeEnum.BUY_TOP_LINK)
                        .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
                        .last(" order by rand() limit 2")
        );
        Set<Long> longIds = advUsers.stream().map(AdvUser::getId).collect(Collectors.toSet());
        AsyncTaskHandler.async(AsyncBean.relatedIncr(longIds));
        return advUsers;
    }
}
