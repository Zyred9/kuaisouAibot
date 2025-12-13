package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.view.AsyncBean;
import com.search.robots.beans.view.vo.AdvShow;
import com.search.robots.beans.view.vo.adv.AdvStatistics;
import com.search.robots.beans.web.adv.AdvUserAudit;
import com.search.robots.config.Constants;
import com.search.robots.config.SelfException;
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
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 用户广告购买记录ServiceImpl
 * <pre>
 * 实现广告购买、续费、查询等功能
 * </pre>
 *
 * @author admin
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
        AdvUser advUser = this.baseMapper.selectById(audit.getId());
        Assert.isNull(advUser, "用户广告不存在");

        if (StrUtil.isAllBlank(advUser.getTempContent(), advUser.getTempUrl())) {
            throw new SelfException("用户未提交新广告内容！");
        }


        Config config = this.configService.queryConfig();
        advUser.setAdvStatus(
                Objects.equals(audit.getAdvStatus(), AdvStatus.APPROVAL_PASS.getCode())
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

        List<AdvUser> advUsers = CollUtil.isEmpty(ids)
                ? this.topLinkRandAdvList()
                : this.keywordAdvList(ids);
        if (CollUtil.isEmpty(advUsers)) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        for (AdvUser advUser : advUsers) {
            if (Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_TOP_LINK)
                    || Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_BOTTOM_BUTTON)) {
                sb.append("[广告] ");
            }
            if (Objects.equals(advUser.getAdvType(), AdvTypeEnum.BUY_KEYWORD_RANK)) {
                sb.append(advUser.getAdvPosition().getIcon());
            }
            sb.append("<a href=\"")
                    .append(advUser.getAdvUrlText())
                    .append("\">")
                    .append(StrHelper.specialResult(advUser.getAdvContentText()))
                    .append("</a>")
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

    @Override
    public AdvUser buttonAdv() {
        AdvUser adv = this.baseMapper.selectOne(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getAdvType, AdvTypeEnum.BUY_BOTTOM_BUTTON)
                        .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
                        .last(" order by rand() limit 1")
        );
        if (Objects.nonNull(adv)) {
            AsyncTaskHandler.async(AsyncBean.directIncr(Set.of(adv.getId())));
            return adv;
        }
        return null;
    }

    @Override
    public String advStatistics(Long userId) {
        List<AdvStatistics> advStatistics = this.selectAdvStatistics(userId);
        Map<AdvTypeEnum, AdvStatistics> map = advStatistics.stream()
                .collect(Collectors.toMap(AdvStatistics::getAdvType, Function.identity()));

        AdvStatistics keywordAdv = map.get(AdvTypeEnum.BUY_KEYWORD_RANK);
        AdvStatistics keywordPageAdv = map.get(AdvTypeEnum.BUY_KEYWORD_PAGE_RANK);
        AdvStatistics brandPage = map.get(AdvTypeEnum.BUY_BRAND_PAGE_RANK);
        AdvStatistics topLinkAdv = map.get(AdvTypeEnum.BUY_TOP_LINK);
        AdvStatistics bottomButtonAdv = map.get(AdvTypeEnum.BUY_BOTTOM_BUTTON);

        Config config = this.configService.queryConfig();
        return StrUtil.format(Constants.SELF_ADV_TEXT,
                keywordAdv.getTotal(), keywordAdv.getDoing(), keywordAdv.getStop(),
                keywordPageAdv.getTotal(), keywordPageAdv.getDoing(), keywordPageAdv.getStop(),
                brandPage.getTotal(), brandPage.getDoing(), brandPage.getStop(),
                topLinkAdv.getTotal(), topLinkAdv.getDoing(), topLinkAdv.getStop(),
                bottomButtonAdv.getTotal(), bottomButtonAdv.getDoing(), bottomButtonAdv.getStop(),
                config.getTutorialUrl(), config.getCommunityName()
        );
    }

    private List<AdvStatistics> selectAdvStatistics(Long userId) {
        List<AdvUser> advUsers = this.baseMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .select(AdvUser::getAdvType, AdvUser::getAdvStatus)
                        .eq(AdvUser::getUserId, userId)
        );
        if (CollUtil.isEmpty(advUsers)) {
            return AdvStatistics.buildDefault();
        }

        Map<AdvTypeEnum, List<AdvUser>> map = advUsers.stream()
                .collect(Collectors.groupingBy(AdvUser::getAdvType));

        for (AdvTypeEnum value : AdvTypeEnum.values()) {
            if (!map.containsKey(value)) {
                map.put(value, Collections.emptyList());
            }
        }

        List<AdvStatistics> advStatistics = new ArrayList<>(5);
        Set<Map.Entry<AdvTypeEnum, List<AdvUser>>> entries = map.entrySet();
        for (Map.Entry<AdvTypeEnum, List<AdvUser>> entry : entries) {
            List<AdvUser> advUserList = entry.getValue();
            AdvStatistics statistics = new AdvStatistics();
            statistics.setAdvType(entry.getKey());
            statistics.setTotal(advUserList.size());
            long doing = advUserList.stream()
                    .filter(a -> Objects.equals(a.getAdvStatus(), AdvStatus.PROMOTION_ING))
                    .count();
            long end = advUserList.stream()
                    .filter(a -> Objects.equals(a.getAdvStatus(), AdvStatus.THE_END))
                    .count();
            statistics.setDoing((int)doing);
            statistics.setStop((int)end);

            advStatistics.add(statistics);
        }
        return advStatistics;
    }

    @Override
    public void updateStatus(Long id, Boolean status) {
        AdvUser advUser = this.baseMapper.selectById(id);
        Assert.isNull(advUser, "用户广告不存在");
        advUser.setStatus(status);

        if (Boolean.TRUE.equals(status)) {
            advUser.setAdvStatus(AdvStatus.PROMOTION_ING);
            if (StrUtil.isNotBlank(advUser.getKeyword())) {
                String key = AdvUser.KEYWORD_ADV_USER + advUser.getKeyword();
                RedisHelper.sAdd(key, String.valueOf(advUser.getId()));
            }
        } else if (Boolean.FALSE.equals(status)) {
            advUser.setAdvStatus(AdvStatus.THE_END);
            if (StrUtil.isNotBlank(advUser.getKeyword())) {
                String key = AdvUser.KEYWORD_ADV_USER + advUser.getKeyword();
                RedisHelper.sRemove(key, String.valueOf(advUser.getId()));
            }
        }

        this.baseMapper.updateById(advUser);
    }

    public List<AdvUser> topLinkRandAdvList() {
        List<AdvUser> advUsers = this.baseMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .eq(AdvUser::getAdvType, AdvTypeEnum.BUY_TOP_LINK)
                        .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
                        .last(" order by rand() limit 2")
        );
        if (CollUtil.isNotEmpty(advUsers)) {
            Set<Long> longIds = advUsers.stream().map(AdvUser::getId).collect(Collectors.toSet());
            AsyncTaskHandler.async(AsyncBean.relatedIncr(longIds));
        }
        return advUsers;
    }

    public List<AdvUser> keywordAdvList(Set<String> ids) {
        Set<Long> longIds = ids.stream().map(Long::parseLong).collect(Collectors.toSet());
        List<AdvUser> advUsers = this.baseMapper.selectList(
                Wrappers.<AdvUser>lambdaQuery()
                        .in(AdvUser::getId, longIds)
                        .eq(AdvUser::getAdvType, AdvTypeEnum.BUY_KEYWORD_RANK)
                        .eq(AdvUser::getAdvStatus, AdvStatus.PROMOTION_ING)
                        .orderByAsc(AdvUser::getRanking)
                        .last(" limit 7")
        );
        if (CollUtil.isNotEmpty(advUsers)) {
            AsyncTaskHandler.async(AsyncBean.directIncr(longIds));
        } else {
            advUsers = this.topLinkRandAdvList();
        }
        return advUsers;
    }
}
