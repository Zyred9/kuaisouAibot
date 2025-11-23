package com.search.robots.database.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.web.included.IncludedAudit;
import com.search.robots.config.BotProperties;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.Config;
import com.search.robots.database.entity.Included;
import com.search.robots.database.enums.Included.IncludedSearchTypeEnum;
import com.search.robots.database.enums.Included.IncludedStatusEnum;
import com.search.robots.database.enums.Included.IncludedTypeEnum;
import com.search.robots.database.enums.Included.PrivacyTypeEnum;
import com.search.robots.database.mapper.IncludedMapper;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.IncludedService;
import com.search.robots.helper.Assert;
import com.search.robots.helper.JacksonHelper;
import com.search.robots.helper.RedisHelper;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;
import java.util.Objects;

/**
 * 收录服务实现类
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Service
public class IncludedServiceImpl extends ServiceImpl<IncludedMapper, Included> implements IncludedService {

    @Resource private BotProperties properties;
    @Resource private ConfigService configService;


    @Override
    public String buildSelfIndexText(Long userId) {
        List<Included> includedList = this.baseMapper.selectList(
                Wrappers.<Included>lambdaQuery()
                        .eq(Included::getUserId, userId)
        );

        // 公开的频道
        long publicChannel = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PUBLIC)
                && Objects.equals(a.getIncludedType(), IncludedTypeEnum.CHANNEL)).count();
        //  收录的公开频道
        long passPublicChannel = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PUBLIC)
                && Objects.equals(a.getIncludedType(), IncludedTypeEnum.CHANNEL)
                        && Objects.equals(a.getIncludedStatus(), IncludedStatusEnum.PASS)).count();

        // 私密的频道
        long privacyChannel = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PRIVATE)
                        && Objects.equals(a.getIncludedType(), IncludedTypeEnum.CHANNEL)).count();
        //  收录的私密频道
        long passPrivacyChannel = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PRIVATE)
                        && Objects.equals(a.getIncludedType(), IncludedTypeEnum.CHANNEL)
                        && Objects.equals(a.getIncludedStatus(), IncludedStatusEnum.PASS)).count();

        // 公开的群组
        long publicGroup = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PUBLIC)
                        && Objects.equals(a.getIncludedType(), IncludedTypeEnum.GROUP)).count();
        //  收录的公开频道
        long passPublicGroup = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PUBLIC)
                        && Objects.equals(a.getIncludedType(), IncludedTypeEnum.GROUP)
                        && Objects.equals(a.getIncludedStatus(), IncludedStatusEnum.PASS)).count();
        // 公开开启搜索的数量
        long publicGroupOpenSearch = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PUBLIC)
                        && Objects.equals(a.getIncludedType(), IncludedTypeEnum.GROUP)
                        && Boolean.TRUE.equals(a.getOpenSearch())).count();

        // 私密的频道
        long privacyGroup = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PRIVATE)
                        && Objects.equals(a.getIncludedType(), IncludedTypeEnum.GROUP)).count();
        //  收录的私密频道
        long passPrivacyGroup = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PRIVATE)
                        && Objects.equals(a.getIncludedType(), IncludedTypeEnum.GROUP)
                        && Objects.equals(a.getIncludedStatus(), IncludedStatusEnum.PASS)).count();
        // 私密开启搜索的数量
        long privacyGroupOpenSearch = includedList.stream()
                .filter(a -> Objects.equals(a.getPrivacyType(), PrivacyTypeEnum.PRIVATE)
                        && Objects.equals(a.getIncludedType(), IncludedTypeEnum.GROUP)
                        && Boolean.TRUE.equals(a.getOpenSearch())).count();

        String msg = Constants.TOTAL_GROUP_CHANNEL_TEXT;
        Config config = this.configService.queryConfig();
        return StrUtil.format(msg,
                publicChannel, passPublicChannel,
                privacyChannel, passPrivacyChannel,
                publicGroup, passPublicGroup, publicGroupOpenSearch,
                privacyGroup, passPrivacyGroup, privacyGroupOpenSearch,
                config.getTutorialUrl(), config.getCommunityName());
    }

    @Override
    public Page<Included> selectPage(int current, Long userId, IncludedSearchTypeEnum searchType) {
        IncludedTypeEnum anEnum = IncludedTypeEnum.of(searchType);
        return this.baseMapper.selectPage(
                Page.of(current, 10),
                Wrappers.<Included>lambdaQuery()
                        .eq(Included::getUserId, userId)
                        .eq(Objects.nonNull(anEnum), Included::getIncludedType, anEnum)
        );
    }

    @Override
    public Page<Included> includedPage(int size, int current, String indexUsername) {
        return this.baseMapper.selectPage(
                Page.of(current, size),
                Wrappers.<Included>lambdaQuery()
                        .eq(StrUtil.isNotBlank(indexUsername),
                                Included::getIndexUsername, indexUsername)
        );
    }

    @Override
    public void includedAudit(IncludedAudit includedAudit) {
        Included included = this.baseMapper.selectById(includedAudit.getIncludedId());
        Assert.isNull(included, "收集的群组/频道不存在");
        IncludedStatusEnum staus = IncludedStatusEnum.of(includedAudit.getAuditCode());
        String message;

        if (Objects.equals(staus, IncludedStatusEnum.REJECT)) {
            if (StrUtil.isEmpty(includedAudit.getRejectReason())) {
                message = staus.getDesc();
            } else {
                message = includedAudit.getRejectReason();
            }
        } else {
            message = staus.getDesc();
        }
        included.setAuditReason(message);
        included.setIncludedStatus(staus);

        this.baseMapper.updateById(included);
    }

    @Override
    public void updateSelf(Included update) {
        this.baseMapper.updateById(update);
        RedisHelper.delete(Included.INCLUDED_PREFIX_KEY + update.getId());
    }

    @Override
    @SneakyThrows
    public Included get(Long chatId) {
        String key = Included.INCLUDED_PREFIX_KEY + chatId;
        String includedJson = RedisHelper.get(key);
        if (StrUtil.isNotBlank(includedJson)) {
            return JacksonHelper.toBean(includedJson, Included.class);
        }
        Included included = this.baseMapper.selectById(chatId);
        if (Objects.nonNull(included)) {
            RedisHelper.set(key, JacksonHelper.toJson(included));
        }
        return included;
    }
}
