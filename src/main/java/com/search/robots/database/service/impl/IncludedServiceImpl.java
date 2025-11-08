package com.search.robots.database.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.config.BotProperties;
import com.search.robots.config.Constants;
import com.search.robots.database.entity.Config;
import com.search.robots.database.entity.Included;
import com.search.robots.database.enums.Included.IncludedSearchTypeEnum;
import com.search.robots.database.enums.Included.IncludedTypeEnum;
import com.search.robots.database.mapper.IncludedMapper;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.IncludedService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
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
public class IncludedServiceImpl extends ServiceImpl<IncludedMapper, Included> implements IncludedService {

    @Resource private BotProperties properties;
    @Resource private ConfigService configService;

    @Override
    public String buildSelfIndexText() {
        String msg = Constants.TOTAL_GROUP_CHANNEL_TEXT;
        Config config = this.configService.queryConfig();
        return StrUtil.format(msg, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
}
