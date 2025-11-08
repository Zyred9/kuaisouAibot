package com.search.robots.database.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.config.BotProperties;
import com.search.robots.database.entity.Config;
import com.search.robots.database.mapper.ConfigMapper;
import com.search.robots.database.service.ConfigService;
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
public class ConfigServiceImpl extends ServiceImpl<ConfigMapper, Config> implements ConfigService {

    @Resource private BotProperties properties;

    @Override
    public Config queryConfig() {
        Config config = this.baseMapper.selectById(this.properties.getBackgroundGroupId());
        if (Objects.isNull(config)) {
            config = Config.buildDefault(this.properties.getBackgroundGroupId());
            this.baseMapper.insert(config);
        }
        return config;
    }
}
