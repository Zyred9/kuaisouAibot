package com.search.robots.database.controller;

import com.search.robots.beans.view.base.Result;
import com.search.robots.database.entity.Config;
import com.search.robots.database.service.ConfigService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;


/**
 * 配置管理
 * 
 * @author admin
 * @since 2025/01/17
 */
@Slf4j
@RestController
@RequestMapping("/config")
@RequiredArgsConstructor
public class ConfigController {

    private final ConfigService configService;

    // 查询所有的配置
    @GetMapping("/self")
    public Result<Config> queryConfig() {
        return Result.success(configService.queryConfig());
    }


    @PostMapping("/update")
    public Result<Void> updateConfig(@RequestBody Config config) {
        this.configService.updateById(config);
        return Result.success();
    }
    
}
