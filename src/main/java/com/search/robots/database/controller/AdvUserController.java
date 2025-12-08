package com.search.robots.database.controller;


import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.web.adv.AdvUserAudit;
import com.search.robots.database.entity.AdvUser;
import com.search.robots.database.service.AdvUserService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

/**
 * 广告用户控制器
 *
 * @author admin
 * @since 2025/11/9 16:32
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/adv_user")
public class AdvUserController {

    private final AdvUserService advUserService;


    @GetMapping("/page")
    public Result<Page<AdvUser>> page(@RequestParam(defaultValue = "1") Integer current,
                                      @RequestParam(defaultValue = "10") Integer size,
                                      @RequestParam(required = false) String username,
                                      @RequestParam(required = false) String keyword,
                                      @RequestParam(required = false) Long libraryId) {
        Page<AdvUser> page = this.advUserService.page(
                Page.of(current, size),
                Wrappers.<AdvUser>lambdaQuery()
                        .like(StrUtil.isNotBlank(username), AdvUser::getUsername, username)
                        .like(StrUtil.isNotBlank(keyword), AdvUser::getKeyword, keyword)
                        .eq(libraryId != null, AdvUser::getLibraryId, libraryId)
                        .orderByDesc(AdvUser::getCreatedAt)
        );
        return Result.success(page);
    }

    /**
     * 后台审批用户的广告
     *
     * @param audit 广告审批的内容
     * @return      结果
     */
    @PostMapping("/audit")
    public Result<Void> auditAdvUser(@Valid @RequestBody AdvUserAudit audit) {
        this.advUserService.auditAdvUser(audit);
        return Result.success();
    }


}


