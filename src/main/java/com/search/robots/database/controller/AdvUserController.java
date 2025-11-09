package com.search.robots.database.controller;


import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.view.vo.adv.AdvUserAudit;
import com.search.robots.database.service.AdvUserService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

/**
 * 广告用户控制器
 *
 * @author zyred
 * @since 2025/11/9 16:32
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/adv_user")
public class AdvUserController {

    private final AdvUserService advUserService;


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


