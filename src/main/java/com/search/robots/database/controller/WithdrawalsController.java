package com.search.robots.database.controller;


import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.web.withdrawals.WithdrawalsAudit;
import com.search.robots.database.entity.Withdrawals;
import com.search.robots.database.service.WithdrawalsService;
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
@RequestMapping("/withdrawals")
public class WithdrawalsController {

    private final WithdrawalsService withdrawalsService;


    // 查询分页结果
    @GetMapping("/page")
    public Result<Page<Withdrawals>> withdrawalsPage(@RequestParam(defaultValue = "1") Integer current,
                                                     @RequestParam(defaultValue = "10") Integer size,
                                                     @RequestParam(required = false) String username) {
        return Result.success(this.withdrawalsService.withdrawalsPage(current, size, username));
    }

    /**
     * 后台审批用户的提现
     *
     * @param audit 提现审批的内容
     * @return      结果
     */
    @PostMapping("/audit")
    public Result<Boolean> auditWithdrawals(@Valid @RequestBody WithdrawalsAudit audit) {
        boolean audited = this.withdrawalsService.audit(audit);
        return Result.success(audited);
    }


}


