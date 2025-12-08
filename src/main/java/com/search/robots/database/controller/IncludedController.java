package com.search.robots.database.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.web.included.IncludedAudit;
import com.search.robots.database.entity.Included;
import com.search.robots.database.service.IncludedService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * TRC20地址管理控制器
 * 
 * @author admin
 * @since 2025/01/17
 */
@Slf4j
@RestController
@RequestMapping("/included")
@RequiredArgsConstructor
public class IncludedController {

    private final IncludedService includedService;


    @GetMapping("/included_page")
    public Result<Page<Included>> includePage (@RequestParam("size") int size, @RequestParam("current") int current,
                                         @RequestParam("indexUsername") String indexUsername) {
        return Result.success(this.includedService.includedPage(size, current, indexUsername));
    }


    @PostMapping("/included_audit")
    public Result<Void> includedAudit (@RequestBody @Validated IncludedAudit includedAudit) {
        this.includedService.includedAudit(includedAudit);
        return Result.success();
    }
}
