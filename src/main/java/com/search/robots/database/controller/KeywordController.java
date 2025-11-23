package com.search.robots.database.controller;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.view.base.Result;
import com.search.robots.database.entity.Keyword;
import com.search.robots.database.service.KeywordService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Objects;

@Slf4j
@RestController
@RequestMapping("/keyword")
@RequiredArgsConstructor
public class KeywordController {

    private final KeywordService keywordService;

    @GetMapping("/page")
    public Result<Page<Keyword>> page(@RequestParam(defaultValue = "1") Integer current,
                                      @RequestParam(defaultValue = "10") Integer size,
                                      @RequestParam(required = false) String keyword) {
        Page<Keyword> page = keywordService.page(
                Page.of(current, size),
                Wrappers.<Keyword>lambdaQuery()
                        .like(StrUtil.isNotBlank(keyword), Keyword::getKeyword, keyword)
                        .orderByDesc(Keyword::getId)
        );
        return Result.success(page);
    }

    @PostMapping("/add")
    public Result<Void> add(@Valid @RequestBody Keyword keyword) {
        if (Objects.isNull(keyword.getStatus())) {
            keyword.setStatus(false);
        }
        keywordService.save(keyword);
        if (Boolean.TRUE.equals(keyword.getStatus())) {
            this.keywordService.updateStatus(keyword.getId(), keyword.getStatus());
        }
        return Result.success();
    }

    @PutMapping("/update")
    public Result<Void> update(@Valid @RequestBody Keyword keyword) {
        if (Objects.isNull(keyword.getId())) {
            return Result.error("关键词ID不能为空");
        }
        keywordService.updateById(keyword);
        if (Objects.nonNull(keyword.getStatus())) {
            this.keywordService.updateStatus(keyword.getId(), keyword.getStatus());
        }
        return Result.success();
    }

    @PutMapping("/status/{id}")
    public Result<Void> updateStatus(@PathVariable Long id, @RequestParam("status") Boolean status) {
        this.keywordService.updateStatus(id, status);
        return Result.success();
    }
}
