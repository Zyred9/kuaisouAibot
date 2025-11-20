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
    public Result<Void> add(@RequestBody Keyword keyword) {
        if (StrUtil.isBlank(keyword.getKeyword())) {
            return Result.error("关键词不能为空");
        }
        if (Objects.isNull(keyword.getStatus())) {
            keyword.setStatus(true);
        }
        keywordService.save(keyword);
        return Result.success();
    }

    @PutMapping("/status/{id}")
    public Result<Void> updateStatus(@PathVariable Long id, @RequestParam Boolean status) {
        this.keywordService.updateStatus(id, status);
        return Result.success();
    }
}
