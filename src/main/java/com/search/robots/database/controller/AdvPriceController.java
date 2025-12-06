package com.search.robots.database.controller;

import com.search.robots.database.service.AdvPriceService;
import com.search.robots.beans.view.base.Result;
import com.search.robots.database.entity.AdvPrice;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 广告库管理控制器
 *
 * @author admin
 * @since 2025/11/24
 */
@Slf4j
@RestController
@RequestMapping("/adv_price")
@RequiredArgsConstructor
public class AdvPriceController {

    private final AdvPriceService advPriceService;

    @GetMapping("/page")
    public Result<Page<AdvPrice>> page(@RequestParam("libraryId") Long libraryId,
                                                  @RequestParam("size") int size,
                                                  @RequestParam("current") int current) {
        Page<AdvPrice> page = advPriceService.pagePrices(libraryId, current, size);
        return Result.success(page);
    }

    @PostMapping("/add")
    public Result<Void> add(@RequestBody @Validated AddAdvPriceRequest req) {
        boolean ok = advPriceService.addPrice(
                req.getLibraryId(),
                req.getAdvPositionCode(),
                req.getRanking(),
                req.getMonthlyPrice(),
                req.getSource(),
                req.getRemark()
        );
        return ok ? Result.success() : Result.error("新增失败");
    }

    @PutMapping("/edit")
    public Result<Void> edit(@RequestBody @Validated EditAdvPriceRequest req) {
        boolean ok = advPriceService.editPrice(req.getId(), req.getMonthlyPrice());
        return ok ? Result.success() : Result.error("更新失败");
    }

    @DeleteMapping("/delete/{id}")
    public Result<Void> delete(@PathVariable Long id) {
        boolean ok = advPriceService.deletePrice(id);
        return ok ? Result.success() : Result.error("删除失败");
    }

    @lombok.Getter
    @lombok.Setter
    public static class AddAdvPriceRequest {
        @NotNull
        private Long libraryId;
        @NotNull
        private Integer advPositionCode;
        private Integer ranking;
        @NotNull
        private BigDecimal monthlyPrice;
        private String source = "direct"; // 默认来源
        private String remark;
    }

    @lombok.Getter
    @lombok.Setter
    public static class EditAdvPriceRequest {
        @NotNull
        private Long id;
        @NotNull
        private BigDecimal monthlyPrice;
    }
}
