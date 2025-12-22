package com.search.robots.database.controller;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.view.vo.AdvShow;
import com.search.robots.database.entity.AdvLibrary;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import com.search.robots.database.service.AdvLibraryService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 广告库管理控制器
 *
 * @author admin
 * @since 2025/11/24
 */
@Slf4j
@RestController
@RequestMapping("/adv_library")
@RequiredArgsConstructor
public class AdvLibraryController {

    private final AdvLibraryService advLibraryService;

    /**
     * 分页查询广告库
     */
    @GetMapping("/page")
    public Result<Map<String, Object>> page(@RequestParam("size") int size,
                                             @RequestParam("current") int current,
                                             @RequestParam(value = "keyword", required = false) String keyword,
                                             @RequestParam(value = "advType", required = false) String advType) {
        LambdaQueryWrapper<AdvLibrary> wrapper = new LambdaQueryWrapper<>();
        
        // 关键词模糊查询
        if (StrUtil.isNotBlank(keyword)) {
            wrapper.like(AdvLibrary::getKeyword, keyword);
        }
        
        // 广告类型筛选（传入的是枚举名称，需要转换）
        if (StrUtil.isNotBlank(advType)) {
            try {
                AdvTypeEnum typeEnum = AdvTypeEnum.valueOf(advType);
                wrapper.eq(AdvLibrary::getAdvType, typeEnum);
            } catch (Exception e) {
                log.warn("无效的广告类型: {}", advType);
            }
        }
        wrapper.orderByDesc(AdvLibrary::getShowCount);
        Page<AdvLibrary> page = advLibraryService.page(Page.of(current, size), wrapper);
        
        // 组装返回数据
        Map<String, Object> result = new HashMap<>();
        result.put("records", page.getRecords());
        result.put("total", page.getTotal());
        result.put("current", page.getCurrent());
        result.put("size", page.getSize());
        
        return Result.success(result);
    }

    /**
     * 获取详情
     */
    @GetMapping("/detail/{id}")
    public Result<AdvLibrary> detail(@PathVariable Long id) {
        AdvLibrary advLibrary = advLibraryService.getById(id);
        if (advLibrary == null) {
            return Result.error("广告库不存在");
        }
        return Result.success(advLibrary);
    }

    /**
     * 新增广告库
     */
    @PostMapping("/add")
    public Result<Void> add(@RequestBody @Validated AddAdvLibraryRequest request) {
        // 检查关键词长度
        if (request.getKeyword().length() > 5) {
            return Result.error("关键词不能超过5个字");
        }
        
        // 检查关键词是否已存在（同一广告类型下）
        LambdaQueryWrapper<AdvLibrary> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(AdvLibrary::getKeyword, request.getKeyword())
               .eq(AdvLibrary::getAdvType, request.getAdvType());
        if (advLibraryService.count(wrapper) > 0) {
            return Result.error("该广告类型下关键词已存在");
        }
        
        AdvLibrary advLibrary = AdvLibrary.buildDefaultLibrary(request.getKeyword(), request.getAdvType());
        advLibrary.setPrice(request.getPrice() != null ? request.getPrice() : BigDecimal.ZERO);
        advLibrary.setShowCount(request.getShowCount() != null ? request.getShowCount() : 0L);
        
        // 调用Service方法
        advLibraryService.addAdvLibrary(advLibrary);
        return Result.success();
    }

    /**
     * 编辑广告库
     */
    @PutMapping("/edit")
    public Result<Void> edit(@RequestBody @Validated EditAdvLibraryRequest request) {
        AdvLibrary advLibrary = advLibraryService.getById(request.getId());
        if (advLibrary == null) {
            return Result.error("广告库不存在");
        }
        
        // 检查关键词长度
        if (request.getKeyword().length() > 5) {
            return Result.error("关键词不能超过5个字");
        }
        
        // 检查关键词是否重复（排除自己）
        LambdaQueryWrapper<AdvLibrary> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(AdvLibrary::getKeyword, request.getKeyword())
               .eq(AdvLibrary::getAdvType, request.getAdvType())
               .ne(AdvLibrary::getId, request.getId());
        if (advLibraryService.count(wrapper) > 0) {
            return Result.error("该广告类型下关键词已存在");
        }
        
        // 设置更新字段（不更新advType）
        advLibrary.setKeyword(request.getKeyword());
        advLibrary.setPrice(request.getPrice());
        advLibrary.setShowCount(request.getShowCount() != null ? request.getShowCount() : 0L);
        
        // 调用Service方法
        advLibraryService.updateAdvLibrary(advLibrary);
        return Result.success();
    }

    /**
     * 删除广告库
     */
    @DeleteMapping("/delete/{id}")
    public Result<Void> delete(@PathVariable Long id) {
        advLibraryService.removeAdvLibrary(id);
        return Result.success();
    }

    /**
     * 编辑近7日展现数据
     */
    @PutMapping("/show7d/{id}")
    public Result<Void> updateShow7d(@PathVariable Long id, @RequestBody @Validated UpdateShow7dRequest request) {
        AdvLibrary advLibrary = advLibraryService.getById(id);
        if (advLibrary == null) {
            return Result.error("广告库不存在");
        }
        
        List<AdvShow> show7d = request.getShow7d();
        if (show7d == null) {
            show7d = new ArrayList<>();
        }
        
        // 校验：最多7条
        if (show7d.size() > 7) {
            return Result.error("最近7天数据最多7条记录");
        }
        
        // 校验：日期唯一性
        java.util.Set<String> dateSet = new java.util.HashSet<>();
        for (AdvShow item : show7d) {
            if (!dateSet.add(item.getDate())) {
                return Result.error("存在重复日期：" + item.getDate());
            }
        }
        
        // 校验：日期范围（最近7天）
        LocalDate today = LocalDate.now();
        LocalDate sevenDaysAgo = today.minusDays(6);
        
        for (AdvShow item : show7d) {
            if (StrUtil.isBlank(item.getDate())) {
                return Result.error("日期不能为空");
            }
            
            LocalDate itemDate;
            try {
                itemDate = LocalDate.parse(item.getDate());
            } catch (Exception e) {
                return Result.error("日期格式错误：" + item.getDate());
            }
            
            // 必须在最近7天内
            if (itemDate.isBefore(sevenDaysAgo) || itemDate.isAfter(today)) {
                return Result.error("日期 " + item.getDate() + " 不在最近7天范围内");
            }
            
            // 校验非负数
            if (item.getDirectShow() == null || item.getDirectShow() < 0) {
                return Result.error("直接展示次数不能为负数");
            }
            if (item.getUniqueUser() == null || item.getUniqueUser() < 0) {
                return Result.error("独立访客数不能为负数");
            }
            if (item.getRelatedShow() == null || item.getRelatedShow() < 0) {
                return Result.error("相关展示次数不能为负数");
            }
        }
        
        // 按日期排序（从旧到新）
        show7d.sort((a, b) -> a.getDate().compareTo(b.getDate()));
        
        // 计算新的7天总展现次数
        long new7DayTotal = show7d.stream()
                .mapToLong(item -> (item.getDirectShow() != null ? item.getDirectShow() : 0L) 
                                 + (item.getRelatedShow() != null ? item.getRelatedShow() : 0L))
                .sum();
        
        // 计算旧的7天总展现次数
        List<AdvShow> oldShow7d = advLibrary.getShow7d();
        long old7DayTotal = 0L;
        if (oldShow7d != null && !oldShow7d.isEmpty()) {
            old7DayTotal = oldShow7d.stream()
                    .mapToLong(item -> (item.getDirectShow() != null ? item.getDirectShow() : 0L) 
                                     + (item.getRelatedShow() != null ? item.getRelatedShow() : 0L))
                    .sum();
        }
        
        // 更新总展现次数：原值 - 旧7天 + 新7天
        long currentShowCount = advLibrary.getShowCount() != null ? advLibrary.getShowCount() : 0L;
        long newShowCount = currentShowCount - old7DayTotal + new7DayTotal;
        // 确保不为负数
        if (newShowCount < 0) {
            newShowCount = new7DayTotal;
        }
        
        // 更新数据库
        advLibrary.setShow7d(show7d);
        advLibrary.setShowCount(newShowCount);
        advLibrary.setUpdatedAt(LocalDateTime.now());
        advLibraryService.updateAdvLibrary(advLibrary);
        
        return Result.success();
    }

    /**
     * 新增请求
     */
    @lombok.Getter
    @lombok.Setter
    public static class AddAdvLibraryRequest {
        @NotNull(message = "广告类型不能为空")
        private AdvTypeEnum advType;
        
        @NotBlank(message = "关键词不能为空")
        private String keyword;
        
        private BigDecimal price;
        
        private Long showCount;
    }

    /**
     * 编辑请求
     */
    @lombok.Getter
    @lombok.Setter
    public static class EditAdvLibraryRequest {
        @NotNull(message = "ID不能为空")
        private Long id;
        
        @NotNull(message = "广告类型不能为空")
        private AdvTypeEnum advType;
        
        @NotBlank(message = "关键词不能为空")
        private String keyword;
        
        private BigDecimal price;
        
        private Long showCount;
    }

    /**
     * 更新最近7日展现数据请求
     */
    @lombok.Getter
    @lombok.Setter
    public static class UpdateShow7dRequest {
        @NotNull(message = "展现数据不能为空")
        private List<AdvShow> show7d;
    }
}
