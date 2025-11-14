package com.search.robots.database.controller;

import cn.hutool.core.util.StrUtil;
import com.alibaba.excel.EasyExcel;
import com.search.robots.beans.view.base.Result;
import com.search.robots.database.service.AddressService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * TRC20地址管理控制器
 * 
 * @author zyred
 * @since 2025/01/17
 */
@Slf4j
@RestController
@RequestMapping("/address")
@RequiredArgsConstructor
public class AddressController {

    private final AddressService addressService;

    /**
     * Excel批量导入地址
     * 支持单列address格式
     * 
     * @param file Excel文件
     * @return 导入结果
     */
    @PostMapping("/import")
    public Result<Integer> importAddresses(@RequestParam("file") MultipartFile file) {
        if (file == null || file.isEmpty()) {
            return Result.error("文件不能为空");
        }
        try {
            List<AddressExcelRow> rows = EasyExcel.read(file.getInputStream())
                    .head(AddressExcelRow.class)
                    .sheet()
                    .doReadSync();
            if (rows == null || rows.isEmpty()) {
                return Result.error("Excel文件为空");
            }
            Set<String> addressSet = new HashSet<>();
            List<String> duplicates = new ArrayList<>();
            
            for (AddressExcelRow row : rows) {
                String address = row.getAddress();
                if (StrUtil.isBlank(address)) {
                    continue;
                }
                String trimmedAddress = address.trim();
                if (addressSet.contains(trimmedAddress)) {
                    duplicates.add(trimmedAddress);
                    continue;
                }
                addressSet.add(trimmedAddress);
            }
            log.info("Excel读取完成, 原始行数: {}, 有效地址: {}, 内存重复: {}",
                    rows.size(), addressSet.size(), duplicates.size());
            int importCount = addressService.batchImport(new ArrayList<>(addressSet));
            return Result.success(importCount);
        } catch (IOException e) {
            log.error("读取Excel文件失败", e);
            return Result.error("读取Excel文件失败: " + e.getMessage());
        } catch (Exception e) {
            log.error("导入地址失败", e);
            return Result.error("导入失败: " + e.getMessage());
        }
    }

    /**
     * Excel行数据模型
     */
    @lombok.Data
    public static class AddressExcelRow {
        /** TRC20地址 **/
        private String address;
    }
}
