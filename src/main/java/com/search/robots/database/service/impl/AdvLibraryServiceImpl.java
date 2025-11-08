package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.AdvLibrary;
import com.search.robots.database.entity.AdvPrice;
import com.search.robots.database.mapper.AdvLibraryMapper;
import com.search.robots.database.service.AdvLibraryService;
import com.search.robots.database.service.AdvPriceService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

/**
 * 广告库ServiceImpl
 * <pre>
 * 实现关键词广告的查询和管理功能
 * 优先使用Mapper层关联查询,避免N+1查询问题
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
@Service
@RequiredArgsConstructor
public class AdvLibraryServiceImpl extends ServiceImpl<AdvLibraryMapper, AdvLibrary> implements AdvLibraryService {

    private final AdvPriceService advPriceService;

    @Override
    public AdvLibrary getByKeyword(String keyword) {
        if (StrUtil.isBlank(keyword)) {
            return null;
        }
        return this.baseMapper.selectOne(
                Wrappers.<AdvLibrary>lambdaQuery()
                        .eq(AdvLibrary::getKeyword, keyword)
                        .last("LIMIT 1")
        );
    }

    @Override
    public AdvLibrary getByKeywordWithPrices(String keyword) {
        if (StrUtil.isBlank(keyword)) {
            return null;
        }
        // 1) 先查询关键词实体
        AdvLibrary library = this.baseMapper.selectOne(
                Wrappers.<AdvLibrary>lambdaQuery()
                        .eq(AdvLibrary::getKeyword, keyword)
                        .last("LIMIT 1")
        );
        if (Objects.isNull(library)) {
            return null;
        }
        // 2) 再根据关键词ID查询其价格信息(仅启用)
        List<AdvPrice> prices = advPriceService.listEnabledByLibraryId(library.getId());
        // 3) 组装到非持久化字段 priceList
        library.setPriceList(prices);
        return library;
    }

    @Override
    public List<AdvLibrary> getByKeywordsWithPrices(List<String> keywords) {
        if (CollUtil.isEmpty(keywords)) {
            return CollUtil.newArrayList();
        }
        // 1) 一次性查询所有关键词实体
        List<AdvLibrary> libraries = this.baseMapper.selectList(
                Wrappers.<AdvLibrary>lambdaQuery()
                        .in(AdvLibrary::getKeyword, keywords)
        );
        if (CollUtil.isEmpty(libraries)) {
            return libraries;
        }
        // 2) 收集所有libraryId,一次性批量查询所有价格(避免N+1)
        List<Long> libraryIds = libraries.stream().map(AdvLibrary::getId).toList();
        List<AdvPrice> allPrices = advPriceService.listEnabledByLibraryIds(libraryIds);
        if (CollUtil.isEmpty(allPrices)) {
            return libraries;
        }
        // 3) 按libraryId分组价格,再填充回各实体
        var pricesByLibraryId = allPrices.stream().collect(java.util.stream.Collectors.groupingBy(AdvPrice::getLibraryId));
        libraries.forEach(lib -> lib.setPriceList(pricesByLibraryId.getOrDefault(lib.getId(), CollUtil.newArrayList())));
        return libraries;
    }

    @Override
    public List<AdvLibrary> getHotKeywords(int limit) {
        return this.baseMapper.selectList(
                Wrappers.<AdvLibrary>lambdaQuery()
                        .orderByDesc(AdvLibrary::getShowCount)
                        .last("LIMIT " + Math.min(limit, 100))
        );
    }

    @Override
    public List<AdvLibrary> getHotKeywordsWithPrices(int limit) {
        return this.baseMapper.selectHotKeywordsWithPrices(Math.min(limit, 100));
    }

    @Override
    public List<AdvPrice> getPriceListByLibraryId(Long libraryId) {
        if (Objects.isNull(libraryId)) {
            return CollUtil.newArrayList();
        }
        return advPriceService.listEnabledByLibraryId(libraryId);
    }
}
