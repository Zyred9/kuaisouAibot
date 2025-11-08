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
                        .orderByDesc(AdvLibrary::getShowCount)
                        .last("LIMIT 1")
        );
        if (Objects.isNull(library)) {
            library = new AdvLibrary();
        }
        library.setPriceList(
                this.advPriceService.listEnabledByLibraryId(library.getId())
        );
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
                        .last("LIMIT " + Math.min(limit, 40))
        );
    }

    @Override
    public List<AdvLibrary> getHotKeywordsWithPrices(int limit) {
        // 1) 先获取热门关键词列表
        List<AdvLibrary> hotKeywords = getHotKeywords(limit);
        if (CollUtil.isEmpty(hotKeywords)) {
            return hotKeywords;
        }
        // 2) 提取关键词列表，批量查询价格信息
        List<String> keywords = hotKeywords.stream().map(AdvLibrary::getKeyword).toList();
        return getByKeywordsWithPrices(keywords);
    }

    @Override
    public List<AdvPrice> getPriceListByLibraryId(Long libraryId) {
        if (Objects.isNull(libraryId)) {
            return CollUtil.newArrayList();
        }
        return advPriceService.listEnabledByLibraryId(libraryId);
    }
}
