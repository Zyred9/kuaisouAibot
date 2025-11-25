package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.view.vo.AdvShow;
import com.search.robots.database.entity.AdvLibrary;
import com.search.robots.database.entity.AdvPrice;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import com.search.robots.database.mapper.AdvLibraryMapper;
import com.search.robots.database.service.AdvLibraryService;
import com.search.robots.database.service.AdvPriceService;
import com.search.robots.helper.RedisHelper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

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
    @Transactional(rollbackFor = Exception.class)
    public AdvLibrary buildDefault(String keyword, String data) {
        AdvLibrary newLibrary = AdvLibrary.buildDefaultLibrary(keyword, AdvTypeEnum.ofData(data));
        this.baseMapper.insert(newLibrary);

        List<AdvPrice> advPrices = this.advPriceService.saveTheLibraryPrice(newLibrary);
        newLibrary.setPriceList(advPrices);

        RedisHelper.hPut(AdvLibrary.ADV_LIBRARY_KEY, keyword, String.valueOf(newLibrary.getId()));
        return newLibrary;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AdvLibrary getByKeywordWithPrices(String keyword, String data) {
        // 参数校验
        if (StrUtil.isBlank(keyword)) {
            return null;
        }
        AdvLibrary library = null;
        if (RedisHelper.hExists(AdvLibrary.ADV_LIBRARY_KEY, keyword)) {
            try {
                String libraryIdStr = RedisHelper.hGet(AdvLibrary.ADV_LIBRARY_KEY, keyword);
                if (StrUtil.isNotBlank(libraryIdStr)) {
                    long libraryId = Long.parseLong(libraryIdStr);
                    library = this.baseMapper.selectById(libraryId);
                    if (Objects.isNull(library)) {
                        RedisHelper.hDelete(AdvLibrary.ADV_LIBRARY_KEY, keyword);
                    }
                }
            } catch (NumberFormatException e) {
                RedisHelper.hDelete(AdvLibrary.ADV_LIBRARY_KEY, keyword);
            }
        }
        if (Objects.isNull(library)) {
            library = this.selectByKeyword(keyword);
        }
        if (Objects.isNull(library)) {
            return this.buildDefault(keyword, data);
        }
        if (Objects.nonNull(library.getId())) {
            library.setPriceList(this.advPriceService.listEnabledByLibraryId(library.getId()));
        }
        
        return library;
    }

    @Override
    public AdvLibrary getByIdWithPrices(Long libraryId) {
        if (Objects.isNull(libraryId)) {
            return null;
        }
        
        AdvLibrary library = this.baseMapper.selectById(libraryId);
        if (Objects.nonNull(library) && Objects.nonNull(library.getId())) {
            library.setPriceList(this.advPriceService.listEnabledByLibraryId(library.getId()));
        }
        
        return library;
    }

    @Override
    public void search(String keyword) {
        AdvLibrary library = this.baseMapper.selectOne(
                Wrappers.<AdvLibrary>lambdaQuery()
                        .eq(AdvLibrary::getKeyword, keyword)
                        .orderByDesc(AdvLibrary::getId)
                        .last("limit 1")
        );
        if (Objects.isNull(library)) {
            return;
        }

        String now = LocalDate.now().toString();
        List<AdvShow> advShows = library.getShow7d();
        if (CollUtil.isEmpty(advShows)) {
            advShows = new ArrayList<>();
        }
        Map<String, AdvShow> map = advShows.stream().collect(Collectors
                .toMap(AdvShow::getDate, Function.identity()));
        if (!map.containsKey(now)) {
            advShows.add(AdvShow.buildDefault(true));
            library.setShow7d(advShows);
        } else {
            AdvShow advShow = map.get(now);
            advShow.setUniqueUser(advShow.getUniqueUser() + 1);
            advShow.setDirectShow(advShow.getDirectShow() + 1);
        }
        this.baseMapper.updateById(library);
    }


    private AdvLibrary selectByKeyword (String keyword) {
        return this.baseMapper.selectOne(
                Wrappers.<AdvLibrary>lambdaQuery()
                        .eq(AdvLibrary::getKeyword, keyword)
                        .orderByDesc(AdvLibrary::getShowCount)
                        .last("LIMIT 1")
        );
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void addAdvLibrary(AdvLibrary advLibrary) {
        // 插入数据库
        this.baseMapper.insert(advLibrary);
        
        // 更新Redis缓存
        RedisHelper.hPut(AdvLibrary.ADV_LIBRARY_KEY, advLibrary.getKeyword(), String.valueOf(advLibrary.getId()));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateAdvLibrary(AdvLibrary advLibrary) {
        // 获取旧数据
        AdvLibrary oldLibrary = this.baseMapper.selectById(advLibrary.getId());
        if (Objects.isNull(oldLibrary)) {
            return;
        }
        
        // 更新数据库
        this.baseMapper.updateById(advLibrary);
        
        // 如果关键词变了，需要更新Redis缓存
        if (!StrUtil.equals(oldLibrary.getKeyword(), advLibrary.getKeyword())) {
            // 删除旧关键词缓存
            RedisHelper.hDelete(AdvLibrary.ADV_LIBRARY_KEY, oldLibrary.getKeyword());
            // 添加新关键词缓存
            RedisHelper.hPut(AdvLibrary.ADV_LIBRARY_KEY, advLibrary.getKeyword(), String.valueOf(advLibrary.getId()));
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void removeAdvLibrary(Long id) {
        // 获取数据
        AdvLibrary library = this.baseMapper.selectById(id);
        if (Objects.isNull(library)) {
            return;
        }
        
        // 删除数据库记录
        this.baseMapper.deleteById(id);
        
        // 删除Redis缓存
        RedisHelper.hDelete(AdvLibrary.ADV_LIBRARY_KEY, library.getKeyword());
    }

}
