package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.AdvPrice;
import com.search.robots.database.enums.adv.AdvPositionEnum;
import com.search.robots.database.mapper.AdvPriceMapper;
import com.search.robots.database.service.AdvPriceService;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

/**
 * 广告价格ServiceImpl
 * <pre>
 * 实现价格配置的增删改查功能
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
@Service
public class AdvPriceServiceImpl extends ServiceImpl<AdvPriceMapper, AdvPrice> implements AdvPriceService {

    @Override
    public List<AdvPrice> listByLibraryId(Long libraryId) {
        if (Objects.isNull(libraryId)) {
            return CollUtil.newArrayList();
        }
        return this.baseMapper.selectList(
                Wrappers.<AdvPrice>lambdaQuery()
                        .eq(AdvPrice::getLibraryId, libraryId)
                        .orderByAsc(AdvPrice::getAdvPosition)
                        .orderByAsc(AdvPrice::getRanking)
        );
    }

    @Override
    public AdvPrice getByLibraryIdAndPosition(Long libraryId, AdvPositionEnum position, String source, Integer ranking) {
        if (Objects.isNull(libraryId) || Objects.isNull(position)) {
            return null;
        }
        return this.baseMapper.selectOne(
                Wrappers.<AdvPrice>lambdaQuery()
                        .eq(AdvPrice::getLibraryId, libraryId)
                        .eq(AdvPrice::getAdvPosition, position)
                        .eq(Objects.nonNull(source), AdvPrice::getSource, source)
                        .eq(Objects.nonNull(ranking), AdvPrice::getRanking, ranking)
                        .eq(AdvPrice::getStatus, 1)
                        .last("LIMIT 1")
        );
    }

    @Override
    public boolean batchInsert(List<AdvPrice> priceList) {
        if (CollUtil.isEmpty(priceList)) {
            return false;
        }
        return this.saveBatch(priceList);
    }

    @Override
    public List<AdvPrice> listEnabledByLibraryId(Long libraryId) {
        if (Objects.isNull(libraryId)) {
            return CollUtil.newArrayList();
        }
        return this.baseMapper.selectList(
                Wrappers.<AdvPrice>lambdaQuery()
                        .eq(AdvPrice::getLibraryId, libraryId)
                        .eq(AdvPrice::getStatus, 1)
                        .orderByAsc(AdvPrice::getAdvPosition)
                        .orderByAsc(AdvPrice::getRanking)
        );
    }

    @Override
    public List<AdvPrice> listEnabledByLibraryIds(List<Long> libraryIds) {
        if (CollUtil.isEmpty(libraryIds)) {
            return CollUtil.newArrayList();
        }
        return this.baseMapper.selectList(
                Wrappers.<AdvPrice>lambdaQuery()
                        .in(AdvPrice::getLibraryId, libraryIds)
                        .eq(AdvPrice::getStatus, 1)
                        .orderByAsc(AdvPrice::getLibraryId)
                        .orderByAsc(AdvPrice::getAdvPosition)
                        .orderByAsc(AdvPrice::getRanking)
        );
    }
}
