package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.AdvLibrary;
import com.search.robots.database.entity.AdvPrice;
import com.search.robots.database.enums.adv.AdvPositionEnum;
import com.search.robots.database.mapper.AdvPriceMapper;
import com.search.robots.database.service.AdvPriceService;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * 广告价格ServiceImpl
 * <pre>
 * 实现价格配置的增删改查功能
 * </pre>
 *
 * @author admin
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
            return selectDefault();
        }
        List<AdvPrice> result = this.baseMapper.selectList(
                Wrappers.<AdvPrice>lambdaQuery()
                        .eq(AdvPrice::getLibraryId, libraryId)
                        .eq(AdvPrice::getStatus, 1)
                        .orderByAsc(AdvPrice::getAdvPosition)
                        .orderByAsc(AdvPrice::getRanking)
        );
        if (CollUtil.isEmpty(result)) {
            result = this.selectDefault();
        }
        return result;
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

    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<AdvPrice> saveTheLibraryPrice(AdvLibrary newLibrary) {
        List<AdvPrice> advPrices = this.selectDefault();

        for (AdvPrice price : advPrices) {
            price.setId(null);
            price.setLibraryId(newLibrary.getId());
            price.setCreatedAt(LocalDateTime.now());
            price.setUpdatedAt(LocalDateTime.now());
            price.setRemark("");
        }
        this.batchInsert(advPrices);
        return advPrices;
    }

    private List<AdvPrice> selectDefault () {
        return this.baseMapper.selectList(
                Wrappers.<AdvPrice>lambdaQuery()
                        .eq(AdvPrice::getLibraryId, 0L)
                        .eq(AdvPrice::getStatus, 1)
                        .orderByAsc(AdvPrice::getAdvPosition)
                        .orderByAsc(AdvPrice::getRanking)
        );
    }

    @Override
    public Page<AdvPrice> pagePrices(Long libraryId, int current, int size) {
        return this.page(Page.of(current, size),
                Wrappers.<AdvPrice>lambdaQuery()
                        .eq(AdvPrice::getLibraryId, libraryId)
                        .orderByAsc(AdvPrice::getAdvPosition)
                        .orderByAsc(AdvPrice::getRanking));
    }

    @Override
    public boolean addPrice(Long libraryId, Integer advPositionCode, Integer ranking, BigDecimal monthlyPrice, String source, String remark) {
        AdvPositionEnum position = toPosition(advPositionCode);
        if (Objects.isNull(libraryId) || Objects.isNull(position) || Objects.isNull(monthlyPrice)) {
            return false;
        }
        AdvPrice price = new AdvPrice()
                .setLibraryId(libraryId)
                .setAdvPosition(position)
                .setMonthlyPrice(monthlyPrice)
                .setCurrency("CNY")
                .setVersion(1)
                .setStatus(1)
                .setIsSold(false)
                .setRemark(remark == null ? "" : remark)
                .setCreatedAt(LocalDateTime.now())
                .setUpdatedAt(LocalDateTime.now())
                .setSource(source);
        if (position.isRankPosition()) {
            price.setRanking(Objects.nonNull(ranking) ? ranking : 1);
        } else {
            price.setRanking(null);
        }
        return this.save(price);
    }

    @Override
    public boolean editPrice(Long id, BigDecimal monthlyPrice) {
        AdvPrice db = this.getById(id);
        if (Objects.isNull(db) || Objects.isNull(monthlyPrice)) {
            return false;
        }
        db.setMonthlyPrice(monthlyPrice);
        db.setUpdatedAt(LocalDateTime.now());
        return this.updateById(db);
    }

    @Override
    public boolean deletePrice(Long id) {
        return this.removeById(id);
    }

    @Override
    public void processorDefault() {
        List<AdvPrice> defaultPrices = this.selectDefault();
        if (CollUtil.isNotEmpty(defaultPrices)) {
            return;
        }
        List<AdvPrice> inserts = new ArrayList<>();
        List<Integer> positions = List.of(1, 2, 3, 4 ,5 ,6 , 7, 101, 102);
        List<Integer> priceList = List.of(36, 34, 31, 29, 26, 24, 21, 519, 512);
        List<BigDecimal> decimals = new ArrayList<>(priceList.size());

        for (Integer i : priceList) {
            decimals.add(BigDecimal.valueOf(i));
        }

        for (int i = 0; i < positions.size(); i++) {
            inserts.add(AdvPrice.buildDefault(i, positions.get(i), decimals.get(i)));
        }
        this.batchInsert(inserts);
    }

    private AdvPositionEnum toPosition(Integer code) {
        if (Objects.isNull(code)) return null;
        for (AdvPositionEnum e : AdvPositionEnum.values()) {
            if (e.getCode() == code) return e;
        }
        return null;
    }
}
