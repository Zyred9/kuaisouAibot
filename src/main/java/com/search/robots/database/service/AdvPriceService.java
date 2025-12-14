package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.AdvLibrary;
import com.search.robots.database.entity.AdvPrice;
import com.search.robots.database.enums.adv.AdvPositionEnum;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import java.math.BigDecimal;

import java.util.List;

/**
 * 广告价格Service
 * <pre>
 * 提供价格配置的增删改查功能
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
public interface AdvPriceService extends IService<AdvPrice> {

    /**
     * 根据广告库ID查询所有价格配置
     */
    List<AdvPrice> listByLibraryId(Long libraryId);

    /**
     * 根据广告库ID和位置查询价格
     */
    AdvPrice getByLibraryIdAndPosition(Long libraryId, AdvPositionEnum position, String source, Integer ranking);

    /**
     * 批量插入价格配置
     */
    boolean batchInsert(List<AdvPrice> priceList);

    /**
     * 查询启用状态的价格配置
     */
    List<AdvPrice> listEnabledByLibraryId(Long libraryId);

    /**
     * 批量查询多个库ID的启用价格配置(一次性查询,避免N+1)
     */
    List<AdvPrice> listEnabledByLibraryIds(List<Long> libraryIds);

    List<AdvPrice> saveTheLibraryPrice(AdvLibrary newLibrary);

    Page<AdvPrice> pagePrices(Long libraryId, int current, int size);

    boolean addPrice(Long libraryId, Integer advPositionCode, Integer ranking, BigDecimal monthlyPrice, String source, String remark);

    boolean editPrice(Long id, BigDecimal monthlyPrice);

    boolean deletePrice(Long id);

    void processorDefault();


}
