package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.Bill;
import com.search.robots.database.entity.Exhibition;

import java.time.LocalDate;
import java.util.List;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface ExhibitionService extends IService<Exhibition> {

    void exhibition(Long chatId, boolean isContent);

    /**
     * 自定义展示次数，一般是给后台使用的
     *
     * @param exhibition    请求参数
     */
    void customExhibition(Exhibition exhibition);

    String querySevenExhibition(List<LocalDate> localDates, long chatId);
}
