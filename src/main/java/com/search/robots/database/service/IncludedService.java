package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.beans.web.included.IncludedAudit;
import com.search.robots.database.entity.Included;
import com.search.robots.database.enums.Included.IncludedSearchTypeEnum;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface IncludedService extends IService<Included> {

    String buildSelfIndexText(Long userId);

    Page<Included> selectPage(int current, Long userId, IncludedSearchTypeEnum searchType);

    Page<Included> includedPage(int size, int current, String indexUsername);

    void includedAudit(IncludedAudit includedAudit);

    void updateSelf(Included update);

    Included get(Long chatId);
}
