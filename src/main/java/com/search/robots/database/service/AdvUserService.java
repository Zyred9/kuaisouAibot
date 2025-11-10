package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.beans.view.vo.adv.AdvUserAudit;
import com.search.robots.database.entity.AdvUser;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.enums.adv.AdvTypeEnum;
import lombok.NonNull;

import java.util.List;

/**
 * 用户广告购买记录Service
 * <pre>
 * 提供广告购买、续费、查询等功能
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
public interface AdvUserService extends IService<AdvUser> {

    /**
     * 查询用户的所有广告
     */
    List<AdvUser> listByUserId(Long userId);

    /**
     * 查询用户指定状态的广告
     */
    List<AdvUser> listByUserIdAndStatus(Long userId, AdvStatus status);

    /**
     * 查询需要自动续费的广告
     */
    List<AdvUser> listAutoRenewAds();

    void auditAdvUser(AdvUserAudit audit);

    Page<AdvUser> selfPage(int current, Long userId, AdvTypeEnum type, AdvStatus status);

    String buildCurrent(String keyword);
}
