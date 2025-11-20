package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.beans.web.withdrawals.WithdrawalsAudit;
import com.search.robots.database.entity.User;
import com.search.robots.database.entity.Withdrawals;

import javax.validation.Valid;
import java.math.BigDecimal;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface WithdrawalsService extends IService<Withdrawals> {

    void create (User user, BigDecimal amount);

    boolean audit(@Valid WithdrawalsAudit audit);
}
