package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.Bill;

import java.util.List;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface BillService extends IService<Bill> {

    List<Bill> userBills(Long userId);
}
