package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.beans.view.trc20.TransferBean;
import com.search.robots.database.entity.Recharge;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface RechargeService extends IService<Recharge> {

    void processorRecharge(TransferBean transferBean, Long userId);

}
