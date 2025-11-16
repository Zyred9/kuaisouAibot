package com.search.robots.database.service.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.view.trc20.TransferBean;
import com.search.robots.database.entity.Recharge;
import com.search.robots.database.mapper.RechargeMapper;
import com.search.robots.database.service.RechargeService;
import org.springframework.stereotype.Service;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Service
public class RechargeServiceImpl extends ServiceImpl<RechargeMapper, Recharge> implements RechargeService {

    @Override
    public void processorRecharge(TransferBean transferBean, Long userId) {
        Long count = this.baseMapper.selectCount(
                Wrappers.<Recharge>lambdaQuery()
                        .eq(Recharge::getTransactionId, transferBean.getTransactionId())
        );
        if (count == 0) {
            Recharge recharge = Recharge.build(transferBean, userId);
            this.baseMapper.insert(recharge);
        }
    }
}
