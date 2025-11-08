package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.Bill;
import com.search.robots.database.entity.User;
import com.search.robots.database.mapper.BillMapper;
import com.search.robots.database.mapper.UserMapper;
import com.search.robots.database.service.BillService;
import com.search.robots.database.service.UserService;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Service
public class BillServiceImpl extends ServiceImpl<BillMapper, Bill> implements BillService {


    @Override
    public List<Bill> userBills(Long userId) {
        List<Bill> bills = this.baseMapper.selectList(
                Wrappers.<Bill>lambdaQuery()
                        .eq(Bill::getUserId, userId)
                        .orderByDesc(Bill::getCreateTime)
                        .last(" limit 20")
        );
        return CollUtil.isEmpty(bills) ? Collections.emptyList() : bills;
    }
}
