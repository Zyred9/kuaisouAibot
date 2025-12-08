package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.view.trc20.TransferBean;
import com.search.robots.database.entity.ListenReceive;
import com.search.robots.database.mapper.ListenReceiveMapper;
import com.search.robots.database.service.ListenReceiveService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * TRC20地址服务实现类
 * 
 * @author admin
 * @since 2025/01/17
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class ListenReceiveServiceImpl extends ServiceImpl<ListenReceiveMapper, ListenReceive> implements ListenReceiveService {

    public List<TransferBean> distinct (List<TransferBean> transferBeans, String address) {
        List<String> transactionIds = transferBeans.stream().map(TransferBean::getTransactionId).toList();
        List<ListenReceive> pushedList = this.baseMapper.selectList(
                Wrappers.<ListenReceive>lambdaQuery()
                        .select(ListenReceive::getTransactionId)
                        .eq(ListenReceive::getAddress, address)
                        .in(ListenReceive::getTransactionId, transactionIds)
        );
        Set<String> tIds = pushedList.stream().map(ListenReceive::getTransactionId).collect(Collectors.toSet());
        transferBeans.removeIf(transferBean -> CollUtil.contains(tIds, transferBean.getTransactionId()));
        return transferBeans;
    }

}
