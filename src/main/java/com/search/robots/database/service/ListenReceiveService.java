package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.beans.view.trc20.TransferBean;
import com.search.robots.database.entity.ListenReceive;

import java.util.List;

/**
 * TRC20地址服务接口
 * 
 * @author admin
 * @since 2025/01/17
 */
public interface ListenReceiveService extends IService<ListenReceive> {

    List<TransferBean> distinct (List<TransferBean> transferBeans, String address);

}
