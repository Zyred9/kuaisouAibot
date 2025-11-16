package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.Address;

import java.util.List;

/**
 * TRC20地址服务接口
 * 
 * @author zyred
 * @since 2025/01/17
 */
public interface AddressService extends IService<Address> {

    /**
     * 获取或分配用户的TRC20充值地址
     * 
     * @param userId 用户ID
     * @return TRC20地址,如果无可用地址返回null
     */
    Address selectEmptyAddress(Long userId);

    /**
     * 批量导入地址
     * 
     * @param addresses 地址列表
     * @return 成功导入数量
     */
    int batchImport(List<String> addresses);

}
