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
    Address getOrAssignAddress(Long userId);

    /**
     * 批量导入地址
     * 
     * @param addresses 地址列表
     * @return 成功导入数量
     */
    int batchImport(List<String> addresses);

    /**
     * 检查地址是否存在
     * 
     * @param address 地址
     * @return true=存在, false=不存在
     */
    boolean exists(String address);

    /**
     * 查询用户的充值地址
     * 
     * @param userId 用户ID
     * @return 地址信息,不存在返回null
     */
    Address getByUserId(Long userId);

    /**
     * 更新地址二维码图片ID
     * 
     * @param addressId 地址ID
     * @param qrImageId 二维码图片ID
     * @return 更新成功true, 失败false
     */
    boolean updateQrImageId(Long addressId, String qrImageId);
}
