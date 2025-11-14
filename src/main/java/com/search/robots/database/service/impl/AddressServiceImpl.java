package com.search.robots.database.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.Address;
import com.search.robots.database.mapper.AddressMapper;
import com.search.robots.database.service.AddressService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * TRC20地址服务实现类
 * 
 * @author zyred
 * @since 2025/01/17
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AddressServiceImpl extends ServiceImpl<AddressMapper, Address> implements AddressService {

    private final StringRedisTemplate redisTemplate;
    
    private static final String LOCK_KEY_PREFIX = "tron:address:lock:";
    private static final String ADDRESS_SET_KEY = "tron:address:set";
    
    @Override
    public Address getByUserId(Long userId) {
        return this.getOne(Wrappers.<Address>lambdaQuery()
                .eq(Address::getUserId, userId)
                .last("LIMIT 1"));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Address getOrAssignAddress(Long userId) {
        if (userId == null) {
            log.warn("用户ID为空,无法分配地址");
            return null;
        }

        // 1. 查询用户是否已有绑定地址
        Address existAddress = getByUserId(userId);
        if (existAddress != null) {
            log.info("用户 {} 已有绑定地址: {}", userId, existAddress.getAddress());
            return existAddress;
        }

        // 2. 使用分布式锁确保并发安全
        String lockKey = LOCK_KEY_PREFIX + userId;
        Boolean locked = redisTemplate.opsForValue().setIfAbsent(lockKey, "1", 10, TimeUnit.SECONDS);
        
        if (Boolean.FALSE.equals(locked)) {
            log.warn("用户 {} 正在分配地址中,请稍后重试", userId);
            return null;
        }

        try {
            // 3. 双重检查,防止并发问题
            existAddress = getByUserId(userId);
            if (existAddress != null) {
                return existAddress;
            }

            // 4. 查询第一个可用地址(带行锁)
            Address availableAddress = baseMapper.selectOne(
                    Wrappers.<Address>lambdaQuery()
                            .orderByAsc(Address::getId)
                            .last("limit 1")
            );
            
            if (availableAddress == null) {
                log.error("没有可用的TRC20地址,请先导入地址");
                return null;
            }

            // 5. 分配地址给用户
            availableAddress.setUserId(userId);
            availableAddress.setAssignTime(LocalDateTime.now());
            availableAddress.setUpdateTime(LocalDateTime.now());
            
            this.updateById(availableAddress);
            
            log.info("成功为用户 {} 分配地址: {}", userId, availableAddress.getAddress());
            return availableAddress;
            
        } finally {
            // 6. 释放锁
            redisTemplate.delete(lockKey);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int batchImport(List<String> addresses) {
        if (addresses == null || addresses.isEmpty()) {
            log.warn("导入地址列表为空");
            return 0;
        }

        // 1. 去重
        Set<String> uniqueAddresses = new HashSet<>(addresses);
        log.info("原始地址数量: {}, 去重后数量: {}", addresses.size(), uniqueAddresses.size());

        // 2. 查询数据库已存在的地址
        List<String> existingAddresses = this.list(Wrappers.<Address>lambdaQuery()
                .select(Address::getAddress)
                .in(Address::getAddress, uniqueAddresses))
                .stream()
                .map(Address::getAddress)
                .toList();

        log.info("数据库已存在地址数量: {}", existingAddresses.size());

        // 3. 过滤掉已存在的地址
        existingAddresses.forEach(uniqueAddresses::remove);

        if (uniqueAddresses.isEmpty()) {
            log.warn("所有地址均已存在,无需导入");
            return 0;
        }

        // 4. 批量插入新地址
        List<Address> newAddresses = new ArrayList<>();
        LocalDateTime now = LocalDateTime.now();
        
        for (String address : uniqueAddresses) {
            if (StrUtil.isBlank(address)) {
                continue;
            }
            
            // 验证地址格式(TRC20地址以T开头,长度34位)
            String trimmedAddress = address.trim();
            if (!isValidTronAddress(trimmedAddress)) {
                log.warn("无效的TRON地址: {}", trimmedAddress);
                continue;
            }
            
            Address tronAddress = new Address()
                    .setAddress(trimmedAddress)
                    .setCreateTime(now)
                    .setUpdateTime(now);
            
            newAddresses.add(tronAddress);
        }

        if (newAddresses.isEmpty()) {
            log.warn("没有有效的地址可以导入");
            return 0;
        }

        // 5. 批量保存
        this.saveBatch(newAddresses);
        
        // 6. 更新Redis缓存
        updateRedisAddressSet(newAddresses);

        log.info("成功导入 {} 个地址", newAddresses.size());
        return newAddresses.size();
    }

    @Override
    public boolean exists(String address) {
        if (StrUtil.isBlank(address)) {
            return false;
        }

        // 1. 先查Redis缓存
        Boolean exists = redisTemplate.opsForSet().isMember(ADDRESS_SET_KEY, address.trim());
        if (Boolean.TRUE.equals(exists)) {
            return true;
        }

        // 2. 查数据库
        long count = this.count(Wrappers.<Address>lambdaQuery()
                .eq(Address::getAddress, address.trim()));
        
        return count > 0;
    }

    @Override
    public boolean updateQrImageId(Long addressId, String qrImageId) {
        if (addressId == null) {
            return false;
        }

        return this.update(Wrappers.<Address>lambdaUpdate()
                .eq(Address::getId, addressId)
                .set(Address::getQrImageId, qrImageId)
                .set(Address::getUpdateTime, LocalDateTime.now()));
    }

    /**
     * 验证TRON地址格式
     * TRC20地址以T开头,长度34位
     */
    private boolean isValidTronAddress(String address) {
        if (StrUtil.isBlank(address)) {
            return false;
        }
        return address.startsWith("T") && address.length() == 34;
    }

    /**
     * 更新Redis地址集合缓存
     */
    private void updateRedisAddressSet(List<Address> addresses) {
        if (addresses == null || addresses.isEmpty()) {
            return;
        }

        String[] addressArray = addresses.stream()
                .map(Address::getAddress)
                .toArray(String[]::new);
        
        redisTemplate.opsForSet().add(ADDRESS_SET_KEY, addressArray);
    }
}
