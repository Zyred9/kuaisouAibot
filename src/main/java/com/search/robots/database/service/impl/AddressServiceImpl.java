package com.search.robots.database.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.config.BotProperties;
import com.search.robots.database.entity.Address;
import com.search.robots.database.mapper.AddressMapper;
import com.search.robots.database.service.AddressService;
import com.search.robots.sender.AsyncSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;

import java.util.*;

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

    private final BotProperties properties;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Address selectEmptyAddress(Long userId) {
        Address address = this.baseMapper.selectOne(
                Wrappers.<Address>lambdaQuery()
                        .isNull(Address::getUserId)
                        .orderByAsc(Address::getAddress)
                        .last(" limit 1")
        );

        Long remain = this.baseMapper.selectCount(
                Wrappers.<Address>lambdaQuery()
                        .isNull(Address::getUserId)
        );

        if (remain <= 10 && StrUtil.isBlank(this.properties.getNotifyChatId())) {
            AsyncSender.async(
                    SendMessage.builder()
                            .chatId(this.properties.getBackgroundGroupId())
                            .text("专属地址已不足10个，请尽快补充")
                            .build()
            );
        }

        if (Objects.nonNull(address) && Objects.nonNull(userId)) {
            address.setUserId(userId);
        }

        return address;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int batchImport(List<String> addresses) {
        if (addresses == null || addresses.isEmpty()) {
            log.warn("导入地址列表为空");
            return 0;
        }
        Set<String> uniqueAddresses = new HashSet<>(addresses);
        log.info("原始地址数量: {}, 去重后数量: {}", addresses.size(), uniqueAddresses.size());
        List<String> existingAddresses = this.list(Wrappers.<Address>lambdaQuery()
                .select(Address::getAddress)
                .in(Address::getAddress, uniqueAddresses))
                .stream()
                .map(Address::getAddress)
                .toList();
        existingAddresses.forEach(uniqueAddresses::remove);
        if (uniqueAddresses.isEmpty()) {
            log.warn("所有地址均已存在,无需导入");
            return 0;
        }

        List<Address> newAddresses = new ArrayList<>();
        for (String address : uniqueAddresses) {
            if (StrUtil.isBlank(address)) {
                continue;
            }
            newAddresses.add(new Address()
                    .setAddress(address.trim())
                    .setListenCount(0)
                    .setPrevTimestamp(0L));
        }
        if (newAddresses.isEmpty()) {
            log.warn("没有有效的地址可以导入");
            return 0;
        }
        this.saveBatch(newAddresses);
        log.info("成功导入 {} 个地址", newAddresses.size());
        return newAddresses.size();
    }
}
