package com.search.robots.database.controller;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.view.base.Result;
import com.search.robots.database.entity.RewardRecord;
import com.search.robots.database.enums.RewardTypeEnum;
import com.search.robots.database.service.RewardRecordService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * 奖励记录管理控制器
 * <pre>
 * 主要功能:
 * 1. 奖励记录分页查询
 * 支持根据用户ID、用户名、奖励类型进行筛选
 * 只读，不支持编辑和删除
 * </pre>
 *
 * @author zyred
 * @since 2025/11/25
 */
@RestController
@RequestMapping("/reward")
@RequiredArgsConstructor
public class RewardRecordController {

    private final RewardRecordService rewardRecordService;

    /**
     * 奖励记录分页查询
     * <pre>
     * 支持根据用户ID、用户名、奖励类型进行搜索。
     * 按创建时间倒序排列。
     * </pre>
     *
     * @param current 当前页码，默认 1
     * @param size    每页条数，默认 10
     * @param userId  用户ID，可选，精准匹配
     * @param username 用户名，可选，模糊匹配
     * @param rewardType 奖励类型，可选，精准匹配（1-直接拉新，2-下级拉新，3-私聊，4-群搜索，5-广告代理，6-充值加赠）
     * @return 分页结果
     */
    @GetMapping("/page")
    public Result<Page<RewardRecord>> page(@RequestParam(defaultValue = "1") Integer current,
                                            @RequestParam(defaultValue = "10") Integer size,
                                            @RequestParam(required = false) Long userId,
                                            @RequestParam(required = false) String username,
                                            @RequestParam(required = false) Integer rewardType) {
        // 构建查询条件
        Page<RewardRecord> page = this.rewardRecordService.page(
                Page.of(current, size),
                Wrappers.<RewardRecord>lambdaQuery()
                        .eq(Objects.nonNull(userId), RewardRecord::getUserId, userId)
                        .like(StrUtil.isNotBlank(username), RewardRecord::getUsername, username)
                        .eq(Objects.nonNull(rewardType), RewardRecord::getRewardType, 
                            Objects.nonNull(rewardType) ? RewardTypeEnum.of(rewardType) : null)
                        .orderByDesc(RewardRecord::getCreateTime)
        );
        return Result.success(page);
    }
}
