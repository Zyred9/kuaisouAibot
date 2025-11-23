package com.search.robots.database.controller;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.search.robots.beans.view.base.Result;
import com.search.robots.beans.web.user.UserEditVO;
import com.search.robots.database.entity.User;
import com.search.robots.database.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * 用户管理控制器
 * <pre>
 * 主要功能:
 * 1. 用户分页查询
 * 2. 用户等级与余额编辑
 * </pre>
 *
 * @author admin
 * @since 2025/11/21
 */
@RestController
@RequestMapping("/user")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;

    /**
     * 用户分页查询
     * <pre>
     * 支持根据用户ID、用户名进行搜索。
     * </pre>
     *
     * @param current 当前页码，默认 1
     * @param size    每页条数，默认 10
     * @param userId  用户ID，可选，精准匹配
     * @param username 用户名，可选，模糊匹配
     * @return 分页结果
     */
    @GetMapping("/page")
    public Result<Page<User>> page(@RequestParam(defaultValue = "1") Integer current,
                                   @RequestParam(defaultValue = "10") Integer size,
                                   @RequestParam(required = false) Long userId,
                                   @RequestParam(required = false) String username) {
        Page<User> page = this.userService.page(
                Page.of(current, size),
                Wrappers.<User>lambdaQuery()
                        .eq(Objects.nonNull(userId), User::getUserId, userId)
                        .like(StrUtil.isNotBlank(username), User::getUsername, username)
                        .orderByDesc(User::getRegisterTime)
        );
        return Result.success(page);
    }

    /**
     * 编辑用户信息
     * <pre>
     * 仅支持编辑 grade、balance 字段。
     * </pre>
     *
     * @param request 包含 userId、grade、balance 的 VO 对象
     * @return 编辑结果
     */
    @PostMapping("/edit")
    public Result<Void> edit(@RequestBody UserEditVO request) {
        if (Objects.isNull(request.getUserId())) {
            return Result.error("用户ID不能为空");
        }
        User user = this.userService.getById(request.getUserId());
        if (Objects.isNull(user)) {
            return Result.error("用户不存在");
        }
        if (Objects.nonNull(request.getGrade())) {
            user.setGrade(request.getGrade());
        }
        if (Objects.nonNull(request.getBalance())) {
            user.setBalance(request.getBalance());
        }
        this.userService.updateById(user);
        return Result.success();
    }
}
