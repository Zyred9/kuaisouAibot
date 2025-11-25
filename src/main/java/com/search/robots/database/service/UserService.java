package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.User;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface UserService extends IService<User> {

    User user(org.telegram.telegrambots.meta.api.objects.User from);

    User select(Long userId);

    void update(User user);

    User selectByInviteCode(String inviteCode);

    String getSpreadStatement(Long userId);

    Page<User> selectChildAdsUsers(Long userId, int current);
}
