package com.search.robots.database.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.search.robots.database.entity.Keyword;

import java.util.List;

/**
 * 关键词服务接口
 * <pre>
 * 主要功能:
 * 1. 关键词查询服务
 * 2. 关键词管理服务
 * 3. 关键词状态控制
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
public interface KeywordService extends IService<Keyword> {

    /**
     * 根据关键词查询
     *
     * @param keyword 关键词
     * @return 关键词信息
     */
    Keyword queryByKeyword(String keyword);

    /**
     * 查询所有启用的关键词
     *
     * @return 启用的关键词列表
     */
    List<Keyword> listEnabled();

    void updateStatus(Long id, Boolean status);
}
