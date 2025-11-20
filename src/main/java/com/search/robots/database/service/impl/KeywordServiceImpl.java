package com.search.robots.database.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.beans.keywords.KeywordsHelper;
import com.search.robots.database.entity.Keyword;
import com.search.robots.database.mapper.KeywordMapper;
import com.search.robots.database.service.KeywordService;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 关键词服务实现类
 * <pre>
 * 主要功能:
 * 1. 关键词查询实现
 * 2. 关键词管理实现
 * 3. 关键词状态控制实现
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
@Service
public class KeywordServiceImpl extends ServiceImpl<KeywordMapper, Keyword> implements KeywordService {

    @Override
    public Keyword queryByKeyword(String keyword) {
        if (StrUtil.isBlank(keyword)) {
            return null;
        }

        return this.baseMapper.selectOne(
                Wrappers.<Keyword>lambdaQuery()
                        .eq(Keyword::getKeyword, keyword)
                        .eq(Keyword::getStatus, true)
                        .last("LIMIT 1")
        );
    }

    @Override
    public List<Keyword> listEnabled() {
        return this.baseMapper.selectList(
                Wrappers.<Keyword>lambdaQuery()
                        .eq(Keyword::getStatus, true)
                        .orderByDesc(Keyword::getId)
        );
    }

    @Override
    public void updateStatus(Long id, Boolean status) {
        Keyword keyword = this.baseMapper.selectById(id);
        keyword.setStatus(status);
        this.baseMapper.updateById(keyword);

        if (Boolean.FALSE.equals(status)) {
            KeywordsHelper.remove(keyword.getKeyword());
        }
        List<Keyword> enableList = this.baseMapper.selectList(
                Wrappers.<Keyword>lambdaQuery()
                        .eq(Keyword::getStatus, Boolean.TRUE)
        );

        List<String> ks = enableList.stream()
                .map(Keyword::getKeyword).toList();
        KeywordsHelper.add(ks);

        for (Keyword k : enableList) {
            KeywordsHelper.addKeywords(k.getKeyword(), k.getId());
        }
    }
}
