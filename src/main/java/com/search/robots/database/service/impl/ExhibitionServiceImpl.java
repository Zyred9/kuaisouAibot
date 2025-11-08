package com.search.robots.database.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.search.robots.database.entity.Exhibition;
import com.search.robots.database.mapper.ExhibitionMapper;
import com.search.robots.database.service.ExhibitionService;
import com.search.robots.helper.StrHelper;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Service
public class ExhibitionServiceImpl extends ServiceImpl<ExhibitionMapper, Exhibition> implements ExhibitionService {

    @Override
    public void exhibition(Long chatId, boolean isContent) {
        Exhibition exhibition = this.baseMapper.selectOne(
                Wrappers.<Exhibition>lambdaQuery()
                        .eq(Exhibition::getChatId, chatId)
                        .eq(Exhibition::getShowDay, LocalDate.now())
        );
        if (Objects.isNull(exhibition)) {
            this.baseMapper.insert(Exhibition.buildDefault(chatId, isContent));
        } else {
            if (isContent) {
                exhibition.setContentCount(exhibition.getContentCount() + 1);
            } else {
                exhibition.setLinkCount(exhibition.getLinkCount() + 1);
            }
            this.baseMapper.updateById(exhibition);
        }
    }

    @Override
    public void customExhibition(Exhibition exhibition) {
        Exhibition query = this.baseMapper.selectById(exhibition.getId());
        if (Objects.isNull(query)) {
            return;
        }
        query.setContentCount(exhibition.getContentCount());
        query.setLinkCount(exhibition.getLinkCount());
        this.baseMapper.updateById(query);
    }

    @Override
    public String querySevenExhibition(List<LocalDate> localDates, long chatId) {
        List<Exhibition> exhibitions = this.baseMapper.selectList(
                Wrappers.<Exhibition>lambdaQuery()
                        .in(Exhibition::getShowDay, localDates)
                        .eq(Exhibition::getChatId, chatId)
                        .orderByDesc(Exhibition::getShowDay)
        );
        if (CollUtil.isEmpty(exhibitions)) {
            StringBuilder sb = new StringBuilder("\\=\\=\\=近7天链接展示\\=\\=\\=").append("\n");
            for (LocalDate localDate : localDates) {
                sb.append(StrHelper.specialResult(localDate.toString())).append("：0").append("\n");
            }
            sb.append("\\=\\=\\=近7天内容展示\\=\\=\\=").append("\n");
            for (LocalDate localDate : localDates) {
                sb.append(StrHelper.specialResult(localDate.toString())).append("：0").append("\n");
            }
            return sb.toString();
        }

        Map<LocalDate, Exhibition> dayOfEx = exhibitions.stream()
                .collect(Collectors.toMap(Exhibition::getShowDay, Function.identity()));

        StringBuilder sb = new StringBuilder("\\=\\=\\=近7天链接展示\\=\\=\\=").append("\n");
        for (LocalDate localDate : localDates) {
            Exhibition exhibition = dayOfEx.get(localDate);
            sb.append(StrHelper.specialResult(localDate.toString()));
            if (Objects.nonNull(exhibition)) {
                sb.append(exhibition.getLinkCount()).append("\n");
            } else {
                sb.append("：0").append("\n");
            }
        }
        sb.append("\\=\\=\\=近7天内容展示\\=\\=\\=").append("\n");
        for (LocalDate localDate : localDates) {
            Exhibition exhibition = dayOfEx.get(localDate);
            sb.append(StrHelper.specialResult(localDate.toString()));
            if (Objects.nonNull(exhibition)) {
                sb.append(exhibition.getContentCount()).append("\n");
            } else {
                sb.append("：0").append("\n");
            }
        }
        return sb.toString();
    }
}
