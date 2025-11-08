package com.search.robots.database.entity;


import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.time.LocalDate;

/**
 * 搜索结果展示统计
 *
 * @author zyred
 * @since 2025/11/4 20:31
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_exhibition")
public class Exhibition {

    @TableId(type = IdType.AUTO)
    private Long id;

    /** 群组id **/
    private Long chatId;
    /** 展示的时间，创建索引 **/
    private LocalDate showDay;
    /** 链接展示次数 **/
    private Integer linkCount;
    /** 内容展示次数 **/
    private Integer contentCount;


    public static Exhibition buildDefault (Long chatId, boolean isContent) {
        Exhibition exhibition = new Exhibition()
                .setChatId(chatId)
                .setShowDay(LocalDate.now());
        if (isContent) {
            exhibition.setContentCount(1);
        } else {
            exhibition.setLinkCount(1);
        }
        return exhibition;
    }
}
