package com.search.robots.database.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * 关键词实体类
 * <pre>
 * 主要功能:
 * 1. 关键词管理
 * 2. 关键词内容配置
 * 3. 关键词启用状态控制
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_keyword")
public class Keyword {

    /** 主键 **/
    @TableId(type = IdType.AUTO)
    private Long id;

    /** 关键词 **/
    private String keyword;

    /** 图片id **/
    private String imageId;

    /** 文本内容 **/
    private String contentText;

    /** 启用状态：0.禁用 1.启用 **/
    private Boolean status;
}
