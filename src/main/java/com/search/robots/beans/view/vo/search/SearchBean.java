package com.search.robots.beans.view.vo.search;


import cn.hutool.core.util.StrUtil;
import com.search.robots.database.enums.content.SourceTypeEnum;
import com.search.robots.helper.StrHelper;
import com.search.robots.helper.TimeHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.util.List;
import java.util.Objects;

/**
 * Elasticsearch 搜索文档类
 * <p>
 * 主要功能:
 * <ul>
 *   <li>支持中文分词的全文搜索(ik_max/ik_smart)</li>
 *   <li>按内容类型(视频/图片/频道/群组等)分类检索</li>
 *   <li>按时间降序展示最新资源</li>
 *   <li>支持频道/群组信息关联查询</li>
 * </ul>
 *
 * @author zyred
 * @since 2025/11/9 22:02
 */
@Setter
@Getter
@Accessors(chain = true)
@Document(indexName = "search_index")
public class SearchBean {

    @Id
    private String id;

    @Field(type = FieldType.Keyword)
    private SourceTypeEnum type;

    @Field(type = FieldType.Text, analyzer = "ik_smart")
    private String sourceName;

    @Field(type = FieldType.Keyword)
    private String sourceUrl;

    @Field(type = FieldType.Keyword)
    private String channelName;

    @Field(type = FieldType.Keyword)
    private String channelUsername;

    @Field(type = FieldType.Keyword)
    private String channelUrl;

    @Field(type = FieldType.Keyword)
    private String subscribers;

    private Long chatId;

    private Long messageId;

    @Field(type = FieldType.Long)
    private Long collectTime;

    @Field(type = FieldType.Integer)
    private Integer times;

    private Integer views;

    @Field(type = FieldType.Keyword)
    private List<String> tags;

    private Boolean marked;

    public String buildLineText() {
        StringBuilder sb = new StringBuilder(type.getIcon());
        if (Objects.equals(this.type, SourceTypeEnum.VIDEO)
                || Objects.equals(this.type, SourceTypeEnum.AUDIO)) {
            sb.append("\\[").append(StrHelper.formatSecondsToTime(this.times)).append("\\]");
        }
        if (Objects.equals(this.type, SourceTypeEnum.TEXT)) {
            sb.append(TimeHelper.formatV2_(this.collectTime));
        }
        sb.append(" [")
                .append(StrHelper.specialResult(this.sourceName))
                .append("](")
                .append(this.sourceUrl).append(")");
        if (Boolean.TRUE.equals(this.marked)) {
            sb.append("\uD83D\uDD1E");
        }

        if (Objects.equals(type, SourceTypeEnum.CHANNEL)
                || Objects.equals(type, SourceTypeEnum.GROUP)) {
            if (StrUtil.isNotBlank(this.subscribers)) {
                sb.append(" ").append(StrHelper.specialResult(this.subscribers));
            }
        }
        sb.append("\n");
        return sb.toString();
    }
}
