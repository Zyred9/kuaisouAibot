package com.search.robots.beans.view.vo;


import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;



/**
 * 广告展现统计VO
 * <pre>
 * 记录每日的展现数据:
 * - directShow: 直接搜索展现次数
 * - relatedShow: 关联搜索展现次数
 * - uniqueUser: 独立用户数
 * </pre>
 *
 * @author zyred
 * @since 1.0
 */
@Getter
@Setter
@Accessors(chain = true)
public class AdvShow {

    /** 统计日期 **/
    private String date;
    
    /** 直接搜索展现次数 **/
    private Long directShow;
    
    /** 关联搜索展现次数 **/
    private Long relatedShow;
    
    /** 独立访客数 **/
    private Long uniqueUser;
    
    /** 总展现次数(计算字段) **/
    public Long getTotalShow() {
        return (directShow == null ? 0L : directShow) + (relatedShow == null ? 0L : relatedShow);
    }

}
