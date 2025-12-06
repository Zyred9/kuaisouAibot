package com.search.robots.beans.web.broadcast;


import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 *
 *
 * @author zyred
 * @since 2025/11/24 18:05
 */
@Setter
@Getter
public class BroadcastRequest implements Serializable {
    /** 图片的id **/
    private String imageFileId;
    /** 视频的id **/
    private String videoFileId;
    /** markdown v2 语法的文案 **/
    private String markdownContent;
}
