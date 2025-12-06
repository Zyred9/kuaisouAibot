package com.search.robots.beans.view;

import cn.hutool.core.util.StrUtil;
import com.search.robots.helper.StrHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Setter
@Getter
@Accessors(chain = true)
public class BotTransfer {

    private Long botId;
    private String botName;
    private String botToken;
    private String backgroundId;

    private String mysqlPassword;
    private String mysqlDatabase;
    private String addr;

    public String buildText () {
        String text = """
                *主键*：`{}`
                *名字*：@{}
                *令牌*: `{}`
                *地址*：`{}`
                """;
        return StrUtil.format(text, this.botId, StrHelper.specialChar(this.botName), this.botToken, this.addr);
    }
}
