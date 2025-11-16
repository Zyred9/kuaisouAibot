package com.search.robots.database.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import com.search.robots.beans.view.trc20.TransferBean;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 *      地址收到的转账
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_listen_receive")
public class ListenReceive {

    private Integer id;

    private String address;

    private String transactionId;

    private Boolean pushed;

    public static ListenReceive buildPushAddress(String address, TransferBean transferBean) {
        return new ListenReceive()
                .setAddress(address)
                .setPushed(Boolean.TRUE)
                .setTransactionId(transferBean.getTransactionId());
    }
}
