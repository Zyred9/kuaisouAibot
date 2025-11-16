package com.search.robots.beans.view.caffeine;


import com.search.robots.database.enums.caffeine.TaskNode;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 *
 *
 * @author zyred
 * @since 2025/11/15 18:08
 */
@Setter
@Getter
@Accessors(chain = true)
public class Task {

    /** 任务类型 **/
    private TaskNode node;
    /*** 充值地址 **/
    private String address;


    public static Task buildRecharge (String address) {
        return new Task()
                .setNode(TaskNode.RECHARGE)
                .setAddress(address);
    }
}
