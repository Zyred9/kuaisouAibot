package com.search.robots.handlers;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.view.AsyncBean;
import com.search.robots.database.service.AdvUserService;
import com.search.robots.database.service.HotSearchService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;

/**
 *
 *
 * @author zyred
 * @since 2025/11/13 15:44
 */
@Slf4j
@Component
public class AsyncTaskHandler extends Thread {

    private static final LinkedBlockingQueue<AsyncBean> ASYNC_QUEUE = new LinkedBlockingQueue<>();

    @Resource private AdvUserService advUserService;
    @Resource private HotSearchService hotSearchService;

    public static void async(AsyncBean ab) {
        if (Objects.isNull(ab)) {
            return;
        }
        ASYNC_QUEUE.add(ab);
    }


    public AsyncTaskHandler() {
        this.start();
    }


    @Override
    public void run() {
        while (!Thread.interrupted()) {
            try {
                AsyncBean bean = ASYNC_QUEUE.take();

                // 热搜词统计
                if (StrUtil.isNotBlank(bean.getSearchKeyword())){
                    this.hotSearchService.search(bean.getSearchKeyword());
                    continue;
                }

                // 记录直接搜索展现次数
                if (CollUtil.isNotEmpty(bean.getDirectIds())) {
                    this.advUserService.incr(bean.getDirectIds(), true);
                    continue;
                }
                // 记录直接搜索展现次数
                if (CollUtil.isNotEmpty(bean.getRelatedIds())) {
                    this.advUserService.incr(bean.getRelatedIds(), false);
                    continue;
                }

            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
