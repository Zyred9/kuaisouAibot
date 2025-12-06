package com.search.robots.helper;

import com.search.robots.config.BotProperties;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Call;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Objects;

/**
 * <p>
 *     进群消息
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class CollectHelper {

    private final BotProperties properties;
    private final OkHttpClient okHttpClient;


    public void history (String url, String inviteLink) {
        log.info("[获取历史聊天记录] 开始 {}， {}", url, inviteLink);
        String fullUrl = this.properties.getHistoryUrl()
                + "?link=" + url
                + "&inviteLink=" + inviteLink
                + "&count=10000";

        Call call = this.okHttpClient.newCall(
                new Request.Builder()
                        .get()
                        .url(fullUrl)
                        .build()
        );
        try (Response execute = call.execute()) {
            if (execute.isSuccessful() && Objects.nonNull(execute.body())) {
                String body = execute.body().string();
                log.info("[获取历史聊天记录] 结果 {}， {}, 结果：{}", url, inviteLink, body);
            } else {
                log.info("[获取历史聊天记录] 失败 {}， {}，错误码：{}", url, inviteLink, execute.code());
            }
        } catch (IOException e) {
            log.error("[获取历史聊天记录] 异常 {}， {}", url, inviteLink, e);
        }
    }
}
