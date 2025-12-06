package com.search.robots.config;

import com.search.robots.config.listener.AdvExpireListener;
import com.search.robots.database.service.AdvPriceService;
import com.search.robots.database.service.AdvUserService;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;

@Configuration
@RequiredArgsConstructor
public class RedisExpirationConfig {

    private final RedisConnectionFactory connectionFactory;

    @Bean
    public RedisMessageListenerContainer redisMessageListenerContainer() {
        RedisMessageListenerContainer container = new RedisMessageListenerContainer();
        container.setConnectionFactory(connectionFactory);
        return container;
    }

    @Bean
    public AdvExpireListener advExpireListener(RedisMessageListenerContainer container,
                                               AdvUserService advUserService, AdvPriceService advPriceService) {
        try {
            RedisConnection conn = connectionFactory.getConnection();
            conn.setConfig("notify-keyspace-events", "Ex");
            conn.close();
        } catch (Exception ignored) {}
        return new com.search.robots.config.listener.AdvExpireListener(container, advUserService, advPriceService);
    }
}

