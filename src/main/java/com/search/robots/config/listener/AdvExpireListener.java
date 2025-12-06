package com.search.robots.config.listener;

import com.search.robots.database.entity.AdvPrice;
import com.search.robots.database.entity.AdvUser;
import com.search.robots.database.enums.adv.AdvStatus;
import com.search.robots.database.service.AdvPriceService;
import com.search.robots.database.service.AdvUserService;
import org.springframework.data.redis.listener.KeyExpirationEventMessageListener;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;

//@Component
public class AdvExpireListener extends KeyExpirationEventMessageListener {

    private final AdvUserService advUserService;
    private final AdvPriceService advPriceService;

    public AdvExpireListener(RedisMessageListenerContainer listenerContainer,
                             AdvUserService advUserService, AdvPriceService advPriceService) {
        super(listenerContainer);
        this.advPriceService = advPriceService;
        this.advUserService = advUserService;
    }

    @Override
    public void onMessage(org.springframework.data.redis.connection.Message message, byte[] pattern) {
        String key = message.toString();
        if (key == null || !key.startsWith(AdvUser.ADV_EXPIRE_KEY_PREFIX)) {
            return;
        }
        String idStr = key.substring(AdvUser.ADV_EXPIRE_KEY_PREFIX.length());
        try {
            Long advUserId = Long.parseLong(idStr);
            AdvUser update = new AdvUser();
            update.setId(advUserId);
            update.setAdvStatus(AdvStatus.THE_END);
            advUserService.updateById(update);

            AdvUser db = advUserService.getById(advUserId);
            if (db != null && db.getPriceId() != null) {
                AdvPrice priceUpdate = new AdvPrice();
                priceUpdate.setId(db.getPriceId());
                priceUpdate.setIsSold(Boolean.FALSE);
                advPriceService.updateById(priceUpdate);
            }
        } catch (Exception ignored) {}
    }
}
