package com.search.robots.handlers;

import com.search.robots.config.BotProperties;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.objects.PhotoSize;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.message.Message;

import javax.annotation.Resource;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Component
public class NotifyHandler extends AbstractHandler {

    @Resource private BotProperties properties;

    @Override
    public boolean support(Update update) {
        return update.hasMessage()
                && (update.getMessage().hasPhoto() || update.getMessage().hasVideo())
                && (update.getMessage().getChat().isGroupChat() || update.getMessage().getChat().isSuperGroupChat())
                && Objects.equals(this.properties.getNotifyChatId(), update.getMessage().getChatId());
    }

    @Override
    protected BotApiMethod<?> execute(Update update) {
        Message message = update.getMessage();
        
        if (message.hasPhoto()) {
            List<PhotoSize> photo = message.getPhoto();
            PhotoSize max = photo.stream().max(Comparator.comparingInt(PhotoSize::getFileSize)).orElse(photo.get(0));
            return ok(message, max.getFileId());
        }

        if (message.hasVideo()) {
            return ok(message, message.getVideo().getFileId());
        }

        return null;
    }
}
