package com.search.robots.beans.initializer;

import com.search.robots.beans.keywords.KeywordsHelper;
import com.search.robots.config.BotProperties;
import com.search.robots.database.entity.Included;
import com.search.robots.database.entity.Keyword;
import com.search.robots.database.service.ConfigService;
import com.search.robots.database.service.IncludedService;
import com.search.robots.database.service.KeywordService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.objects.User;

import javax.annotation.Resource;
import java.util.List;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class InitializerHandler {

    private final BotProperties properties;
    private final ConfigService configService;
    private final KeywordService keywordService;
    private final IncludedService includedService;


    public void init (User user) {
        this.properties.setBotUsername(user.getUserName());

        this.configService.queryConfig();

        List<Included> includedList = this.includedService.list();
        for (Included included : includedList) {
            included.updateEveryAdv();
        }

        List<Keyword> ks = this.keywordService.list();
        List<String> keys = ks.stream().map(Keyword::getKeyword).toList();
        KeywordsHelper.add(keys);

        for (Keyword keyword : ks) {
            KeywordsHelper.addKeywords(keyword.getKeyword(), keyword.getId());
        }
    }
}
