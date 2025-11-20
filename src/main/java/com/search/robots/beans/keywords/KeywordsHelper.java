package com.search.robots.beans.keywords;

import cn.hutool.core.util.StrUtil;
import toolgood.words.StringSearch;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public class KeywordsHelper {

    private static final Map<String, Long> MAP_SEARCH = new HashMap<>(128);
    private static final StringSearch SEARCH = new StringSearch();


    public static void add (List<String> kws) {
        SEARCH.SetKeywords(kws);
    }
    public static String illegal(String k) {
        if (StrUtil.isNotBlank(k)) {
            return SEARCH.FindFirst(k);
        }
        return null;
    }

    public static void addKeywords (String key, Long keywordId) {
        MAP_SEARCH.put(key, keywordId);
    }

    public static Long getKeywordId(String key) {
        String illegal = KeywordsHelper.illegal(key);
        if (StrUtil.isNotBlank(illegal)) {
            return MAP_SEARCH.get(key);
        }
        return null;
    }

    public static void remove (String key) {
        MAP_SEARCH.remove(key);
    }

}
