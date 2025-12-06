package com.search.robots.beans.cache;


import com.search.robots.beans.view.DialogueCtx;
import com.search.robots.config.Constants;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <p>
 *
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public class CommonCache {

    private static final Map<Long, DialogueCtx> CACHE = new ConcurrentHashMap<>();

    public static void putDialogue(Long userId, DialogueCtx dialogue) {
        CACHE.put(userId, dialogue);
    }
    public static boolean hasDialogue(Long userId) {
        return CACHE.containsKey(userId);
    }
    public static DialogueCtx getDialogue(Long userId) {
        return CACHE.get(userId);
    }
    public static void removeDialogue(Long userId) {
        CACHE.remove(userId);
    }




    private static final Map<Long, String> CACHE_DATA = new ConcurrentHashMap<>();
    public static void putData(Long userId, String data) {CACHE_DATA.put(userId, data);}
    public static String getData(Long userId) {
        return CACHE_DATA.remove(userId);
    }

    private static final Map<String, Long> USER_MAP = new ConcurrentHashMap<>(128);
    static {
        USER_MAP.put(Constants.VAL_1, 7874756166L);
        USER_MAP.put(Constants.VAL_2, 7653000728L);
    }
    public static Map<String, Long> getUser () {
        return USER_MAP;
    }

}
