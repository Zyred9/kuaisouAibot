package com.search.robots.beans.cache;


import com.search.robots.beans.view.DialogueCtx;

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

}
