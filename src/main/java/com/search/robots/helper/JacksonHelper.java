package com.search.robots.helper;


import cn.hutool.json.JSONUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.search.robots.config.SelfException;
import com.search.robots.database.entity.Included;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.stereotype.Component;

import java.util.Objects;

/**
 *
 *
 * @author admin
 * @since 2025/11/19 17:13
 */
@Slf4j
@Component
public class JacksonHelper {

    private static ObjectMapper objectMapper;

    public JacksonHelper (ObjectMapper objectMapper) {
        JacksonHelper.objectMapper = objectMapper;
    }



    public static String toJson (Object obj) {
        try {
            return objectMapper.writeValueAsString(obj);
        } catch (JsonProcessingException e) {
            log.error("序列化失败: {}", JSONUtil.toJsonStr(obj), e);
            throw new SelfException("序列化失败");
        }
    }

    public static <T> T toBean (String json, Class<T> clazz) {
        try {
            return objectMapper.readValue(json, clazz);
        } catch (JsonProcessingException e) {
            log.error("反序列化失败: {}", json, e);
            throw new SelfException("反序列化失败");
        }
    }

}
