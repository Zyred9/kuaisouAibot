package com.search.robots.config;

import cn.hutool.core.util.StrUtil;
import com.search.robots.beans.view.base.Result;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Objects;

/**
 * 全局异常处理器
 * <pre>
 * 统一拦截Controller层抛出的异常并返回友好提示
 * 
 * 异常处理优先级：
 * 1. SelfException - 业务异常，返回自定义错误信息
 * 2. 参数校验异常 - 返回字段校验错误信息
 * 3. 其他所有异常 - 统一返回"系统错误"
 * 
 * 日志级别：所有异常使用 info 级别记录
 * </pre>
 *
 * @author admin
 * @since 1.0
 */
@Slf4j
@RestControllerAdvice(basePackages = "com.search.robots.database.controller")
public class GlobalExceptionHandler {

    /**
     * 处理自定义业务异常
     *
     * @param e SelfException
     * @return 包含异常信息的Result对象
     */
    @ExceptionHandler(SelfException.class)
    public Result<Void> handleSelfException(SelfException e) {
        log.info("业务异常: code={}, message={}", e.getCode(), e.getMessage());
        return Result.error(e.getCode(), e.getMessage());
    }

    /**
     * 处理参数绑定异常（@Validated 表单校验）
     *
     * @param e BindException
     * @return 包含字段校验错误信息的Result对象
     */
    @ExceptionHandler(BindException.class)
    public Result<Void> handleBindException(BindException e) {
        String errorMessage = extractValidationErrorMessage(e.getFieldError());
        log.info("参数校验异常: {}", errorMessage);
        return Result.error(400, errorMessage);
    }

    /**
     * 处理参数校验异常（@Validated @RequestBody 校验）
     *
     * @param e MethodArgumentNotValidException
     * @return 包含字段校验错误信息的Result对象
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public Result<Void> handleMethodArgumentNotValidException(MethodArgumentNotValidException e) {
        String errorMessage = extractValidationErrorMessage(e.getBindingResult().getFieldError());
        log.info("参数校验异常: {}", errorMessage);
        return Result.error(400, errorMessage);
    }

    /**
     * 处理运行时异常
     *
     * @param e RuntimeException
     * @return 统一返回"系统错误"
     */
    @ExceptionHandler(RuntimeException.class)
    public Result<Void> handleRuntimeException(RuntimeException e) {
        log.info("运行时异常: {}", e.getMessage(), e);
        return Result.error("系统错误");
    }

    /**
     * 处理所有未捕获的异常
     *
     * @param e Exception
     * @return 统一返回"系统错误"
     */
    @ExceptionHandler(Exception.class)
    public Result<Void> handleException(Exception e) {
        log.info("系统异常: {}", e.getMessage(), e);
        return Result.error("系统错误");
    }

    /**
     * 提取字段校验错误信息
     *
     * @param fieldError 字段错误对象
     * @return 格式化的错误信息
     */
    private String extractValidationErrorMessage(FieldError fieldError) {
        if (Objects.isNull(fieldError)) {
            return "参数校验失败";
        }
        
        String field = fieldError.getField();
        String message = fieldError.getDefaultMessage();
        
        if (StrUtil.isBlank(message)) {
            return StrUtil.format("字段 {} 校验失败", field);
        }
        
        return message;
    }
}
