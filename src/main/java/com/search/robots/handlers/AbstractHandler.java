package com.search.robots.handlers;

import cn.hutool.json.JSONUtil;
import com.search.robots.config.Constants;
import lombok.extern.slf4j.Slf4j;
import org.telegram.telegrambots.meta.api.methods.AnswerCallbackQuery;
import org.telegram.telegrambots.meta.api.methods.ParseMode;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.methods.send.SendAnimation;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.methods.send.SendPhoto;
import org.telegram.telegrambots.meta.api.methods.send.SendVideo;
import org.telegram.telegrambots.meta.api.methods.updatingmessages.*;
import org.telegram.telegrambots.meta.api.objects.CallbackQuery;
import org.telegram.telegrambots.meta.api.objects.InputFile;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.media.InputMediaPhoto;
import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard;

import java.io.File;
import java.util.ArrayList;
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
@SuppressWarnings("all")
@Slf4j
public abstract class AbstractHandler {

    private static final List<AbstractHandler> HANDLERS = new ArrayList<>(128);

    protected AbstractHandler() {
        HANDLERS.add(this);
    }

    private static List<AbstractHandler> getHandlers () {
        return HANDLERS;
    }


    public static BotApiMethod<?> doExecute(Update update, boolean logs) {
        for (AbstractHandler handler : getHandlers()) {
            if (handler.support(update)) {
                BotApiMethod<?> result = handler.execute(update);
                if (Objects.nonNull(result) && logs) {
                    log.info("处理器：{}，\n处理消息内容：{}\n响应结果：{} \n\n",
                            handler.getClass().getSimpleName(),
                            JSONUtil.toJsonStr(update),
                            JSONUtil.toJsonStr(result)
                    );
                }
                return result;
            }
        }
        return null;
    }

    public abstract boolean support(Update update);

    protected abstract BotApiMethod<?> execute(Update update);


    protected SendMessage ok(Message message) {
        return ok(message, Constants.SUCCESS);
    }

    protected SendMessage ok(Long chatId) {
        return ok(chatId, Constants.SUCCESS);
    }

    protected SendMessage ok(Message message, String text) {
        return this.ok(message, text, null);
    }

    protected SendMessage ok(Long chatId, String text) {
        return this.ok(chatId, text, null);
    }

    protected SendMessage ok(Message message, String text, ReplyKeyboard markup) {
        return ok(message.getChatId(), text, markup);
    }

    protected SendMessage ok(Long chatId, String text, ReplyKeyboard markup) {
        return SendMessage.builder()
                .chatId(chatId)
                .disableWebPagePreview(true)
                .replyMarkup(markup)
                .text(text)
                .build();
    }

    protected SendMessage reply(Message message) {
        return this.reply(message, Constants.SUCCESS);
    }

    protected SendMessage reply(Message message, String text) {
        return this.reply(message, text, null);
    }

    protected SendMessage reply(Message message, String text, InlineKeyboardMarkup markup) {
        return SendMessage.builder()
                .chatId(message.getChatId())
                .replyMarkup(markup)
                .disableWebPagePreview(true)
                .replyToMessageId(message.getMessageId())
                .text(text)
                .build();
    }

    protected SendMessage reply(Message message, String text, ReplyKeyboard markup) {
        return SendMessage.builder()
                .chatId(message.getChatId())
                .replyMarkup(markup)
                .disableWebPagePreview(true)
                .replyToMessageId(message.getMessageId())
                .text(text)
                .build();
    }


    protected SendMessage markdown(Message message, String text) {
        return markdown(message, text, null);
    }

    protected SendMessage markdown(Long chatId, String text) {
        return markdown(chatId, text, null);
    }

    protected SendMessage markdown(Message message, String text, InlineKeyboardMarkup markup) {
        return markdown(message.getChatId(), text, markup, ParseMode.MARKDOWN);
    }

    protected SendMessage markdown(Long chatId, String text, InlineKeyboardMarkup markup) {
        return markdown(chatId, text, markup, ParseMode.MARKDOWN);
    }

    protected SendMessage markdownV2(Message message, String text) {
        return markdownV2(message, text, null);
    }

    protected SendMessage markdownV2(Long chatId, String text) {
        return markdownV2(chatId, text, null);
    }

    protected SendMessage markdownV2(Message message, String text, InlineKeyboardMarkup markup) {
        return markdown(message.getChatId(), text, markup, ParseMode.MARKDOWNV2);
    }

    protected SendMessage markdownV2(Long chatId, String text, InlineKeyboardMarkup markup) {
        return markdown(chatId, text, markup, ParseMode.MARKDOWNV2);
    }

    protected SendMessage markdown(Long chatId, String text, InlineKeyboardMarkup markup, String parseModel) {
        return SendMessage.builder()
                .chatId(chatId)
                .replyMarkup(markup)
                .parseMode(parseModel)
                .disableWebPagePreview(true)
                .text(text)
                .build();
    }

    protected SendMessage markdownReply(Message message, String text) {
        return this.markdownReply(message, text, null);
    }

    protected SendMessage markdownReply(Message message, String text, ReplyKeyboard markup) {
        return SendMessage.builder()
                .replyToMessageId(message.getMessageId())
                .chatId(message.getChatId())
                .parseMode(ParseMode.MARKDOWN)
                .disableWebPagePreview(true)
                .replyMarkup(markup)
                .text(text)
                .build();
    }



    protected DeleteMessage delete(Message message) {
        return DeleteMessage.builder()
                .chatId(message.getChatId())
                .messageId(message.getMessageId())
                .build();
    }

    protected SendPhoto photoMarkdown(Message message, String fileId, String caption) {
        return photoMarkdown(message, fileId, caption, null, ParseMode.MARKDOWN);
    }

    protected SendPhoto photoMarkdownV2(Message message, String fileId, String caption) {
        return photoMarkdown(message, fileId, caption, null, ParseMode.MARKDOWNV2);
    }

    protected SendPhoto photoMarkdown(Message message, String fileId, String caption, InlineKeyboardMarkup markup) {
        return photoMarkdown(message, fileId, caption, markup, ParseMode.MARKDOWN);
    }

    protected SendPhoto photoMarkdownV2(Message message, String fileId, String caption, InlineKeyboardMarkup markup) {
        return photoMarkdown(message, fileId, caption, markup, ParseMode.MARKDOWNV2);
    }

    protected SendPhoto photoMarkdownV2(Long chatId, String fileId, String caption, InlineKeyboardMarkup markup) {
        return photoMarkdown(chatId, fileId, caption, markup, ParseMode.MARKDOWNV2);
    }

    protected SendPhoto photoMarkdown(Long chatId, String fileId, String caption, InlineKeyboardMarkup markup, String parseModel) {
        return SendPhoto.builder()
                .chatId(chatId)
                .photo(new InputFile(fileId))
                .parseMode(parseModel)
                .replyMarkup(markup)
                .caption(caption)
                .build();
    }

    protected SendPhoto photoMarkdown(Message message, String fileId, String caption, InlineKeyboardMarkup markup, String parseModel) {
        return SendPhoto.builder()
                .chatId(message.getChatId())
                .photo(new InputFile(fileId))
                .parseMode(parseModel)
                .replyMarkup(markup)
                .caption(caption)
                .build();
    }

    protected SendPhoto photoMarkdown(Message message, File file, String caption, InlineKeyboardMarkup markup) {
        return SendPhoto.builder()
                .chatId(message.getChatId())
                .photo(new InputFile(file))
                .parseMode(ParseMode.MARKDOWN)
                .replyMarkup(markup)
                .caption(caption)
                .build();
    }

    protected SendVideo video(Message message, String fileId, String caption, InlineKeyboardMarkup markup) {
        return SendVideo.builder()
                .chatId(message.getChatId())
                .video(new InputFile(fileId))
                .parseMode(ParseMode.MARKDOWN)
                .replyMarkup(markup)
                .caption(caption)
                .build();
    }

    protected EditMessageText editText(Message message, String text) {
        return EditMessageText.builder()
                .chatId(message.getChatId())
                .disableWebPagePreview(true)
                .messageId(message.getMessageId())
                .text(text)
                .build();
    }

    protected EditMessageText editText(Message message, String text, InlineKeyboardMarkup markup) {
        return EditMessageText.builder()
                .chatId(message.getChatId())
                .disableWebPagePreview(true)
                .messageId(message.getMessageId())
                .replyMarkup(markup)
                .text(text)
                .build();
    }

    protected EditMessageText editMarkdown(Message message, String text) {
        return editMarkdown(message, text, null);
    }

    protected EditMessageText editMarkdown(Message message, String text, InlineKeyboardMarkup markup) {
        return editMarkdown(message, text, markup, ParseMode.MARKDOWN);
    }

    protected EditMessageText editMarkdown(Message message, String text, InlineKeyboardMarkup markup, String parseMode) {
        return EditMessageText.builder()
                .chatId(message.getChatId())
                .disableWebPagePreview(true)
                .parseMode(parseMode)
                .messageId(message.getMessageId())
                .replyMarkup(markup)
                .text(text)
                .build();
    }

    protected EditMessageText editMarkdownV2(Message message, String text) {
        return editMarkdownV2(message, text, null);
    }

    protected EditMessageText editMarkdownV2(Message message, String text, InlineKeyboardMarkup markup) {
        return editMarkdown(message, text, markup, ParseMode.MARKDOWNV2);
    }

    protected EditMessageCaption editCaption(Message message, String caption) {
        return EditMessageCaption.builder()
                .messageId(message.getMessageId())
                .messageId(message.getMessageId())
                .chatId(message.getChatId())
                .caption(caption)
                .build();
    }

    protected EditMessageCaption editCaption(Message message, String caption, InlineKeyboardMarkup markup) {
        return EditMessageCaption.builder()
                .messageId(message.getMessageId())
                .chatId(message.getChatId())
                .replyMarkup(markup)
                .caption(caption)
                .build();
    }

    protected EditMessageCaption editCaptionMarkdown(Message message, String caption) {
        return this.editCaptionMarkdown(message, caption, null);
    }

    protected EditMessageCaption editCaptionMarkdown(Message message, String caption, InlineKeyboardMarkup markup) {
        return this.editCaptionMarkdown(message, caption, markup, ParseMode.MARKDOWN);
    }

    protected EditMessageCaption editCaptionMarkdownV2(Message message, String caption) {
        return this.editCaptionMarkdownV2(message, caption, null);
    }

    protected EditMessageCaption editCaptionMarkdownV2(Message message, String caption, InlineKeyboardMarkup markup) {
        return this.editCaptionMarkdown(message, caption, markup, ParseMode.MARKDOWNV2);
    }


    protected EditMessageCaption editCaptionMarkdown(Message message, String caption, InlineKeyboardMarkup markup, String parseModel) {
        return EditMessageCaption.builder()
                .messageId(message.getMessageId())
                .parseMode(parseModel)
                .chatId(message.getChatId())
                .replyMarkup(markup)
                .caption(caption)
                .build();
    }

    protected EditMessageMedia editMediaMarkdown(Message message, String fileId, InlineKeyboardMarkup markup) {
        return EditMessageMedia.builder()
                .messageId(message.getMessageId())
                .chatId(message.getChatId())
                .media(new InputMediaPhoto(fileId)) //  new InputFile(fileId))
                .replyMarkup(markup)
                .build();
    }

    protected EditMessageReplyMarkup editKeyboard(Message message, InlineKeyboardMarkup markup) {
        return EditMessageReplyMarkup.builder()
                .chatId(message.getChatId())
                .messageId(message.getMessageId())
                .replyMarkup(markup)
                .build();
    }

    protected AnswerCallbackQuery answerAlert (CallbackQuery callbackQuery, String text) {
        return AnswerCallbackQuery.builder()
                .callbackQueryId(callbackQuery.getId())
                .showAlert(true)
                .text(text)
                .build();
    }

    protected AnswerCallbackQuery answer (CallbackQuery callbackQuery, String text) {
        return AnswerCallbackQuery.builder()
                .callbackQueryId(callbackQuery.getId())
                .text(text)
                .build();
    }

    protected SendAnimation animation (Message message, String caption, String fileId) {
        return SendAnimation.builder()
                .caption(caption)
                .chatId(message.getChatId())
                .animation(new InputFile(fileId))
                .build();
    }

    protected SendAnimation animation (Message message, String caption, String fileId, InlineKeyboardMarkup markup) {
        return SendAnimation.builder()
                .caption(caption)
                .replyMarkup(markup)
                .chatId(message.getChatId())
                .animation(new InputFile(fileId))
                .build();
    }

    protected SendAnimation animationMarkdown (Message message, String caption, String fileId) {
        return SendAnimation.builder()
                .caption(caption)
                .chatId(message.getChatId())
                .parseMode(ParseMode.MARKDOWN)
                .animation(new InputFile(fileId))
                .build();
    }

    protected SendAnimation animationMarkdown (Message message, String caption, String fileId, InlineKeyboardMarkup markup) {
        return SendAnimation.builder()
                .caption(caption)
                .replyMarkup(markup)
                .chatId(message.getChatId())
                .parseMode(ParseMode.MARKDOWN)
                .animation(new InputFile(fileId))
                .build();
    }

    protected SendMessage fail (Message message) {
        return ok(message, Constants.FAILED);
    }
}
