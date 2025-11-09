package com.search.robots.handlers;


import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.AnswerCallbackQuery;
import org.telegram.telegrambots.meta.api.methods.botapimethods.BotApiMethod;
import org.telegram.telegrambots.meta.api.methods.send.SendAnimation;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.methods.send.SendPhoto;
import org.telegram.telegrambots.meta.api.methods.send.SendVideo;
import org.telegram.telegrambots.meta.api.methods.updatingmessages.*;
import org.telegram.telegrambots.meta.api.objects.CallbackQuery;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.message.Message;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.ReplyKeyboard;

/**
 *
 *
 * @author zyred
 * @since 2025/11/9 17:04
 */
@Component
public class EmptyHandler extends AbstractHandler{
    @Override
    public boolean support(Update update) {
        return false;
    }

    @Override
    public BotApiMethod<?> execute(Update update) {
        return null;
    }

    @Override
    public SendMessage ok(Message message) {
        return super.ok(message);
    }

    @Override
    public SendMessage ok(Long chatId) {
        return super.ok(chatId);
    }

    @Override
    public SendMessage ok(Message message, String text) {
        return super.ok(message, text);
    }

    @Override
    public SendMessage ok(Long chatId, String text) {
        return super.ok(chatId, text);
    }

    @Override
    public SendMessage ok(Message message, String text, ReplyKeyboard markup) {
        return super.ok(message, text, markup);
    }

    @Override
    public SendMessage ok(Long chatId, String text, ReplyKeyboard markup) {
        return super.ok(chatId, text, markup);
    }

    @Override
    public SendMessage reply(Message message) {
        return super.reply(message);
    }

    @Override
    public SendMessage reply(Message message, String text) {
        return super.reply(message, text);
    }

    @Override
    public SendMessage reply(Message message, String text, InlineKeyboardMarkup markup) {
        return super.reply(message, text, markup);
    }

    @Override
    public SendMessage reply(Message message, String text, ReplyKeyboard markup) {
        return super.reply(message, text, markup);
    }

    @Override
    public SendMessage markdown(Message message, String text) {
        return super.markdown(message, text);
    }

    @Override
    public SendMessage markdown(Long chatId, String text) {
        return super.markdown(chatId, text);
    }

    @Override
    public SendMessage markdown(Message message, String text, InlineKeyboardMarkup markup) {
        return super.markdown(message, text, markup);
    }

    @Override
    public SendMessage markdown(Long chatId, String text, InlineKeyboardMarkup markup) {
        return super.markdown(chatId, text, markup);
    }

    @Override
    public SendMessage markdownV2(Message message, String text) {
        return super.markdownV2(message, text);
    }

    @Override
    public SendMessage markdownV2(Long chatId, String text) {
        return super.markdownV2(chatId, text);
    }

    @Override
    public SendMessage markdownV2(Message message, String text, InlineKeyboardMarkup markup) {
        return super.markdownV2(message, text, markup);
    }

    @Override
    public SendMessage markdownV2(Long chatId, String text, InlineKeyboardMarkup markup) {
        return super.markdownV2(chatId, text, markup);
    }

    @Override
    public SendMessage markdown(Long chatId, String text, InlineKeyboardMarkup markup, String parseModel) {
        return super.markdown(chatId, text, markup, parseModel);
    }

    @Override
    public SendMessage markdownReply(Message message, String text) {
        return super.markdownReply(message, text);
    }

    @Override
    public SendMessage markdownReply(Message message, String text, ReplyKeyboard markup) {
        return super.markdownReply(message, text, markup);
    }

    @Override
    public DeleteMessage delete(Message message) {
        return super.delete(message);
    }

    @Override
    public SendPhoto photoMarkdown(Message message, String fileId, String caption) {
        return super.photoMarkdown(message, fileId, caption);
    }

    @Override
    public SendPhoto photoMarkdownV2(Message message, String fileId, String caption) {
        return super.photoMarkdownV2(message, fileId, caption);
    }

    @Override
    public SendPhoto photoMarkdown(Message message, String fileId, String caption, InlineKeyboardMarkup markup) {
        return super.photoMarkdown(message, fileId, caption, markup);
    }

    @Override
    public SendPhoto photoMarkdownV2(Message message, String fileId, String caption, InlineKeyboardMarkup markup) {
        return super.photoMarkdownV2(message, fileId, caption, markup);
    }

    @Override
    public SendPhoto photoMarkdown(Message message, String fileId, String caption, InlineKeyboardMarkup markup, String parseModel) {
        return super.photoMarkdown(message, fileId, caption, markup, parseModel);
    }

    @Override
    public SendVideo video(Message message, String fileId, String caption, InlineKeyboardMarkup markup) {
        return super.video(message, fileId, caption, markup);
    }

    @Override
    public EditMessageText editText(Message message, String text) {
        return super.editText(message, text);
    }

    @Override
    public EditMessageText editText(Message message, String text, InlineKeyboardMarkup markup) {
        return super.editText(message, text, markup);
    }

    @Override
    public EditMessageText editMarkdown(Message message, String text) {
        return super.editMarkdown(message, text);
    }

    @Override
    public EditMessageText editMarkdown(Message message, String text, InlineKeyboardMarkup markup) {
        return super.editMarkdown(message, text, markup);
    }

    @Override
    public EditMessageText editMarkdown(Message message, String text, InlineKeyboardMarkup markup, String parseMode) {
        return super.editMarkdown(message, text, markup, parseMode);
    }

    @Override
    public EditMessageText editMarkdownV2(Message message, String text) {
        return super.editMarkdownV2(message, text);
    }

    @Override
    public EditMessageText editMarkdownV2(Message message, String text, InlineKeyboardMarkup markup) {
        return super.editMarkdownV2(message, text, markup);
    }

    @Override
    public EditMessageCaption editCaption(Message message, String caption) {
        return super.editCaption(message, caption);
    }

    @Override
    public EditMessageCaption editCaption(Message message, String caption, InlineKeyboardMarkup markup) {
        return super.editCaption(message, caption, markup);
    }

    @Override
    public EditMessageCaption editCaptionMarkdown(Message message, String caption) {
        return super.editCaptionMarkdown(message, caption);
    }

    @Override
    public EditMessageCaption editCaptionMarkdown(Message message, String caption, InlineKeyboardMarkup markup) {
        return super.editCaptionMarkdown(message, caption, markup);
    }

    @Override
    public EditMessageCaption editCaptionMarkdownV2(Message message, String caption) {
        return super.editCaptionMarkdownV2(message, caption);
    }

    @Override
    public EditMessageCaption editCaptionMarkdownV2(Message message, String caption, InlineKeyboardMarkup markup) {
        return super.editCaptionMarkdownV2(message, caption, markup);
    }

    @Override
    public EditMessageCaption editCaptionMarkdown(Message message, String caption, InlineKeyboardMarkup markup, String parseModel) {
        return super.editCaptionMarkdown(message, caption, markup, parseModel);
    }

    @Override
    public EditMessageMedia editMediaMarkdown(Message message, String fileId, InlineKeyboardMarkup markup) {
        return super.editMediaMarkdown(message, fileId, markup);
    }

    @Override
    public EditMessageReplyMarkup editKeyboard(Message message, InlineKeyboardMarkup markup) {
        return super.editKeyboard(message, markup);
    }

    @Override
    public AnswerCallbackQuery answerAlert(CallbackQuery callbackQuery, String text) {
        return super.answerAlert(callbackQuery, text);
    }

    @Override
    public AnswerCallbackQuery answer(CallbackQuery callbackQuery, String text) {
        return super.answer(callbackQuery, text);
    }

    @Override
    public SendAnimation animation(Message message, String caption, String fileId) {
        return super.animation(message, caption, fileId);
    }

    @Override
    public SendAnimation animation(Message message, String caption, String fileId, InlineKeyboardMarkup markup) {
        return super.animation(message, caption, fileId, markup);
    }

    @Override
    public SendAnimation animationMarkdown(Message message, String caption, String fileId) {
        return super.animationMarkdown(message, caption, fileId);
    }

    @Override
    public SendAnimation animationMarkdown(Message message, String caption, String fileId, InlineKeyboardMarkup markup) {
        return super.animationMarkdown(message, caption, fileId, markup);
    }

    @Override
    public SendMessage fail(Message message) {
        return super.fail(message);
    }
}
