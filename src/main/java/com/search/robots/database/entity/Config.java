package com.search.robots.database.entity;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.search.robots.beans.view.ButtonTransfer;
import com.search.robots.beans.view.KeyboardTransfer;
import com.search.robots.beans.view.vo.AdvButton;
import com.search.robots.config.Constants;
import com.search.robots.helper.StrHelper;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
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
@Setter
@Getter
@Accessors(chain = true)
@TableName("t_config")
public class Config {

    @TableId(type = IdType.INPUT)
    private Long chatId;

    /** 启动日志 **/
    private String startMessage;
    /** 最低提现金额 **/
    private BigDecimal withdrawalThreshold;
    /** 邀请的活动描述 **/
    private String inviteActivityText;
    /** 推广的图片文件id **/
    private String inviteImageId;
    /** 收录指南消息地址 **/
    private String tutorialUrl;
    /** 交流群用户名 **/
    private String communityName;
    /** 添加定向搜索提示内容 **/
    private String addTargetedSearch;
    /** 进去群组/频道发送的内容 **/
    private String joinSendMessage;
    /** 群组曝光数据按钮配置 **/
    private String chatExposureKeyboard;
    /** 广告优惠百分比 **/
    private Integer preferentialRate;

    // 参考推广文案配置
    /** 参考推广文案图片文件id **/
    private String helpfulPopularizeFileId;
    /** 参考推广文案文案内容 **/
    private String helpfulPopularizeMarkdown;
    /** 参考推广文案按钮JSON **/
    private String helpfulPopularizeKeyboard;

    // 提交记录按钮内容配置
    /** 提交记录文本 **/
    private String commitRecordMarkdown;
    /** 提交记录文本按钮JSON **/
    private String commitRecordKeyboard;

    // 广告文案配置
    /** 广告文案文本(支持Markdown) **/
    private String advertisingMarkdown;
    /** 广告文案按钮JSON **/
    private String advertisingKeyboard;


    /** 顶部链接套餐的展示自定义 **/
    private String topLinkPackage;
    /** 底部按钮套餐的展示自定义 **/
    private String bottomButtonPackage;

    // 品牌专页配置
    /** 品牌专业回复内容 **/
    private String brandPageText;
    /** 关键词专页 **/
    private String keywordPageText;
    /** 关键词排行 **/
    private String keywordRankText;

    // 充值配置
    /** 充值地址：用户进行USDT等充值时跳转的网页地址或目标地址，支持http/https或协议规定的地址格式。 **/
    private String rechargeAddress;
    /** 充值二维码图片ID：用于展示充值收款二维码的Telegram图片file_id。若为空则不展示图片，仅发送文本。 **/
    private String rechargeQrImageId;
    /** 充值提示文本：点击充值按钮后展示的Markdown文案，支持包含充值说明、到账时间、注意事项等。 **/
    private String rechargeTipMarkdown;

    /** 客服 **/
    private String customUsername;

    public static Config buildDefault(Long chatId) {
        return new Config()
                .setChatId(chatId)
                .setInviteActivityText("""
                        🔥拉新奖励限时翻倍！每拉一位新用户，您将获得0.2U奖励~
                        (活动截止2025-11-16 00:00:00)
                        """)
                .setCommunityName("kuaisouqz")
                .setTutorialUrl("https://t.me/kuaisoupd/64")
                .setStartMessage(Constants.START_MESSAGE_TEXT)
                .setAddTargetedSearch(Constants.ADD_TARGETED_SEARCH_TEXT)
                .setJoinSendMessage(Constants.JOIN_SEND_MESSAGE_TEXT)
                .setWithdrawalThreshold(BigDecimal.TEN)
                .setPreferentialRate(5);
    }

    public String buildInviteText(String botUsername, String inviteCode) {
        String activity = "";
        if (StrUtil.isNotBlank(this.inviteActivityText)) {
            StringBuilder lines = new StringBuilder().append("\n\n");
            String[] split = this.inviteActivityText.split("\n");
            for (int i = 0; i < split.length; i++) {
                String sp = split[i];
                lines.append("**>").append(StrHelper.specialResult(sp));
                if (i < split.length - 1) {
                    lines.append("\n");
                }
            }
            activity = lines.toString();
        }

        String normalCode = "a\\_" + inviteCode; // 专属链接
        String advCode = "ad\\_" + inviteCode;    // 广告链接

        return StrUtil.format(Constants.INVITATION_PRE_TEXT, activity,
                botUsername, normalCode, botUsername, advCode);
    }


    public List<AdvButton> parseAdvButton(boolean top) {
        String buttonJson = top ? topLinkPackage : bottomButtonPackage;
        String tag = top ? "top_link" : "bottom_button";

        if (StrUtil.isEmpty(buttonJson)) {
            return null;
        }

        List<AdvButton> buttons = new ArrayList<>();
        KeyboardTransfer transfer = JSONUtil.toBean(buttonJson, KeyboardTransfer.class);

        List<List<ButtonTransfer>> keyboard = transfer.getKeyboard();
        for (List<ButtonTransfer> transfers : keyboard) {
            ButtonTransfer buttonTransfer = transfers.get(0);

            if (StrUtil.isNotBlank(buttonTransfer.getCallback_data())) {
                String callbackData = buttonTransfer.getCallback_data();
                if (StrUtil.contains(callbackData, tag)) {

                    List<String> split = StrUtil.split(callbackData, "#");
                    String last = split.get(split.size() - 1);


                    int[] ints = StrHelper.parseCallbackNumbers(last);

                    if (Objects.nonNull(ints)) {
                        AdvButton button = new AdvButton();
                        button.setAmount(BigDecimal.valueOf(ints[0]));
                        button.setShowNumber((long) ints[1]);
                        button.setName(buttonTransfer.getText());

                        buttons.add(button);
                    }
                }
            }
        }
        return buttons;
    }
}

