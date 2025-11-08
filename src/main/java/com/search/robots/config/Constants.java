package com.search.robots.config;

/**
 * <p>
 * 常量池
 * </p>
 *
 * @author admin
 * @since v 0.0.1
 */
public interface Constants {

    String TOKEN_KEY = "BOT_TOKEN";

    String SUCCESS = "✅操作成功";

    String FAILED = "❌操作失败";

    String START_GROUP_URL = "https://t.me/{}?startgroup=true";

    String START_MESSAGE_TEXT = """
            🔍我是个资源搜索引擎，向我发送关键词，帮你找到有趣的群组、频道、视频、音乐、电影、新闻。[简体中文包](https://t.me/setlanguage/zh-hans-beta)
            
            🔍Hey there, Explorer! I’m your personal discovery engine. Just send me a keyword, and I’ll help you uncover awesome groups, channels, videos, music, movies, and the latest news.
            """;

    String SELF_MESSAGE_TEXT = """
            昵称：{}
            🆔：`{}`
            您的广告等级：`{}`
            
            💰余额：{} $
            💰待入账金额：{} $
            💰总提现金额：{} $
            💰总奖励金额：{} $
            """;

    String ADV_CENTER_TEXT = """
            📈广告中心
            
            昵称：{}
            🆔：`{}`
            
            💰余额：{} $
            💰总充值金额：{} $
            💰总推广金额：{} $
            """;

    String MARKDOWN_V2_TEXT = """
            *bold \\*text*
            _italic \\*text_
            __underline__
            ~strikethrough~
            ||spoiler||
            *bold _italic bold ~italic bold strikethrough ||italic bold strikethrough spoiler||~ __underline italic bold___ bold*
            [inline URL](http://www.example.com/)
            [inline mention of a user](tg://user?id=123456789)
            ![👍](tg://emoji?id=5368324170671202286)
            `inline fixed-width code`
            ```
            pre-formatted fixed-width code block
            ```
            ```python
            pre-formatted fixed-width code block written in the Python programming language
            ```
            >Block quotation started
            >Block quotation continued
            >Block quotation continued
            >Block quotation continued
            >The last line of the block quotation
            **>The expandable block quotation started right after the previous block quotation
            >It is separated from the previous block quotation by an empty bold entity
            >Expandable block quotation continued
            >Hidden by default part of the expandable block quotation started
            >Expandable block quotation continued
            >The last line of the expandable block quotation with the expandability mark||
            """;

    String SELF_WALLET_TEXT = """
            **>总览
            昵称：{}
            🆔：`{}`
            
            💰余额：{} $
            💰总提现金额：{} $
            💰TRC20地址：{}
            
            *__最低提现金额：10\\$，单次手续费：1\\$__*
            *__如需提现或更新TRC20地址，请点击下方按钮。__*
            
            **>动账记录\\(近20笔\\)
            {}
            """;

    String SELF_BILL_LINE_TEXT = "`{}`\\|{}\\|`{}`\\|{}";

    String UPDATE_ADDR_TEXT = """
            您的TRC20地址更新成功, ||{}||
            """;

    String INVITATION_PRE_TEXT = """
            邀请好友使用快搜，您就能持续从好友的搜索中获得收益。{}
            
            单击复制你的专属分享链接：
            `🔍快搜：Telegram必备的中文搜索引擎，帮你轻松找到有趣的群组/频道、视频、音乐👉 t\\.me/{}?start={}`
            
            单击复制你的专属广告代理链接：
            `🔍快搜：Telegram必备的中文搜索引擎，想让更多人看到你的频道/商品？快来投放 快搜广告！👉 t\\.me/{}?start={}`
            """;

    String TOTAL_GROUP_CHANNEL_TEXT = """
            公开频道：{}个，已收录：{}个
            私密频道：{}个，已收录：{}个
            公开群组：{}个，已收录：{}个，搜索群：{}个
            私密群组：{}个，已收录：{}个，搜索群：{}个
            
            [收录指南]({})，如有更多疑问，前往交流群获取帮助 @{}
            """;

    String INDEX_DETAIL_TEXT = """
            ID：`{}`
            类型：{}
            标题：{}
            昵称：https\\://t\\.me/{}
            人数：{}
            权重：{}
            色情限制：{}
            创建时间：`{}`
            提交时间：`{}`
            聊天记录：{}
            每日内容曝光上限：{}
            曝光加权：{}%
            已记录资源数：{}
            全局搜索资源数：{}
            收录状态：{}
            
            ▪️[将我设为管理员]({})可以收录历史广播、监听新广播
            ▪️[收录指南]({})，如有更多疑问，前往交流群获取帮助 @{}
            """;

    String PRIVACY_INDEX_DETAIL_TEXT = """
            ID：`{}`
            类型：{}
            标题：{}
            人数：{}
            权重：{}
            色情限制：{}
            提交时间：`{}`
            聊天记录：{}
            每日内容曝光上限：{}
            曝光加权：{}%
            已记录资源数：{}
            全局搜索资源数：{}
            收录状态：{}
            
            ▪️[将我设为管理员]({})可以收录历史广播、监听新广播
            ▪️[收录指南]({})，如有更多疑问，前往交流群获取帮助 @{}
            """;

    String ADD_TARGETED_SEARCH_TEXT = """
            请回复目标频道/群组，使用逗号分隔，支持填写用户名或tgId。
            例如：`kuaisoupd,kuaisouqz`
            例如：`-1002661172192,-1002527772704`
            """;

    String JOIN_SEND_MESSAGE_TEXT = """
            我是快搜@kuai中文搜索机器人！感谢邀请我加入频道 🎉
            
            机器人支持采集最新消息，历史消息将在收录后进行抓取
            
            机器人： @kuai @kuaia @kuaiaa
            频道： @kuaisoupd
            群组 ： @kuaisouqz
            """;

    String QUERY_EXPOSURE_DATA_TEXT = """
            {}
            **> 展现详情
            {}
            """;

        String SELF_PROMOTION_REPORT_TEXT = """
            **> 总览
            当前广告代理等级：`{}`
            今日待入账奖励：`{}$`
            总共待入账奖励：`{}$`
            累计入账奖励：`{}`$
            **> 下级详情
            一级下级数：{}
            二级下级数：{}
            广告下级数：{}
            **> 直接拉新奖励\\(近3天\\)
            {}
            **> 下级拉新奖励\\(近3天\\)
            {}
            **> 下级私聊奖励\\(近3天\\)
            {}
            **> 群搜索奖励\\(近3天\\)
            2025\\-11\\-05 预估奖励：0$ 状态：待入账⏳
                [BACKGROUND](https://t.me/ajsakasdk) \\-  0 次有效群搜索
                隐私群组 \\-  0 次有效群搜索
            
            **> 广告代理奖励\\(近3天\\)
            {}
            """;

    String USER_ADV_NEXT_INVITE_TEXT = """
            下级广告优惠码绑定成功 【[](https://t.me/{})】 【[](https://t.me/{})】 【[](https://t.me/{})】
            """;

    String MY_LEADER_TEXT = """
            广告优惠码绑定成功，享受充值加赠{}%~ 【[您的广告上级](t.me/{})】
            """;

    String AUDIT_GROUP_END_TEXT = """
            昵称：DevelopBotAny668
            
            ==群组/频道审批通知==
            资源详情
            标题：隐私群组
            类型：群组
            人数：3
            链接：待更新
            权重：1
            记录时间：2025-11-03T22:14:46
            状态：审批拒绝❌其他原因
            
            ❌群组/频道审批未通过。收录指南 (https://t.me/kuaisoupd/64)
            如您需要了解更多详情，请随时联系客服获取帮助。 @kuaisouqz
            """;

    // 充值后发送给用户的
    String RECHARGE_USER_MESSAGE_TEXT = """
            🆔：7653000728
            昵称：DevelopBotAny668
            当前余额：11.0150$
            
            ==金额动账通知==
            交易号: 20251102143902
            申请时间：11-02 14:39
            类型：充值
            金额：11.0000$
            """;

    String NEXT_DETAIL_TEXT = """
            🎯 您的广告等级：`{}`
            👥 已邀请广告下级人数：{}
            🔗 邀请链接：https://t\\.me/{}?start\\=ad\\_{}
            
            {}
            """;

    String NEXT_DEFAULT_USER_TEXT = """
            {}\\. 👤[{}\t{}](tg\\://user?id\\={})
            总充值：{}，注册时间：`{}`
            """;

    String TO_BUY_TEXT = """
            📣*支付确认*
            
            *类型*：{}
            *展现次数*：`{}万次`
            *支付金额*：`{}$`
            """;

    String SELF_ADV_TEXT = """
            已购买关键词排行广告：`{}`个，推广中：`{}`个，已完成：`{}`个
            已购买关键词专页广告：`{}`个，推广中：`{}`个，已完成：`{}`个
            已购买品牌专页广告：`{}`个，推广中：`{}`个，已完成：`{}`个
            已购买顶部链接广告：`{}`个，推广中：`{}`个，已完成：`{}`个
            已购买底部按钮广告：`{}`个，推广中：`{}`个，已完成：`{}`个
            
            [广告指南]({})，如有更多疑问，前往交流群获取帮助 @{}
            """;

    String KEYWORD_QUERY_TEXT = """
            🔑 关键词：{}
            
            近期 直接搜索次数 与 关联搜索次数 与 搜索人数：
            {}
            
            购买后预计展示：
            每天展示：   {} 次
            一个月展示： {} 次
            
            💡关联搜索也会展现广告，例如：购买了“数据”这个关键词，如果“海外数据”这一关键词广告无人购买，那么用户搜索“海外数据”也会展现出“数据”的关键词的广告。
            
            💡关键词专页：当用户搜索相关内容时，将再次发送您的专属推广页，精准引流，效果立竿见影！效果参考：[点我查看专页效果](https://t.me/{}?start=query_5b-r5pCc)
            """;

    String KEYWORD_QUERY_OF_DAY_TEXT = "{}：直接：`{}`， 关联：`{}` ， 人数：`{}`";

    String TO_BUY_ADV_TEXT = """
            📣*支付确认*
            
            *类型*：{}
            *排名*：`{}`
            *支付金额*：`{}$`
            """;
}


