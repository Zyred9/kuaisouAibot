-- ===================================================================
-- 关键词广告业务数据库表设计
-- 作者: zyred
-- 版本: 1.0
-- 日期: 2025-11-08
-- 兼容: MySQL 5.7 - 8.x
-- ===================================================================

-- -------------------------------------------------------------------
-- 表1: t_adv_library (广告库表)
-- 功能: 存储关键词的基础信息、价格梯度和展现统计
-- 特点: 
--   - priceList: 不同位置/来源的价格梯度(JSON)
--   - show7d: 近7日展现轨迹(JSON)
-- -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS t_adv_library (
    id BIGINT AUTO_INCREMENT PRIMARY KEY COMMENT '主键ID',
    adv_type INT NOT NULL COMMENT '广告类型(1-顶部链接,2-底部按钮,3-关键词排行,4-关键词专页,5-品牌专页)',
    keyword VARCHAR(255) NOT NULL COMMENT '关键词',
    price DECIMAL(10,2) DEFAULT 0.00 COMMENT '基础价格(最低价)',
    show_count BIGINT DEFAULT 0 COMMENT '总展现次数',
    create_time DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    price_list JSON COMMENT '价格梯度列表(JSON数组)',
    show_7d JSON COMMENT '近7日展现轨迹(JSON数组)',
    
    INDEX idx_keyword (keyword),
    INDEX idx_adv_type (adv_type),
    INDEX idx_show_count (show_count)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='广告库表';

-- -------------------------------------------------------------------
-- 表2: t_user_adv (用户广告购买记录表)
-- 功能: 记录用户购买的广告,采用冗余设计避免JOIN查询
-- 特点:
--   - 冗余关键词、价格、展现等快照数据
--   - 支持自动续费和账单关联
--   - JSON字段记录购买时的完整快照
-- -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS t_user_adv (
    id BIGINT AUTO_INCREMENT PRIMARY KEY COMMENT '主键ID',
    user_id BIGINT NOT NULL COMMENT '用户ID',
    library_id BIGINT COMMENT '关联广告库ID',
    keyword VARCHAR(255) COMMENT '关键词快照',
    adv_type INT COMMENT '广告类型快照',
    adv_position INT COMMENT '广告位置枚举(1-10榜单,101直接专页,102关联专页)',
    ranking INT COMMENT '具体排名(1-10)',
    source VARCHAR(50) COMMENT '来源类型(direct/related)',
    price_month DECIMAL(10,2) COMMENT '月费价格快照',
    currency VARCHAR(10) DEFAULT 'CNY' COMMENT '货币单位',
    show_count_snapshot BIGINT COMMENT '购买时展现量快照',
    adv_status INT DEFAULT 0 COMMENT '广告状态(0-审批中,1-未开始,2-推广中,3-暂停中,4-已结束)',
    effective_time DATETIME COMMENT '生效时间',
    expiration_time DATETIME COMMENT '失效时间',
    adv_source INT DEFAULT 0 COMMENT '购买来源',
    bill_no VARCHAR(100) COMMENT '账单号',
    auto_renew TINYINT(1) DEFAULT 0 COMMENT '是否自动续费',
    adv_content TEXT COMMENT '广告文本',
    adv_url VARCHAR(500) COMMENT '广告链接',
    show_count BIGINT DEFAULT 0 COMMENT '当前实时展现次数',
    adv_show JSON COMMENT '广告7天的展示(实时更新)',
    adv_price_snapshot JSON COMMENT '购买时价格条目快照',
    adv_show_snapshot JSON COMMENT '购买时展现轨迹快照',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    
    INDEX idx_user_id (user_id),
    INDEX idx_library_id (library_id),
    INDEX idx_keyword (keyword),
    INDEX idx_status_time (adv_status, effective_time, expiration_time),
    INDEX idx_bill_no (bill_no)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='用户广告购买记录表';

-- -------------------------------------------------------------------
-- 示例数据: 广告库
-- -------------------------------------------------------------------
INSERT INTO t_adv_library (adv_type, keyword, price, show_count, price_list, show_7d) 
VALUES (
    3, 
    'AI机器人', 
    299.00, 
    152847,
    JSON_ARRAY(
        JSON_OBJECT('position', 1, 'source', 'direct', 'rank', 1, 'monthlyPrice', 999.00, 'currency', 'CNY', 'version', 1, 'remark', '榜单第1位-直接搜索'),
        JSON_OBJECT('position', 2, 'source', 'direct', 'rank', 2, 'monthlyPrice', 699.00, 'currency', 'CNY', 'version', 1, 'remark', '榜单第2位-直接搜索'),
        JSON_OBJECT('position', 3, 'source', 'direct', 'rank', 3, 'monthlyPrice', 499.00, 'currency', 'CNY', 'version', 1, 'remark', '榜单第3位-直接搜索'),
        JSON_OBJECT('position', 101, 'source', 'direct', 'rank', null, 'monthlyPrice', 1299.00, 'currency', 'CNY', 'version', 1, 'remark', '关键词直接搜索专页'),
        JSON_OBJECT('position', 102, 'source', 'related', 'rank', null, 'monthlyPrice', 899.00, 'currency', 'CNY', 'version', 1, 'remark', '关键词关联搜索专页')
    ),
    JSON_ARRAY(
        JSON_OBJECT('date', '2025-11-02', 'directShow', 21245, 'relatedShow', 8763, 'uniqueUser', 18234),
        JSON_OBJECT('date', '2025-11-03', 'directShow', 22341, 'relatedShow', 9124, 'uniqueUser', 19456),
        JSON_OBJECT('date', '2025-11-04', 'directShow', 23456, 'relatedShow', 9876, 'uniqueUser', 20123),
        JSON_OBJECT('date', '2025-11-05', 'directShow', 21987, 'relatedShow', 8945, 'uniqueUser', 18765),
        JSON_OBJECT('date', '2025-11-06', 'directShow', 24123, 'relatedShow', 10234, 'uniqueUser', 21345),
        JSON_OBJECT('date', '2025-11-07', 'directShow', 22678, 'relatedShow', 9456, 'uniqueUser', 19876),
        JSON_OBJECT('date', '2025-11-08', 'directShow', 23234, 'relatedShow', 9789, 'uniqueUser', 20345)
    )
);

-- -------------------------------------------------------------------
-- 示例数据: 用户购买记录
-- -------------------------------------------------------------------
INSERT INTO t_user_adv (
    user_id, library_id, keyword, adv_type, adv_position, ranking, 
    source, price_month, currency, show_count_snapshot, adv_status, 
    effective_time, expiration_time, adv_source, bill_no, auto_renew, 
    adv_content, adv_url, show_count, adv_price_snapshot, adv_show_snapshot
) VALUES (
    10001, 
    1, 
    'AI机器人', 
    3, 
    1, 
    1, 
    'direct', 
    999.00, 
    'CNY', 
    152847, 
    2, 
    '2025-11-08 00:00:00', 
    '2025-12-08 23:59:59', 
    0, 
    'BILL202511080001', 
    1, 
    '专业AI机器人解决方案', 
    'https://example.com/ai-robot', 
    12345,
    JSON_OBJECT('position', 1, 'source', 'direct', 'rank', 1, 'monthlyPrice', 999.00, 'currency', 'CNY', 'version', 1, 'remark', '榜单第1位-直接搜索'),
    JSON_ARRAY(
        JSON_OBJECT('date', '2025-11-02', 'directShow', 21245, 'relatedShow', 8763, 'uniqueUser', 18234),
        JSON_OBJECT('date', '2025-11-03', 'directShow', 22341, 'relatedShow', 9124, 'uniqueUser', 19456),
        JSON_OBJECT('date', '2025-11-04', 'directShow', 23456, 'relatedShow', 9876, 'uniqueUser', 20123),
        JSON_OBJECT('date', '2025-11-05', 'directShow', 21987, 'relatedShow', 8945, 'uniqueUser', 18765),
        JSON_OBJECT('date', '2025-11-06', 'directShow', 24123, 'relatedShow', 10234, 'uniqueUser', 21345),
        JSON_OBJECT('date', '2025-11-07', 'directShow', 22678, 'relatedShow', 9456, 'uniqueUser', 19876),
        JSON_OBJECT('date', '2025-11-08', 'directShow', 23234, 'relatedShow', 9789, 'uniqueUser', 20345)
    )
);

-- -------------------------------------------------------------------
-- 查询验证
-- -------------------------------------------------------------------
-- 查询广告库
SELECT 
    id, keyword, adv_type, price, show_count, 
    JSON_LENGTH(price_list) as price_count, 
    JSON_LENGTH(show_7d) as show_days 
FROM t_adv_library;

-- 查询用户购买记录
SELECT 
    id, user_id, keyword, adv_position, ranking, 
    price_month, currency, show_count_snapshot, 
    adv_status, bill_no, auto_renew 
FROM t_user_adv;

-- -------------------------------------------------------------------
-- 枚举说明
-- -------------------------------------------------------------------
/*
广告类型 (adv_type):
  1 - 顶部链接
  2 - 底部按钮
  3 - 关键词排行广告
  4 - 关键词专页广告
  5 - 品牌专页广告

广告位置 (adv_position):
  1-10   - 榜单第1-10位
  101    - 关键词直接搜索专页
  102    - 关键词关联搜索专页

广告状态 (adv_status):
  0 - 审批中
  1 - 未开始
  2 - 推广中
  3 - 暂停中
  4 - 已结束

来源类型 (source):
  direct  - 直接搜索
  related - 关联搜索
*/

-- -------------------------------------------------------------------
-- JSON字段结构说明
-- -------------------------------------------------------------------
/*
price_list 结构:
[
  {
    "position": 1,           // 位置枚举值
    "source": "direct",      // 来源类型
    "rank": 1,               // 排名(1-10,专页为null)
    "monthlyPrice": 999.00,  // 月费
    "currency": "CNY",       // 货币
    "version": 1,            // 版本号
    "remark": "说明"         // 备注
  }
]

show_7d / adv_show / adv_show_snapshot 结构:
[
  {
    "date": "2025-11-08",    // 日期
    "directShow": 23234,     // 直接搜索展现
    "relatedShow": 9789,     // 关联搜索展现
    "uniqueUser": 20345      // 独立访客
  }
]
*/
