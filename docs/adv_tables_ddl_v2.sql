-- ===================================================================
-- 关键词广告业务数据库表设计 V2.0
-- 作者: zyred
-- 版本: 2.0 (重构版本 - AdvPrice独立表)
-- 日期: 2025-11-08
-- 兼容: MySQL 5.7 - 8.x
-- 变更: 将价格配置从JSON字段重构为独立表 t_adv_price
-- ===================================================================

-- -------------------------------------------------------------------
-- 表1: t_adv_library (广告库表) - 已移除 price_list 字段
-- 功能: 存储关键词的基础信息和展现统计
-- -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS t_adv_library (
    id BIGINT AUTO_INCREMENT PRIMARY KEY COMMENT '主键ID',
    adv_type INT NOT NULL COMMENT '广告类型(1-顶部链接,2-底部按钮,3-关键词排行,4-关键词专页,5-品牌专页)',
    keyword VARCHAR(255) NOT NULL COMMENT '关键词',
    price DECIMAL(10,2) DEFAULT 0.00 COMMENT '基础价格(最低价)',
    show_count BIGINT DEFAULT 0 COMMENT '总展现次数',
    create_time DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    show_7d JSON COMMENT '近7日展现轨迹(JSON数组)',
    
    INDEX idx_keyword (keyword),
    INDEX idx_adv_type (adv_type),
    INDEX idx_show_count (show_count)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='广告库表';

-- -------------------------------------------------------------------
-- 表2: t_adv_price (广告价格表) - 新增独立表
-- 功能: 存储不同位置、来源的价格配置
-- 特点: 
--   - 支持价格版本管理
--   - 支持启用/停用状态
--   - 唯一索引保证同一位置不重复
-- -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS t_adv_price (
    id BIGINT AUTO_INCREMENT PRIMARY KEY COMMENT '主键ID',
    library_id BIGINT NOT NULL COMMENT '关联广告库ID',
    adv_position INT NOT NULL COMMENT '广告位置枚举(1-10榜单,101直接专页,102关联专页)',
    source VARCHAR(50) COMMENT '来源类型(direct/related)',
    ranking INT COMMENT '具体排名(1-10)',
    monthly_price DECIMAL(10,2) NOT NULL COMMENT '月费价格',
    currency VARCHAR(10) DEFAULT 'CNY' COMMENT '货币单位',
    version INT DEFAULT 1 COMMENT '价格版本',
    status TINYINT(1) DEFAULT 1 COMMENT '状态(1-启用,0-停用)',
    is_sold TINYINT(1) DEFAULT 0 COMMENT '是否已售出(0-未售出,1-已售出)',
    remark VARCHAR(500) COMMENT '备注说明',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    
    INDEX idx_library_id (library_id),
    INDEX idx_position (adv_position),
    INDEX idx_status (status),
    INDEX idx_is_sold (is_sold),
    UNIQUE KEY uk_library_position (library_id, adv_position, source, ranking)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='广告价格表';

-- -------------------------------------------------------------------
-- 表3: t_user_adv (用户广告购买记录表) - 已调整快照策略
-- 功能: 记录用户购买的广告,通过 price_id 关联价格表
-- 变更:
--   - 新增 price_id 字段关联 t_adv_price
--   - 移除 adv_price_snapshot JSON字段
--   - 保留其他冗余字段和展现快照
-- -------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS t_user_adv (
    id BIGINT AUTO_INCREMENT PRIMARY KEY COMMENT '主键ID',
    user_id BIGINT NOT NULL COMMENT '用户ID',
    library_id BIGINT COMMENT '关联广告库ID',
    price_id BIGINT COMMENT '关联价格ID',
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
    adv_show_snapshot JSON COMMENT '购买时展现轨迹快照',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    
    INDEX idx_user_id (user_id),
    INDEX idx_library_id (library_id),
    INDEX idx_price_id (price_id),
    INDEX idx_keyword (keyword),
    INDEX idx_status_time (adv_status, effective_time, expiration_time),
    INDEX idx_bill_no (bill_no)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='用户广告购买记录表';

-- -------------------------------------------------------------------
-- 示例数据: 广告库
-- -------------------------------------------------------------------
INSERT INTO t_adv_library (adv_type, keyword, price, show_count, show_7d) 
VALUES (
    3, 
    'AI机器人', 
    299.00, 
    152847,
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
-- 示例数据: 价格配置
-- -------------------------------------------------------------------
INSERT INTO t_adv_price (library_id, adv_position, source, ranking, monthly_price, currency, version, status, is_sold, remark) 
VALUES 
    (1, 1, 'direct', 1, 999.00, 'CNY', 1, 1, 0, '榜单第1位-直接搜索'),
    (1, 2, 'direct', 2, 699.00, 'CNY', 1, 1, 0, '榜单第2位-直接搜索'),
    (1, 3, 'direct', 3, 499.00, 'CNY', 1, 1, 0, '榜单第3位-直接搜索'),
    (1, 101, 'direct', NULL, 1299.00, 'CNY', 1, 1, 0, '关键词直接搜索专页'),
    (1, 102, 'related', NULL, 899.00, 'CNY', 1, 1, 0, '关键词关联搜索专页');

-- -------------------------------------------------------------------
-- 示例数据: 用户购买记录
-- -------------------------------------------------------------------
INSERT INTO t_user_adv (
    user_id, library_id, price_id, keyword, adv_type, adv_position, ranking, 
    source, price_month, currency, show_count_snapshot, adv_status, 
    effective_time, expiration_time, adv_source, bill_no, auto_renew, 
    adv_content, adv_url, show_count, adv_show_snapshot
) VALUES (
    10001, 
    1, 
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
-- 数据迁移脚本 (从 V1.0 升级到 V2.0)
-- -------------------------------------------------------------------
-- 步骤1: 创建新表 t_adv_price
-- (已在上方定义)

-- 步骤2: 从 t_adv_library.price_list JSON 迁移数据到 t_adv_price
-- 注意: 需要根据实际 JSON 结构编写迁移脚本
-- 示例迁移逻辑:
/*
INSERT INTO t_adv_price (library_id, adv_position, source, ranking, monthly_price, currency, version, status, remark)
SELECT 
    al.id AS library_id,
    JSON_EXTRACT(price_item, '$.position') AS adv_position,
    JSON_UNQUOTE(JSON_EXTRACT(price_item, '$.source')) AS source,
    JSON_EXTRACT(price_item, '$.rank') AS ranking,
    JSON_EXTRACT(price_item, '$.monthlyPrice') AS monthly_price,
    JSON_UNQUOTE(JSON_EXTRACT(price_item, '$.currency')) AS currency,
    COALESCE(JSON_EXTRACT(price_item, '$.version'), 1) AS version,
    1 AS status,
    JSON_UNQUOTE(JSON_EXTRACT(price_item, '$.remark')) AS remark
FROM t_adv_library al
CROSS JOIN JSON_TABLE(
    al.price_list,
    '$[*]' COLUMNS(
        price_item JSON PATH '$'
    )
) AS jt
WHERE al.price_list IS NOT NULL;
*/

-- 步骤3: 为 t_user_adv 添加 price_id 字段
-- ALTER TABLE t_user_adv ADD COLUMN price_id BIGINT COMMENT '关联价格ID' AFTER library_id;
-- ALTER TABLE t_user_adv ADD INDEX idx_price_id (price_id);

-- 步骤4: 更新 t_user_adv.price_id 关联到 t_adv_price.id
/*
UPDATE t_user_adv ua
INNER JOIN t_adv_price ap 
    ON ua.library_id = ap.library_id 
    AND ua.adv_position = ap.adv_position
    AND (ua.source = ap.source OR (ua.source IS NULL AND ap.source IS NULL))
    AND (ua.ranking = ap.ranking OR (ua.ranking IS NULL AND ap.ranking IS NULL))
SET ua.price_id = ap.id
WHERE ua.price_id IS NULL;
*/

-- 步骤5: 删除旧字段
-- ALTER TABLE t_adv_library DROP COLUMN price_list;
-- ALTER TABLE t_user_adv DROP COLUMN adv_price_snapshot;

-- -------------------------------------------------------------------
-- 查询验证
-- -------------------------------------------------------------------
-- 查询广告库
SELECT id, keyword, adv_type, price, show_count FROM t_adv_library;

-- 查询价格配置
SELECT id, library_id, adv_position, source, ranking, monthly_price, currency, status, remark 
FROM t_adv_price 
ORDER BY library_id, adv_position;

-- 查询用户购买记录(关联价格表)
SELECT 
    ua.id, ua.user_id, ua.keyword, ua.adv_position, ua.ranking,
    ua.price_month, ua.currency, ua.price_id,
    ap.monthly_price AS current_price, ap.remark AS price_remark
FROM t_user_adv ua
LEFT JOIN t_adv_price ap ON ua.price_id = ap.id
ORDER BY ua.created_at DESC;

-- -------------------------------------------------------------------
-- 性能优化建议
-- -------------------------------------------------------------------
/*
1. 价格查询缓存:
   - 使用 Redis 缓存热门关键词的价格列表
   - key: adv:price:library:{library_id}
   - TTL: 1小时

2. 批量查询优化:
   - 使用 IN 查询批量获取多个关键词的价格
   - SELECT * FROM t_adv_price WHERE library_id IN (1,2,3) AND status = 1

3. 索引优化:
   - 确保 uk_library_position 唯一索引生效
   - 定期分析慢查询并优化
*/

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

价格状态 (status):
  1 - 启用
  0 - 停用

是否已售出 (is_sold):
  1 - 已售出
  0 - 未售出

来源类型 (source):
  direct  - 直接搜索
  related - 关联搜索
*/
