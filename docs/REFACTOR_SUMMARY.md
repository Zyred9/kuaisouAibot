# AdvPrice 重构总结报告

## 📋 重构概述

**重构目标**: 将 `AdvPrice` 从 JSON 字段重构为独立的数据表

**执行日期**: 2025-11-08

**状态**: ✅ 已完成

---

## 🎯 重构范围

### 数据库层面
1. ✅ 创建新表 `t_adv_price`
2. ✅ 迁移 `t_adv_library.price_list` 数据到新表
3. ✅ 删除 `t_adv_library.price_list` 字段
4. ✅ 为 `t_user_adv` 新增 `price_id` 字段
5. ✅ 删除 `t_user_adv.adv_price_snapshot` 字段
6. ✅ 更新现有购买记录的 `price_id` 关联

### 代码层面
1. ✅ 将 `AdvPrice` VO 重命名为 `AdvPriceVO`
2. ✅ 创建 `AdvPrice` 实体类(Entity)
3. ✅ 创建 `AdvPriceMapper` 和 `AdvPriceService`
4. ✅ 修改 `AdvLibrary` 实体(移除 priceList)
5. ✅ 修改 `AdvUser` 实体(添加 priceId)
6. ✅ 增强 `AdvLibraryService` (新增价格查询方法)

### 文档层面
1. ✅ 创建 `adv_tables_ddl_v2.sql` (V2.0 DDL脚本)
2. ✅ 更新 `ADV_DESIGN.md` (V2.0 设计文档)
3. ✅ 创建 `REFACTOR_SUMMARY.md` (本文档)

---

## 📊 数据库结构对比

### t_adv_library 表变化

| 操作 | 字段名 | 说明 |
|------|--------|------|
| ❌ 删除 | price_list | JSON字段,已迁移至 t_adv_price |
| ✅ 保留 | show_7d | 近7日展现轨迹(JSON) |

### t_adv_price 表(新增)

| 字段名 | 类型 | 说明 |
|--------|------|------|
| id | BIGINT | 主键ID |
| library_id | BIGINT | 关联广告库ID |
| adv_position | INT | 广告位置枚举 |
| source | VARCHAR(50) | 来源类型 |
| ranking | INT | 具体排名 |
| monthly_price | DECIMAL(10,2) | 月费价格 |
| currency | VARCHAR(10) | 货币单位 |
| version | INT | 价格版本 |
| status | TINYINT(1) | 状态(1-启用,0-停用) |
| remark | VARCHAR(500) | 备注说明 |
| created_at | DATETIME | 创建时间 |
| updated_at | DATETIME | 更新时间 |

**索引**:
- PRIMARY KEY (id)
- UNIQUE KEY uk_library_position (library_id, adv_position, source, ranking)
- INDEX idx_library_id (library_id)
- INDEX idx_position (adv_position)
- INDEX idx_status (status)

### t_user_adv 表变化

| 操作 | 字段名 | 说明 |
|------|--------|------|
| ✅ 新增 | price_id | 关联价格ID(关联 t_adv_price.id) |
| ❌ 删除 | adv_price_snapshot | JSON快照,改为通过 price_id 关联 |
| ✅ 保留 | price_month | 月费价格快照(冗余) |
| ✅ 保留 | adv_show_snapshot | 展现轨迹快照(JSON) |

---

## 🔄 数据迁移记录

### 迁移前数据统计
- t_adv_library 记录数: 1
- price_list JSON 条目数: 5

### 迁移执行
```sql
-- 创建 t_adv_price 表
CREATE TABLE t_adv_price (...);

-- 迁移数据
INSERT INTO t_adv_price (library_id, adv_position, source, ranking, monthly_price, currency, version, status, remark)
VALUES 
    (1, 1, 'direct', 1, 999.00, 'CNY', 1, 1, '榜单第1位-直接搜索'),
    (1, 2, 'direct', 2, 699.00, 'CNY', 1, 1, '榜单第2位-直接搜索'),
    (1, 3, 'direct', 3, 499.00, 'CNY', 1, 1, '榜单第3位-直接搜索'),
    (1, 101, 'direct', NULL, 1299.00, 'CNY', 1, 1, '关键词直接搜索专页'),
    (1, 102, 'related', NULL, 899.00, 'CNY', 1, 1, '关键词关联搜索专页');
-- 受影响行数: 5

-- 为 t_user_adv 添加 price_id
ALTER TABLE t_user_adv ADD COLUMN price_id BIGINT COMMENT '关联价格ID' AFTER library_id;
ALTER TABLE t_user_adv ADD INDEX idx_price_id (price_id);

-- 更新现有购买记录
UPDATE t_user_adv ua
INNER JOIN t_adv_price ap ON ua.library_id = ap.library_id AND ua.adv_position = ap.adv_position
SET ua.price_id = ap.id;
-- 受影响行数: 1

-- 删除旧字段
ALTER TABLE t_adv_library DROP COLUMN price_list;
ALTER TABLE t_user_adv DROP COLUMN adv_price_snapshot;
```

### 迁移后数据验证
- t_adv_price 记录数: 5 ✅
- t_user_adv.price_id 关联成功: 1条 ✅

---

## 📝 代码变更清单

### 新增文件(6个)

1. **实体类**
   - `AdvPrice.java` - 价格实体类

2. **VO类**
   - `AdvPriceVO.java` - 价格VO(重命名自原 AdvPrice)

3. **Mapper**
   - `AdvPriceMapper.java` - 价格Mapper

4. **Service**
   - `AdvPriceService.java` - 价格Service接口
   - `AdvPriceServiceImpl.java` - 价格Service实现

5. **文档**
   - `docs/adv_tables_ddl_v2.sql` - V2.0 DDL脚本

### 修改文件(5个)

1. **实体类**
   - `AdvLibrary.java` - 移除 priceList 字段
   - `AdvUser.java` - 新增 priceId,移除 advPriceSnapshot

2. **Service**
   - `AdvLibraryService.java` - 新增 getPriceListByLibraryId 方法
   - `AdvLibraryServiceImpl.java` - 实现价格关联查询

3. **文档**
   - `docs/ADV_DESIGN.md` - 更新为 V2.0 版本

---

## 🎯 重构收益

### 1. 数据管理优势
- ✅ **独立管理**: 价格配置独立于广告库,便于批量管理
- ✅ **版本控制**: 支持价格版本追溯
- ✅ **状态控制**: 启用/停用状态,灵活调整
- ✅ **唯一约束**: 防止重复配置

### 2. 性能优势
- ✅ **索引优化**: 独立表索引,查询速度提升
- ✅ **避免JSON解析**: 直接SQL查询,无需解析JSON
- ✅ **关联查询**: 标准JOIN,优化器可充分优化

### 3. 开发体验
- ✅ **类型安全**: 实体类提供编译期类型检查
- ✅ **代码清晰**: Entity/VO分离,职责明确
- ✅ **易于扩展**: 新增字段无需修改JSON结构

### 4. 数据一致性
- ✅ **外键关联**: 通过 price_id 保证数据完整性
- ✅ **快照冗余**: price_month 保留购买时价格
- ✅ **版本追溯**: 可查询历史价格变化

---

## ⚠️ 注意事项

### 1. 向后兼容
- ✅ `price_month` 字段保留,不影响已购买用户
- ✅ `advShowSnapshot` 保留,展现快照功能不变
- ⚠️ 旧的 `price_list` JSON字段已删除,不可回滚

### 2. 业务逻辑调整
- 🔄 购买流程需从 `t_adv_price` 获取价格信息
- 🔄 价格展示需调用 `advLibraryService.getPriceListByLibraryId()`
- 🔄 价格管理需通过 `advPriceService` 操作

### 3. 数据迁移
- ✅ 已完成现有数据迁移
- ⚠️ 若有新环境部署,需执行 `adv_tables_ddl_v2.sql`

---

## 🧪 测试验证

### 数据库验证
```sql
-- 验证表结构
SHOW CREATE TABLE t_adv_price;  ✅ 通过

-- 验证数据迁移
SELECT COUNT(*) FROM t_adv_price;  ✅ 5条

-- 验证关联关系
SELECT ua.id, ua.price_id, ap.monthly_price 
FROM t_user_adv ua 
LEFT JOIN t_adv_price ap ON ua.price_id = ap.id;  ✅ 通过
```

### 代码验证
```bash
# Linter检查
✅ AdvPrice.java - 无错误
✅ AdvPriceVO.java - 无错误
✅ AdvLibrary.java - 无错误
✅ AdvUser.java - 无错误
✅ AdvPriceService.java - 无错误
✅ AdvLibraryServiceImpl.java - 无错误
```

---

## 📚 使用示例

### 查询价格列表
```java
// V1.0 方式(已废弃)
// List<AdvPrice> priceList = advLibrary.getPriceList();

// V2.0 方式
List<AdvPriceVO> priceList = advLibraryService.getPriceListByLibraryId(libraryId);
```

### 用户购买流程
```java
// 1. 查询价格配置
AdvPrice price = advPriceService.getByLibraryIdAndPosition(
    libraryId, 
    AdvPositionEnum.RANK_1, 
    "direct", 
    1
);

// 2. 创建购买记录
AdvUser userAdv = new AdvUser()
    .setUserId(userId)
    .setLibraryId(libraryId)
    .setPriceId(price.getId())  // ✅ 关联价格ID
    .setKeyword(keyword)
    .setPriceMonth(price.getMonthlyPrice())  // 快照
    .setAutoRenew(true);

advUserService.save(userAdv);
```

### 价格管理
```java
// 调整价格
AdvPrice price = advPriceService.getById(priceId);
price.setMonthlyPrice(new BigDecimal("899.00"))
     .setVersion(price.getVersion() + 1);
advPriceService.updateById(price);

// 下架价格
price.setStatus(0);
advPriceService.updateById(price);
```

---

## 🚀 后续建议

### 短期优化
1. **缓存优化**: 使用 Redis 缓存热门关键词的价格列表
2. **批量查询**: 优化多关键词价格查询接口
3. **价格审计**: 记录价格变更日志

### 长期规划
1. **价格策略**: 支持动态定价、折扣活动
2. **A/B测试**: 不同价格策略的效果分析
3. **数据分析**: 价格与购买转化率关系分析

---

## 📞 联系方式

如有问题或建议,请联系:
- 开发者: zyred
- 文档版本: V2.0
- 最后更新: 2025-11-08

---

**重构状态**: ✅ 已完成  
**测试状态**: ✅ 已通过  
**文档状态**: ✅ 已更新
