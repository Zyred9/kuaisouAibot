# 广告价格已售出状态功能说明

## 功能概述
为 `t_adv_price` 表新增 `is_sold` 字段,用于标识广告位是否已被购买,并在前端按钮中展示相应状态。

## 数据库变更

### 新增字段
```sql
ALTER TABLE t_adv_price 
ADD COLUMN is_sold TINYINT(1) DEFAULT 0 COMMENT '是否已售出(0-未售出,1-已售出)' 
AFTER status;
```

### 字段说明
- **字段名**: `is_sold`
- **类型**: `TINYINT(1)`
- **默认值**: `0` (未售出)
- **枚举值**:
  - `0` - 未售出
  - `1` - 已售出

### 索引优化
新增索引以提升查询性能:
```sql
INDEX idx_is_sold (is_sold)
```

## 实体类变更

### AdvPrice.java
新增属性:
```java
/** 是否已售出(0-未售出,1-已售出) **/
private Integer isSold;
```

## 前端按钮逻辑

### KeyboardHelper.buildKeywordQueryKeyboard()
**变更内容**:
- 检查 `price.getIsSold()` 状态
- 已售出时显示 "已被购买" 按钮(不可点击)
- 未售出时显示 "购买" 按钮(可点击)

**实现代码**:
```java
// 判断是否已售出
boolean isSold = price.getIsSold() != null && price.getIsSold() == 1;
String buyButtonText = isSold ? "已被购买" : "购买";
String buyButtonCallback = isSold ? "ignore" : callbackData;

rows.add(row(
    buttonText(priceText, "kw_price_info#" + price.getId()),
    buttonText(buyButtonText, buyButtonCallback)
));
```

## 使用场景

### 场景1: 用户查询关键词价格
1. 系统查询 `t_adv_price` 表获取价格列表
2. 根据 `is_sold` 字段判断售出状态
3. 前端展示对应按钮状态

### 场景2: 管理员标记已售出
```sql
-- 标记某个价格为已售出
UPDATE t_adv_price 
SET is_sold = 1 
WHERE id = 6;

-- 批量标记某关键词的多个位置已售出
UPDATE t_adv_price 
SET is_sold = 1 
WHERE library_id = 1 AND adv_position IN (1, 2, 3);
```

### 场景3: 查询未售出的价格
```sql
SELECT * FROM t_adv_price 
WHERE library_id = 1 
  AND status = 1 
  AND is_sold = 0
ORDER BY monthly_price DESC;
```

## 测试数据

### 当前数据状态
```sql
-- library_id=1, adv_position=1 已标记为已售出
SELECT id, library_id, adv_position, monthly_price, is_sold, remark 
FROM t_adv_price 
WHERE library_id = 1;
```

**结果示例**:
| id | library_id | adv_position | monthly_price | is_sold | remark |
|----|------------|--------------|---------------|---------|--------|
| 6  | 1          | 1            | 184.00        | **1**   | 榜单第1位 |
| 7  | 1          | 2            | 175.00        | 0       | 榜单第2位 |
| 8  | 1          | 3            | 164.00        | 0       | 榜单第3位 |

## 按钮展示效果

### 未售出状态
```
🔥 184$/月  [购买]  ← 可点击
🍇 175$/月  [购买]  ← 可点击
```

### 已售出状态
```
🔥 184$/月  [已被购买]  ← 灰色不可点击
🍇 175$/月  [购买]      ← 可点击
```

## 相关文件
1. **数据库表**: `t_adv_price`
2. **实体类**: `AdvPrice.java`
3. **键盘助手**: `KeyboardHelper.java`
4. **DDL文档**: `docs/adv_tables_ddl_v2.sql`

## 注意事项
1. **null安全**: 代码中使用 `price.getIsSold() != null && price.getIsSold() == 1` 避免空指针
2. **默认值**: 数据库字段默认为 `0`,确保新插入数据未售出状态
3. **按钮回调**: 已售出按钮使用 `"ignore"` 作为回调,防止误点击
4. **状态同步**: 用户成功购买后,应在业务逻辑中同步更新 `is_sold = 1`

## 后续优化建议
1. **自动标记**: 创建 `t_user_adv` 记录时,自动将对应 `t_adv_price.is_sold` 设为 `1`
2. **过期释放**: 广告过期后,将 `is_sold` 重置为 `0` 供他人购买
3. **冲突检测**: 防止同一位置被多次购买的并发问题
4. **统计报表**: 添加已售出/未售出数量统计功能

---
**作者**: zyred  
**版本**: 1.0  
**日期**: 2025-11-08
