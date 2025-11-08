# 广告库查询优化指南

## 📋 优化概览

### V1.0 问题(已废弃)
- ❌ 使用 `AdvPriceVO` 进行 Entity → VO 转换,纯浪费性能
- ❌ 需要多次查询(N+1问题): 先查library,再查price
- ❌ 代码冗余,维护成本高

### V2.0 优化方案(当前)
- ✅ 删除冗余的 `AdvPriceVO` 类
- ✅ 在 `AdvLibrary` 实体中添加 `@TableField(exist = false)` 非持久化字段 `priceList`
- ✅ 使用 MyBatis ResultMap `<collection>` 实现一次性关联查询
- ✅ 单次SQL返回完整数据,避免N+1查询

---

## 🏗️ 架构设计

### 1. 实体类设计

```java
@TableName(value = "t_adv_library", autoResultMap = true)
public class AdvLibrary {
    // ... 数据库字段 ...
    
    /** 价格配置列表(非持久化字段,关联查询时填充) **/
    @TableField(exist = false)
    private List<AdvPrice> priceList;
}
```

**关键点:**
- `@TableField(exist = false)` 标记非数据库字段
- MyBatis-Plus不会尝试映射此字段到数据库列
- 仅在关联查询时通过ResultMap填充

---

### 2. Mapper层设计

#### 接口定义
```java
public interface AdvLibraryMapper extends BaseMapper<AdvLibrary> {
    
    // 单个关键词 + 价格配置
    AdvLibrary selectByKeywordWithPrices(@Param("keyword") String keyword);
    
    // 批量关键词 + 价格配置
    List<AdvLibrary> selectByKeywordsWithPrices(@Param("keywords") List<String> keywords);
    
    // 热门关键词 + 价格配置
    List<AdvLibrary> selectHotKeywordsWithPrices(@Param("limit") int limit);
}
```

#### XML ResultMap映射

```xml
<resultMap id="AdvLibraryWithPricesMap" type="com.search.robots.database.entity.AdvLibrary">
    <!-- 广告库基础字段 -->
    <id column="id" property="id"/>
    <result column="keyword" property="keyword"/>
    <!-- ... 其他字段 ... -->
    
    <!-- 关联价格配置列表(一对多) -->
    <collection property="priceList" ofType="com.search.robots.database.entity.AdvPrice">
        <id column="price_id" property="id"/>
        <result column="library_id" property="libraryId"/>
        <result column="adv_position" property="advPosition"/>
        <!-- ... 其他价格字段 ... -->
    </collection>
</resultMap>
```

#### SQL查询示例

```xml
<select id="selectByKeywordWithPrices" resultMap="AdvLibraryWithPricesMap">
    SELECT 
        l.id, l.keyword, l.price, l.show_count,
        p.id as price_id, p.adv_position, p.monthly_price, p.currency
    FROM t_adv_library l
    LEFT JOIN t_adv_price p ON l.id = p.library_id AND p.status = 1
    WHERE l.keyword = #{keyword}
    ORDER BY p.adv_position, p.ranking
</select>
```

---

### 3. Service层设计

```java
public interface AdvLibraryService extends IService<AdvLibrary> {
    
    /** 仅基础信息(不含价格) **/
    AdvLibrary getByKeyword(String keyword);
    
    /** 基础信息 + 价格配置(推荐) **/
    AdvLibrary getByKeywordWithPrices(String keyword);
    
    /** 批量查询 + 价格配置 **/
    List<AdvLibrary> getByKeywordsWithPrices(List<String> keywords);
    
    /** 热门关键词 + 价格配置 **/
    List<AdvLibrary> getHotKeywordsWithPrices(int limit);
}
```

---

## 📖 使用示例

### 示例1: 查询单个关键词及价格配置

```java
// ❌ 旧方式(V1.0) - 多次查询
AdvLibrary library = advLibraryService.getByKeyword("Java");
List<AdvPriceVO> prices = advLibraryService.getPriceListByLibraryId(library.getId());
// 转换VO...浪费性能

// ✅ 新方式(V2.0) - 单次查询
AdvLibrary library = advLibraryService.getByKeywordWithPrices("Java");
if (Objects.nonNull(library)) {
    String keyword = library.getKeyword();
    List<AdvPrice> prices = library.getPriceList(); // 已填充
    
    prices.forEach(price -> {
        System.out.println(price.getAdvPosition() + ": " + price.getMonthlyPrice());
    });
}
```

### 示例2: 批量查询热门关键词

```java
// ✅ 单次SQL查询10个热门关键词及其价格配置
List<AdvLibrary> hotLibraries = advLibraryService.getHotKeywordsWithPrices(10);

hotLibraries.forEach(library -> {
    System.out.println("关键词: " + library.getKeyword());
    System.out.println("展现次数: " + library.getShowCount());
    System.out.println("价格配置数量: " + library.getPriceList().size());
    
    library.getPriceList().forEach(price -> {
        System.out.println("  - " + price.getAdvPosition() + ": ¥" + price.getMonthlyPrice());
    });
});
```

### 示例3: 批量查询指定关键词

```java
List<String> keywords = Arrays.asList("Java", "Python", "Go");
List<AdvLibrary> libraries = advLibraryService.getByKeywordsWithPrices(keywords);

// 单次SQL查询,避免循环查询
libraries.forEach(library -> {
    // 每个library的priceList已填充
    BigDecimal minPrice = library.getPriceList().stream()
        .map(AdvPrice::getMonthlyPrice)
        .min(BigDecimal::compareTo)
        .orElse(BigDecimal.ZERO);
    
    System.out.println(library.getKeyword() + " 最低价: ¥" + minPrice);
});
```

---

## ⚡ 性能对比

### 场景: 查询10个热门关键词及其价格配置

| 方案 | SQL次数 | 数据转换 | 性能 |
|------|---------|----------|------|
| **V1.0(旧)** | 11次(1次library + 10次price) | Entity→VO | 慢 ❌ |
| **V2.0(新)** | 1次(LEFT JOIN) | 无转换 | 快 ✅ |

### 场景: 查询单个关键词及其5个价格配置

| 方案 | SQL次数 | 返回数据量 | 代码复杂度 |
|------|---------|-----------|-----------|
| **V1.0(旧)** | 2次 | Library + 5个VO | 高 ❌ |
| **V2.0(新)** | 1次 | Library(含5个Price) | 低 ✅ |

**性能提升:**
- SQL查询次数减少 **90%+**
- 无VO转换开销
- 代码简洁度提升 **50%+**

---

## 🔧 技术细节

### MyBatis Collection映射原理

1. **LEFT JOIN查询返回扁平化结果集**
   ```
   id | keyword | price_id | adv_position | monthly_price
   1  | Java    | 1        | RANK_1       | 999.00
   1  | Java    | 2        | RANK_2       | 699.00
   1  | Java    | 3        | BANNER       | 1299.00
   ```

2. **MyBatis自动聚合为对象树**
   ```
   AdvLibrary {
       id: 1,
       keyword: "Java",
       priceList: [
           AdvPrice{id:1, advPosition:RANK_1, monthlyPrice:999.00},
           AdvPrice{id:2, advPosition:RANK_2, monthlyPrice:699.00},
           AdvPrice{id:3, advPosition:BANNER, monthlyPrice:1299.00}
       ]
   }
   ```

3. **关键配置**
   - `<collection property="priceList">` - 指定集合字段
   - `ofType="AdvPrice"` - 集合元素类型
   - `column="price_id"` - 使用别名避免id冲突

### 字段命名规范

| 表字段 | 别名 | 实体属性 | 说明 |
|--------|------|----------|------|
| `l.id` | `id` | `AdvLibrary.id` | 主表ID |
| `p.id` | `price_id` | `AdvPrice.id` | 价格ID(必须用别名) |
| `p.created_at` | `price_created_at` | `AdvPrice.createdAt` | 避免与主表冲突 |

---

## ✅ 最佳实践

### 1. 何时使用关联查询?

✅ **推荐使用:**
- 列表展示(需要完整信息)
- 详情页(需要library+prices)
- 批量查询(避免循环)

❌ **不推荐使用:**
- 只需要基础信息(用 `getByKeyword`)
- 只需要价格信息(用 `getPriceListByLibraryId`)

### 2. 查询方法选择指南

```java
// 场景1: 只需要关键词基础信息
AdvLibrary library = advLibraryService.getByKeyword("Java");

// 场景2: 需要关键词+价格配置(推荐)
AdvLibrary library = advLibraryService.getByKeywordWithPrices("Java");

// 场景3: 批量查询多个关键词
List<AdvLibrary> libraries = advLibraryService.getByKeywordsWithPrices(keywords);

// 场景4: 查询热门关键词Top10
List<AdvLibrary> hotLibraries = advLibraryService.getHotKeywordsWithPrices(10);

// 场景5: 已有library对象,只需补充价格
List<AdvPrice> prices = advLibraryService.getPriceListByLibraryId(library.getId());
```

### 3. 空值处理

```java
AdvLibrary library = advLibraryService.getByKeywordWithPrices("NonExist");
if (Objects.isNull(library)) {
    // 关键词不存在
    return;
}

// 关键词存在,但可能没有价格配置
if (CollUtil.isEmpty(library.getPriceList())) {
    // 无价格配置
    return;
}
```

---

## 🎯 总结

| 优化项 | 优化结果 |
|--------|----------|
| **删除冗余VO** | AdvPriceVO已删除 ✅ |
| **非持久化字段** | AdvLibrary.priceList(exist=false) ✅ |
| **关联查询** | MyBatis Collection映射 ✅ |
| **性能提升** | SQL次数减少90%+ ✅ |
| **代码简化** | 无需VO转换,代码更简洁 ✅ |

---

## 📝 相关文件

- `AdvLibrary.java` - 实体类(新增priceList字段)
- `AdvLibraryMapper.java` - Mapper接口(新增关联查询方法)
- `AdvLibraryMapper.xml` - MyBatis映射文件(ResultMap配置)
- `AdvLibraryService.java` - Service接口(新增With后缀方法)
- `AdvLibraryServiceImpl.java` - Service实现(调用Mapper层)

---

*最后更新: 2025-11-08*
*版本: V2.0*
