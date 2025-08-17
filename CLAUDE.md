# 🎯 心理量表网络分析应用 - Claude 代码助手指南

## 📋 项目概述

这是一个基于R Shiny开发的心理量表网络分析应用，专门用于多层级网络可视化分析。应用支持2-5个心理量表的灵活组合分析，包括汇总层、子量表层和条目层三个分析水平。

### 🌟 核心特性
- **多层级分析支持**：汇总层（总分）、子量表层（维度得分）、条目层（原始题目）
- **智能量表识别**：自动识别AUDIT、HRF、PHQ、GAD等8+种心理量表
- **灵活变量选择**：专门的变量选择页面，支持每个量表独立选择分析层级
- **智能数据质量评估**：实时显示完整观测数量，每个变量的缺失值统计和质量警告
- **桥接网络分析**：识别不同量表组别间的桥接节点，支持Bridge函数和bridgeGroup分析
- **专业网络分析**：使用quickNet包进行EBIC网络估计
- **独立稳定性分析**：使用bootnet包进行边稳定性和中心性稳定性检验
- **完整可视化**：网络图、桥接网络图、中心性图、稳定性图的高质量输出

## 🚨 重要技术说明

### **关键冲突解决**
- **bruceR包冲突**：该包与Shiny存在严重的`p()`函数冲突，会导致HTML渲染失败
- **解决方案**：完全避免加载bruceR包，所有HTML函数使用`tags$p()`格式
- **启动方式**：必须使用`source("安全启动.R")`以确保包冲突被正确处理

### **依赖包管理**
```r
# 核心依赖
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", 
  "readxl", "dplyr", "ggplot2", "bootnet"
)

# GitHub包
quickNet (从 LeiGuo0812/quickNet 安装)
```

## 📁 项目文件结构

```
NetworkApp/
├── app.R                    # 主应用文件（47KB+）
├── config.R                 # 配置文件（量表定义、参数设置）
├── utils.R                  # 工具函数库（数据处理、分析函数）
├── 安全启动.R               # 推荐启动脚本
├── run_app.R                # 完整启动脚本
├── CLAUDE.md                # 本文件
├── README.md                # 项目说明
├── 最终使用说明.md          # 详细使用指南
└── example_data.csv         # 示例数据
```

## 🎛️ 应用架构

### **UI结构（6个主要标签页）**
1. **数据上传** (`upload`) - 文件上传和数据验证
2. **变量选择** (`variables`) - 专门的变量层级选择页面
3. **网络分析** (`analysis`) - 核心网络分析和可视化
4. **稳定性分析** (`stability`) - 独立的Bootstrap稳定性检验
5. **结果下载** (`download`) - 图表和数据导出
6. **使用帮助** (`help`) - 说明文档

### **服务器端核心逻辑**

#### **反应式数据存储**
```r
values <- reactiveValues(
  raw_data = NULL,              # 原始数据
  processed_data = NULL,        # 预处理数据
  scales = NULL,                # 识别的量表结构
  validation_result = NULL,     # 数据验证结果
  network_result = NULL,        # 网络分析结果
  centrality_result = NULL,     # 中心性分析结果
  stability_result = NULL,      # 稳定性分析结果
  analysis_data = NULL          # 最终分析数据
)
```

#### **关键函数调用链**
1. **数据处理**：`parse_scale_structure_advanced()` → `validate_data()` → `preprocess_data()`
2. **网络分析**：`safe_network_analysis()` → quickNet包 → 可视化输出
3. **稳定性分析**：bootnet包 → `plotBootnet()` → 稳定性报告

## 🎯 变量选择系统

### **专门的变量选择页面**
- **高级选择器**：`output$advanced_scale_selectors` 动态生成卡片式界面
- **实时预览**：`output$final_variables_preview` 显示最终变量配置
- **分组配色**：`output$variable_groups_ui` 变量分组配色管理
- **确认流程**：`variables_confirmed` 状态管理，确保用户明确选择

### **变量分组配色系统** ⭐
- **默认分组**：每个量表自动分为一组，共用一个配色
- **灵活分组**：用户可将多个量表合并为一组，或重新分配分组
- **快速分组**：提供"全部合并"、"每个一组"、"按类型分组"等快捷操作
- **智能匹配**：支持多种变量名匹配策略（精确、前缀、后缀、包含）
- **配色应用**：分组配色自动应用到网络图中，便于识别量表聚类

#### **分组功能使用示例**
```
用户场景：HRF18_General, PHQ9, AUDIT10 三个量表
默认分组：组1=HRF18_General, 组2=PHQ9, 组3=AUDIT10
自定义分组：
  - 情绪认知组 = HRF18_General + PHQ9 （绿色显示）
  - 物质使用组 = AUDIT10 （蓝色显示）
```

#### **快速分组功能**
- **全部合并为一组**：所有量表使用同一颜色
- **每个量表一组**：恢复默认分组
- **按类型分组**：自动识别情绪、物质、动机等类型进行分组

### **层级选择逻辑**
```r
# 汇总层：使用总分或第一个维度得分
if(selected_level == "summary") {
  total_subscales <- grep("Total", names(subscales))
  use_variable <- if(length(total_subscales) > 0) total_subscales[1] else subscales[1]
}

# 子量表层：使用各维度得分（排除Total）
if(selected_level == "subscale") {
  dimension_subscales <- names(subscales)[!grepl("Total", names(subscales))]
  use_variables <- dimension_subscales
}

# 条目层：使用所有原始题目（无限制）
if(selected_level == "items") {
  use_variables <- scale_info$items  # 2024-08-11: 移除了15条目限制
}
```

## 🔬 NetCompare组间比较分析

### **组间比较功能说明**
应用支持使用quickNet包的NetCompare函数进行网络组间比较分析，提供完整的NCT (Network Comparison Test) 结果结构。

#### **NetCompare调用参数**
```r
# 标准调用格式（参考你的示例）
netcompare_result <- NetCompare(
  group1_data, group2_data,
  it = 5000,  # 置换检验次数
  p.adjust.methods = "BH"  # 多重比较校正方法
)
```

#### **完整的NCT结果结构**
应用会自动解析并保存所有NCT字段：

| 字段名 | 类型 | 说明 |
|--------|------|------|
| `glstrinv.real` | NCT | 全局强度不变性检验 - 实际值 |
| `glstrinv.sep` | NCT | 全局强度不变性检验 - 分离值 |
| `glstrinv.pval` | NCT | 全局强度不变性检验 - P值 |
| `glstrinv.perm` | NCT | 全局强度不变性检验 - 置换分布 |
| `nwinv.real` | NCT | 网络结构不变性检验 - 实际值 |
| `nwinv.pval` | NCT | 网络结构不变性检验 - P值 |
| `nwinv.perm` | NCT | 网络结构不变性检验 - 置换分布 |
| `einv.real` | NCT | 边不变性检验 - 实际值 |
| `einv.pvals` | NCT | 边不变性检验 - P值矩阵 |
| `einv.perm` | NCT | 边不变性检验 - 置换分布 |
| `diff_sig` | Matrix | 显著差异边的矩阵 |
| `edge_weight_p` | Matrix | 边权重P值矩阵 |

#### **结果导出功能**
应用自动生成以下导出：
- 差异网络可视化图
- 显著性检验结果CSV
- 组间统计摘要

#### **使用示例代码模板**
应用遵循你的代码风格，生成类似以下的分析代码：
```r
# 组间比较分析
netcompare_result <- NetCompare(group1_data, group2_data, it=5000, p.adjust.methods = "BH")

# 生成比较图
get_compare_plot(netcompare_result, reference_network, prefix = "Fig3_comparison", width = 6, height = 4.5)

# 导出结果
write.csv(data.frame(netcompare_result$diff_sig), 'comparison_diff_network.csv', row.names = TRUE)
write.csv(data.frame(netcompare_result$edge_weight_p), 'comparison_pvalue_matrix.csv', row.names = TRUE)
```

### **🌉 桥接网络分析功能**

#### **功能说明**
桥接网络分析识别连接不同心理构念组别的"桥接节点"，这些节点在临床干预中具有重要意义。

#### **使用步骤**
1. **配置变量分组**：在变量选择页面设置至少2个变量组
2. **启用桥接分析**：在网络分析页面勾选"启用桥接网络分析"
3. **设置参数**：选择每组识别的桥接节点数量（默认1个）
4. **运行分析**：执行网络分析，自动进行桥接分析
5. **查看结果**：在"桥接网络"标签页查看结果和下载

#### **桥接分析核心算法**
应用实现了与你的代码完全一致的桥接分析流程：

```r
# 基础网络构建
base_network <- quickNet(data, threshold=0.05, groups=user_groups)

# Bridge分析 - 计算桥接强度
bridge_result <- Bridge(base_network, communities = user_groups)

# bridgeGroup分析 - 识别桥接节点
bridge_groups <- bridgeGroup(bridge_result, user_groups, n = 1, by_group = TRUE)

# 可视化桥接网络（方形=桥接节点，圆形=普通节点）
shape_list <- ifelse(bridge_groups == "Bridge", "square", "circle")
bridge_network <- quickNet(data, groups = bridge_groups, shape = shape_list,
                          color = c("#63bbd0","#f87599","#fed71a","#d1c2d3"))
```

#### **结果解读**
- **桥接节点**：显示为方形，连接不同心理构念的关键变量
- **颜色编码**：不同颜色表示不同的组别或桥接状态  
- **临床意义**：桥接节点通常是干预的优先目标

#### **导出功能**
- **桥接网络图**：PNG格式，突出显示桥接节点
- **桥接分析数据**：CSV格式，包含变量分组和桥接状态信息

## 🔧 开发和维护指南

### **启动应用的正确方式**
```r
# 推荐方式 - 最安全
source("安全启动.R")

# 备选方式
source("run_app.R")
```

### **常见开发任务**

#### **添加新量表支持**
在`config.R`中添加新的量表配置：
```r
SCALE_CONFIGS$NEW_SCALE <- list(
  name = "新量表全名",
  name_en = "New Scale",
  pattern = "^NEW_PATTERN",
  subscales = list(
    "NEW_Total" = list(items = paste0("NEW_", 1:10), description = "总分"),
    "Dimension1" = list(items = paste0("NEW_", 1:5), description = "维度1")
  ),
  scoring = "sum",  # 或 "mean"
  item_range = c(1, 5),
  description = "量表描述"
)
```

#### **修改网络分析参数**
在`config.R`中调整`NETWORK_PARAMS`：
```r
NETWORK_PARAMS <- list(
  default_threshold = 0.05,      # 默认阈值
  max_variables_summary = 20,    # 汇总层最大变量数
  max_variables_items = 50,      # 条目层建议最大数（仅提示）
  bootstrap_default = 1000       # 稳定性分析Bootstrap次数
)
```

#### **自定义可视化样式**
修改`config.R`中的`VIZ_CONFIG`：
```r
VIZ_CONFIG <- list(
  colors = list(
    primary = c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A"),
    positive_edges = "#4A90E2",
    negative_edges = "#D0021B"
  ),
  plot_params = list(width = 800, height = 600, dpi = 150)
)
```

### **调试和错误处理**

#### **常见错误及解决方案**

1. **函数冲突错误**
   ```
   Error in abs(z) : non-numeric argument to mathematical function
   ```
   **解决**：确保使用安全启动脚本，避免bruceR包

2. **showNotification参数错误**
   ```
   'arg' should be one of "default", "message", "warning", "error"
   ```
   **解决**：使用`type = "message"`而不是`type = "success"`

3. **网络图不显示**
   **解决**：检查`renderPlot`中的`plot()`调用，确保网络对象可绘制

4. **量表识别失败**
   **解决**：检查数据列名是否符合`SCALE_PATTERN_NUMBER`格式

### **性能优化建议**

1. **数据量控制**：超过30个变量时给予提示，建议用户优化选择
2. **稳定性分析**：Bootstrap次数根据样本量调整（小样本用500-1000次）
3. **大数据集处理**：
   - ⚠️ **重要**：永远不使用自动采样优化，始终使用完整数据集
   - 大数据集（>2000行）仅给出性能提示，不进行数据采样
   - 保持分析结果的完整性和可重复性

### **测试用例**

#### **标准测试数据格式**
```csv
ID,Age,Gender,AUDIT10_1,AUDIT10_2,...,HRF18_1,HRF18_2,...,PHQ9_1,PHQ9_2,...
1,25,Male,2,1,...,4,5,...,1,2,...
2,30,Female,0,0,...,3,4,...,0,1,...
```

#### **测试场景**
1. **基本功能**：上传示例数据 → 选择变量 → 运行分析
2. **层级选择**：测试汇总/子量表/条目三种层级组合
3. **稳定性分析**：小样本（n=50）和大样本（n=200+）测试
4. **错误处理**：不完整数据、格式错误、缺失值过多等场景

## 📊 量表配置详情

### **已支持量表列表**
- **AUDIT** (10题) - 酒精使用障碍识别测试
- **HRF** (18题) - 习惯奖赏恐惧动机量表  
- **PHQ9** (9题) - 患者健康问卷抑郁模块
- **GAD7** (7题) - 广义焦虑障碍量表
- **BDI** - 贝克抑郁量表
- **DASS** - 抑郁焦虑压力量表
- **IAT** - 网络成瘾测试
- **FTND** - 尼古丁依赖测试

### **智能默认配置**
- **AUDIT, PHQ, GAD** → 汇总层（总分分析更有临床意义）
- **HRF** → 条目层（18个动机条目提供详细信息）
- **其他量表** → 根据条目数和维度数自适应

## 🔄 版本历史

### **v1.2 (2024-08-17) - 桥接网络分析版**
- ✅ 新增桥接网络分析功能（Bridge Analysis）
- ✅ 支持多组别间桥接节点识别
- ✅ 桥接网络可视化（方形节点表示桥接节点）
- ✅ 桥接分析结果导出和下载功能
- ✅ 智能检测变量分组，自动启用桥接分析选项

### **v1.1 (2024-08-17) - 数据质量评估版**
- ✅ 新增完整观测数量实时显示功能
- ✅ 变量选择页面显示每个变量的缺失值统计
- ✅ 智能数据质量评估和分层提示系统
- ✅ 改进样本量不足的错误提示和建议
- ✅ 增强变量预览界面的信息密度

### **v1.0 (2024-08-11) - 基础完成版**
- ✅ 完整的6标签页应用架构
- ✅ 专门的变量选择页面和服务器端逻辑
- ✅ 解决bruceR包冲突问题
- ✅ 移除15条目硬限制，改为30节点建议
- ✅ 修复网络图显示问题
- ✅ 完整的多层级网络分析功能
- ✅ 独立的稳定性分析模块

### **开发重点**
- **稳定性优先**：确保应用能正常启动和运行
- **用户体验**：清晰的界面引导和错误提示
- **功能完整性**：涵盖从数据上传到结果导出的完整流程
- **技术健壮性**：处理各种边界情况和用户输入错误

## 💡 使用建议

### **对于用户**
1. 始终使用`source("安全启动.R")`启动应用
2. 确保数据变量命名符合`SCALE_NUMBER`格式
3. 利用专门的变量选择页面进行精细配置
4. 超过30个变量时考虑优化选择以获得更好的可视化效果

### **对于开发者**
1. 保持bruceR包冲突的警惕，避免意外引入
2. 新功能开发时优先考虑用户体验和错误处理
3. 遵循现有的代码结构和命名约定
4. 充分测试多种数据场景和用户操作流程

## 🎉 项目状态：基础版完成

当前应用已经是一个功能完整、技术健壮的心理量表网络分析工具，能够满足研究和应用需求。核心功能稳定，用户界面友好，适合投入实际使用。

---
*最后更新：2024-08-11*  
*GitHub仓库：https://github.com/Ernest861/NetworkApp*