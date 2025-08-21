# 🎯 心理量表网络分析应用 v3.0 - Claude 代码助手指南

## 📋 项目概述

这是一个基于R Shiny开发的企业级心理量表网络分析应用，支持完整的横断面网络分析研究流程。经过多轮优化迭代，现已成为功能完整、技术健壮、研究友好的专业心理网络分析工具。

### 🏗️ 应用规模与架构 
- **代码规模**：超过15,000行专业R代码（app.R: 9,200+行，utils.R: 4,000+行）
- **支持量表**：8+种核心心理量表（AUDIT、HRF、PHQ9、GAD7、BDI、DASS、IAT、FTND等）
- **分析模块**：11个主要功能模块，28个响应式事件处理器，104个专业函数
- **UI页面**：11个完整功能页面，专业的分析导航系统
- **代码导出**：完整的R脚本自动生成，记录所有实际执行的分析步骤

### 🌟 v3.0 核心特性
- **🎛️ 多网络估计方法**：EBICglasso（推荐）、MGM（混合数据）、Ising（二元数据）、相关网络、偏相关、TMFG等，含详细使用建议
- **📊 多层级分析体系**：汇总层（总分）、子量表层（维度得分）、条目层（原始题目）
- **📏 样本量计算**：基于powerly包的动态样本量计算，支持网络密度和节点数的自动提取
- **🔧 变量构造系统**：高级量表变量自动构造和计分系统，支持CSV配置化管理
- **📖 智能故事生成**：根据量表组合和分析结果自动生成个性化研究故事线
- **🔍 智能数据质量评估**：实时显示完整观测数量、每个变量的缺失值统计和质量警告
- **🌉 桥接网络分析**：识别不同量表组别间的桥接节点，支持Bridge函数和bridgeGroup分析，方形节点表示桥接变量
- **🌡️ 网络温度分析**：基于psychonetrics的多组Ising模型温度计算（8模型约束层次M1-M8）
- **🧠 贝叶斯网络分析**：因果推理和DAG结构学习，支持约束规则和Bootstrap稳定性
- **⚖️ 组别对比分析**：NetCompare函数进行网络不变性检验（全局强度、网络结构、边差异）
- **📈 完整稳定性分析**：quickNet的Stability()函数、bootnet包的边稳定性和中心性稳定性检验
- **📝 实时代码记录**：自动记录用户实际执行的所有分析步骤，生成完整的可重现R脚本
- **🎨 专业可视化**：所有图表输出PDF格式，使用quickNet的专业绘图函数（get_network_plot、get_centrality_plot、get_stability_plot）

## 🚨 重要技术说明

### **关键冲突解决**
- **函数冲突保护**：实现了多层次的基础函数保护机制，避免包冲突导致的 `abs(z)` 等错误
- **bruceR包冲突**：完全避免加载bruceR包，所有HTML函数使用`tags$p()`格式
- **启动方式**：推荐使用`source("run_app.R", encoding = "UTF-8")`以确保包冲突被正确处理

### **依赖包管理**
```r
# 核心依赖
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", 
  "readxl", "dplyr", "ggplot2", "bootnet", "psychonetrics", "powerly"
)

# GitHub包  
quickNet (从 LeiGuo0812/quickNet 安装)
```

## 📁 项目文件结构

```
NetworkApp/
├── app.R                    # 主应用文件（9,231行，600KB+）
├── utils.R                  # 工具函数库（4,035行，104个函数）
├── config.R                 # 配置文件（451行，8种量表定义）
├── scale_calculator.R       # 量表计算引擎（483行）
├── 05_ScaleCalculation.R    # 高级量表计算（650行）
├── story_generator.R        # 分析故事生成器（311行）
├── run_app.R                # 完整启动脚本（192行）
├── 安全启动.R               # 推荐启动脚本（70行）
├── CLAUDE.md                # 使用指南（本文件）
├── CLAUDE_ARCHITECTURE.md   # 完整技术架构文档
├── README.md                # 项目说明
├── 最终使用说明.md          # 详细使用指南
└── example_data.csv         # 示例数据
```

### 📊 代码结构统计
| 文件 | 行数 | 主要内容 | 占比 |
|------|------|----------|------|
| app.R | 9,231 | UI界面(2,000行) + 服务器逻辑(7,200行) | 60% |
| utils.R | 4,035 | 104个专业分析函数 | 26% |
| config.R | 451 | 8种量表配置 | 3% |
| scale_calculator.R | 483 | 量表计算引擎 | 3% |
| 05_ScaleCalculation.R | 650 | 高级计算功能 | 4% |
| story_generator.R | 311 | 分析报告生成 | 2% |
| run_app.R | 192 | 启动管理 | 1% |
| **总计** | **15,353** | **完整网络分析平台** | **100%** |

## 🎛️ 应用架构

### **UI结构（11个主要功能模块）**
1. **首页** (`homepage`) - 横断面网络分析研究框架介绍
2. **数据上传** (`upload`) - 文件上传和数据验证
3. **变量构造** (`construct`) - 高级量表变量构造和计分
4. **变量选择** (`variables`) - 专门的变量层级选择页面
5. **网络分析** (`analysis`) - 核心网络分析和可视化
6. **网络温度分析** (`temperature`) - Ising模型温度分析
7. **贝叶斯网络** (`bayesian`) - 因果推理和DAG分析
8. **稳定性分析** (`stability`) - 独立的Bootstrap稳定性检验
9. **样本量计算** (`samplesize`) - 基于powerly的样本量计算
10. **结果下载** (`download`) - 图表和数据导出
11. **使用说明** (`help`) - 说明文档

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
  analysis_data = NULL,         # 最终分析数据
  temperature_result = NULL,    # 温度分析结果
  bayesian_result = NULL,       # 贝叶斯网络结果
  code_recorder = NULL          # 代码记录器
)
```

#### **关键函数调用链**
1. **数据处理**：`parse_scale_structure_advanced()` → `validate_data()` → `preprocess_data()`
2. **变量构造**：`construct_scale_variables()` → `calculate_scale_scores()`
3. **网络分析**：`safe_network_analysis()` → quickNet包 → 可视化输出
4. **温度分析**：`network_temperature_analysis()` → psychonetrics包 → 8模型比较
5. **贝叶斯分析**：`bayesian_network_analysis()` → bnlearn包 → DAG推理
6. **稳定性分析**：bootnet包 → `plotBootnet()` → 稳定性报告
7. **代码生成**：`generate_complete_script()` → 完整R脚本输出

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

## 🔬 高级分析功能

### **📏 样本量计算**
基于powerly包的动态样本量计算：
- **动态参数提取**：自动从网络结果中提取节点数和密度
- **Bootstrap验证**：支持Bootstrap置信区间估计
- **并行计算**：支持多核并行加速计算
- **结果导出**：样本量曲线图和详细统计表

### **🔧 变量构造系统** ⭐
- **智能量表识别**：自动识别AUDIT、HRF、PHQ9、GAD7等8种量表
- **多层级变量生成**：总分、维度得分、条目得分三个层级
- **自动质量检查**：Cronbach's α信度检验、缺失值分析、分布检验
- **灵活计分规则**：支持sum、mean两种计分方式，可自定义权重

### **📖 智能故事生成器**
- **量表组合识别**：自动分析量表组合模式（如：抑郁-焦虑组合、物质使用组合等）
- **个性化故事线**：根据检测到的量表生成对应的研究假设和理论框架
- **分析建议**：提供针对性的网络分析策略和解释指导
- **研究背景**：自动生成相关的文献背景和理论依据

### **🌡️ 网络温度分析**
基于psychonetrics包的Ising模型温度分析，支持8个约束层级的模型比较：
- M1/M2: 所有参数自由（稠密/稀疏）
- M3/M4: 网络结构相等（稠密/稀疏）
- M5/M6: 网络结构+阈值相等（稠密/稀疏）
- M7/M8: 所有参数相等（稠密/稀疏）

### **🧠 贝叶斯网络分析**
- **结构学习**：HC、TABU、MMHC等算法
- **因果推理**：DAG结构和条件独立性检验
- **Bootstrap验证**：结构稳定性评估
- **可视化**：使用Rgraphviz包的专业DAG图

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

### **v3.0 (2024-08-21) - 企业级完整版**
- ✅ 新增样本量计算功能（基于powerly包）
- ✅ 完善变量构造系统（高级量表计算引擎）
- ✅ 新增分析故事生成器（自动化报告功能）
- ✅ 增强代码记录系统（完整的可重现脚本生成）
- ✅ 优化函数冲突保护（多层基础函数锁定机制）
- ✅ 改进温度分析脚本生成（修复raw_data定义等问题）
- ✅ 扩展支持的估计方法（MGM、Ising、TMFG等）
- ✅ 增强错误处理和用户引导

### **v2.0 (2024-08-17) - 桥接网络分析版**
- ✅ 新增桥接网络分析功能（Bridge Analysis）
- ✅ 支持多组别间桥接节点识别
- ✅ 桥接网络可视化（方形节点表示桥接节点）
- ✅ 新增完整观测数量实时显示功能
- ✅ 变量选择页面显示每个变量的缺失值统计
- ✅ 智能数据质量评估和分层提示系统

### **v1.0 (2024-08-11) - 基础完成版**
- ✅ 完整的11标签页应用架构
- ✅ 专门的变量选择页面和服务器端逻辑
- ✅ 解决bruceR包冲突问题
- ✅ 完整的多层级网络分析功能
- ✅ 独立的稳定性分析模块
- ✅ 网络温度分析和贝叶斯网络分析

## 💡 使用建议

### **对于研究者**
1. **数据准备**：确保数据变量命名符合`SCALE_NUMBER`格式
2. **分析流程**：遵循数据上传→变量构造→变量选择→网络分析→稳定性检验的标准流程
3. **结果解读**：利用自动生成的R脚本进行结果复现和进一步分析
4. **发表准备**：使用专业的PDF图表输出和详细的分析报告

### **对于开发者**
1. **保持函数冲突的警惕**，避免意外引入有冲突的包
2. **新功能开发时优先考虑用户体验和错误处理**
3. **遵循现有的代码结构和命名约定**
4. **充分测试多种数据场景和用户操作流程**

### **对于团队协作**
1. **版本控制**：使用Git进行版本管理，保持代码的可追溯性
2. **文档维护**：及时更新CLAUDE.md和相关技术文档
3. **测试覆盖**：建立完整的测试用例库，确保功能稳定性
4. **用户反馈**：建立用户反馈机制，持续改进用户体验

## 🎉 项目状态：v3.0 企业级版本

当前应用已经是一个功能完整、技术健壮、研究友好的企业级心理量表网络分析工具。支持完整的横断面网络分析研究流程，从数据导入到结果发表的全流程覆盖。代码质量高，用户界面专业，适合学术研究和临床应用。

### **核心竞争优势**
- **功能完整性**：涵盖网络分析的所有主要方法和技术
- **技术健壮性**：15,000+行代码，104个专业函数，经过充分测试
- **研究友好性**：自动代码生成，完整的可重现分析流程
- **用户体验**：11个专业功能模块，智能引导和错误处理
- **可扩展性**：模块化设计，易于添加新量表和新功能

---
*最后更新：2024-08-21*  
*版本：v3.0 企业级完整版*  
*开发团队：心理网络分析实验室*