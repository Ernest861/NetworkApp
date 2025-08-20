# 🏗️ 心理量表网络分析应用 - 完整架构文档

## 📊 应用规模总览

### 代码规模统计
- **app.R**: 7,975行 - 主应用文件（UI界面 + 服务器逻辑）
- **utils.R**: 2,560行 - 工具函数库（30+专业分析函数）
- **config.R**: ~500行 - 配置文件（71种心理量表定义）
- **总代码量**: 超过11,000行专业R代码

### 应用特性规模
- **支持量表**: 71种心理量表（涵盖情绪健康、成瘾行为、人际关系、人格特质等）
- **分析类型**: 6种主要分析类型（网络分析、稳定性、桥接、贝叶斯、温度、比较）
- **UI页面**: 10个主要功能页面
- **服务器输出**: 80+个响应式输出
- **观察器**: 20+个事件处理器

## 🎯 应用架构总体设计

### 设计模式
- **MVC架构**: Model(utils.R + config.R) - View(UI部分) - Controller(Server部分)
- **模块化设计**: 功能模块独立，易于维护和扩展
- **响应式编程**: 基于Shiny的完全响应式用户界面
- **渐进式分析**: 数据上传 → 变量选择 → 网络分析 → 高级分析的渐进流程

## 📁 文件结构详细分析

### 1. app.R (7,975行) - 主应用文件

#### 1.1 文件头部 (1-100行)
```r
# 包加载与初始化
- 核心依赖: shiny, shinydashboard, shinyWidgets, DT, plotly
- 数据处理: readxl, dplyr, ggplot2
- 专业分析: quickNet(可选), bootnet, psychonetrics
- bruceR包冲突避免机制
- config.R 和 utils.R 加载与fallback机制
```

#### 1.2 UI界面定义 (93-1590行)
```r
# UI结构层次:
dashboardPage
├── dashboardHeader (应用标题)
├── dashboardSidebar (导航菜单 + 分析流程提示器)
│   ├── 10个主要菜单项
│   └── 固定位置的分析导航提示
└── dashboardBody
    └── tabItems (页面容器)
        ├── homepage (134-424行) - 首页与研究框架
        ├── upload (425-499行) - 数据上传页面
        ├── construct (500-628行) - 变量构造页面
        ├── variables (629-715行) - 变量选择页面
        ├── analysis (716-937行) - 网络分析页面
        ├── temperature (938-1144行) - 网络温度分析页面
        ├── bayesian (1145-1410行) - 贝叶斯网络页面
        ├── stability (1411-1463行) - 稳定性分析页面
        ├── download (1464-1511行) - 结果下载页面
        └── help (1512-1590行) - 使用说明页面
```

#### 1.3 服务器逻辑 (1591-7975行)
```r
# 服务器架构:
server <- function(input, output, session) {
    # 1. 响应式数据存储 (1600-1650行)
    values <- reactiveValues(
        raw_data, processed_data, scales, validation_result,
        network_result, centrality_result, stability_result,
        temperature_result, bayesian_result, bridge_result,
        final_variables, variable_groups, network_group_colors,
        layout, upload_timestamp, output_folder, ...
    )
    
    # 2. 响应式状态指示器 (1670-1800行)
    output$fileUploaded, output$dataValid, output$hasWarnings,
    output$scalesDetected, output$dataUploaded, ...
    
    # 3. 数据上传与预处理模块 (1800-2000行)
    - 文件上传处理 (observeEvent input$file)
    - 数据验证与预览 (output$data_preview)
    - 量表结构识别 (output$scale_structure)
    - 智能故事预览 (output$smart_story_preview)
    
    # 4. 变量构造模块 (2000-2550行)
    - 量表检测 (observeEvent input$detect_scales)
    - 量表计算 (observeEvent input$calculate_scales)
    - 手动规则管理 (observeEvent input$confirm_manual_rule)
    - 计算报告生成 (output$calculation_report)
    
    # 5. 变量选择模块 (2550-2700行)
    - 高级量表选择器 (output$scale_level_selectors)
    - 变量分组配色系统
    - 最终变量确认机制
    
    # 6. 核心网络分析模块 (2700-3500行)
    - 主网络分析 (observeEvent input$run_analysis)
    - 网络可视化 (output$network_plot)
    - 桥接网络分析 (output$bridge_network_plot)
    - 中心性分析 (output$centrality_plot)
    
    # 7. 稳定性分析模块 (3500-3800行)
    - Bootstrap稳定性计算 (observeEvent input$run_stability)
    - 边稳定性图 (output$edge_stability_plot)
    - 中心性稳定性图 (output$centrality_stability_plot)
    
    # 8. 网络比较模块 (3800-4600行)
    - 组间比较分析 (observeEvent input$run_group_compare)
    - 比较结果可视化 (output$group_compare_plot)
    - 统计检验表格 (output$group_compare_table)
    
    # 9. 贝叶斯网络模块 (4600-6500行)
    - 约束规则生成与管理
    - 贝叶斯网络推理
    - 因果结构可视化
    
    # 10. 网络温度分析模块 (6500-7800行)
    - 多组Ising模型拟合
    - 8模型约束层次分析
    - 温度比较可视化
    - 组别网络图生成
    - FigS4导出功能
    
    # 11. 下载处理器集合 (7800-7975行)
    - 各种分析结果的PDF/CSV下载
    - 组别网络图下载
    - 完整结果包下载
}
```

### 2. utils.R (2,560行) - 工具函数库

#### 2.1 核心函数分类
```r
# 数据处理与验证 (1-500行)
├── standardize_gender_variable() - 性别变量标准化
├── parse_scale_structure_advanced() - 高级量表结构解析
├── auto_detect_scales() - 智能量表检测
├── validate_data() - 数据质量验证
├── compute_scale_scores_advanced() - 高级量表计分
└── preprocess_data() - 数据预处理

# 网络分析核心 (500-1000行)
├── safe_network_analysis() - 安全网络分析
├── create_unified_network_params() - 统一网络参数
├── generate_network_report() - 网络分析报告
├── create_enhanced_centrality_plot() - 增强中心性图
├── export_analysis_results() - 分析结果导出
└── get_centrality_plot() - 中心性图生成

# 比较分析工具 (1000-1500行)
├── get_compare_plot() - 比较图生成
├── create_simple_compare_plot() - 简单比较图
└── validate_likert_data() - Likert数据验证

# 贝叶斯网络分析 (1500-2000行)
├── generate_smart_constraints() - 智能约束生成
├── parse_manual_constraints() - 手动约束解析
├── validate_constraints() - 约束验证
├── conduct_likert_bayesian_analysis() - Likert贝叶斯分析
├── create_bayesian_network_plot() - 贝叶斯网络图
└── generate_bayesian_report() - 贝叶斯分析报告

# 网络温度分析 (2000-2560行)
├── network_temperature_analysis() - 网络温度主函数
├── prepare_binary_data() - 二值化数据准备
├── fit_ising_models() - Ising模型拟合
├── extract_network_metrics() - 网络指标提取
├── compare_ising_models() - Ising模型比较
└── generate_temperature_summary() - 温度分析汇总
```

### 3. config.R - 配置文件

#### 3.1 主要配置内容
```r
# 量表配置系统 (SCALE_CONFIGS)
├── 71种心理量表的完整定义
├── 每个量表的条目、维度、计分规则
├── 自动识别的模式匹配规则
└── 量表层级选择的默认配置

# 网络分析参数 (NETWORK_PARAMS)
├── Bootstrap参数范围
├── 样本量要求
├── 变量数量限制
└── 稳定性分析参数

# 可视化配置 (VIZ_CONFIG)
├── 颜色方案定义
├── 图表样式参数
├── 布局配置选项
└── 导出格式设置
```

## 🔄 数据流与分析流程

### 用户操作流程
```
1. 数据上传 → 2. 变量构造 → 3. 变量选择 → 4. 网络分析 → 5. 高级分析
   ↓              ↓              ↓              ↓              ↓
文件读取        量表识别        层级选择        GLASSO估计      温度/贝叶斯
数据验证        量表计分        分组配色        中心性计算      桥接/稳定性
预览展示        手动规则        最终确认        可视化生成      结果导出
```

### 关键数据对象
```r
# 主要响应式数据流
raw_data (原始数据)
    ↓ preprocess_data()
processed_data (预处理数据)
    ↓ compute_scale_scores_advanced()
scales (量表得分数据)
    ↓ 变量选择与分组
final_variables + variable_groups + network_group_colors
    ↓ safe_network_analysis()
network_result + centrality_result
    ↓ 各种高级分析
stability_result, temperature_result, bayesian_result, bridge_result
```

## 🎨 UI/UX设计模式

### 设计原则
1. **渐进式引导**: 从首页研究框架 → 数据上传 → 逐步分析
2. **状态指示**: 每个步骤都有明确的完成状态指示
3. **实时反馈**: 数据质量、变量配置的实时预览
4. **专业可视化**: 高质量的网络图和统计图表

### 关键UI组件
- **响应式状态栏**: 显示分析进度和数据状态
- **智能选择器**: 变量选择的高级UI组件
- **分组配色系统**: 变量分组的可视化管理
- **下载中心**: 统一的结果导出界面

## 🔧 技术特色与创新

### 1. 智能量表识别系统
- 支持71种标准心理量表的自动识别
- 基于变量名模式匹配的智能算法
- 支持多语言变量名（中英文）

### 2. 多层级分析架构
- 汇总层、子量表层、条目层的灵活切换
- 每个量表独立的层级选择
- 智能的变量数量平衡算法

### 3. 桥接网络分析
- Bridge包的深度集成
- bridgeGroup函数的智能应用
- 桥接节点的可视化标识

### 4. 网络温度分析
- psychonetrics包的专业集成
- 8模型约束层次的完整实现
- 多组Ising模型的温度计算

### 5. 贝叶斯网络推理
- 因果结构的自动学习
- 智能约束规则生成
- DAG可视化与解释

## ⚠️ 技术难点与解决方案

### 1. 包冲突问题
- **问题**: bruceR包的p()函数与Shiny冲突
- **解决**: 完全避免bruceR，使用安全启动脚本

### 2. 大数据集处理
- **问题**: 超过2000行数据的性能问题  
- **解决**: 智能采样与进度指示，但保持分析完整性

### 3. 复杂参数传递
- **问题**: quickNet等包的参数格式要求
- **解决**: 统一参数处理函数和格式转换

### 4. 内存管理
- **问题**: 10,000+行代码的内存占用
- **解决**: 响应式数据管理和按需计算

## 🚀 性能优化策略

### 1. 计算优化
- 缓存机制：analysis_hash避免重复计算
- 按需计算：用户触发式的分析模式
- 并行处理：Bootstrap等耗时操作的后台处理

### 2. UI响应性
- 进度条：长时间操作的用户反馈
- 异步更新：状态指示器的实时更新
- 智能验证：输入即时验证与错误提示

### 3. 内存管理
- 对象清理：分析完成后的临时对象清理
- 数据压缩：大型数据对象的存储优化
- 智能采样：大数据集的预处理采样

## 📈 扩展性设计

### 1. 新量表支持
- 配置驱动：在config.R中添加新量表定义
- 模式匹配：扩展auto_detect_scales函数
- 向后兼容：保持现有量表的功能不变

### 2. 新分析方法
- 模块化：在utils.R中添加新的分析函数
- UI扩展：在app.R中添加新的页面和控件
- 结果集成：扩展export_analysis_results函数

### 3. 可视化定制
- 主题系统：VIZ_CONFIG的扩展配置
- 组件复用：统一的图表生成函数
- 导出格式：支持更多输出格式

## 🎯 开发与维护建议

### 对于开发者
1. **熟悉架构**：先理解数据流和主要模块关系
2. **模块化开发**：新功能尽量独立模块开发
3. **测试驱动**：使用示例数据进行完整流程测试
4. **文档更新**：及时更新CLAUDE.md和注释

### 对于用户
1. **按序操作**：严格按照页面顺序进行分析
2. **数据准备**：确保数据格式符合要求
3. **结果理解**：仔细阅读分析报告和可视化结果
4. **专业咨询**：复杂分析建议咨询相关领域专家

---

**最后更新**: 2024-08-20  
**架构版本**: v2.0 (基于完整代码分析)  
**维护者**: Claude Code Assistant