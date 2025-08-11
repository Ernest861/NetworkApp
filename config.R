# =============================================================================
# 量表配置文件 - Scale Configuration
# 定义各种心理量表的结构和计分规则
# =============================================================================

# 量表结构配置
SCALE_CONFIGS <- list(
  
  # AUDIT量表 - 酒精使用障碍筛查
  "AUDIT" = list(
    name = "AUDIT - 酒精使用障碍识别测试",
    name_en = "Alcohol Use Disorders Identification Test",
    pattern = "^AUDIT",
    total_items = 10,
    scoring = "sum",  # sum 或 mean
    subscales = list(
      "AUDIT_Total" = list(
        items = paste0("AUDIT10_", 1:10),
        description = "AUDIT总分"
      )
    ),
    item_range = c(0, 4),
    description = "世界卫生组织开发的酒精使用障碍筛查工具",
    reference = "Saunders et al. 1993"
  ),
  
  # HRF量表 - 习惯-奖赏-恐惧动机
  "HRF" = list(
    name = "HRF - 习惯奖赏恐惧动机量表",
    name_en = "Habit-Reward-Fear Motivation Scale",
    pattern = "^HRF",
    total_items = 18,
    scoring = "mean",
    subscales = list(
      "Habit" = list(
        items = paste0("HRF18_", c(3, 6, 7, 10, 14, 16)),
        description = "习惯动机"
      ),
      "Reward" = list(
        items = paste0("HRF18_", c(2, 4, 9, 12, 15, 17)),
        description = "奖赏动机"
      ),
      "Fear" = list(
        items = paste0("HRF18_", c(1, 5, 8, 11, 13, 18)),
        description = "恐惧动机"
      )
    ),
    item_range = c(1, 7),
    description = "评估成瘾行为的三种核心动机",
    reference = "Custom scale"
  ),
  
  # PHQ-9量表 - 抑郁症状
  "PHQ" = list(
    name = "PHQ-9 - 患者健康问卷抑郁量表",
    name_en = "Patient Health Questionnaire-9",
    pattern = "^PHQ",
    total_items = 9,
    scoring = "sum",
    subscales = list(
      "PHQ_Total" = list(
        items = paste0("PHQ9_", 1:9),
        description = "PHQ-9总分"
      )
    ),
    item_range = c(0, 3),
    description = "评估抑郁症状严重程度",
    reference = "Kroenke et al. 2001"
  ),
  
  # GAD-7量表 - 焦虑症状
  "GAD" = list(
    name = "GAD-7 - 广泛性焦虑量表",
    name_en = "Generalized Anxiety Disorder-7",
    pattern = "^GAD",
    total_items = 7,
    scoring = "sum",
    subscales = list(
      "GAD_Total" = list(
        items = paste0("GAD7_", 1:7),
        description = "GAD-7总分"
      )
    ),
    item_range = c(0, 3),
    description = "评估焦虑症状严重程度",
    reference = "Spitzer et al. 2006"
  ),
  
  # BDI-II量表 - 贝克抑郁量表
  "BDI" = list(
    name = "BDI-II - 贝克抑郁量表第二版",
    name_en = "Beck Depression Inventory-II",
    pattern = "^BDI",
    total_items = 21,
    scoring = "sum",
    subscales = list(
      "BDI_Cognitive" = list(
        items = paste0("BDI21_", c(1, 2, 3, 5, 6, 7, 8, 9, 13, 14)),
        description = "认知症状"
      ),
      "BDI_Somatic" = list(
        items = paste0("BDI21_", c(4, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21)),
        description = "躯体症状"
      ),
      "BDI_Total" = list(
        items = paste0("BDI21_", 1:21),
        description = "BDI-II总分"
      )
    ),
    item_range = c(0, 3),
    description = "评估抑郁症状的金标准工具",
    reference = "Beck et al. 1996"
  ),
  
  # DASS-21量表 - 抑郁焦虑压力量表
  "DASS" = list(
    name = "DASS-21 - 抑郁焦虑压力量表",
    name_en = "Depression Anxiety Stress Scales-21",
    pattern = "^DASS",
    total_items = 21,
    scoring = "sum",
    subscales = list(
      "DASS_Depression" = list(
        items = paste0("DASS21_", c(3, 5, 10, 13, 16, 17, 21)),
        description = "抑郁"
      ),
      "DASS_Anxiety" = list(
        items = paste0("DASS21_", c(2, 4, 7, 9, 15, 19, 20)),
        description = "焦虑"
      ),
      "DASS_Stress" = list(
        items = paste0("DASS21_", c(1, 6, 8, 11, 12, 14, 18)),
        description = "压力"
      )
    ),
    item_range = c(0, 3),
    description = "同时评估抑郁、焦虑和压力症状",
    reference = "Lovibond & Lovibond 1995"
  ),
  
  # IAT量表 - 网络成瘾测试
  "IAT" = list(
    name = "IAT - 网络成瘾测试",
    name_en = "Internet Addiction Test",
    pattern = "^IAT",
    total_items = 20,
    scoring = "sum",
    subscales = list(
      "IAT_Total" = list(
        items = paste0("IAT20_", 1:20),
        description = "IAT总分"
      )
    ),
    item_range = c(1, 5),
    description = "评估网络使用成瘾程度",
    reference = "Young 1998"
  ),
  
  # FTND量表 - 烟草依赖测试
  "FTND" = list(
    name = "FTND - 法格斯特姆烟草依赖测试",
    name_en = "Fagerstrom Test for Nicotine Dependence",
    pattern = "^FTND",
    total_items = 6,
    scoring = "sum",
    subscales = list(
      "FTND_Total" = list(
        items = paste0("FTND6_", 1:6),
        description = "FTND总分"
      )
    ),
    item_range = c(0, 3),
    description = "评估尼古丁依赖程度",
    reference = "Heatherton et al. 1991"
  )
)

# 网络分析参数配置
NETWORK_PARAMS <- list(
  default_threshold = 0.05,
  bootstrap_min = 100,
  bootstrap_max = 10000,
  bootstrap_default = 1000,
  min_sample_size = 50,
  max_variables_items = 50,  # 条目层级最大变量数
  max_variables_subscale = 20  # 维度层级最大变量数
)

# 可视化配置
VIZ_CONFIG <- list(
  colors = list(
    primary = c("#1ba784","#63bbd0","#f87599","#fed71a",
               "#d1c2d3","#304fb0","#c6dfc8","#a8456b","#2486b9",
               "#e16c96","#fc8c23","#280c1c",
               "#fbb957","#de1c31","#ee3f4d",
               "#c0c4c3","#c6e6e8",
               "#12a182","#eb3c70","#eaad1a","#45b787","#d11a2d",
               "#eea08c","#cfccc9"),
    
    positive_edges = c("#2376b7","#134857"),
    negative_edges = c("#d2568c","#62102e"),
    
    scale_colors = list(
      "AUDIT" = "#1ba784",
      "HRF" = "#63bbd0",
      "PHQ" = "#f87599",
      "GAD" = "#fed71a",
      "BDI" = "#d1c2d3",
      "DASS" = "#304fb0",
      "IAT" = "#a8456b",
      "FTND" = "#2486b9"
    )
  ),
  
  plot_params = list(
    width = 800,
    height = 600,
    dpi = 300,
    node_size_range = c(3, 15),
    edge_width_range = c(1, 8),
    font_size = 12
  )
)

# 数据验证规则
VALIDATION_RULES <- list(
  min_subjects = 30,
  max_subjects = 10000,
  min_valid_rate = 0.7,  # 最少70%的数据完整
  max_missing_per_variable = 0.3,  # 单个变量最多30%缺失
  
  # 异常值检测
  outlier_detection = list(
    method = "iqr",  # iqr 或 zscore
    threshold = 3
  )
)

# 报告模板配置
REPORT_CONFIG <- list(
  title = "心理量表网络分析报告",
  author = "Network Analysis App",
  
  sections = list(
    "data_overview" = "数据概览",
    "network_structure" = "网络结构",
    "centrality_analysis" = "中心性分析",
    "stability_analysis" = "稳定性分析",
    "interpretation" = "结果解释"
  ),
  
  output_formats = c("html", "pdf", "word")
)