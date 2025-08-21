# =============================================================================
# 心理量表网络分析 Shiny 应用
# Psychological Scale Network Analysis Shiny App
# 基于 toturial.R 开发 - 增强版
# =============================================================================

# 加载必要的包
suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(plotly)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  
  # 尝试加载专业包
  if(requireNamespace("quickNet", quietly = TRUE)) {
    library(quickNet)
  }
  
  # 完全跳过bruceR以避免p()函数冲突
  bruceR_available <- FALSE
})

# 加载配置和工具函数
tryCatch({
  source("config.R", encoding = "UTF-8")
}, error = function(e) {
  cat("Warning: config.R loading failed:", e$message, "\n")
})

tryCatch({
  source("utils.R", encoding = "UTF-8")  
}, error = function(e) {
  cat("Warning: utils.R loading failed:", e$message, "\n")
  # 提供基本的fallback函数
  parse_scale_structure_advanced <<- function(data) {
    return(list())
  }
  validate_data <<- function(data) {
    return(list(valid = TRUE, warnings = c(), errors = c(), 
                statistics = list(n_subjects = nrow(data), n_variables = ncol(data),
                                complete_cases = nrow(na.omit(data)), 
                                complete_rate = nrow(na.omit(data))/nrow(data),
                                missing_rates = colSums(is.na(data))/nrow(data))))
  }
  compute_scale_scores_advanced <<- function(data, scales) { return(data) }
  safe_network_analysis <<- function(data, threshold = 0.05, edge_labels = TRUE, colors = NULL, groups = NULL, shape = NULL, title = NULL, estimator = "EBICglasso", ...) { 
    if(requireNamespace("quickNet", quietly = TRUE)) {
      
      # 如果用户选择了quickNet默认以外的估计方法，先用bootnet估计网络
      if(estimator != "EBICglasso") {
        if(requireNamespace("bootnet", quietly = TRUE)) {
          tryCatch({
            # 使用bootnet进行网络估计
            net_result <- bootnet::estimateNetwork(data, default = estimator, threshold = TRUE)
            
            # 提取网络矩阵
            if(!is.null(net_result$graph)) {
              # 使用估计的网络矩阵，通过qgraph可视化
              if(requireNamespace("qgraph", quietly = TRUE)) {
                return(qgraph::qgraph(net_result$graph, 
                                    layout = "spring",
                                    edge.labels = edge_labels,
                                    threshold = threshold,
                                    color = colors,
                                    groups = groups,
                                    shape = shape,
                                    title = title,
                                    ...))
              }
            }
          }, error = function(e) {
            warning(paste("使用", estimator, "估计失败，回退到quickNet默认方法:", e$message))
          })
        }
      }
      
      # 默认使用quickNet（通常是EBICglasso）
      args <- list(
        data = data,
        threshold = threshold,
        edge.labels = edge_labels
      )
      
      # 添加可选参数
      if(!is.null(colors)) args$color <- colors
      if(!is.null(groups)) args$groups <- groups
      if(!is.null(shape)) args$shape <- shape
      if(!is.null(title)) args$title <- title
      
      # 添加其他传递的参数
      args <- c(args, list(...))
      
      return(do.call(quickNet::quickNet, args))
    } else {
      stop("quickNet package is required")
    }
  }
  
  # 基本配置fallback
  VIZ_CONFIG <<- list(
    colors = list(
      primary = c("#1ba784","#63bbd0","#f87599","#fed71a","#d1c2d3"),
      positive_edges = c("#2376b7","#134857"),
      negative_edges = c("#d2568c","#62102e")
    )
  )
  
  # 变量分组配色已内联到网络分析函数中
  
  NETWORK_PARAMS <<- list(
    bootstrap_min = 100,
    bootstrap_max = 10000,
    min_sample_size = 30,
    max_variables_items = 50
  )
})

# =============================================================================
# UI 界面定义
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "心理量表网络分析应用"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("首页", tabName = "homepage", icon = icon("home")),
      menuItem("数据上传", tabName = "upload", icon = icon("upload")),
      menuItem("变量构造", tabName = "construct", icon = icon("calculator")),
      menuItem("变量选择", tabName = "variables", icon = icon("check-square")),
      menuItem("网络分析", tabName = "analysis", icon = icon("project-diagram")),
      menuItem("网络温度分析", tabName = "temperature", icon = icon("thermometer-half")),
      menuItem("贝叶斯网络", tabName = "bayesian", icon = icon("brain")),
      menuItem("稳定性分析", tabName = "stability", icon = icon("chart-line")),
      menuItem("样本量计算", tabName = "samplesize", icon = icon("calculator")),
      menuItem("结果下载", tabName = "download", icon = icon("download")),
      menuItem("使用说明", tabName = "help", icon = icon("question-circle"))
    ),
    
    # 添加分析流程提示器
    tags$div(style = "position: fixed; bottom: 10px; left: 10px; width: 210px;",
      div(class = "panel panel-default",
        div(class = "panel-heading",
          tags$h6("🗺️ 分析导航", class = "panel-title", style = "margin: 0; font-size: 12px;")
        ),
        div(class = "panel-body", style = "padding: 8px; font-size: 11px;",
          tags$div(
            "📊 ", tags$strong("第一步："), tags$span("GLASSO网络", style = "color: #337ab7;"), tags$br(),
            "🧠 ", tags$strong("第二步："), tags$span("贝叶斯推理", style = "color: #f0ad4e;"), tags$br(),
            "📝 ", tags$strong("第三步："), tags$span("整理故事", style = "color: #5cb85c;")
          )
        )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      # 首页 - 横断面网络分析研究框架
      tabItem(
        tabName = "homepage",
        fluidRow(
          box(
            title = "横断面网络分析研究框架", status = "primary", solidHeader = TRUE, width = 12,
            div(style = "text-align: center; margin-bottom: 30px;",
              tags$h2("心理量表网络分析应用", style = "color: #3c8dbc; margin-bottom: 10px;"),
              tags$h4("Cross-sectional Network Analysis for Psychological Scales", style = "color: #666; font-style: italic;"),
              tags$hr(),
              tags$p("基于图论的心理构念关系建模与可视化分析平台", style = "font-size: 16px; color: #555;")
            )
          )
        ),
        
        fluidRow(
          # 研究设计框架
          box(
            title = "🏗️ 研究设计框架", status = "info", solidHeader = TRUE, width = 6,
            tags$h4("数据收集设计", style = "color: #3c8dbc;"),
            tags$ul(
              tags$li(tags$strong("横断面设计："), "单时间点数据收集，关注构念间关系模式"),
              tags$li(tags$strong("大样本策略："), "推荐样本量 ≥ 1000，确保网络估计稳定性"),
              tags$li(tags$strong("质量控制："), "多重验证机制，异常值检测与处理")
            ),
            
            tags$h4("测量工具选择", style = "color: #3c8dbc; margin-top: 20px;"),
            tags$ul(
              tags$li(tags$strong("标准化量表："), "使用经过验证的心理测量工具"),
              tags$li(tags$strong("多维度评估："), "涵盖目标构念的不同方面"),
              tags$li(tags$strong("信效度保证："), "确保测量质量与可重复性")
            )
          ),
          
          # 支持的量表类型
          box(
            title = "📊 支持的量表类型 (共71种)", status = "success", solidHeader = TRUE, width = 6,
            div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 12px; font-size: 13px;",
              div(
                tags$h5("🧠 情绪与心理健康", style = "color: #00a65a; font-size: 15px;"),
                tags$p("• PHQ-9 (抑郁症状筛查)", style = "margin: 2px 0;"),
                tags$p("• GAD-7 (广泛性焦虑)", style = "margin: 2px 0;"),
                tags$p("• ERS-21/6 (情绪反应性)", style = "margin: 2px 0;"),
                tags$p("• DERS-16 (情绪调节困难)", style = "margin: 2px 0;"),
                tags$p("• COPE-30 (应对方式)", style = "margin: 2px 0;"),
                tags$p("• SES-10 (自尊量表)", style = "margin: 2px 0;"),
                tags$p("• UCLA-20 (孤独感)", style = "margin: 2px 0;"),
                tags$p("• OCD-20 (强迫症状)", style = "margin: 2px 0;"),
                tags$p("• OCI-18 (强迫行为)", style = "margin: 2px 0;"),
                tags$p("• MHT-100 (心理健康)", style = "margin: 2px 0;")
              ),
              div(
                tags$h5("⚡ 成瘾与冲动行为", style = "color: #00a65a; font-size: 15px;"),
                tags$p("• AUDIT-10 (酒精使用障碍)", style = "margin: 2px 0;"),
                tags$p("• IAT-20 (网络成瘾)", style = "margin: 2px 0;"),
                tags$p("• IGD-9SF (游戏障碍)", style = "margin: 2px 0;"),
                tags$p("• SABAS-6 (智能手机成瘾)", style = "margin: 2px 0;"),
                tags$p("• BSMAS-6 (社交媒体成瘾)", style = "margin: 2px 0;"),
                tags$p("• SVAS-6 (短视频成瘾)", style = "margin: 2px 0;"),
                tags$p("• INAS-6 (小说成瘾)", style = "margin: 2px 0;"),
                tags$p("• OSA-6、CGPS-18 (购物成瘾)", style = "margin: 2px 0;"),
                tags$p("• CSA-6 (色情成瘾)", style = "margin: 2px 0;"),
                tags$p("• YFAS (食物成瘾13/16/35项)", style = "margin: 2px 0;"),
                tags$p("• BIS-11 (冲动性量表)", style = "margin: 2px 0;"),
                tags$p("• BISBAS-18 (行为抑制/激活)", style = "margin: 2px 0;"),
                tags$p("• HRF-18系列 (习惯-奖赏-恐惧)", style = "margin: 2px 0;"),
                tags$p("• DSMIGD9 (游戏激情)", style = "margin: 2px 0;")
              )
            ),
            div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 12px; font-size: 13px; margin-top: 15px;",
              div(
                tags$h5("🏠 人际关系与社会支持", style = "color: #00a65a; font-size: 15px;"),
                tags$p("• IPPA-25 (亲子同伴依恋)", style = "margin: 2px 0;"),
                tags$p("• SUMBU (父母教养方式)", style = "margin: 2px 0;"),
                tags$p("• Friend-20 (朋友支持)", style = "margin: 2px 0;"),
                tags$p("• FFSSS-10 (家庭经济支持)", style = "margin: 2px 0;"),
                tags$p("• SSS-14 (社会支持)", style = "margin: 2px 0;"),
                tags$p("• CBul/CBed (网络霸凌)", style = "margin: 2px 0;"),
                tags$p("• Bul-12 (霸凌受害)", style = "margin: 2px 0;")
              ),
              div(
                tags$h5("🌟 人格特质与复原力", style = "color: #00a65a; font-size: 15px;"),
                tags$p("• CFPS-15 (大五人格1/2/3版)", style = "margin: 2px 0;"),
                tags$p("• RICS-25 (疾病复原力)", style = "margin: 2px 0;"),
                tags$p("• RISC-25 (复原力量表)", style = "margin: 2px 0;"),
                tags$p("• RSCA-27 (成人复原力)", style = "margin: 2px 0;"),
                tags$p("• EIS (民族认同A23/B21)", style = "margin: 2px 0;"),
                tags$p("• CSES-26 (儿童自尊)", style = "margin: 2px 0;")
              )
            ),
            div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 12px; font-size: 13px; margin-top: 15px;",
              div(
                tags$h5("🏥 临床诊断与评估", style = "color: #00a65a; font-size: 15px;"),
                tags$p("• NSSI相关 (DSM-5/K24/变化)", style = "margin: 2px 0;"),
                tags$p("• STBI-101 (自伤思维行为)", style = "margin: 2px 0;"),
                tags$p("• PSQI-19 (睡眠质量)", style = "margin: 2px 0;"),
                tags$p("• COVID-19 (疫情症状)", style = "margin: 2px 0;"),
                tags$p("• MSSMHS-60 (中学生心理)", style = "margin: 2px 0;"),
                tags$p("• PPC-12 (精神分裂)", style = "margin: 2px 0;")
              ),
              div(
                tags$h5("🍽️ 进食与其他行为", style = "color: #00a65a; font-size: 15px;"),
                tags$p("• YFAS (食物成瘾13/16/35项)", style = "margin: 2px 0;"),
                tags$p("• CNCS-10 (邻里环境)", style = "margin: 2px 0;"),
                tags$p("• BAS-10 (行为激活)", style = "margin: 2px 0;"),
                tags$p("• FoMos-8 (购物错失恐惧)", style = "margin: 2px 0;"),
                tags$p("• AAS-5 (动物态度)", style = "margin: 2px 0;"),
                tags$p("• PPS-14 (感知压力)", style = "margin: 2px 0;")
              )
            ),
            div(style = "text-align: center; margin-top: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
              tags$p(tags$strong("💡 智能识别"), " - 应用自动识别数据中的量表结构", style = "margin: 5px 0; color: #666;"),
              tags$p(tags$strong("🔧 灵活配置"), " - 支持汇总/子量表/条目三个层级的分析", style = "margin: 5px 0; color: #666;"),
              tags$p(tags$strong("📈 专业标准"), " - 遵循心理测量学和网络分析最佳实践", style = "margin: 5px 0; color: #666;")
            )
          )
        ),
        
        fluidRow(
          # 统计分析流程
          box(
            title = "📈 四步统计分析流程", status = "warning", solidHeader = TRUE, width = 12,
            div(style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; margin: 20px 0;",
              # 第一步
              div(class = "text-center",
                  div(style = "background: #f39c12; color: white; border-radius: 50%; width: 60px; height: 60px; line-height: 60px; margin: 0 auto 15px; font-size: 24px; font-weight: bold;", "1"),
                  tags$h4("描述性分析", style = "color: #f39c12;"),
                  tags$p("• 样本特征描述", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 变量分布检验", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 缺失值分析", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 量表信效度", style = "font-size: 14px; margin: 5px 0;")
              ),
              # 第二步
              div(class = "text-center",
                  div(style = "background: #00a65a; color: white; border-radius: 50%; width: 60px; height: 60px; line-height: 60px; margin: 0 auto 15px; font-size: 24px; font-weight: bold;", "2"),
                  tags$h4("传统分析", style = "color: #00a65a;"),
                  tags$p("• 相关性分析", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 回归分析", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 中介调节", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 组间比较", style = "font-size: 14px; margin: 5px 0;")
              ),
              # 第三步
              div(class = "text-center",
                  div(style = "background: #3c8dbc; color: white; border-radius: 50%; width: 60px; height: 60px; line-height: 60px; margin: 0 auto 15px; font-size: 24px; font-weight: bold;", "3"),
                  tags$h4("网络核心分析", style = "color: #3c8dbc;"),
                  tags$p("• GLASSO网络估计", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 中心性指标计算", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 网络可视化", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 稳定性检验", style = "font-size: 14px; margin: 5px 0;")
              ),
              # 第四步
              div(class = "text-center",
                  div(style = "background: #dd4b39; color: white; border-radius: 50%; width: 60px; height: 60px; line-height: 60px; margin: 0 auto 15px; font-size: 24px; font-weight: bold;", "4"),
                  tags$h4("拓展分析", style = "color: #dd4b39;"),
                  tags$p("• 桥接网络分析", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 贝叶斯网络推理", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 网络比较测试", style = "font-size: 14px; margin: 5px 0;"),
                  tags$p("• 临床意义解释", style = "font-size: 14px; margin: 5px 0;")
              )
            )
          )
        ),
        
        fluidRow(
          # 网络分析核心方法
          box(
            title = "🧮 网络分析核心方法", status = "primary", solidHeader = TRUE, width = 6,
            tags$h4("GLASSO网络估计", style = "color: #3c8dbc;"),
            tags$ul(
              tags$li(tags$strong("图形LASSO算法："), "稀疏高斯图形模型，控制边的密度"),
              tags$li(tags$strong("EBIC模型选择："), "扩展贝叶斯信息准则优化网络结构"),
              tags$li(tags$strong("偏相关系数："), "控制其他变量后的直接关联强度"),
              tags$li(tags$strong("正则化参数："), "平衡模型复杂度与拟合优度")
            ),
            
            tags$h4("中心性指标体系", style = "color: #3c8dbc; margin-top: 20px;"),
            tags$ul(
              tags$li(tags$strong("强度中心性："), "节点连接边权重之和"),
              tags$li(tags$strong("紧密中心性："), "节点到其他节点的平均最短路径"),
              tags$li(tags$strong("介数中心性："), "节点在最短路径上的频率"),
              tags$li(tags$strong("特征向量中心性："), "考虑邻居节点重要性的递归中心性")
            )
          ),
          
          # 稳定性与验证
          box(
            title = "🔬 稳定性与验证", status = "danger", solidHeader = TRUE, width = 6,
            tags$h4("边稳定性评估", style = "color: #dd4b39;"),
            tags$ul(
              tags$li(tags$strong("Bootstrap重采样："), "评估边权重的置信区间"),
              tags$li(tags$strong("边精度分析："), "检验边是否显著不为零"),
              tags$li(tags$strong("边差异检验："), "比较不同边权重的显著性差异")
            ),
            
            tags$h4("中心性稳定性", style = "color: #dd4b39; margin-top: 20px;"),
            tags$ul(
              tags$li(tags$strong("样本丢弃分析："), "逐步丢弃样本评估中心性指标稳定性"),
              tags$li(tags$strong("CS-coefficient："), "中心性稳定性系数，建议 > 0.25"),
              tags$li(tags$strong("中心性差异："), "不同中心性指标间的显著性检验")
            ),
            
            tags$h4("网络比较检验", style = "color: #dd4b39; margin-top: 20px;"),
            tags$ul(
              tags$li(tags$strong("置换检验："), "Network Comparison Test (NCT)"),
              tags$li(tags$strong("网络结构不变性："), "整体网络结构差异检验"),
              tags$li(tags$strong("全局强度不变性："), "网络连接强度差异检验")
            )
          )
        ),
        
        fluidRow(
          # 结果呈现标准
          box(
            title = "📋 结果呈现标准", status = "success", solidHeader = TRUE, width = 12,
            div(style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 30px;",
              div(
                tags$h4("网络可视化", style = "color: #00a65a;"),
                tags$ul(
                  tags$li("节点大小反映中心性指标"),
                  tags$li("边厚度反映关联强度"),
                  tags$li("颜色编码区分变量类型"),
                  tags$li("布局算法优化可读性")
                )
              ),
              div(
                tags$h4("中心性图表", style = "color: #00a65a;"),
                tags$ul(
                  tags$li("标准化中心性指标对比"),
                  tags$li("置信区间显示不确定性"),
                  tags$li("排序展示相对重要性"),
                  tags$li("多指标综合评估")
                )
              ),
              div(
                tags$h4("稳定性结果", style = "color: #00a65a;"),
                tags$ul(
                  tags$li("Bootstrap置信带可视化"),
                  tags$li("CS-coefficient数值报告"),
                  tags$li("显著性检验结果表"),
                  tags$li("稳定性解释与建议")
                )
              )
            )
          )
        ),
        
        fluidRow(
          # 快速开始指南
          box(
            title = "🚀 快速开始指南", status = "info", solidHeader = TRUE, width = 12,
            div(style = "background: #f4f4f4; padding: 20px; border-radius: 8px; margin: 15px 0;",
              tags$h4("五步完成您的网络分析：", style = "color: #3c8dbc; margin-bottom: 20px;"),
              div(style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 15px;",
                div(class = "text-center",
                    tags$strong("步骤 1", style = "color: #f39c12; font-size: 16px;"),
                    tags$p("上传数据文件", style = "margin: 10px 0 5px; font-weight: bold;"),
                    tags$small("支持CSV/Excel格式")
                ),
                div(class = "text-center",
                    tags$strong("步骤 2", style = "color: #00a65a; font-size: 16px;"),
                    tags$p("构造量表变量", style = "margin: 10px 0 5px; font-weight: bold;"),
                    tags$small("自动识别量表结构")
                ),
                div(class = "text-center",
                    tags$strong("步骤 3", style = "color: #3c8dbc; font-size: 16px;"),
                    tags$p("选择分析变量", style = "margin: 10px 0 5px; font-weight: bold;"),
                    tags$small("多层级分析选项")
                ),
                div(class = "text-center",
                    tags$strong("步骤 4", style = "color: #dd4b39; font-size: 16px;"),
                    tags$p("运行网络分析", style = "margin: 10px 0 5px; font-weight: bold;"),
                    tags$small("一键生成网络图")
                ),
                div(class = "text-center",
                    tags$strong("步骤 5", style = "color: #605ca8; font-size: 16px;"),
                    tags$p("下载分析结果", style = "margin: 10px 0 5px; font-weight: bold;"),
                    tags$small("图表和数据导出")
                )
              )
            ),
            
            div(style = "text-align: center; margin-top: 25px;",
              actionButton("start_analysis", "开始您的网络分析", 
                          icon = icon("play"), 
                          class = "btn-primary btn-lg",
                          style = "font-size: 18px; padding: 12px 30px;")
            )
          )
        )
      ),
      
      # 数据上传页面
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "数据上传", status = "primary", solidHeader = TRUE, width = 6,
            fileInput("file", "选择数据文件",
                     accept = c(".csv", ".xlsx", ".xls"),
                     placeholder = "支持CSV和Excel格式"),
            
            checkboxInput("header", "包含列名", TRUE),
            
            checkboxInput("remove_outliers", "移除异常值", FALSE),
            
            conditionalPanel(
              condition = "output.fileUploaded",
              h4("数据预览："),
              DT::dataTableOutput("data_preview", height = "300px")
            )
          ),
          
          box(
            title = "数据质量检查", status = "info", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "output.fileUploaded",
              
              # 数据验证结果
              conditionalPanel(
                condition = "output.dataValid == false",
                div(class = "alert alert-danger", 
                    icon("exclamation-triangle"), 
                    " 数据质量问题：",
                    verbatimTextOutput("validation_errors", placeholder = FALSE))
              ),
              
              conditionalPanel(
                condition = "output.dataValid == true && output.hasWarnings == true",
                div(class = "alert alert-warning", 
                    icon("exclamation-triangle"), 
                    " 数据质量警告：",
                    verbatimTextOutput("validation_warnings", placeholder = FALSE))
              ),
              
              conditionalPanel(
                condition = "output.dataValid == true",
                div(class = "alert alert-success", 
                    icon("check-circle"), 
                    " 数据验证通过")
              ),
              
              h4("识别到的量表："),
              verbatimTextOutput("scale_structure"),
              
              # 智能故事预览
              conditionalPanel(
                condition = "output.scalesDetected",
                div(class = "panel panel-info",
                  div(class = "panel-heading",
                    tags$h5("🎯 为您定制的研究故事预览", class = "panel-title")
                  ),
                  div(class = "panel-body",
                    uiOutput("smart_story_preview"),
                    tags$p(class = "text-muted small",
                      "💡 完成分析后，系统将根据实际结果优化您的研究故事！")
                  )
                )
              ),
              
              h4("数据统计："),
              verbatimTextOutput("data_statistics")
            )
          )
        )
      ),
      
      # 变量构造页面
      tabItem(
        tabName = "construct",
        fluidRow(
          box(
            title = "📊 量表配置检测", status = "info", solidHeader = TRUE, width = 6,
            
            conditionalPanel(
              condition = "!output.dataUploaded",
              div(class = "text-center", style = "padding: 30px;",
                icon("upload", class = "fa-2x text-muted"), br(), br(),
                h5("请先上传数据", class = "text-muted"))
            ),
            
            conditionalPanel(
              condition = "output.dataUploaded",
              div(
                h5("🔍 检测到的可用量表："),
                uiOutput("available_scales_list"),
                br(),
                actionButton("detect_scales", "🔄 重新检测量表", class = "btn-info btn-sm")
              )
            )
          ),
          
          box(
            title = "⚙️ 量表计算设置", status = "warning", solidHeader = TRUE, width = 6,
            
            conditionalPanel(
              condition = "!output.scalesDetected",
              div(class = "text-center", style = "padding: 30px;",
                icon("calculator", class = "fa-2x text-muted"), br(), br(),
                h5("检测量表后显示计算选项", class = "text-muted"))
            ),
            
            conditionalPanel(
              condition = "output.scalesDetected",
              div(
                # 显示可用量表选择（如果有的话）
                conditionalPanel(
                  condition = "output.hasAvailableScales",
                  div(
                    h5("选择要计算的量表："),
                    checkboxGroupInput("selected_scales_to_calculate", "",
                                      choices = NULL),
                    br(),
                    
                    h6("📋 计算选项："),
                    checkboxInput("calculate_subscales", "计算子量表/维度分数", value = TRUE),
                    checkboxInput("apply_reverse_coding", "应用反向计分", value = TRUE),
                    
                    sliderInput("min_valid_ratio", "最少有效题目比例",
                               min = 0.5, max = 1.0, value = 0.8, step = 0.1),
                    br()
                  )
                ),
                
                # 如果没有检测到量表，显示提示
                conditionalPanel(
                  condition = "!output.hasAvailableScales",
                  div(class = "alert alert-info",
                    "💡 未检测到完整的预配置量表，但您可以使用手动规则创建自定义量表计算。"
                  )
                ),
                
                # 操作按钮（始终显示）
                div(class = "text-center", 
                  actionButton("show_manual_rules", "📝 手动添加计算规则", 
                              class = "btn-warning btn-sm", width = "48%", 
                              style = "margin-right: 4%;"),
                  conditionalPanel(
                    condition = "output.hasAvailableScales",
                    actionButton("calculate_scales", "📊 应用预配置量表", 
                                class = "btn-success btn-sm", width = "48%")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📝 手动规则管理", status = "primary", solidHeader = TRUE, width = 12,
            conditionalPanel(
              condition = "!output.dataUploaded",
              div(class = "text-center", style = "padding: 20px;",
                h6("上传数据后可管理手动规则", class = "text-muted"))
            ),
            
            conditionalPanel(
              condition = "output.dataUploaded",
              div(
                h6("🎯 已添加的手动规则："),
                uiOutput("manual_rules_list"),
                conditionalPanel(
                  condition = "output.hasManualRules",
                  actionButton("clear_all_manual_rules", "🗑️ 清空所有手动规则", 
                             class = "btn-danger btn-xs", style = "margin-top: 10px;")
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📈 计算结果", status = "success", solidHeader = TRUE, width = 12,
            
            conditionalPanel(
              condition = "!output.scalesCalculated",
              div(class = "text-center", style = "padding: 30px;",
                icon("hourglass-half", class = "fa-2x text-muted"), br(), br(),
                h5("点击上方按钮开始计算", class = "text-muted"))
            ),
            
            conditionalPanel(
              condition = "output.scalesCalculated",
              div(
                uiOutput("calculation_report"),
                br(),
                h5("📋 新增变量预览："),
                DT::dataTableOutput("new_variables_preview")
              )
            )
          )
        )
      ),
      
      # 变量选择页面
      tabItem(
        tabName = "variables",
        fluidRow(
          box(
            title = "🎯 变量层级选择", status = "primary", solidHeader = TRUE, width = 8,
            
            conditionalPanel(
              condition = "output.fileUploaded && output.scalesDetected",
              
              h4("为每个量表选择分析层级："),
              helpText("不同层级适用于不同的研究目标"),
              
              br(),
              uiOutput("advanced_scale_selectors"),
              
              br(),
              div(
                style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px; border: 1px solid #ddd;",
                h5("💡 层级选择建议："),
                tags$ul(
                  tags$li("汇总层：适合研究总体严重程度或整体水平"),
                  tags$li("子量表层：适合验证理论模型或维度关系"), 
                  tags$li("条目层：适合探索详细机制或症状网络")
                )
              )
            ),
            
            conditionalPanel(
              condition = "!output.fileUploaded || !output.scalesDetected",
              div(class = "text-center", style = "padding: 50px;",
                  icon("upload", class = "fa-3x text-muted"), br(), br(),
                  h4("请先上传数据", class = "text-muted"),
                  tags$p("数据上传成功后，这里将显示变量选择选项", class = "text-muted"))
            )
          ),
          
          box(
            title = "📋 分析变量预览", status = "info", solidHeader = TRUE, width = 4,
            
            conditionalPanel(
              condition = "output.variablesSelected",
              
              h5("将要分析的变量："),
              verbatimTextOutput("final_variables_preview"),
              
              br(),
              
              h5("🎨 变量分组配色："),
              helpText("同组量表的变量将使用相同颜色在网络图中显示，便于识别量表聚类。"),
              div(
                class = "alert alert-info", 
                style = "padding: 8px 12px; margin-bottom: 10px; font-size: 12px;",
                tags$strong("💡 使用示例："),
                tags$br(),
                "• 将 HRF18_General 和 PHQ9 合并为 ", tags$strong("\"情绪认知组\""), " → 它们在网络图中显示为相同颜色",
                tags$br(),
                "• 将 AUDIT10 单独设为 ", tags$strong("\"物质使用组\""), " → 使用不同颜色显示",
                tags$br(),
                "• 这样可以快速识别不同心理构念在网络中的聚类模式"
              ),
              div(id = "variable_groups_config",
                  uiOutput("variable_groups_ui")
              ),
              
              br(),
              
              div(class = "text-center",
                  actionButton("confirm_variables", "✓ 确认变量选择", 
                              class = "btn-success btn-lg", 
                              style = "width: 90%;"),
                  br(), br(),
                  tags$p("确认后可以进行网络分析", class = "text-muted")
              )
            ),
            
            conditionalPanel(
              condition = "!output.variablesSelected",
              div(class = "text-center", style = "padding: 50px;",
                  icon("list", class = "fa-3x text-muted"), br(), br(),
                  h5("变量预览", class = "text-muted"),
                  tags$p("选择变量后将在此显示", class = "text-muted"))
            )
          )
        )
      ),
      
      # 网络分析页面
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "网络分析参数", status = "primary", solidHeader = TRUE, width = 3,
            
            conditionalPanel(
              condition = "output.variablesConfirmed",
              
              h4("📊 分析设置"),
              
              numericInput("threshold", "网络阈值", 
                          value = 0.05, min = 0, max = 0.5, step = 0.01),
              helpText("控制显示边的最小强度，推荐0.05"),
              
              br(),
              
              selectInput("network_estimator", "网络估计方法",
                         choices = list(
                           "EBICglasso (推荐)" = "EBICglasso",
                           "MGM (混合数据)" = "mgm", 
                           "Ising (二元数据)" = "IsingFit",
                           "Correlation (相关网络)" = "cor",
                           "Partial Correlation" = "pcor",
                           "TMFG (三角最大滤波图)" = "TMFG"
                         ),
                         selected = "EBICglasso"),
              
              # 动态显示模型说明
              uiOutput("network_estimator_help"),
              
              br(),
              
              checkboxInput("show_edge_labels", "显示边权重", TRUE),
              helpText("在网络图上显示具体的相关系数"),
              
              conditionalPanel(
                condition = "output.hasMultipleGroups",
                br(),
                checkboxInput("enable_bridge_analysis", "启用桥接网络分析", FALSE),
                helpText("分析不同组别间的桥接节点（需要已配置变量分组）"),
                
                conditionalPanel(
                  condition = "input.enable_bridge_analysis",
                  numericInput("bridge_n", "桥接节点数量", 
                              value = 1, min = 1, max = 5, step = 1),
                  helpText("每组识别的桥接节点数量")
                )
              ),
              
              br(),
              
              div(class = "text-center",
                  actionButton("run_analysis", "🚀 开始网络分析", 
                              class = "btn-success btn-lg", 
                              style = "width: 90%;"))
            ),
            
            conditionalPanel(
              condition = "!output.variablesConfirmed",
              div(class = "text-center", style = "padding: 50px;",
                  icon("cog", class = "fa-3x text-muted"), br(), br(),
                  h5("请先选择变量", class = "text-muted"),
                  tags$p("在'变量选择'页面配置分析变量", class = "text-muted"),
                
                # 第一步引导
                div(class = "alert alert-info",
                  tags$h5("🕵️ 侦探故事 - 第一步：发现线索"),
                  tags$p("您正在进行", tags$strong("GLASSO网络分析"), "，这是探索变量关系的第一步："),
                  tags$ul(class = "small",
                    tags$li("📊 识别所有变量间的相关模式（无向关系）"),
                    tags$li("🔍 发现哪些心理症状或行为倾向于共同出现"),
                    tags$li("🎯 为下一步的因果推理提供基础线索")
                  ),
                  tags$p(class = "small text-muted", 
                    "💡 完成这一步后，建议继续进行贝叶斯分析来推断因果方向！")
                ))
            )
          ),
          
          box(
            title = "标准网络图", status = "success", solidHeader = TRUE, width = 9,
            conditionalPanel(
              condition = "output.analysisComplete",
              plotOutput("network_plot", height = "500px")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "中心性分析", status = "info", solidHeader = TRUE, width = 12,
            conditionalPanel(
              condition = "output.analysisComplete",
              plotOutput("centrality_plot", height = "400px")
            )
          )
        ),
        
        # 桥接网络分析独立显示区域
        conditionalPanel(
          condition = "output.hasBridgeAnalysis",
          fluidRow(
            box(
              title = "🌉 桥接网络分析", status = "primary", solidHeader = TRUE, width = 8,
              plotOutput("bridge_network_plot", height = "500px")
            ),
            box(
              title = "桥接分析信息", status = "info", solidHeader = TRUE, width = 4,
              h5("📋 桥接中心性详表"),
              DT::dataTableOutput("bridge_centrality_table"),
              br(),
              downloadButton("download_bridge_centrality", "下载桥接中心性数据", class = "btn-info")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "组间网络差异比较", status = "warning", solidHeader = TRUE, width = 12,
            conditionalPanel(
              condition = "output.analysisComplete",
              
              h4("📊 分组设置"),
              fluidRow(
                column(6,
                  selectInput("group_variable", "选择分组变量",
                             choices = NULL,  # 动态填充
                             multiple = FALSE),
                  helpText("选择用于分组比较的变量（包括原始数据中的分类变量）")
                ),
                column(6,
                  selectInput("group_method", "分组方法",
                             choices = list(
                               "中位数分组" = "median",
                               "均值分组" = "mean", 
                               "三分位数分组" = "tertile",
                               "前后27%分组" = "extreme_27",
                               "自定义阈值" = "custom",
                               "分类变量分组" = "categorical"
                             ), selected = "median"),
                  conditionalPanel(
                    condition = "input.group_method == 'custom'",
                    numericInput("custom_threshold", "自定义阈值", value = 0, step = 0.1)
                  ),
                  conditionalPanel(
                    condition = "input.group_method == 'categorical'",
                    helpText("将按照分类变量的不同值进行分组")
                  )
                )
              ),
              
              br(),
              
              fluidRow(
                column(6,
                  numericInput("permutation_n", "置换检验次数", 
                              value = 1000, min = 100, max = 5000, step = 100),
                  helpText("建议1000-5000次，次数越多结果越稳定但耗时更长")
                ),
                column(6,
                  selectInput("p_adjust_method", "多重比较校正",
                             choices = list(
                               "Benjamini-Hochberg (BH)" = "BH",
                               "Bonferroni" = "bonferroni",
                               "False Discovery Rate (FDR)" = "fdr",
                               "无校正" = "none"
                             ), selected = "BH")
                )
              ),
              
              # 桥接网络比较选项
              conditionalPanel(
                condition = "output.hasBridgeAnalysis",
                hr(),
                h5("🌉 桥接网络比较"),
                checkboxInput("enable_bridge_compare", "启用桥接网络组间比较", FALSE),
                helpText("比较两组之间的桥接节点差异（需要已进行桥接网络分析）")
              ),
              
              br(),
              
              div(class = "text-center",
                  actionButton("run_group_compare", "🔍 执行组间比较分析", 
                              class = "btn-warning btn-lg", 
                              style = "width: 50%;"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "组间比较结果", status = "success", solidHeader = TRUE, width = 12,
            conditionalPanel(
              condition = "output.groupCompareComplete",
              tabsetPanel(
                tabPanel("差异网络图", 
                  plotOutput("group_compare_plot", height = "500px"),
                  br(),
                  div(class = "alert alert-info",
                    h5("📊 图注说明"),
                    tags$p(tags$strong("组间差异网络图解读："), "本图显示两组在网络连接强度上的统计学显著差异"),
                    tags$hr(),
                    tags$ul(
                      tags$li(tags$span(style = "color: #2376b7; font-weight: bold;", "蓝色边："), 
                             "组1连接强度 > 组2连接强度的显著差异"),
                      tags$li(tags$span(style = "color: #d2568c; font-weight: bold;", "红色边："), 
                             "组2连接强度 > 组1连接强度的显著差异"),
                      tags$li(tags$strong("边的粗细："), "表示组间差异的效应大小"),
                      tags$li(tags$strong("显示条件："), "仅显示p < 0.05的统计学显著差异"),
                      tags$li(tags$strong("差异计算："), "组1网络 - 组2网络的边权重差值")
                    ),
                    tags$hr(),
                    tags$p(tags$em("提示："), "蓝色边表示组1在该连接上更强，红色边表示组2在该连接上更强。",
                          "这有助于识别两组在心理网络结构上的核心差异。")
                  )
                ),
                tabPanel("差异矩阵(diff_sig)", DT::dataTableOutput("group_compare_table")),
                tabPanel("P值矩阵(edge_weight_p)", DT::dataTableOutput("p_value_matrix_table")),
                tabPanel("组间统计", DT::dataTableOutput("group_stats_table")),
                conditionalPanel(
                  condition = "output.hasBridgeCompareResult",
                  tabPanel("桥接网络比较",
                    h5("🌉 桥接网络组间比较结果"),
                    plotOutput("bridge_compare_plot", height = "500px"),
                    br(),
                    h5("📊 桥接节点统计"),
                    verbatimTextOutput("bridge_compare_stats"),
                    br(),
                    h5("📋 桥接强度差异表"),
                    DT::dataTableOutput("bridge_diff_table")
                  )
                )
              )
            )
          )
        )
      ),
      
      # 网络温度分析页面
      tabItem(
        tabName = "temperature",
        fluidRow(
          # 参数设置区
          box(
            title = "🌡️ 网络温度分析设置", status = "primary", solidHeader = TRUE, width = 6,
            
            conditionalPanel(
              condition = "!output.variablesConfirmed",
              div(class = "text-center", style = "padding: 30px;",
                icon("check-square", class = "fa-2x text-muted"), br(), br(),
                h5("请先在变量选择页面确认分析变量", class = "text-muted"),
                tags$small("建议流程：数据上传 → 变量构造 → 变量选择 → 网络温度分析")
              )
            ),
            
            conditionalPanel(
              condition = "output.variablesConfirmed",
              
              # 二值化设置
              h5("📊 数据二值化设置"),
              wellPanel(
                fluidRow(
                  column(6,
                    selectInput("temp_binary_method", "二值化方法",
                               choices = list(
                                 "中位数分割" = "median",
                                 "均值分割" = "mean", 
                                 "自定义阈值" = "custom",
                                 "0-1标准化后分割" = "normalize",
                                 "保持原值(如已二值化)" = "keep"
                               ),
                               selected = "median")
                  ),
                  column(6,
                    selectInput("temp_binary_encoding", "编码格式",
                               choices = list(
                                 "0/1编码" = "01",
                                 "-1/1编码" = "neg11"
                               ),
                               selected = "neg11")
                  )
                ),
                
                conditionalPanel(
                  condition = "input.temp_binary_method == 'custom'",
                  numericInput("temp_binary_threshold", "自定义阈值", 
                              value = 0.5, min = -10, max = 10, step = 0.1)
                ),
                
                tags$small("💡 ", tags$strong("提示："), 
                          "Ising模型要求二值数据。中位数分割适用于大多数量表数据。")
              ),
              
              # 分组设置（可选）
              h5("👥 分组分析设置（可选）"),
              wellPanel(
                checkboxInput("enable_grouping", "启用分组分析", value = FALSE),
                
                conditionalPanel(
                  condition = "input.enable_grouping",
                  fluidRow(
                    column(8,
                      uiOutput("temp_group_var_selector")
                    ),
                    column(4,
                      conditionalPanel(
                        condition = "input.temp_group_var == 'Age'",
                        numericInput("age_groups", "年龄分组数", 
                                    value = 2, min = 2, max = 5, step = 1)
                      )
                    )
                  ),
                  tags$small("📝 ", tags$strong("说明："), 
                            "启用分组后将进行多组Ising网络温度比较分析")
                )
              ),
              
              # 高级设置
              h5("⚙️ 高级设置"),
              wellPanel(
                fluidRow(
                  column(6,
                    selectInput("temp_estimator", "估计方法",
                               choices = list("最大似然估计 (ML)" = "ML"),
                               selected = "ML")
                  ),
                  column(6,
                    numericInput("temp_alpha", "显著性水平",
                                value = 0.05, min = 0.01, max = 0.1, step = 0.01)
                  )
                )
              ),
              
              br(),
              actionButton("run_temperature_analysis", "🚀 开始网络温度分析", 
                          class = "btn-primary btn-lg", style = "width: 100%;")
            )
          ),
          
          # 分析进度和状态
          box(
            title = "📈 分析进度", status = "info", solidHeader = TRUE, width = 6,
            
            conditionalPanel(
              condition = "!output.temperatureAnalysisRunning && !output.temperatureAnalysisComplete",
              div(class = "text-center", style = "padding: 50px;",
                icon("play-circle", class = "fa-3x text-muted"), br(), br(),
                h5("点击开始分析按钮运行网络温度分析", class = "text-muted")
              )
            ),
            
            conditionalPanel(
              condition = "output.temperatureAnalysisRunning",
              div(class = "text-center", style = "padding: 20px;",
                div(class = "progress progress-striped active",
                  div(class = "progress-bar progress-bar-primary", role = "progressbar", 
                      style = "width: 100%", "分析进行中...")
                ),
                br(),
                h5("🔥 正在进行网络温度分析...", class = "text-primary"),
                tags$small("这可能需要几分钟时间，请耐心等待")
              )
            ),
            
            conditionalPanel(
              condition = "output.temperatureAnalysisComplete",
              div(
                h5("✅ 分析完成！", class = "text-success"),
                uiOutput("temp_analysis_summary"),
                br(),
                downloadButton("download_temp_results", "📥 下载完整结果", 
                              class = "btn-success btn-sm"),
                br(), br(),
                downloadButton("download_temp_script", "📝 下载完整R脚本", 
                              class = "btn-info btn-sm")
              )
            ),
            
            conditionalPanel(
              condition = "output.temperatureAnalysisError",
              div(class = "alert alert-danger",
                h5("❌ 分析失败"),
                p("请检查数据格式和参数设置，然后重试。"),
                verbatimTextOutput("temp_error_message")
              )
            )
          )
        ),
        
        # 结果展示区 - 无条件显示所有结果
        fluidRow(
          # 分析报告
          box(
            title = "📋 网络温度分析报告", status = "success", solidHeader = TRUE, width = 12,
            htmlOutput("temperature_analysis_report"),
            br(),
            downloadButton("download_temp_report", "📄 下载分析报告", 
                         class = "btn-primary btn-sm")
          )
        ),
        
        # 可视化结果
        fluidRow(
          # 温度比较图
          box(
            title = "🌡️ 网络温度比较", status = "primary", solidHeader = TRUE, width = 6,
            plotOutput("temp_comparison_plot"),
            br(),
            downloadButton("download_temp_plot", "下载温度图", class = "btn-primary btn-sm")
          ),
          
          # 症状协方差热图
          box(
            title = "🔥 症状协方差热图", status = "warning", solidHeader = TRUE, width = 6,
            p("显示分析变量间的协方差矩阵热图，反映症状间的共变关系"),
            plotOutput("temp_network_heatmap"),
            br(),
            downloadButton("download_temp_heatmap", "下载热图", class = "btn-warning btn-sm")
          )
        ),
        
        # 多组分析时的分组网络图
        conditionalPanel(
          condition = "input.enable_grouping",
          fluidRow(
            # 组别1网络图
            box(
              title = "👨 组别1网络图", status = "info", solidHeader = TRUE, width = 6,
              div(id = "group1_title", style = "font-weight: bold; margin-bottom: 10px;", 
                  textOutput("group1_title_text", inline = TRUE)),
              plotOutput("temp_group1_network"),
              br(),
              downloadButton("download_group1_network", "下载组别1网络图", class = "btn-info btn-sm")
            ),
            
            # 组别2网络图  
            box(
              title = "👩 组别2网络图", status = "success", solidHeader = TRUE, width = 6,
              div(id = "group2_title", style = "font-weight: bold; margin-bottom: 10px;", 
                  textOutput("group2_title_text", inline = TRUE)),
              plotOutput("temp_group2_network"),
              br(),
              downloadButton("download_group2_network", "下载组别2网络图", class = "btn-success btn-sm")
            )
          )
        )
      ),
      
      # 贝叶斯网络分析页面
      tabItem(
        tabName = "bayesian",
        fluidRow(
          # 约束规则控制区
          box(
            title = "⚖️ 网络约束规则", status = "warning", solidHeader = TRUE, width = 12,
            
            # 约束规则类型选择
            radioButtons("constraint_mode", "约束规则模式",
                        choices = list(
                          "智能约束 (推荐)" = "smart",
                          "手动约束" = "manual", 
                          "无约束" = "none"
                        ), selected = "smart", inline = TRUE),
            
            # 智能约束配置
            conditionalPanel(
              condition = "input.constraint_mode == 'smart'",
              wellPanel(
                h5("🤖 智能约束规则"),
                div(class = "alert alert-info",
                  tags$p(class = "small mb-1", 
                    tags$strong("智能约束说明："), "系统根据心理测量学理论自动生成约束规则，提高网络结构的合理性")),
                
                checkboxGroupInput("smart_constraints",
                                  label = "选择约束类型",
                                  choices = list(
                                    "量表间理论约束 (AUDIT→HRF等)" = "inter_scale",
                                    "同量表内远程约束 (题目1不直接影响题目10)" = "intra_scale_distant",
                                    "逻辑时序约束 (基于题目逻辑顺序)" = "temporal_logic",
                                    "维度内聚约束 (同维度题目优先连接)" = "dimension_cohesion"
                                  ),
                                  selected = c("inter_scale", "intra_scale_distant")),
                
                # 详细约束说明
                div(class = "panel panel-default",
                  div(class = "panel-body", style = "padding: 10px;",
                    tags$small(
                      tags$strong("📋 约束规则详解："), tags$br(),
                      "🔹 ", tags$strong("量表间理论约束"), "：禁止理论上不合理的因果关系（如AUDIT影响HRF动机形成）", tags$br(),
                      "🔹 ", tags$strong("远程约束"), "：防止同一量表中相距4个以上位置的题目直接连接", tags$br(), 
                      "🔹 ", tags$strong("时序约束"), "：确保因果关系符合逻辑顺序（题目N+1不能影响题目N）", tags$br(),
                      "🔹 ", tags$strong("内聚约束"), "：促进同一维度的相邻题目优先建立连接"
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.smart_constraints && input.smart_constraints.indexOf('inter_scale') != -1",
                  sliderInput("inter_scale_strength", "量表间约束强度",
                             min = 0, max = 1, value = 0.8, step = 0.1)
                )
              )
            ),
            
            # 手动约束配置
            conditionalPanel(
              condition = "input.constraint_mode == 'manual'",
              fluidRow(
                column(6,
                  wellPanel(
                    h5("🚫 黑名单 (禁止连接)"),
                    textAreaInput("manual_blacklist", 
                                 label = "禁止的边 (from,to 格式，每行一个)",
                                 placeholder = "AUDIT10_1,HRF18_1\nAUDIT10_2,HRF18_2\nPHQ9_1,GAD7_1",
                                 rows = 6),
                    actionButton("validate_blacklist", "验证黑名单", class = "btn-warning btn-sm"),
                    br(), br(),
                    verbatimTextOutput("blacklist_status")
                  )
                ),
                column(6,
                  wellPanel(
                    h5("✅ 白名单 (强制连接)"),
                    textAreaInput("manual_whitelist",
                                 label = "强制的边 (from,to 格式，每行一个)", 
                                 placeholder = "HRF18_1,HRF18_2\nPHQ9_1,PHQ9_2",
                                 rows = 6),
                    actionButton("validate_whitelist", "验证白名单", class = "btn-success btn-sm"),
                    br(), br(),
                    verbatimTextOutput("whitelist_status")
                  )
                )
              )
            ),
            
            # 约束规则预览
            conditionalPanel(
              condition = "input.constraint_mode != 'none'",
              wellPanel(
                h5("📋 当前约束规则预览"),
                fluidRow(
                  column(4,
                    h6("黑名单规则数量:"), 
                    verbatimTextOutput("blacklist_count")
                  ),
                  column(4,
                    h6("白名单规则数量:"),
                    verbatimTextOutput("whitelist_count") 
                  ),
                  column(4,
                    br(),
                    actionButton("preview_constraints", "📊 预览约束", class = "btn-info btn-sm")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          # 参数配置区
          box(
            title = "🧠 贝叶斯网络参数", status = "primary", solidHeader = TRUE, width = 4,
            
            selectInput("bn_algorithm", "学习算法",
                       choices = list(
                         "Hill Climbing (推荐)" = "hc",
                         "Tabu Search" = "tabu", 
                         "PC Algorithm" = "pc",
                         "IAMB" = "iamb",
                         "IAMB-FDR (稀疏网络推荐)" = "iamb.fdr",
                         "MMHC (混合方法)" = "mmhc",
                         "RSMAX2 (连续变量)" = "rsmax2"
                       ), selected = "hc"),
            
            selectInput("score_function", "评分函数", 
                       choices = list(
                         "BGe (贝叶斯高斯)" = "bge",
                         "BIC-G (高斯BIC)" = "bic-g",
                         "AIC-G (高斯AIC)" = "aic-g",
                         "对数似然-G (高斯)" = "loglik-g"
                       ), selected = "bge"),
            
            numericInput("bootstrap_rounds", "Bootstrap轮数",
                        value = 1000, min = 500, max = 10000, step = 500),
            
            numericInput("strength_threshold", "边强度阈值", 
                        value = 0.85, min = 0.5, max = 1.0, step = 0.05),
            
            numericInput("direction_threshold", "方向阈值",
                        value = 0.5, min = 0.3, max = 1.0, step = 0.05),
            
            checkboxInput("enable_cv", "启用交叉验证", TRUE),
            
            helpText("💡 算法选择建议："),
            helpText("• 变量<10且边稀疏: IAMB-FDR"),
            helpText("• 变量10-20: Hill Climbing"),
            helpText("• 混合数据类型: MMHC"),
            helpText("• 纯连续变量: RSMAX2"),
            
            br(),
            actionButton("run_bayesian", "🚀 运行贝叶斯分析", 
                        class = "btn-primary btn-lg", width = "100%")
          ),
          
          # 结果展示区
          box(
            title = "📊 分析状态", status = "info", solidHeader = TRUE, width = 8,
            conditionalPanel(
              condition = "!output.bayesianComplete",
              div(
                style = "text-align: center; padding: 50px;",
                h4("请配置参数并点击运行分析"),
                
                # 第二步引导  
                div(class = "alert alert-warning",
                  tags$h5("🧠 侦探故事 - 第二步：推理方向"),
                  tags$p("现在进行", tags$strong("贝叶斯网络分析"), "，基于第一步的发现推断因果方向："),
                  tags$ul(class = "small",
                    tags$li("🔄 将无向的相关关系转换为有向的因果关系"),
                    tags$li("⚖️ 利用统计学和心理学理论约束推理过程"),
                    tags$li("📈 识别哪个变量更可能是\"原因\"，哪个是\"结果\"")
                  ),
                  tags$p(class = "small", 
                    tags$strong("举例："), "网络分析发现", tags$em("酒精使用"), "和", tags$em("抑郁症状"), "相关，",
                    "贝叶斯分析帮您判断是", tags$em("酒精→抑郁"), "还是", tags$em("抑郁→酒精"), "！")
                ),
                
                tags$p("贝叶斯网络分析将识别变量间的有向因果关系"),
                conditionalPanel(
                  condition = "!output.bayesianReady",
                  div(class = "alert alert-warning",
                    "⚠️ 需要先上传数据并通过李克特量表验证")
                )
              )
            ),
            conditionalPanel(
              condition = "output.bayesianComplete",
              tabsetPanel(
                id = "bayesian_results",
                tabPanel("网络结构", 
                  h5("🧠 学习的因果网络结构"),
                  plotOutput("bayesian_network_plot", height = "500px"),
                  hr(),
                  h6("📊 网络统计"),
                  verbatimTextOutput("bayesian_network_stats")
                ),
                tabPanel("Bootstrap稳定性", 
                  h5("🔄 Bootstrap稳定性分析"),
                  plotOutput("bayesian_stability_plot", height = "500px"),
                  hr(),
                  h6("📈 稳定性指标"),
                  verbatimTextOutput("stability_metrics")
                ),
                tabPanel("平均网络",
                  h5("📊 Bootstrap平均网络"),
                  plotOutput("bayesian_averaged_plot", height = "500px"),
                  hr(),
                  h6("✨ 与GLASSO网络对比"),
                  verbatimTextOutput("network_comparison")
                ),
                tabPanel("边强度表", 
                  h5("📋 稳定边强度详表"),
                  DT::dataTableOutput("bayesian_edges_table"),
                  br(),
                  downloadButton("download_bayesian_edges", "下载边强度数据", class = "btn-info")
                ),
                tabPanel("模型评估",
                  h5("🎯 模型评估指标"),
                  fluidRow(
                    column(6,
                      h6("📊 评分指标"),
                      verbatimTextOutput("model_scores")
                    ),
                    column(6,
                      h6("🔍 交叉验证"),
                      plotOutput("cv_plot", height = "300px")
                    )
                  ),
                  hr(),
                  h6("📈 特征值分析"),
                  plotOutput("eigenvalue_plot", height = "300px")
                ),
                tabPanel("参数拟合",
                  h5("🔧 条件概率分布"),
                  plotOutput("bn_fit_plot", height = "500px"),
                  hr(),
                  h6("📊 残差分析"),
                  verbatimTextOutput("residual_analysis")
                ),
                tabPanel("分析报告", 
                  h5("📄 完整分析报告"),
                  uiOutput("bayesian_report")
                )
              )
            )
          )
        ),
        
        # 下载区
        conditionalPanel(
          condition = "output.bayesianComplete",
          fluidRow(
            box(
              title = "📥 结果下载", status = "success", solidHeader = TRUE, width = 12,
              column(3, downloadButton("download_bn_plot", "网络图", class = "btn-success")),
              column(3, downloadButton("download_bn_stability", "稳定性图", class = "btn-success")), 
              column(3, downloadButton("download_bn_edges", "边强度数据", class = "btn-success")),
              column(3, downloadButton("download_bn_report", "完整报告", class = "btn-success"))
            )
          )
        )
      ),
      
      # 稳定性分析页面（独立版块）
      tabItem(
        tabName = "stability",
        fluidRow(
          box(
            title = "稳定性分析设置", status = "primary", solidHeader = TRUE, width = 3,
            
            conditionalPanel(
              condition = "output.analysisComplete",
              
              numericInput("stability_bootstrap", "Bootstrap次数", 
                          value = 1000, min = 100, max = 5000, step = 100),
              
              checkboxInput("run_edge_stability", "边稳定性分析", TRUE),
              checkboxInput("run_centrality_stability", "中心性稳定性", TRUE),
              
              actionButton("run_stability", "运行稳定性分析", class = "btn-warning"),
              
              br(), br(),
              
              conditionalPanel(
                condition = "output.stabilityComplete",
                downloadButton("download_stability", "下载稳定性报告", class = "btn-info")
              )
            ),
            
            conditionalPanel(
              condition = "!output.analysisComplete",
              tags$p("请先完成网络分析")
            )
          ),
          
          box(
            title = "稳定性结果", status = "warning", solidHeader = TRUE, width = 9,
            conditionalPanel(
              condition = "output.stabilityComplete",
              tabsetPanel(
                tabPanel("边稳定性", plotOutput("edge_stability_plot", height = "400px")),
                tabPanel("中心性稳定性", plotOutput("centrality_stability_plot", height = "400px")),
                tabPanel("稳定性摘要", verbatimTextOutput("stability_summary"))
              )
            ),
            
            conditionalPanel(
              condition = "!output.stabilityComplete && output.analysisComplete",
              div(class = "text-center", style = "padding: 100px;",
                  icon("chart-line", class = "fa-3x text-muted"), br(), br(),
                  tags$p("点击左侧按钮开始稳定性分析", class = "text-muted"))
            )
          )
        )
      ),
      
      # 样本量计算页面
      tabItem(
        tabName = "samplesize",
        fluidRow(
          box(
            title = "样本量计算设置", status = "primary", solidHeader = TRUE, width = 4,
            
            conditionalPanel(
              condition = "output.analysisComplete",
              
              h4("🔬 基于当前网络的样本量分析"),
              
              tags$div(style = "background-color: #f4f4f4; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                h5("网络特征信息", style = "margin-top: 0;"),
                verbatimTextOutput("network_features_info", placeholder = TRUE),
                
                # 添加调试信息展开按钮
                conditionalPanel(
                  condition = "output.analysisComplete",
                  br(),
                  actionButton("show_debug_info", "显示调试信息", class = "btn-xs btn-default", 
                              style = "font-size: 10px;"),
                  conditionalPanel(
                    condition = "input.show_debug_info % 2 == 1",
                    br(), br(),
                    tags$div(style = "background-color: #fff; padding: 8px; border: 1px solid #ddd; border-radius: 3px; font-size: 10px;",
                      h6("调试信息：", style = "margin-top: 0; color: #666;"),
                      verbatimTextOutput("debug_network_info", placeholder = TRUE)
                    )
                  )
                )
              ),
              
              h5("分析参数设置"),
              
              selectInput("powerly_preset", "预设配置", 
                         choices = list(
                           "平衡设置 (推荐)" = "balanced",
                           "保守设置" = "conservative", 
                           "探索性设置" = "exploratory"
                         ),
                         selected = "balanced"),
              
              numericInput("target_sensitivity", "目标敏感性", 
                          value = 0.6, min = 0.3, max = 0.9, step = 0.05),
              
              numericInput("target_power", "目标功效", 
                          value = 0.8, min = 0.6, max = 0.95, step = 0.05),
              
              fluidRow(
                column(6, numericInput("sample_range_lower", "样本量下限", 
                                     value = 300, min = 50, max = 1000, step = 50)),
                column(6, numericInput("sample_range_upper", "样本量上限", 
                                     value = 2000, min = 500, max = 5000, step = 100))
              ),
              
              numericInput("powerly_boots", "Bootstrap次数", 
                          value = 1000, min = 200, max = 5000, step = 200),
              
              fluidRow(
                column(6, numericInput("powerly_cores", "并行核心数", 
                                     value = 2, min = 1, max = 6, step = 1)),
                column(6, numericInput("powerly_iterations", "最大迭代数", 
                                     value = 10, min = 5, max = 20, step = 1))
              ),
              
              br(),
              
              actionButton("run_sample_size", "开始样本量计算", 
                          class = "btn-warning btn-block", 
                          style = "font-weight: bold;"),
              
              br(), br(),
              
              conditionalPanel(
                condition = "output.sampleSizeComplete",
                tags$div(
                  h5("💡 快速应用推荐样本量"),
                  tags$p("基于计算结果的研究设计建议：", style = "font-size: 12px; color: #666;"),
                  verbatimTextOutput("sample_size_recommendation", placeholder = TRUE),
                  br(),
                  downloadButton("download_sample_size", "下载样本量报告", class = "btn-info btn-block")
                )
              )
            ),
            
            conditionalPanel(
              condition = "!output.analysisComplete",
              tags$div(style = "text-align: center; padding: 30px;",
                icon("calculator", class = "fa-3x text-muted"), br(), br(),
                tags$p("请先完成网络分析", class = "text-muted", style = "font-size: 14px;"),
                tags$p("样本量计算需要基于网络密度和节点数进行", class = "text-muted", style = "font-size: 12px;")
              )
            )
          ),
          
          box(
            title = "样本量分析结果", status = "warning", solidHeader = TRUE, width = 8,
            
            conditionalPanel(
              condition = "output.sampleSizeComplete",
              tabsetPanel(
                id = "sample_size_tabs",
                
                tabPanel("分析概览",
                  tags$div(style = "padding: 20px;",
                    h4("📊 样本量推荐结果"),
                    verbatimTextOutput("sample_size_summary"),
                    
                    br(),
                    
                    tags$div(style = "background-color: #dff0d8; padding: 15px; border-radius: 5px; border-left: 4px solid #5cb85c;",
                      h5("📋 研究设计建议", style = "color: #3c763d; margin-top: 0;"),
                      htmlOutput("research_design_suggestions")
                    )
                  )
                ),
                
                tabPanel("Step 1: 蒙特卡洛模拟",
                  tags$div(style = "padding: 10px;",
                    h5("样本量 vs 性能测量散点图"),
                    plotOutput("powerly_step1_plot", height = "400px"),
                    tags$p("显示不同样本量下的网络检测性能", class = "text-muted", style = "font-size: 12px;")
                  )
                ),
                
                tabPanel("Step 2: 曲线拟合",
                  tags$div(style = "padding: 10px;",
                    h5("单调曲线拟合和插值"),
                    plotOutput("powerly_step2_plot", height = "400px"),
                    tags$p("平滑的性能曲线和置信带", class = "text-muted", style = "font-size: 12px;")
                  )
                ),
                
                tabPanel("Step 3: Bootstrap分布",
                  tags$div(style = "padding: 10px;",
                    h5("样本量推荐的不确定性"),
                    plotOutput("powerly_step3_plot", height = "400px"),
                    tags$p("Bootstrap分布显示推荐样本量的置信区间", class = "text-muted", style = "font-size: 12px;")
                  )
                ),
                
                tabPanel("详细报告",
                  tags$div(style = "padding: 20px;",
                    h5("样本量分析详细报告"),
                    verbatimTextOutput("detailed_sample_report"),
                    
                    br(),
                    
                    tags$div(style = "background-color: #f5f5f5; padding: 10px; border-radius: 3px;",
                      h6("方法学说明："),
                      tags$p("本分析基于Constantin等人(2021)开发的三步递归蒙特卡洛方法，专门用于网络模型的样本量计算。", 
                             style = "font-size: 12px; margin-bottom: 5px;"),
                      tags$p("参考文献：Constantin, M. A., Schuurman, N. K., & Vermunt, J. (2021). A General Monte Carlo Method for Sample Size Analysis in the Context of Network Models.", 
                             style = "font-size: 11px; color: #666; margin-bottom: 0;")
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "!output.sampleSizeComplete && output.analysisComplete",
              tags$div(style = "text-align: center; padding: 100px;",
                icon("calculator", class = "fa-3x text-muted"), br(), br(),
                tags$h4("基于当前网络进行样本量分析", class = "text-muted"),
                tags$p("点击左侧按钮开始计算推荐样本量", class = "text-muted"),
                br(),
                tags$div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 20px;",
                  tags$h6("💡 样本量计算说明"),
                  tags$ul(style = "text-align: left; font-size: 12px; color: #666;",
                    tags$li("自动提取当前网络的节点数和连接密度"),
                    tags$li("使用蒙特卡洛方法模拟不同样本量下的网络检测性能"),
                    tags$li("提供基于统计功效的样本量推荐"),
                    tags$li("生成完整的分析报告和可视化结果")
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "!output.analysisComplete",
              tags$div(style = "text-align: center; padding: 100px;",
                icon("exclamation-circle", class = "fa-3x text-muted"), br(), br(),
                tags$p("需要先完成网络分析才能进行样本量计算", class = "text-muted")
              )
            )
          )
        )
      ),
      
      # 结果下载页面
      tabItem(
        tabName = "download",
        fluidRow(
          box(
            title = "下载选项", status = "primary", solidHeader = TRUE, width = 12,
            
            conditionalPanel(
              condition = "output.analysisComplete",
              h4("📁 一键下载所有结果："),
              downloadButton("download_all_results", "📦 下载完整结果包 (ZIP)", class = "btn-success btn-lg", style = "margin-bottom: 20px;"),
              
              hr(),
              
              h4("📊 单独下载文件："),
              br(),
              downloadButton("download_network_plot", "下载网络图 (PDF)", class = "btn-primary"),
              br(), br(),
              conditionalPanel(
                condition = "output.hasBridgeAnalysis",
                downloadButton("download_bridge_plot", "下载桥接网络图 (PDF)", class = "btn-primary"),
                br(), br(),
                downloadButton("download_bridge_centrality", "下载桥接中心性图 (PDF)", class = "btn-primary"),
                br(), br(),
                downloadButton("download_bridge_data", "下载桥接分析结果 (CSV)", class = "btn-info"),
                br(), br()
              ),
              downloadButton("download_centrality_plot", "下载中心性图 (PDF)", class = "btn-primary"),
              br(), br(),
              conditionalPanel(
                condition = "output.groupCompareComplete",
                downloadButton("download_compare_plot", "下载网络比较图 (PDF)", class = "btn-primary"),
                br(), br(),
                downloadButton("download_compare_diff", "下载差异数据 (CSV)", class = "btn-info"),
                br(), br(),
                downloadButton("download_compare_pval", "下载P值数据 (CSV)", class = "btn-info"),
                br(), br()
              ),
              downloadButton("download_data", "下载分析数据 (CSV)", class = "btn-info"),
              br(), br(),
              downloadButton("download_code", "📝 下载分析代码 (R)", class = "btn-warning"),
              br(), br(),
              downloadButton("download_report", "下载分析报告 (HTML)", class = "btn-success")
            )
          )
        )
      ),
      
      # 使用说明页面
      tabItem(
        tabName = "help",
        fluidRow(
          box(
            title = "使用说明", status = "primary", solidHeader = TRUE, width = 12,
            
            h3("数据格式要求"),
            tags$p("1. 支持CSV和Excel文件格式"),
            tags$p("2. 每行代表一个被试，每列代表一个测量指标"),
            tags$p("3. 变量命名规范："),
            tags$ul(
              tags$li("AUDIT量表：AUDIT10_1, AUDIT10_2, ... AUDIT10_10"),
              tags$li("HRF量表：HRF18_1, HRF18_2, ... HRF18_18"),
              tags$li("PHQ量表：PHQ9_1, PHQ9_2, ... PHQ9_9"),
              tags$li("其他量表：[量表名]_[题号]，如 BDI_1, BDI_2...")
            ),
            
            h3("分析层级说明"),
            tags$ul(
              tags$li("汇总层：使用量表总分或维度得分构建网络"),
              tags$li("子量表层：使用各个维度得分构建网络"),
              tags$li("条目层：使用原始题目得分构建网络")
            ),
            
            h3("参数设置"),
            tags$ul(
              tags$li("网络阈值：控制显示的边的最小强度，建议0.05"),
              tags$li("Bootstrap次数：用于稳定性检验，建议1000-5000次")
            ),
            
            h3("示例数据"),
            # 添加分析思路引导故事
            div(class = "panel panel-info",
              div(class = "panel-heading",
                tags$h4("🕵️ 心理网络分析的侦探故事", class = "panel-title")
              ),
              div(class = "panel-body",
                tags$h5("📖 如何用这个工具讲述变量关系的故事？"),
                tags$div(class = "well well-sm",
                  tags$p(class = "lead", "假设您想研究：", tags$strong("酒精使用、动机模式和抑郁症状之间的复杂关系")),
                  
                  tags$h6("🎯 第一步：发现线索（网络分析）"),
                  tags$ul(
                    tags$li("使用", tags$strong("GLASSO网络分析"), "找出所有变量间的相关模式"),
                    tags$li("就像侦探收集案件中所有人物的关系线索"),
                    tags$li("结果：一张显示哪些变量彼此相关的", tags$em("无向网络图"))
                  ),
                  
                  tags$h6("🧠 第二步：推理方向（贝叶斯分析）"),
                  tags$ul(
                    tags$li("使用", tags$strong("贝叶斯网络分析"), "推断因果关系的方向"),
                    tags$li("就像侦探分析：是A导致B，还是B导致A？"),
                    tags$li("结果：一张显示变量间", tags$em("有向因果关系"), "的网络图")
                  ),
                  
                  tags$h6("📝 第三步：整理故事（结果解释）"),
                  tags$ul(
                    tags$li("结合两种分析，构建完整的理论模型"),
                    tags$li("例如：", tags$em("恐惧动机 → 酒精使用 → 抑郁症状")),
                    tags$li("为干预策略提供科学依据")
                  )
                ),
                
                tags$div(class = "alert alert-success",
                  tags$strong("💡 实践建议："), 
                  "先运行网络分析探索变量关系，再用贝叶斯分析验证因果假设！"
                )
              )
            ),
            
            tags$p("可以下载示例数据文件来了解正确的数据格式："),
            downloadButton("download_example", "下载示例数据", class = "btn-info")
          )
        )
      )
    )
  )
)

# =============================================================================
# Server 逻辑定义
# =============================================================================

server <- function(input, output, session) {
  
  # 自动保存结果的通用函数
  auto_save_result <- function(result_type, result_object = NULL, plot_object = NULL, 
                              data_frame = NULL, filename_prefix = "", width = 800, height = 600) {
    if(is.null(values$output_folder)) return()
    
    timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
    
    tryCatch({
      # 保存图片
      if(!is.null(plot_object)) {
        plot_file <- file.path(values$output_folder, paste0(filename_prefix, "_", timestamp, ".png"))
        png(plot_file, width = width, height = height, res = 150)
        print(plot_object)
        dev.off()
        cat("已保存图片:", plot_file, "\n")
      }
      
      # 保存数据框
      if(!is.null(data_frame)) {
        csv_file <- file.path(values$output_folder, paste0(filename_prefix, "_", timestamp, ".csv"))
        write.csv(data_frame, csv_file, row.names = FALSE)
        cat("已保存数据:", csv_file, "\n")
      }
      
      # 保存RDS对象
      if(!is.null(result_object)) {
        rds_file <- file.path(values$output_folder, paste0(filename_prefix, "_", timestamp, ".rds"))
        saveRDS(result_object, rds_file)
        cat("已保存对象:", rds_file, "\n")
      }
      
    }, error = function(e) {
      cat("自动保存失败 (", result_type, "):", e$message, "\n")
    })
  }
  
  # 反应性数据存储
  values <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    scales = NULL,
    validation_result = NULL,
    network_result = NULL,
    centrality_result = NULL,
    stability_result = NULL,
    analysis_data = NULL,
    # 新增变量构造相关
    scale_config = NULL,
    available_scales = NULL,
    calculated_scales = NULL,
    bayesian_result = NULL,
    # 桥接分析相关
    bridge_result = NULL,
    bridge_network = NULL,
    bridge_groups = NULL,
    group_compare_result = NULL,
    bridge_compare_result = NULL,
    # 文件输出相关
    upload_timestamp = NULL,
    output_folder = NULL
  )
  
  # 加载量表配置
  observe({
    tryCatch({
      values$scale_config <- load_scale_config("scale_config_complete.csv")
    }, error = function(e) {
      # 如果无法加载配置文件，使用空配置
      values$scale_config <- data.frame()
    })
  })
  
  # 文件上传处理
  observeEvent(input$file, {
    req(input$file)
    
    withProgress(message = '正在读取数据...', value = 0, {
      
      ext <- tools::file_ext(input$file$datapath)
      
      tryCatch({
        if(ext == "csv") {
          values$raw_data <- read.csv(input$file$datapath, header = input$header, 
                                    stringsAsFactors = FALSE, check.names = FALSE)
        } else if(ext %in% c("xlsx", "xls")) {
          values$raw_data <- read_excel(input$file$datapath, col_names = input$header)
          values$raw_data <- as.data.frame(values$raw_data)
        }
        # 移除只选择数值列的限制，让用户自己决定
        incProgress(0.3, detail = "验证数据质量...")
        
        # 使用简化的数据验证（只检查缺失值和数值类型）
        values$validation_result <- validate_data(values$raw_data)
        
        if(values$validation_result$valid) {
          incProgress(0.5, detail = "处理数据类型...")
          
          # 设置上传时间戳并创建输出文件夹
          values$upload_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          values$output_folder <- file.path(getwd(), paste0("results_", values$upload_timestamp))
          
          # 创建输出文件夹
          if(!dir.exists(values$output_folder)) {
            dir.create(values$output_folder, recursive = TRUE)
            cat("创建输出文件夹:", values$output_folder, "\n")
          }
          
          # 使用处理后的数据（已自动转换数值类型）
          values$processed_data <- values$validation_result$processed_data
          
          incProgress(0.7, detail = "检测可用量表...")
          
          # 检测可用量表
          if(!is.null(values$scale_config) && nrow(values$scale_config) > 0) {
            values$available_scales <- detect_available_scales(values$processed_data, values$scale_config)
          } else {
            values$available_scales <- list()
          }
          
          incProgress(1, detail = "完成!")
          
          showNotification("数据加载成功！", type = "message")
        } else {
          showNotification("数据验证失败，请检查数据质量", type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("文件读取失败:", e$message), type = "error")
        values$raw_data <- NULL
      })
    })
  })
  
  # 判断文件是否已上传
  output$fileUploaded <- reactive({
    return(!is.null(values$raw_data))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # 数据验证状态输出
  output$dataValid <- reactive({
    if(is.null(values$validation_result)) return(FALSE)
    return(values$validation_result$valid)
  })
  outputOptions(output, "dataValid", suspendWhenHidden = FALSE)
  
  output$hasWarnings <- reactive({
    if(is.null(values$validation_result)) return(FALSE)
    return(length(values$validation_result$warnings) > 0)
  })
  outputOptions(output, "hasWarnings", suspendWhenHidden = FALSE)
  
  # 手动规则状态
  output$hasManualRules <- reactive({
    if(is.null(values$calculated_scales) || is.null(values$calculated_scales$summary)) {
      return(FALSE)
    }
    manual_rules <- sapply(values$calculated_scales$summary, function(x) !is.null(x$is_manual) && x$is_manual)
    return(any(manual_rules))
  })
  outputOptions(output, "hasManualRules", suspendWhenHidden = FALSE)
  
  output$scalesDetected <- reactive({
    # 在变量构造页面，检查available_scales或者允许手动规则
    return((!is.null(values$available_scales) && length(values$available_scales) > 0) || 
           !is.null(values$processed_data))
  })
  outputOptions(output, "scalesDetected", suspendWhenHidden = FALSE)
  
  # 是否有可用的预配置量表
  output$hasAvailableScales <- reactive({
    return(!is.null(values$available_scales) && length(values$available_scales) > 0)
  })
  outputOptions(output, "hasAvailableScales", suspendWhenHidden = FALSE)
  
  # 数据上传状态（用于变量构造页面）
  output$dataUploaded <- reactive({
    return(!is.null(values$processed_data))
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  # 量表计算完成状态
  output$scalesCalculated <- reactive({
    return(!is.null(values$calculated_scales))
  })
  outputOptions(output, "scalesCalculated", suspendWhenHidden = FALSE)
  
  # 验证错误信息
  output$validation_errors <- renderText({
    req(values$validation_result)
    if(length(values$validation_result$errors) > 0) {
      paste(values$validation_result$errors, collapse = "\n")
    }
  })
  
  # 验证警告信息
  output$validation_warnings <- renderText({
    req(values$validation_result)
    if(length(values$validation_result$warnings) > 0) {
      paste(values$validation_result$warnings, collapse = "\n")
    }
  })
  
  # 数据预览
  output$data_preview <- DT::renderDataTable({
    req(values$raw_data)
    DT::datatable(
      head(values$raw_data, 200), 
      options = list(
        scrollX = TRUE,
        scrollY = "250px",
        pageLength = 10,
        dom = 'tip'
      ),
      rownames = FALSE
    )
  })
  
  # 显示量表结构
  output$scale_structure <- renderText({
    req(values$scales)
    
    if(length(values$scales) == 0) {
      return("未识别到标准量表结构，请检查变量命名规范")
    }
    
    result <- ""
    for(scale_name in names(values$scales)) {
      scale_info <- values$scales[[scale_name]]
      result <- paste0(result, "📊 ", scale_info$name, "\n")
      result <- paste0(result, "   条目数: ", length(scale_info$items), "\n")
      result <- paste0(result, "   维度数: ", length(scale_info$subscales), "\n")
      
      for(sub_name in names(scale_info$subscales)) {
        sub_info <- scale_info$subscales[[sub_name]]
        result <- paste0(result, "   ➤ ", sub_name, " (", length(sub_info$items), "题)\n")
        if(!is.null(sub_info$description)) {
          result <- paste0(result, "     ", sub_info$description, "\n")
        }
      }
      result <- paste0(result, "\n")
    }
    
    return(result)
  })
  
  # 数据统计信息
  output$data_statistics <- renderText({
    req(values$validation_result)
    
    stats <- values$validation_result$statistics
    
    paste0(
      "样本量: ", stats$n_subjects, " 个被试\n",
      "变量数: ", stats$n_variables, " 个变量\n",
      "完整观测: ", stats$complete_cases, " 例\n", 
      "数据完整率: ", round(stats$complete_rate * 100, 1), "%\n",
      "平均缺失率: ", round(mean(stats$missing_rates) * 100, 1), "%"
    )
  })
  
  # 量表检测状态 (已在上方定义，此处删除重复)
  
  # 智能故事预览
  output$smart_story_preview <- renderUI({
    req(values$scales)
    
    tryCatch({
      # 生成基础故事（不包含分析结果）
      story_html <- generate_smart_story(values$scales, NULL, NULL)
      HTML(story_html)
    }, error = function(e) {
      div(class = "alert alert-warning",
        "故事生成暂时不可用，请继续进行分析")
    })
  })
  
  # =============================================================================
  # 变量构造模块服务器端逻辑
  # =============================================================================
  
  # 可用量表列表显示
  output$available_scales_list <- renderUI({
    if(is.null(values$available_scales) || length(values$available_scales) == 0) {
      # 检测下划线分隔的变量并提供建议
      if(!is.null(values$processed_data)) {
        underscore_vars <- detect_underscore_patterns(values$processed_data)
        if(length(underscore_vars) > 0) {
          return(div(
            div(class = "alert alert-warning", 
                "未检测到完整的量表，但发现可能的量表变量："),
            div(class = "alert alert-info",
              tags$h6("💡 检测到下划线分隔的变量模式："),
              tags$ul(
                lapply(names(underscore_vars), function(pattern) {
                  vars <- underscore_vars[[pattern]]
                  if(length(vars) >= 3) {
                    tags$li(
                      tags$strong(pattern), ": ",
                      paste(head(vars, 5), collapse = ", "),
                      if(length(vars) > 5) "..." else "",
                      " (", length(vars), "个变量)"
                    )
                  }
                })
              ),
              tags$p("建议手动添加计算规则进行变量构造")
            )
          ))
        } else {
          return(div(class = "alert alert-warning", 
                     "未检测到完整的量表。请确保数据包含符合命名规范的量表题目，或手动添加计算规则。"))
        }
      } else {
        return(div(class = "alert alert-warning", 
                   "未检测到完整的量表。请确保数据包含符合命名规范的量表题目。"))
      }
    }
    
    scale_list <- list()
    for(scale_name in names(values$available_scales)) {
      scale_info <- values$available_scales[[scale_name]]
      coverage_color <- if(scale_info$coverage_rate >= 0.9) "success" else if(scale_info$coverage_rate >= 0.7) "warning" else "danger"
      
      # 确保所有字符串都是长度为1的向量
      existing_items_count <- length(scale_info$existing_items)
      total_items_count <- scale_info$total_items
      calculation_method_str <- as.character(scale_info$calculation_method %||% "unknown")[1]  # 确保只取第一个元素
      notes_str <- as.character(scale_info$notes %||% "")[1]  # 确保只取第一个元素
      
      scale_list[[length(scale_list) + 1]] <- div(class = "panel panel-default",
        div(class = "panel-body", style = "padding: 10px;",
          tags$h6(as.character(scale_name)),
          tags$small(
            "题目覆盖: ", existing_items_count, "/", total_items_count,
            " (", tags$span(class = paste0("label label-", coverage_color), 
                           paste0(round(scale_info$coverage_rate * 100, 1), "%")), ")", tags$br(),
            "计算方法: ", calculation_method_str, tags$br(),
            if(nchar(notes_str) > 0) tagList("备注: ", notes_str) else NULL
          )
        )
      )
    }
    
    return(do.call(tagList, scale_list))
  })
  
  # 量表选择选项更新
  observe({
    if(!is.null(values$available_scales) && length(values$available_scales) > 0) {
      choices <- setNames(names(values$available_scales), names(values$available_scales))
      updateCheckboxGroupInput(session, "selected_scales_to_calculate", 
                              choices = choices, selected = names(values$available_scales))
    }
  })
  
  # 重新检测量表
  observeEvent(input$detect_scales, {
    req(values$processed_data)
    req(values$scale_config)
    
    showNotification("正在重新检测量表...", type = "message")
    
    values$available_scales <- detect_available_scales(values$processed_data, values$scale_config)
    
    if(length(values$available_scales) > 0) {
      showNotification(paste("检测到", length(values$available_scales), "个可用量表"), type = "message")
    } else {
      showNotification("未检测到完整的量表", type = "warning")
    }
  })
  
  # 计算量表分数
  observeEvent(input$calculate_scales, {
    req(values$processed_data)
    req(values$scale_config)
    
    # 如果没有选择任何量表，给出提示
    if(is.null(input$selected_scales_to_calculate) || length(input$selected_scales_to_calculate) == 0) {
      showNotification("请选择要计算的量表，或使用手动规则添加自定义量表", type = "warning")
      return()
    }
    
    withProgress(message = '正在计算量表分数...', value = 0, {
      
      incProgress(0.1, detail = "准备计算...")
      
      tryCatch({
        # 使用scale_calculator模块计算量表分数
        calculation_result <- calculate_all_scales(
          values$processed_data, 
          values$scale_config, 
          input$selected_scales_to_calculate
        )
        
        incProgress(0.8, detail = "整理结果...")
        
        if(calculation_result$success) {
          # 更新处理后的数据
          values$processed_data <- calculation_result$data
          values$calculated_scales <- calculation_result
          
          incProgress(1, detail = "完成!")
          
          showNotification(paste("成功计算", length(calculation_result$summary), "个量表，新增", 
                                length(calculation_result$new_variables), "个变量"), type = "message")
        } else {
          showNotification(paste("量表计算失败:", calculation_result$message), type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("计算过程出错:", e$message), type = "error")
      })
    })
  })
  
  # 手动添加计算规则模态框
  observeEvent(input$show_manual_rules, {
    req(values$processed_data)
    
    # 获取所有可用变量
    all_vars <- names(values$processed_data)
    # 排除ID相关变量
    id_vars <- c("research_uuid", "subject_id", "city_code", "session_code")
    available_vars <- all_vars[!all_vars %in% id_vars]
    
    showModal(modalDialog(
      title = "📝 手动添加计算规则",
      size = "l",
      
      fluidRow(
        column(6,
          h5("📋 基本信息"),
          textInput("manual_scale_name", "量表名称", placeholder = "例如: 自定义焦虑量表"),
          selectInput("manual_calculation_method", "计算方法",
                     choices = list(
                       "基础计算" = "",
                       "求和" = "sum", 
                       "均值" = "mean", 
                       "加权平均" = "weighted_mean", 
                       "最大值" = "max_score",
                       "高级计算" = "",
                       "CFA加权分数" = "cfa_weighted",
                       "主成分分析" = "pca_score",
                       "标准化求和" = "standardized_sum",
                       "因子分数" = "factor_score"
                     )),
          numericInput("manual_min_valid", "最少有效题目数", value = 3, min = 1),
          
          h5("🔄 反向计分设置"),
          selectInput("manual_reverse_method", "反向计分方法",
                     choices = list("自动检测" = "auto", "6-原值" = "6minus", "5-原值" = "5minus")),
          
          conditionalPanel(
            condition = "input.manual_calculation_method == 'weighted_mean'",
            h5("⚖️ 权重设置"),
            helpText("为选择的变量设置权重，用逗号分隔，例如: 1,2,1,1"),
            textInput("manual_weights", "权重值", placeholder = "1,2,1,1")
          ),
          
          conditionalPanel(
            condition = "input.manual_calculation_method == 'cfa_weighted'",
            h5("🏗️ CFA加权设置"),
            helpText("基于验证性因子分析的因子载荷进行加权"),
            textInput("cfa_loadings", "因子载荷", placeholder = "0.8,0.7,0.9,0.6"),
            checkboxInput("cfa_standardize", "标准化载荷", value = TRUE),
            numericInput("cfa_min_loading", "最小载荷阈值", value = 0.3, min = 0, max = 1, step = 0.1)
          ),
          
          conditionalPanel(
            condition = "input.manual_calculation_method == 'pca_score'",
            h5("📊 主成分分析设置"),
            helpText("使用第一主成分作为综合得分"),
            numericInput("pca_components", "主成分数量", value = 1, min = 1, max = 5),
            checkboxInput("pca_rotation", "使用方差最大旋转", value = FALSE)
          ),
          
          conditionalPanel(
            condition = "input.manual_calculation_method == 'factor_score'",
            h5("🔬 因子分析设置"),
            helpText("基于因子分析提取因子得分"),
            selectInput("factor_method", "因子提取方法",
                       choices = list("主轴因子法" = "pa", "最大似然法" = "ml", "主成分法" = "pc")),
            numericInput("factor_number", "因子数量", value = 1, min = 1, max = 5),
            selectInput("factor_rotation", "旋转方法",
                       choices = list("无旋转" = "none", "方差最大" = "varimax", "斜交旋转" = "oblimin"))
          )
        ),
        
        column(6,
          h5("📊 选择变量"),
          div(style = "height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px;",
            checkboxGroupInput("manual_selected_vars", NULL,
                              choices = setNames(available_vars, available_vars),
                              selected = character(0))
          ),
          
          div(class = "text-center", style = "margin-top: 10px;",
            actionButton("select_all_vars", "全选", class = "btn-info btn-sm"),
            actionButton("clear_all_vars", "清空", class = "btn-warning btn-sm", style = "margin-left: 10px;")
          )
        )
      ),
      
      br(),
      
      conditionalPanel(
        condition = "input.manual_selected_vars && input.manual_selected_vars.length > 0",
        div(class = "alert alert-info",
          h5("🔍 预览设置"),
          verbatimTextOutput("manual_rule_preview")
        )
      ),
      
      conditionalPanel(
        condition = "input.manual_calculation_method == 'weighted_mean'",
        textInput("manual_weights", "变量权重 (逗号分隔)", 
                 placeholder = "例如: 1,2,1,1,1")
      ),
      
      textAreaInput("manual_reverse_items", "反向计分变量 (逗号分隔)", 
                   placeholder = "输入需要反向计分的变量名，用逗号分隔"),
      
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_manual_rule", "确认添加", class = "btn-success")
      )
    ))
  })
  
  # 全选变量
  observeEvent(input$select_all_vars, {
    all_vars <- names(values$processed_data)
    id_vars <- c("research_uuid", "subject_id", "city_code", "session_code")
    available_vars <- all_vars[!all_vars %in% id_vars]
    updateCheckboxGroupInput(session, "manual_selected_vars", selected = available_vars)
  })
  
  # 清空变量选择
  observeEvent(input$clear_all_vars, {
    updateCheckboxGroupInput(session, "manual_selected_vars", selected = character(0))
  })
  
  # 手动规则预览
  output$manual_rule_preview <- renderText({
    req(input$manual_selected_vars)
    req(input$manual_scale_name)
    
    n_vars <- length(input$manual_selected_vars)
    method_name <- switch(input$manual_calculation_method %||% "sum",
                         "sum" = "求和", 
                         "mean" = "均值", 
                         "weighted_mean" = "加权平均", 
                         "max_score" = "最大值",
                         "cfa_weighted" = "CFA加权分数",
                         "pca_score" = "主成分分析",
                         "standardized_sum" = "标准化求和",
                         "factor_score" = "因子分数",
                         "均值")
    
    preview_text <- paste0(
      "量表名称: ", input$manual_scale_name, "\n",
      "选择变量: ", n_vars, " 个 (", paste(head(input$manual_selected_vars, 3), collapse = ", "), 
      if(n_vars > 3) "..." else "", ")\n",
      "计算方法: ", method_name, "\n",
      "最少有效题目: ", input$manual_min_valid %||% 3, " 个\n"
    )
    
    # 添加方法特定的参数信息
    if(input$manual_calculation_method == "weighted_mean" && !is.null(input$manual_weights) && input$manual_weights != "") {
      preview_text <- paste0(preview_text, "权重: ", input$manual_weights, "\n")
    } else if(input$manual_calculation_method == "cfa_weighted" && !is.null(input$cfa_loadings) && input$cfa_loadings != "") {
      preview_text <- paste0(preview_text, 
                           "CFA载荷: ", input$cfa_loadings, "\n",
                           "载荷阈值: ", input$cfa_min_loading %||% 0.3, "\n",
                           "标准化载荷: ", if(input$cfa_standardize %||% TRUE) "是" else "否", "\n")
    } else if(input$manual_calculation_method == "pca_score") {
      preview_text <- paste0(preview_text, "主成分数量: ", input$pca_components %||% 1, "\n")
    } else if(input$manual_calculation_method == "factor_score") {
      preview_text <- paste0(preview_text, 
                           "因子数量: ", input$factor_number %||% 1, "\n",
                           "提取方法: ", input$factor_method %||% "pa", "\n",
                           "旋转方法: ", input$factor_rotation %||% "none", "\n")
    }
    
    preview_text <- paste0(preview_text, "将生成变量: ", input$manual_scale_name, "_Total")
    
    return(preview_text)
  })
  
  # 确认手动规则
  observeEvent(input$confirm_manual_rule, {
    req(input$manual_scale_name)
    req(input$manual_selected_vars)
    req(input$manual_calculation_method)
    
    scale_name <- trimws(input$manual_scale_name)
    if(scale_name == "") {
      showNotification("请输入量表名称", type = "error")
      return()
    }
    
    if(length(input$manual_selected_vars) < (input$manual_min_valid %||% 1)) {
      showNotification("选择的变量数量少于最少有效题目数", type = "error")
      return()
    }
    
    tryCatch({
      # 准备计算数据
      scale_data <- values$processed_data[, input$manual_selected_vars, drop = FALSE]
      
      # 处理反向计分
      reverse_items <- character(0)
      if(!is.null(input$manual_reverse_items) && input$manual_reverse_items != "") {
        reverse_items <- trimws(strsplit(input$manual_reverse_items, ",")[[1]])
        reverse_items <- intersect(reverse_items, input$manual_selected_vars)
        
        if(length(reverse_items) > 0) {
          scale_data <- apply_reverse_coding(scale_data, reverse_items, input$manual_reverse_method %||% "auto")
        }
      }
      
      # 计算量表分数
      valid_count <- rowSums(!is.na(scale_data))
      min_valid <- input$manual_min_valid %||% 3
      
      # 根据计算方法进行计算
      tryCatch({
        if(input$manual_calculation_method == "sum") {
          scale_score <- ifelse(valid_count >= min_valid,
                               rowSums(scale_data, na.rm = TRUE), NA)
                               
        } else if(input$manual_calculation_method == "mean") {
          scale_score <- ifelse(valid_count >= min_valid,
                               rowMeans(scale_data, na.rm = TRUE), NA)
                               
        } else if(input$manual_calculation_method == "max_score") {
          scale_score <- ifelse(valid_count >= min_valid,
                               apply(scale_data, 1, max, na.rm = TRUE), NA)
                               
        } else if(input$manual_calculation_method == "weighted_mean") {
          # 处理加权平均
          if(is.null(input$manual_weights) || input$manual_weights == "") {
            showNotification("请输入权重值", type = "error")
            return()
          }
          
          weights_str <- trimws(strsplit(input$manual_weights, ",")[[1]])
          weights <- as.numeric(weights_str)
          
          if(length(weights) != ncol(scale_data)) {
            showNotification("权重数量与变量数量不匹配", type = "error")
            return()
          }
          
          if(any(is.na(weights))) {
            showNotification("权重必须是数值", type = "error")
            return()
          }
          
          scale_score <- apply(scale_data, 1, function(x) {
            if(sum(!is.na(x)) >= min_valid) {
              weighted.mean(x, weights, na.rm = TRUE)
            } else {
              NA
            }
          })
          
        } else if(input$manual_calculation_method == "cfa_weighted") {
          # CFA加权分数
          if(is.null(input$cfa_loadings) || input$cfa_loadings == "") {
            showNotification("请输入因子载荷", type = "error")
            return()
          }
          
          loadings_str <- trimws(strsplit(input$cfa_loadings, ",")[[1]])
          loadings <- as.numeric(loadings_str)
          
          if(length(loadings) != ncol(scale_data)) {
            showNotification("载荷数量与变量数量不匹配", type = "error")
            return()
          }
          
          if(any(is.na(loadings))) {
            showNotification("载荷必须是数值", type = "error")
            return()
          }
          
          # 应用载荷阈值
          min_loading <- input$cfa_min_loading %||% 0.3
          loadings[abs(loadings) < min_loading] <- 0
          
          # 标准化载荷（如果选择）
          if(input$cfa_standardize %||% TRUE) {
            loadings <- loadings / sqrt(sum(loadings^2))
          }
          
          scale_score <- apply(scale_data, 1, function(x) {
            if(sum(!is.na(x)) >= min_valid) {
              valid_idx <- !is.na(x)
              weighted.mean(x[valid_idx], loadings[valid_idx], na.rm = TRUE)
            } else {
              NA
            }
          })
          
        } else if(input$manual_calculation_method == "pca_score") {
          # 主成分分析
          complete_data <- scale_data[complete.cases(scale_data), ]
          
          if(nrow(complete_data) < 5) {
            showNotification("完整案例太少，无法进行主成分分析", type = "error")
            return()
          }
          
          pca_result <- prcomp(complete_data, scale. = TRUE, center = TRUE)
          
          # 获取第一主成分的载荷
          pc1_loadings <- pca_result$rotation[, 1]
          
          scale_score <- apply(scale_data, 1, function(x) {
            if(sum(!is.na(x)) >= min_valid) {
              # 标准化变量
              x_scaled <- scale(x)
              if(any(is.na(x_scaled))) return(NA)
              sum(x_scaled * pc1_loadings, na.rm = TRUE)
            } else {
              NA
            }
          })
          
        } else if(input$manual_calculation_method == "factor_score") {
          # 因子分析（需要安装psych包）
          if(!requireNamespace("psych", quietly = TRUE)) {
            showNotification("因子分析需要安装psych包", type = "error")
            return()
          }
          
          complete_data <- scale_data[complete.cases(scale_data), ]
          
          if(nrow(complete_data) < 10) {
            showNotification("完整案例太少，无法进行因子分析", type = "error")
            return()
          }
          
          factor_method <- input$factor_method %||% "pa"
          factor_number <- input$factor_number %||% 1
          factor_rotation <- input$factor_rotation %||% "none"
          
          fa_result <- psych::fa(complete_data, nfactors = factor_number, 
                                fm = factor_method, rotate = factor_rotation)
          
          # 获取第一因子的载荷
          factor1_loadings <- fa_result$loadings[, 1]
          
          scale_score <- apply(scale_data, 1, function(x) {
            if(sum(!is.na(x)) >= min_valid) {
              # 标准化变量
              x_scaled <- scale(x)
              if(any(is.na(x_scaled))) return(NA)
              sum(x_scaled * factor1_loadings, na.rm = TRUE)
            } else {
              NA
            }
          })
          
        } else if(input$manual_calculation_method == "standardized_sum") {
          # 标准化求和
          scale_score <- apply(scale_data, 1, function(x) {
            if(sum(!is.na(x)) >= min_valid) {
              # 对每行进行标准化后求和
              x_scaled <- scale(x)
              if(any(is.na(x_scaled))) return(NA)
              sum(x_scaled, na.rm = TRUE)
            } else {
              NA
            }
          })
          
        } else {
          # 默认使用均值
          scale_score <- ifelse(valid_count >= min_valid,
                               rowMeans(scale_data, na.rm = TRUE), NA)
        }
        
      }, error = function(e) {
        showNotification(paste("计算过程出错:", e$message), type = "error")
        return()
      })
      
      # 添加新变量到数据 - 根据计算方法命名
      if(input$manual_calculation_method == "mean") {
        new_var_name <- paste0(scale_name, "_mean")
      } else if(input$manual_calculation_method == "sum") {
        new_var_name <- paste0(scale_name, "_sum")
      } else if(input$manual_calculation_method == "weighted_mean") {
        new_var_name <- paste0(scale_name, "_weighted")
      } else if(input$manual_calculation_method == "max_score") {
        new_var_name <- paste0(scale_name, "_max")
      } else if(input$manual_calculation_method == "cfa_weighted") {
        new_var_name <- paste0(scale_name, "_cfa")
      } else if(input$manual_calculation_method == "pca_score") {
        new_var_name <- paste0(scale_name, "_pca")
      } else if(input$manual_calculation_method == "factor_score") {
        new_var_name <- paste0(scale_name, "_factor")
      } else if(input$manual_calculation_method == "standardized_sum") {
        new_var_name <- paste0(scale_name, "_std")
      } else {
        # 默认使用Total后缀
        new_var_name <- paste0(scale_name, "_Total")
      }
      values$processed_data[[new_var_name]] <- scale_score
      
      # 更新可用量表信息
      if(is.null(values$calculated_scales)) {
        values$calculated_scales <- list(success = TRUE, summary = list(), new_variables = character(0))
      }
      
      # 添加到已计算量表
      values$calculated_scales$summary[[scale_name]] <- list(
        total_items = length(input$manual_selected_vars),
        existing_items = length(input$manual_selected_vars),
        coverage_rate = 1.0,
        new_variables = new_var_name,
        calculation_method = input$manual_calculation_method,
        is_manual = TRUE
      )
      
      values$calculated_scales$new_variables <- c(values$calculated_scales$new_variables, new_var_name)
      
      removeModal()
      
      valid_cases <- sum(!is.na(scale_score))
      showNotification(paste0("成功添加手动规则: ", scale_name, 
                             "\n新变量: ", new_var_name,
                             "\n有效案例: ", valid_cases, "/", nrow(values$processed_data)), 
                      type = "message")
      
    }, error = function(e) {
      showNotification(paste("手动规则添加失败:", e$message), type = "error")
    })
  })
  
  # 计算结果报告
  output$calculation_report <- renderUI({
    req(values$calculated_scales)
    
    if(values$calculated_scales$success) {
      report_html <- generate_calculation_report(values$calculated_scales)
      HTML(report_html)
    } else {
      div(class = "alert alert-danger", values$calculated_scales$message)
    }
  })
  
  # 新增变量预览
  output$new_variables_preview <- DT::renderDataTable({
    req(values$calculated_scales)
    req(values$calculated_scales$success)
    
    new_vars <- values$calculated_scales$new_variables
    if(length(new_vars) == 0) {
      return(data.frame("提示" = "没有新增变量"))
    }
    
    preview_data <- values$processed_data[, new_vars, drop = FALSE]
    
    DT::datatable(
      head(preview_data, 100),
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        pageLength = 10,
        dom = 'tip'
      ),
      rownames = FALSE
    ) %>% DT::formatRound(columns = 1:ncol(preview_data), digits = 2)
  })
  
  # 手动规则列表显示
  output$manual_rules_list <- renderUI({
    if(is.null(values$calculated_scales) || is.null(values$calculated_scales$summary)) {
      return(div(class = "text-muted", style = "text-align: center; padding: 20px;", 
                "暂无手动规则"))
    }
    
    manual_rules <- Filter(function(x) !is.null(x$is_manual) && x$is_manual, 
                          values$calculated_scales$summary)
    
    if(length(manual_rules) == 0) {
      return(div(class = "text-muted", style = "text-align: center; padding: 20px;", 
                "暂无手动规则"))
    }
    
    rule_cards <- lapply(names(manual_rules), function(scale_name) {
      rule_info <- manual_rules[[scale_name]]
      
      div(class = "panel panel-default", style = "margin-bottom: 10px;",
        div(class = "panel-body", style = "padding: 10px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              tags$strong("📝 ", scale_name),
              tags$br(),
              tags$small(
                "变量数: ", rule_info$total_items, " | ",
                "计算方法: ", rule_info$calculation_method, " | ",
                "生成变量: ", paste(rule_info$new_variables, collapse = ", ")
              )
            ),
            actionButton(paste0("delete_rule_", scale_name), "🗑️", 
                        class = "btn-danger btn-xs",
                        onclick = paste0("Shiny.setInputValue('delete_manual_rule', '", scale_name, "');"))
          )
        )
      )
    })
    
    return(do.call(tagList, rule_cards))
  })
  
  # 删除单个手动规则
  observeEvent(input$delete_manual_rule, {
    req(input$delete_manual_rule)
    scale_name <- input$delete_manual_rule
    
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary[[scale_name]])) {
      # 删除变量
      var_to_remove <- values$calculated_scales$summary[[scale_name]]$new_variables
      for(var in var_to_remove) {
        values$processed_data[[var]] <- NULL
      }
      
      # 从摘要中删除
      values$calculated_scales$summary[[scale_name]] <- NULL
      values$calculated_scales$new_variables <- setdiff(values$calculated_scales$new_variables, var_to_remove)
      
      showNotification(paste("已删除手动规则:", scale_name), type = "message")
    }
  })
  
  # 清空所有手动规则
  observeEvent(input$clear_all_manual_rules, {
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary)) {
      manual_rules <- Filter(function(x) !is.null(x$is_manual) && x$is_manual, 
                            values$calculated_scales$summary)
      
      if(length(manual_rules) > 0) {
        # 删除所有手动规则生成的变量
        for(scale_name in names(manual_rules)) {
          var_to_remove <- manual_rules[[scale_name]]$new_variables
          for(var in var_to_remove) {
            values$processed_data[[var]] <- NULL
          }
          values$calculated_scales$summary[[scale_name]] <- NULL
        }
        
        # 更新new_variables列表
        manual_vars <- unlist(lapply(manual_rules, function(x) x$new_variables))
        values$calculated_scales$new_variables <- setdiff(values$calculated_scales$new_variables, manual_vars)
        
        showNotification(paste("已清空", length(manual_rules), "个手动规则"), type = "message")
      }
    }
  })
  
  # =============================================================================
  # 原有逻辑继续
  # =============================================================================
  
  # 动态生成量表层级选择器
  output$scale_level_selectors <- renderUI({
    req(values$scales)
    
    if(length(values$scales) == 0) {
      return(tags$p("未识别到量表"))
    }
    
    selectors <- list()
    
    for(scale_name in names(values$scales)) {
      scale_info <- values$scales[[scale_name]]
      
      # 构建选项说明
      choices <- list()
      
      # 汇总层选项
      if(length(scale_info$subscales) == 1) {
        choices[["汇总层 (总分)"]] <- "summary"
      } else {
        total_subscales <- names(scale_info$subscales)[grepl("Total", names(scale_info$subscales))]
        if(length(total_subscales) > 0) {
          choices[["汇总层 (总分)"]] <- "summary"
        } else {
          choices[["汇总层 (第一维度)"]] <- "summary"
        }
      }
      
      # 子量表层选项  
      if(length(scale_info$subscales) > 1) {
        dimension_subscales <- names(scale_info$subscales)[!grepl("Total", names(scale_info$subscales))]
        if(length(dimension_subscales) > 1) {
          choices[[paste0("子量表层 (", length(dimension_subscales), "个维度)")]] <- "subscale"
        }
      }
      
      # 条目层选项
      item_count <- length(scale_info$items)
      if(item_count <= 25) {
        choices[[paste0("条目层 (", item_count, "个题目)")]] <- "items"
      } else {
        choices[[paste0("条目层 (前15题,共", item_count, "题)")]] <- "items"
      }
      
      # 智能默认选择
      default_value <- if(scale_name == "AUDIT" || scale_name == "PHQ" || scale_name == "GAD") {
        "summary"
      } else if(scale_name == "HRF" || item_count <= 18) {
        "items"
      } else {
        "subscale"
      }
      
      if(!default_value %in% unlist(choices)) default_value <- unlist(choices)[1]
      
      # 添加样式和说明
      selectors[[scale_name]] <- div(
        style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 5px;",
        h5(paste0("📊 ", scale_info$name), style = "margin-top: 0; color: #337ab7;"),
        selectInput(
          inputId = paste0("level_", scale_name),
          label = NULL,
          choices = choices,
          selected = default_value,
          width = "100%"
        ),
        # 显示维度信息
        if(length(scale_info$subscales) > 1 && !all(grepl("Total", names(scale_info$subscales)))) {
          div(
            style = "font-size: 0.8em; color: #666; margin-top: -10px;",
            paste("维度:", paste(names(scale_info$subscales)[!grepl("Total", names(scale_info$subscales))], collapse = ", "))
          )
        } else NULL
      )
    }
    
    return(tagList(selectors))
  })
  
  # 预览选中的变量
  output$selected_variables_preview <- renderText({
    req(values$scales)
    
    if(length(values$scales) == 0) {
      return("未识别到量表")
    }
    
    preview_vars <- c()
    
    for(scale_name in names(values$scales)) {
      scale_info <- values$scales[[scale_name]]
      level_input_id <- paste0("level_", scale_name)
      selected_level <- input[[level_input_id]]
      
      if(is.null(selected_level)) selected_level <- "summary"
      
      if(selected_level == "summary") {
        subscale_names <- names(scale_info$subscales)
        if(length(subscale_names) == 1) {
          var_name <- subscale_names[1]
        } else {
          total_subscale <- subscale_names[grep("Total", subscale_names)]
          var_name <- if(length(total_subscale) > 0) total_subscale[1] else subscale_names[1]
        }
        preview_vars <- c(preview_vars, paste0("• ", var_name, " (", scale_name, "总分)"))
        
      } else if(selected_level == "subscale") {
        subscale_names <- names(scale_info$subscales)
        dimension_subscales <- subscale_names[!grepl("Total", subscale_names)]
        if(length(dimension_subscales) > 0) {
          for(sub_name in dimension_subscales) {
            preview_vars <- c(preview_vars, paste0("• ", sub_name, " (", scale_name, "维度)"))
          }
        }
        
      } else {
        items <- scale_info$items
        if(length(items) <= 5) {
          preview_vars <- c(preview_vars, paste0("• ", paste(items, collapse = ", "), " (", scale_name, "条目)"))
        } else {
          preview_vars <- c(preview_vars, paste0("• ", paste(head(items, 5), collapse = ", "), "... (", scale_name, " ", length(items), "个条目)"))
        }
      }
    }
    
    if(length(preview_vars) == 0) {
      return("未选择变量")
    }
    
    result <- paste0("总共 ", sum(sapply(strsplit(paste(preview_vars, collapse = " "), " "), function(x) sum(grepl("_", x)))), " 个变量：\n\n")
    result <- paste0(result, paste(preview_vars, collapse = "\n"))
    
    return(result)
  })
  
  # 网络估计方法帮助文本
  output$network_estimator_help <- renderUI({
    if(is.null(input$network_estimator)) return(NULL)
    
    help_content <- switch(input$network_estimator,
      "EBICglasso" = list(
        title = "📊 EBICglasso（推荐）",
        description = "基于扩展贝叶斯信息准则的稀疏高斯图模型估计",
        advantages = c("• 适用于连续数据", "• 自动选择最优正则化参数", "• 产生稀疏网络结构", "• 理论基础扎实"),
        usage = "适合: 连续心理量表数据（如李克特量表）",
        note = "这是最常用和推荐的方法，适合大多数心理网络分析"
      ),
      "mgm" = list(
        title = "🔗 MGM（混合图模型）",
        description = "处理不同类型变量的混合图模型",
        advantages = c("• 支持连续、二元、计数变量混合", "• 可建模非线性关系", "• 适合复杂数据结构"),
        usage = "适合: 混合数据类型（如量表+人口学变量）",
        note = "当数据包含不同类型变量时的首选方法"
      ),
      "IsingFit" = list(
        title = "⚡ Ising模型",
        description = "专门用于二元数据的Ising模型",
        advantages = c("• 专为二元数据设计", "• 可计算网络温度", "• 支持组间比较"),
        usage = "适合: 二元症状数据（有/无症状）",
        note = "当数据为二元编码时使用，支持温度分析"
      ),
      "cor" = list(
        title = "📈 相关网络",
        description = "基于零阶相关的网络模型",
        advantages = c("• 简单直观", "• 计算速度快", "• 易于解释"),
        usage = "适合: 探索性分析和教学演示",
        note = "简单方法，但可能包含虚假连接"
      ),
      "pcor" = list(
        title = "📊 偏相关网络",
        description = "基于偏相关系数的网络模型",
        advantages = c("• 控制其他变量影响", "• 显示直接关系", "• 相对简单"),
        usage = "适合: 中等规模的连续数据",
        note = "比相关网络更准确，但不如EBICglasso稀疏"
      ),
      "TMFG" = list(
        title = "🌳 三角最大滤波图",
        description = "基于图论的稀疏网络构建方法",
        advantages = c("• 产生层次化结构", "• 固定稀疏度", "• 计算高效"),
        usage = "适合: 大规模数据和层次分析",
        note = "产生固定稀疏度的层次网络结构"
      )
    )
    
    if(is.null(help_content)) return(NULL)
    
    div(
      style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 10px; margin: 5px 0;",
      h5(help_content$title, style = "color: #007bff; margin-bottom: 8px;"),
      tags$p(help_content$description, style = "margin-bottom: 8px; font-size: 14px;"),
      
      div(
        style = "margin-bottom: 8px;",
        tags$strong("优势:", style = "color: #28a745;"),
        HTML(paste(help_content$advantages, collapse = "<br/>"))
      ),
      
      div(
        style = "margin-bottom: 8px;",
        tags$strong("适用场景:", style = "color: #17a2b8;"),
        tags$span(help_content$usage, style = "font-style: italic;")
      ),
      
      div(
        style = "background-color: #fff3cd; padding: 6px; border-radius: 4px; font-size: 13px;",
        tags$i(class = "fa fa-lightbulb-o", style = "color: #856404; margin-right: 5px;"),
        help_content$note
      )
    )
  })
  
  # 运行网络分析
  observeEvent(input$run_analysis, {
    # 检查变量选择是否已确认且分析数据已准备
    if(is.null(values$variables_confirmed) || !values$variables_confirmed) {
      showNotification("请先在变量选择页面确认变量选择", type = "error")
      return()
    }
    
    if(is.null(values$analysis_data) || ncol(values$analysis_data) < 2) {
      showNotification("分析数据不足，请检查变量选择", type = "error")
      return()
    }
    
    # 检查是否已有相同配置的网络分析结果（简单缓存）
    analysis_hash <- tryCatch({
      if(requireNamespace("digest", quietly = TRUE)) {
        digest::digest(list(
          data = values$analysis_data,
          threshold = input$threshold
        ), algo = "md5")
      } else {
        # 如果没有digest包，使用简单的字符串标识
        paste0(ncol(values$analysis_data), "_", nrow(values$analysis_data), "_", input$threshold)
      }
    }, error = function(e) {
      paste0(ncol(values$analysis_data), "_", nrow(values$analysis_data), "_", input$threshold)
    })
    
    if (!is.null(values$last_analysis_hash) && 
        values$last_analysis_hash == analysis_hash &&
        !is.null(values$network_result)) {
      showNotification("使用缓存的分析结果", type = "message")
      return()
    }
    
    withProgress(message = '正在进行网络分析...', value = 0, {
      
      incProgress(0.1, detail = "检查分析数据...")
      
      # 使用变量选择页面准备好的分析数据
      n_vars <- ncol(values$analysis_data)
      
      # 总体变量数建议
      if(n_vars > 30) {
        showNotification(paste("当前选择了", n_vars, "个变量，建议控制在30个以内以获得更好的可视化效果"), type = "warning")
      }
      
      # 检查数据质量
      complete_cases <- sum(complete.cases(values$analysis_data))
      total_cases <- nrow(values$analysis_data)
      
      if(complete_cases < 5) {
        showNotification(
          paste0("⚠️ 完整案例不足 (", complete_cases, "/", total_cases, ")：无法进行网络分析\n",
                "建议：\n",
                "• 减少变量数量（优先保留核心变量）\n", 
                "• 检查数据质量，确认缺失值模式\n",
                "• 考虑使用汇总层分析（总分变量通常缺失较少）"),
          type = "error", duration = 8
        )
        return()
      }
      
      if(complete_cases < 20) {
        showNotification(
          paste0("⚠️ 完整案例较少 (", complete_cases, "/", total_cases, ")：网络分析结果可能不稳定\n",
                "建议：\n", 
                "• 尽量达到50+个完整案例以获得可靠结果\n",
                "• 考虑减少变量数量或切换到汇总层分析\n",
                "• 谨慎解释分析结果"),
          type = "warning", duration = 6
        )
      } else if(complete_cases / total_cases < 0.5) {
        showNotification(
          paste0("📊 数据缺失提醒：完整案例占比 ", round(complete_cases/total_cases*100, 1), "% (", complete_cases, "/", total_cases, ")\n",
                "当前可进行分析，但建议检查缺失数据模式"), 
          type = "warning", duration = 4
        )
      }
      
      # 使用完整数据进行分析（不进行采样）
      analysis_data_final <- values$analysis_data
      
      # 大数据集性能提示
      if(total_cases > 2000) {
        showNotification(paste("检测到大数据集(", total_cases, "行)，分析可能需要较长时间"), type = "warning")
      }
      
      incProgress(0.3, detail = "构建网络...")
      
      # 使用安全的网络分析函数
      tryCatch({
        # 生成基于分组的配色
        colors <- tryCatch({
          if(!is.null(values$variable_groups) && length(values$variable_groups) > 0) {
            # 转换为变量索引格式的分组
            variable_names <- colnames(analysis_data_final)
            n_vars <- length(variable_names)
            available_colors <- VIZ_CONFIG$colors$primary
            
            # 使用在变量选择时预构建的groups参数
            groups_by_index <- values$scale_groups
            
            # 要求必须有预构建的groups参数
            if(is.null(groups_by_index) || length(groups_by_index) == 0) {
              stop("缺少groups参数，请先确认变量选择")
            }
            
            # 生成颜色向量 - 按问卷分配颜色
            color_vector <- rep("#999999", n_vars)  # 默认灰色
            
            # 为每个问卷分配颜色
            for(i in seq_along(groups_by_index)) {
              scale_name <- names(groups_by_index)[i]
              color_index <- ((i-1) %% length(available_colors)) + 1
              scale_color <- available_colors[color_index]
              scale_indices <- groups_by_index[[i]]
              
              # 直接分配颜色，如果索引错误就让它报错
              color_vector[scale_indices] <- scale_color
            }
            
            # 检查未分配的变量
            unassigned_vars <- which(color_vector == "#999999")
            if(length(unassigned_vars) > 0) {
              # 为未分配的变量分配颜色
              for(idx in unassigned_vars) {
                color_index <- ((idx-1) %% length(available_colors)) + 1
                color_vector[idx] <- available_colors[color_index]
              }
            }
            
            # 直接保存groups_by_index用于quickNet
            values$network_groups_by_index <- groups_by_index
            
            # 创建组级别的颜色向量（用于quickNet的groups参数）
            group_colors <- character(length(groups_by_index))
            for(i in seq_along(groups_by_index)) {
              color_index <- ((i-1) %% length(available_colors)) + 1
              group_colors[i] <- available_colors[color_index]
            }
            values$network_group_colors <- group_colors
            
            # 简化调试输出
            cat("Groups: list(", paste(sapply(names(groups_by_index), function(x) {
              indices <- groups_by_index[[x]]
              if(length(indices) == 1) {
                paste0(x, "=", indices)
              } else if(all(diff(indices) == 1) && length(indices) > 1) {
                paste0(x, "=", min(indices), ":", max(indices))
              } else {
                paste0(x, "=c(", paste(indices, collapse = ","), ")")
              }
            }), collapse = ", "), ")\n")
            
            color_vector
          } else {
            # 如果没有分组，使用默认配色
            VIZ_CONFIG$colors$primary[1:min(ncol(analysis_data_final), length(VIZ_CONFIG$colors$primary))]
          }
        }, error = function(e) {
          # 如果分组配色失败，使用默认配色
          VIZ_CONFIG$colors$primary[1:min(ncol(analysis_data_final), length(VIZ_CONFIG$colors$primary))]
        })
        
        # 初始化代码记录器（如果还没有的话）
        if(is.null(values$code_recorder)) {
          values$code_recorder <- init_code_recorder()
          
          # 记录数据加载代码
          if(!is.null(values$upload_filepath)) {
            values$code_recorder <- record_data_loading(values$code_recorder, values$upload_filepath, values$sheet_name)
          }
          
          # 记录数据预处理代码
          values$code_recorder <- record_data_preprocessing(
            values$code_recorder, 
            colnames(analysis_data_final), 
            "无",  # 网络分析通常不二值化
            "0/1编码", 
            NULL  # 无分组
          )
        }
        
        # 记录网络分析代码
        values$code_recorder <- record_network_analysis(
          values$code_recorder,
          colnames(analysis_data_final),
          input$threshold %||% 0.05,
          values$variable_groups,
          estimator = input$network_estimator %||% "EBICglasso"
        )
        
        values$network_result <- safe_network_analysis(
          data = analysis_data_final,
          title = "Network Analysis",
          groups = values$network_groups_by_index,  # 使用正确的索引格式分组
          threshold = input$threshold %||% 0.05,
          edge.labels = TRUE,  # 按您的要求显示边权重
          colors = values$network_group_colors,  # 使用组级别的颜色向量
          layout = values$layout,
          estimator = input$network_estimator %||% "EBICglasso"  # 添加网络估计方法参数
        )
        
        # 保存layout和配色信息供后续网络分析使用
        if(!is.null(values$network_result)) {
          # 保存配色方案
          values$colors <- colors
          # 保存groups信息
          values$groups <- values$variable_groups
          # 从quickNet结果中提取layout（如果可用）
          if(is.list(values$network_result) && !is.null(values$network_result$layout)) {
            values$layout <- values$network_result$layout
          }
        }
        
        # 使用get_network_plot函数保存网络分析结果
        tryCatch({
          if(requireNamespace("quickNet", quietly = TRUE)) {
            timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
            prefix <- paste0("Fig1_network_", timestamp)
            
            # 切换到输出文件夹
            if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
              old_wd <- getwd()
              setwd(values$output_folder)
              on.exit(setwd(old_wd))
            }
            
            # 调用get_network_plot保存图片
            get_network_plot(values$network_result, 
                           prefix = prefix, 
                           width = 6, height = 4.5)
            
            # 保存PDF文件路径供网页显示使用
            values$network_pdf_path <- file.path(values$output_folder, paste0(prefix, "_network_plot.pdf"))
            
            cat("已使用get_network_plot保存网络结果\n")
            cat("PDF文件:", values$network_pdf_path, "\n")
          }
        }, error = function(e) {
          cat("get_network_plot调用失败:", e$message, "\n")
          # 备用保存方式
          auto_save_result("network", 
                          result_object = values$network_result,
                          plot_object = values$network_result,
                          filename_prefix = "Fig1_network_plot")
        })
        
        incProgress(0.7, detail = "计算中心性指标...")
        
        # 中心性分析
        tryCatch({
          if(requireNamespace("quickNet", quietly = TRUE)) {
            values$centrality_result <- Centrality(values$network_result)
            
            # 使用get_centrality_plot函数保存中心性结果
            tryCatch({
              if(requireNamespace("quickNet", quietly = TRUE)) {
                timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
                prefix <- paste0("Fig2_centrality_", timestamp)
                
                # 切换到输出文件夹
                if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
                  old_wd <- getwd()
                  setwd(values$output_folder)
                  on.exit(setwd(old_wd))
                }
                
                # 调用get_centrality_plot保存图片
                get_centrality_plot(values$centrality_result, 
                                   prefix = prefix, 
                                   width = 6, height = 4.5)
                
                # 保存PDF文件路径供网页显示使用
                values$centrality_pdf_path <- file.path(values$output_folder, paste0(prefix, "_centrality_plot.pdf"))
                
                cat("已使用get_centrality_plot保存中心性结果\n")
                cat("PDF文件:", values$centrality_pdf_path, "\n")
              }
            }, error = function(e) {
              cat("get_centrality_plot调用失败:", e$message, "\n")
              # 备用保存方式
              auto_save_result("centrality", 
                              result_object = values$centrality_result,
                              plot_object = get_centrality_plot(values$centrality_result),
                              filename_prefix = "Fig2_centrality_plot")
            })
          } else {
            showNotification("quickNet包不可用，跳过中心性计算", type = "warning")
            values$centrality_result <- NULL
          }
        }, error = function(e) {
          showNotification(paste("中心性计算失败:", e$message), type = "warning")
          values$centrality_result <- NULL
        })
        
        # 保存分析hash用于缓存（提前保存，确保主网络分析已完成）
        values$last_analysis_hash <- analysis_hash
        
        # 桥接网络分析（如果启用且有多个组别）- 完全独立的分析步骤
        # 桥接分析条件检查和调试
        cat("桥接分析条件检查:\n")
        cat("enable_bridge_analysis:", input$enable_bridge_analysis, "\n")
        cat("variable_groups存在:", !is.null(values$variable_groups), "\n")
        cat("variable_groups长度:", if(!is.null(values$variable_groups)) length(values$variable_groups) else 0, "\n")
        cat("variable_groups内容:", str(values$variable_groups), "\n")
        
        if(!is.null(input$enable_bridge_analysis) && input$enable_bridge_analysis && 
           !is.null(values$variable_groups) && length(values$variable_groups) >= 2) {
          
          incProgress(0.8, detail = "进行桥接网络分析...")
          cat("开始桥接网络分析...\n")
          
          tryCatch({
            # 准备桥接分析的分组信息
            bridge_groups <- tryCatch({
              if(!is.null(values$variable_groups) && length(values$variable_groups) > 0) {
                # 转换为变量索引格式的分组（与前面的逻辑一致）
                variable_names <- colnames(analysis_data_final)
                groups_by_index <- list()
                
                for(i in seq_along(values$variable_groups)) {
                  group_name <- names(values$variable_groups)[i]
                  scales_in_group <- values$variable_groups[[i]]
                  
                  # 找到属于这个分组的变量索引
                  group_indices <- c()
                  for(scale_name in scales_in_group) {
                    matching_indices <- which(
                      variable_names == scale_name |
                      startsWith(variable_names, paste0(scale_name, "_")) |
                      endsWith(variable_names, paste0("_", scale_name)) |
                      grepl(scale_name, variable_names, fixed = TRUE)
                    )
                    group_indices <- c(group_indices, matching_indices)
                  }
                  
                  if(length(group_indices) > 0) {
                    groups_by_index[[group_name]] <- sort(unique(group_indices))
                  }
                }
                
                groups_by_index
              } else {
                NULL
              }
            }, error = function(e) {
              showNotification(paste("分组配置错误:", e$message), type = "warning")
              NULL
            })
            
            if(!is.null(bridge_groups) && length(bridge_groups) >= 2) {
              
              # 执行桥接分析
              if(requireNamespace("quickNet", quietly = TRUE)) {
                
                # 验证并调整communities格式
                total_vars <- ncol(analysis_data_final)
                
                # 确保所有变量都被分配到某个组
                all_assigned <- unique(unlist(bridge_groups))
                missing_vars <- setdiff(1:total_vars, all_assigned)
                
                if(length(missing_vars) > 0) {
                  # 为未分配的变量创建合理的组名
                  missing_var_names <- variable_names[missing_vars]
                  # 尝试从变量名推断量表名
                  inferred_groups <- list()
                  for(var_name in missing_var_names) {
                    scale_prefix <- gsub("_.*$", "", var_name)  # 提取下划线前的部分
                    if(!scale_prefix %in% names(inferred_groups)) {
                      inferred_groups[[scale_prefix]] <- c()
                    }
                    inferred_groups[[scale_prefix]] <- c(inferred_groups[[scale_prefix]], which(variable_names == var_name))
                  }
                  # 合并到bridge_groups
                  bridge_groups <- c(bridge_groups, inferred_groups)
                }
                
                # 验证索引范围和communities格式
                max_index <- max(unlist(bridge_groups))
                if(max_index > total_vars) {
                  showNotification(paste0("分组索引超出变量范围，桥接分析失败 (最大索引:", max_index, ", 变量数:", total_vars, ")"), type = "error")
                  values$bridge_result <- NULL
                  values$bridge_network <- NULL
                } else {
                  # 打印调试信息
                  cat("桥接分析 - 变量数:", total_vars, "\n")
                  cat("桥接分析 - 分组数:", length(bridge_groups), "\n")
                  cat("桥接分析 - 分组结构:", str(bridge_groups), "\n")
                  cat("桥接分析 - 变量名称:", colnames(analysis_data_final), "\n")
                  cat("桥接分析 - values$variable_groups:", str(values$variable_groups), "\n")
                  # 使用主网络结果进行桥接分析（避免重复计算）
                  bridge_network_base <- values$network_result
                  
                  # Bridge分析 - 使用验证过的communities
                  values$bridge_result <- Bridge(bridge_network_base, communities = bridge_groups)
                  
                  # 记录桥接分析代码
                  bridge_code <- c(
                    "# ===== 桥接网络分析 Bridge Network Analysis =====",
                    "library(quickNet)",
                    "",
                    "# 定义变量分组",
                    paste0('bridge_groups <- list('),
                    paste0('  ', names(bridge_groups), ' = c(', 
                           sapply(bridge_groups, function(x) paste(x, collapse = ', ')), ')', collapse = ',\n  '),
                    ')',
                    "",
                    "# 进行Bridge分析",
                    "bridge_result <- Bridge(network_result, communities = bridge_groups)",
                    "",
                    paste0("# 识别桥接节点 (每组", input$bridge_n %||% 1, "个)"),
                    paste0("bridge_groups_result <- bridgeGroup(bridge_result, bridge_groups, n = ", input$bridge_n %||% 1, ", by_group = TRUE)"),
                    ""
                  )
                  values$code_recorder <- record_actual_code(values$code_recorder, bridge_code, "bridge_analysis", "桥接网络分析")
                  
                  # bridgeGroup分析 - 识别桥接节点
                  bridge_n <- input$bridge_n %||% 1
                  values$bridge_groups <- bridgeGroup(values$bridge_result, bridge_groups, 
                                                     labels = NULL, n = bridge_n, by_group = TRUE)
                  
                  # 调试输出：检查Bridge和bridgeGroup返回值
                  cat("====== 桥接分析调试信息 ======\n")
                  cat("Bridge分析结果:\n")
                  if(!is.null(values$bridge_result)) {
                    cat("- Bridge result存在，类型:", class(values$bridge_result), "\n")
                    if(is.list(values$bridge_result)) {
                      cat("- Bridge result元素:", names(values$bridge_result), "\n")
                      if(!is.null(values$bridge_result$bridge_data)) {
                        cat("- bridge_data中心性长度:", length(values$bridge_result$bridge_data), "\n")
                        cat("- bridge_data中心性范围:", range(values$bridge_result$bridge_data, na.rm = TRUE), "\n")
                      } else {
                        cat("- bridge_data中心性为NULL\n")
                      }
                    }
                  } else {
                    cat("- Bridge result为NULL\n")
                  }
                  
                  cat("bridgeGroup返回值类型:", class(values$bridge_groups), "\n")
                  cat("bridgeGroup返回值长度:", length(values$bridge_groups), "\n")
                  if(is.vector(values$bridge_groups) && !is.null(names(values$bridge_groups))) {
                    cat("bridgeGroup返回值结构（向量）:\n")
                    print(head(values$bridge_groups, 10))
                    cat("唯一值:", unique(values$bridge_groups), "\n")
                  } else if(is.list(values$bridge_groups)) {
                    cat("bridgeGroup返回值结构（列表）:\n")
                    print(str(values$bridge_groups))
                  }
                  cat("===============================\n")
                  
                  # 创建英文组名映射
                  group_name_mapping <- list()
                  if(!is.null(values$scales) && !is.null(values$scales$summary)) {
                    for(scale_name in names(values$scales$summary)) {
                      scale_info <- values$scales$summary[[scale_name]]
                      if(!is.null(scale_info$config) && !is.null(scale_info$config$name_en)) {
                        # 使用英文名称的缩写
                        if(scale_name == "AUDIT") {
                          group_name_mapping[[scale_name]] <- "AUDIT"
                        } else if(scale_name == "HRF") {
                          group_name_mapping[["Habit"]] <- "Habit"
                          group_name_mapping[["Reward"]] <- "Reward"
                          group_name_mapping[["Fear"]] <- "Fear"
                        } else {
                          group_name_mapping[[scale_name]] <- scale_name
                        }
                      }
                    }
                  }
                  
                  # 创建桥接节点信息：保持原有量表分组，但用形状区分桥接节点
                  variable_names <- colnames(analysis_data_final)
                  n_vars <- length(variable_names)
                  
                  # 根据原始分组重建组名（使用量表名而不是Bridge）
                  bridge_groups_display <- rep("未分组", n_vars)
                  names(bridge_groups_display) <- variable_names
                  
                  # 根据原始变量分组重新分配组名
                  for(group_name in names(values$variable_groups)) {
                    scales_in_group <- values$variable_groups[[group_name]]
                    for(scale_name in scales_in_group) {
                      matching_indices <- which(
                        startsWith(variable_names, paste0(scale_name, "_")) |
                        grepl(paste0("_", scale_name, "_"), variable_names) |
                        endsWith(variable_names, paste0("_", scale_name)) |
                        variable_names == scale_name
                      )
                      bridge_groups_display[matching_indices] <- group_name
                    }
                  }
                  
                  # 创建形状信息：桥接节点用方形，普通节点用圆形
                  shape_list <- ifelse(values$bridge_groups == "Bridge", "square", "circle")
                  bridge_groups_en <- bridge_groups_display
                  
                  # 使用您指定的确切配色方案
                  zcolor <- c("#63bbd0","#f87599","#f1f0ed","#fc8c23","#1ba784","#63bbd0","#f87599","#fed71a",
                              "#d1c2d3","#304fb0","#c6dfc8","#a8456b","#2486b9",
                              "#e16c96","#fc8c23","#280c1c",
                              "#fbb957","#de1c31","#ee3f4d",
                              "#c0c4c3","#c6e6e8",
                              "#12a182","#eb3c70","#eaad1a","#45b787","#d11a2d",
                              "#eea08c","#cfccc9",
                              "#2b1216","#61649f","#93b5cf","#c4cbcf",
                              "#c4d7d6","#248067","#fbda41","#f1f0ed")
                  
                  # 使用组级别的颜色向量
                  bridge_colors <- if(!is.null(values$network_group_colors)) {
                    values$network_group_colors
                  } else {
                    zcolor[1:length(values$network_groups_by_index)]
                  }
                  
                  # 记录桥接网络可视化代码
                  bridge_viz_code <- c(
                    "# 桥接网络可视化",
                    "shape_list <- ifelse(bridge_groups_result == 'Bridge', 'square', 'circle')",
                    "",
                    "# 生成桥接网络图（方形=桥接节点，圆形=普通节点）",
                    "bridge_network <- quickNet(",
                    "  data = analysis_data,",
                    "  title = 'Bridge Network Analysis',",
                    "  groups = groups,",
                    "  shape = shape_list,",
                    paste0("  threshold = ", input$threshold %||% 0.05, ","),
                    paste0("  edge.labels = ", input$show_edge_labels %||% TRUE, ","),
                    "  posCol = c('#2376b7', '#134857'),",
                    "  negCol = c('#d2568c', '#62102e'),",
                    "  color = c('#63bbd0', '#f87599', '#fed71a', '#d1c2d3')",
                    ")",
                    "",
                    "# 保存桥接网络图",
                    "get_network_plot(bridge_network, prefix = 'Fig3_bridge_network', width = 6, height = 4.5)"
                  )
                  values$code_recorder <- record_actual_code(values$code_recorder, bridge_viz_code, "bridge_visualization", "桥接网络可视化")
                  
                  # 生成桥接网络图（使用quickNet，突出显示桥接节点）
                  values$bridge_network <- quickNet(
                    analysis_data_final,
                    title = "Bridge Network Analysis", 
                    groups = values$network_groups_by_index,  # 使用正确的索引格式分组
                    shape = shape_list,
                    threshold = input$threshold %||% 0.05,
                    edge.labels = input$show_edge_labels %||% TRUE,
                    posCol = c("#2376b7", "#134857"),  # 正边颜色
                    negCol = c("#d2568c", "#62102e"),  # 负边颜色
                    color = bridge_colors,  # 使用组级别的配色
                    layout = values$layout,  # 使用统一的layout
                    legend = TRUE, 
                    legend.cex = 0.4,
                    vsize = 6, 
                    esize = 5, 
                    asize = 5, 
                    edge.label.cex = 1
                  )
                
                # 生成桥接网络图PDF（使用get_network_plot）
                if(!is.null(values$bridge_network)) {
                  tryCatch({
                    if(requireNamespace("quickNet", quietly = TRUE)) {
                      timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
                      prefix_network <- paste0("Fig3B_bridge_network_", timestamp)
                      
                      # 设置工作目录到输出文件夹
                      if(!is.null(values$output_folder)) {
                        old_wd <- setwd(values$output_folder)
                        on.exit(setwd(old_wd))
                      }
                      
                      # 调用get_network_plot生成桥接网络图PDF
                      get_network_plot(values$bridge_network, 
                                     prefix = prefix_network, 
                                     width = 6, height = 4.5)
                      
                      # 保存桥接网络图PDF路径
                      values$bridge_network_pdf_path <- file.path(values$output_folder, paste0(prefix_network, "_network_plot.pdf"))
                      cat("桥接网络图PDF:", values$bridge_network_pdf_path, "\n")
                    }
                  }, error = function(e) {
                    cat("桥接网络图生成失败:", e$message, "\n")
                  })
                }
                
                # 生成桥接中心性图PDF和CSV（使用get_bridge_plot）
                if(!is.null(values$bridge_result)) {
                  tryCatch({
                    if(requireNamespace("quickNet", quietly = TRUE)) {
                      timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
                      prefix_centrality <- paste0("Fig3c_bridge_centrality_", timestamp)
                      
                      # 设置工作目录到输出文件夹
                      if(!is.null(values$output_folder)) {
                        old_wd <- setwd(values$output_folder)
                        on.exit(setwd(old_wd))
                      }
                      
                      # 调用get_bridge_plot生成桥接中心性PDF图和CSV数据
                      get_bridge_plot(values$bridge_result, 
                                     prefix = prefix_centrality, 
                                     width = 6, height = 4.5)
                      
                      # 保存桥接中心性文件路径
                      values$bridge_pdf_path <- file.path(values$output_folder, paste0(prefix_centrality, "_bridge_plot.pdf"))
                      values$bridge_csv_path <- file.path(values$output_folder, paste0(prefix_centrality, "_bridge_table.csv"))
                      
                      cat("桥接中心性PDF:", values$bridge_pdf_path, "\n")
                      cat("桥接中心性CSV:", values$bridge_csv_path, "\n")
                    }
                  }, error = function(e) {
                    cat("get_bridge_plot调用失败:", e$message, "\n")
                  })
                }
                
                showNotification("桥接网络分析完成！", type = "message")
                
                # 更新完整脚本（包含桥接分析）
                if(!is.null(values$code_recorder) && !is.null(values$output_folder)) {
                  tryCatch({
                    script_path <- file.path(values$output_folder, "NetworkAnalysis_Complete_Script.R")
                    generate_complete_script(values$code_recorder, script_path)
                    cat("📝 已更新完整脚本（包含桥接分析）:", script_path, "\n")
                  }, error = function(e) {
                    cat("⚠️ 桥接分析脚本更新失败:", e$message, "\n")
                  })
                }
                }
              } else {
                showNotification("quickNet包不可用，跳过桥接分析", type = "warning")
                values$bridge_result <- NULL
                values$bridge_network <- NULL
              }
            } else {
              showNotification("桥接分析需要至少2个变量组", type = "warning")
              values$bridge_result <- NULL
              values$bridge_network <- NULL
            }
            
          }, error = function(e) {
            showNotification(paste("桥接分析失败:", e$message), type = "warning")
            values$bridge_result <- NULL
            values$bridge_network <- NULL
            values$bridge_groups <- NULL
            # 确保桥接分析失败不影响主网络分析
            cat("桥接分析错误（不影响主网络）:", e$message, "\n")
          })
        } else {
          # 如果没有启用桥接分析，清空相关结果
          values$bridge_result <- NULL
          values$bridge_network <- NULL
        }
        
        # 记录中心性分析代码
        if(!is.null(values$code_recorder)) {
          values$code_recorder <- add_code_record(
            values$code_recorder,
            "network_analysis",
            c(
              "",
              "# 中心性分析 Centrality Analysis",
              "centrality_result <- Centrality(network_result)",
              "print(centrality_result)",
              "",
              "# 中心性可视化",
              "png('Fig2_centrality_plot.png', width = 800, height = 600, res = 300)",
              "centralityPlot(network_result, include = c('Strength', 'Closeness', 'Betweenness'))",
              "dev.off()"
            ),
            "中心性分析阶段"
          )
          
          # 记录网络可视化代码
          values$code_recorder <- add_code_record(
            values$code_recorder,
            "visualization",
            c(
              "# ===== 网络可视化 Network Visualization =====",
              "",
              "# 主网络图",
              "png('Fig1_network_plot.png', width = 800, height = 600, res = 300)",
              "plot(network_result, ",
              "     layout = 'spring',",
              "     theme = 'colorblind',",
              "     edge.labels = TRUE,",
              "     node.width = 1.2,",
              "     title = 'Network Analysis')",
              "dev.off()",
              "",
              "# 网络布局保存",
              "layout_coords <- network_result$layout",
              "write.csv(layout_coords, 'network_layout.csv')"
            ),
            "网络可视化阶段"
          )
          
          # 记录结果保存代码
          values$code_recorder <- add_code_record(
            values$code_recorder,
            "exports",
            c(
              "# ===== 结果保存 Results Export =====",
              "",
              "# 保存网络对象",
              "saveRDS(network_result, 'network_result.rds')",
              "",
              "# 保存中心性结果",
              "if(exists('centrality_result')) {",
              "  saveRDS(centrality_result, 'centrality_result.rds')",
              "  write.csv(centrality_result$centrality_table, 'centrality_measures.csv')",
              "}",
              "",
              "# 网络连接矩阵导出",
              "adjacency_matrix <- network_result$graph$adjacency",
              "write.csv(adjacency_matrix, 'adjacency_matrix.csv')",
              "",
              "# 边权重矩阵导出",
              "if(!is.null(network_result$graph$weights)) {",
              "  weights_matrix <- network_result$graph$weights",
              "  write.csv(weights_matrix, 'weights_matrix.csv')",
              "}"
            ),
            "结果导出阶段"
          )
          
          # 生成完整脚本并保存
          # 注意：不在网络分析完成后立即生成脚本
          # 因为可能还有后续的桥接分析、稳定性分析、贝叶斯分析等
          # 脚本会在下载时或所有分析完成后生成
          if(!is.null(values$code_recorder)) {
            cat("📝 代码记录器已更新，包含网络分析代码\n")
          }
        }
        
        incProgress(1, detail = "网络分析完成!")
        
        showNotification(paste0("网络分析完成！使用了 ", n_vars, " 个变量，", complete_cases, " 个完整案例"), type = "message")
        
      }, error = function(e) {
        showNotification(paste("网络分析失败:", e$message), type = "error")
        values$network_result <- NULL
        values$centrality_result <- NULL
        values$stability_result <- NULL
      })
    })
  })
  
  # 判断分析是否完成
  output$analysisComplete <- reactive({
    return(!is.null(values$network_result))
  })
  outputOptions(output, "analysisComplete", suspendWhenHidden = FALSE)
  
  output$stabilityComplete <- reactive({
    return(!is.null(values$stability_result))
  })
  outputOptions(output, "stabilityComplete", suspendWhenHidden = FALSE)
  
  # 网络图输出
  output$network_plot <- renderPlot({
    req(values$network_result)
    
    # 调试信息
    cat("正在渲染网络图，network_result类型:", class(values$network_result), "\n")
    
    # 检查network_result是否为有效对象
    if (is.null(values$network_result)) {
      plot.new()
      text(0.5, 0.5, "网络结果为空，请重新运行分析", cex = 1.2, col = "red")
      return(NULL)
    }
    
    # 显示PDF文件生成位置信息（如果存在），但仍显示网络图
    
    # 确保plot正确显示
    tryCatch({
      # 检查对象是否具有plot方法
      if (inherits(values$network_result, c("quickNet", "qgraph", "igraph"))) {
        plot(values$network_result)
      } else {
        # 如果不是标准网络对象，尝试其他方法
        if (is.list(values$network_result) && !is.null(values$network_result$graph)) {
          plot(values$network_result$graph)
        } else {
          plot.new()
          text(0.5, 0.5, "无法绘制网络图：格式不支持", cex = 1.2, col = "orange")
        }
      }
    }, error = function(e) {
      # 绘图失败时显示错误信息
      plot.new()
      text(0.5, 0.5, paste("绘图失败:", e$message), cex = 1, col = "red", adj = c(0.5, 0.5))
    })
  })
  
  # 桥接网络图输出
  output$bridge_network_plot <- renderPlot({
    req(values$bridge_network)
    
    # 检查桥接网络结果是否为有效对象
    if (is.null(values$bridge_network)) {
      plot.new()
      text(0.5, 0.5, "桥接网络结果为空", cex = 1.2, col = "red")
      return(NULL)
    }
    
    # 显示PDF文件生成位置信息（如果存在），但仍显示桥接网络图
    
    # 绘制桥接网络图
    tryCatch({
      if (inherits(values$bridge_network, c("quickNet", "qgraph", "igraph"))) {
        plot(values$bridge_network)
      } else {
        if (is.list(values$bridge_network) && !is.null(values$bridge_network$graph)) {
          plot(values$bridge_network$graph)
        } else {
          plot.new()
          text(0.5, 0.5, "无法绘制桥接网络图：格式不支持", cex = 1.2, col = "orange")
        }
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("桥接网络绘图失败:", e$message), cex = 1, col = "red", adj = c(0.5, 0.5))
    })
  })
  
  
  # 桥接中心性数据表格 - 读取get_bridge_plot生成的CSV文件
  output$bridge_centrality_table <- DT::renderDataTable({
    # 依赖分析按钮确保更新
    input$run_analysis
    
    tryCatch({
      cat("=== 桥接中心性表格调试 ===\n")
      
      # 首先检查是否有CSV文件路径
      if(!is.null(values$bridge_csv_path) && file.exists(values$bridge_csv_path)) {
        cat("读取get_bridge_plot生成的CSV文件:", values$bridge_csv_path, "\n")
        
        # 读取CSV文件
        bridge_table <- read.csv(values$bridge_csv_path, stringsAsFactors = FALSE)
        cat("成功读取CSV文件，行数:", nrow(bridge_table), "，列数:", ncol(bridge_table), "\n")
        cat("列名:", colnames(bridge_table), "\n")
        
        # 添加序号列
        bridge_table$序号 <- 1:nrow(bridge_table)
        
        # 重新排列列的顺序
        if("序号" %in% colnames(bridge_table)) {
          bridge_table <- bridge_table[, c("序号", setdiff(colnames(bridge_table), "序号"))]
        }
        
        cat("返回桥接中心性表格，行数:", nrow(bridge_table), "\n")
        return(bridge_table)
      }
      
      # 如果没有CSV文件，检查bridge_result
      if(is.null(values$bridge_result)) {
        cat("桥接分析未运行\n")
        return(data.frame(状态 = "桥接分析未运行", 
                         提示 = "请在网络分析页面启用桥接分析"))
      }
      
      cat("bridge_result存在，元素:", names(values$bridge_result), "\n")
      
      # 检查是否有bridge_data
      if(is.null(values$bridge_result$bridge_data)) {
        cat("bridge_data为NULL，等待get_bridge_plot生成结果\n")
        return(data.frame(状态 = "正在生成桥接中心性数据", 
                         提示 = "请稍等，桥接分析正在生成结果文件"))
      }
      
      # 备用方案：直接从bridge_result读取数据
      bridge_centrality <- values$bridge_result$bridge_data
      cat("从bridge_result读取数据，长度:", length(bridge_centrality), "\n")
      
      # 获取变量名
      all_vars <- NULL
      if(!is.null(values$analysis_data)) {
        all_vars <- colnames(values$analysis_data)
      } else {
        all_vars <- paste0("Var", 1:length(bridge_centrality))
      }
      
      # 创建数据框
      bridge_table <- data.frame(
        序号 = 1:length(bridge_centrality),
        变量名 = all_vars[1:length(bridge_centrality)],
        桥接中心性 = round(as.numeric(bridge_centrality), 6),
        排名 = rank(-bridge_centrality, ties.method = "min"),
        stringsAsFactors = FALSE
      )
      
      # 按桥接中心性降序排列
      bridge_table <- bridge_table[order(-bridge_table$桥接中心性), ]
      bridge_table$序号 <- 1:nrow(bridge_table)
      
      cat("备用方案生成表格，行数:", nrow(bridge_table), "\n")
      return(bridge_table)
      
    }, error = function(e) {
      cat("桥接中心性表格生成错误:", e$message, "\n")
      return(data.frame(错误类型 = "表格生成失败", 
                       错误信息 = e$message,
                       建议 = "请检查桥接分析是否正常完成"))
    })
  }, options = list(
    pageLength = 15,
    scrollX = TRUE,
    order = list(list(2, 'desc')),
    columnDefs = list(
      list(targets = c(2), render = DT::JS("function(data, type, row) { return parseFloat(data).toFixed(6); }"))
    ),
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
  
  # 桥接中心性数据下载
  output$download_bridge_centrality <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("bridge_centrality_", timestamp, ".csv")
    },
    content = function(file) {
      tryCatch({
        # 获取桥接中心性数据
        if(!is.null(values$bridge_result) && !is.null(values$bridge_result$bridge)) {
          bridge_centrality <- values$bridge_result$bridge
          
          if(!is.null(values$analysis_data)) {
            all_vars <- colnames(values$analysis_data)
          } else {
            all_vars <- paste0("V", 1:length(bridge_centrality))
          }
          
          # 确保长度匹配
          if(length(all_vars) > length(bridge_centrality)) {
            all_vars <- all_vars[1:length(bridge_centrality)]
          } else if(length(all_vars) < length(bridge_centrality)) {
            additional_vars <- paste0("V", (length(all_vars)+1):length(bridge_centrality))
            all_vars <- c(all_vars, additional_vars)
          }
          
          bridge_table <- data.frame(
            Variable = all_vars,
            BridgeCentrality = bridge_centrality,
            Rank = rank(-bridge_centrality, ties.method = "min"),
            stringsAsFactors = FALSE
          )
          
          # 添加分组信息
          if(!is.null(values$bridge_groups)) {
            tryCatch({
              if(is.vector(values$bridge_groups) && !is.null(names(values$bridge_groups))) {
                bridge_table$Group <- values$bridge_groups[bridge_table$Variable]
                bridge_table$Group[is.na(bridge_table$Group)] <- "Ungrouped"
              } else {
                bridge_table$Group <- "Ungrouped"
              }
            }, error = function(e) {
              bridge_table$Group <- "Ungrouped"
            })
          } else {
            bridge_table$Group <- "Ungrouped"
          }
          
          # 按桥接中心性降序排列
          bridge_table <- bridge_table[order(-bridge_table$BridgeCentrality), ]
          
          # 保存到指定文件夹
          if(!is.null(values$output_folder)) {
            output_file <- file.path(values$output_folder, basename(file))
            write.csv(bridge_table, output_file, row.names = FALSE)
            cat("桥接中心性数据已保存到:", output_file, "\n")
          }
          
          # 同时保存到下载文件
          write.csv(bridge_table, file, row.names = FALSE)
        } else {
          # 空数据情况
          empty_data <- data.frame(Message = "桥接中心性数据不可用")
          write.csv(empty_data, file, row.names = FALSE)
        }
      }, error = function(e) {
        error_data <- data.frame(Error = paste("导出失败:", e$message))
        write.csv(error_data, file, row.names = FALSE)
      })
    }
  )
  
  # 中心性图输出
  output$centrality_plot <- renderPlot({
    req(values$centrality_result)
    plot(values$centrality_result$centralityPlot)
  })
  
  # 独立的稳定性分析
  observeEvent(input$run_stability, {
    req(values$analysis_data, values$network_result)
    
    withProgress(message = '正在进行稳定性分析...', value = 0, {
      
      tryCatch({
        incProgress(0.3, detail = "准备Bootstrap分析...")
        
        # 使用bootnet包进行稳定性分析（避免quickNet的Stability函数）
        if(requireNamespace("bootnet", quietly = TRUE)) {
          library(bootnet, quietly = TRUE)
          
          incProgress(0.5, detail = "计算边稳定性...")
          
          if(input$run_edge_stability) {
            tryCatch({
              # 记录边稳定性分析代码
              edge_stability_code <- c(
                "# ===== 边稳定性分析 Edge Stability Analysis =====",
                "library(bootnet)",
                "",
                paste0("# Bootstrap边稳定性分析 (", input$stability_bootstrap, "次重采样)"),
                paste0("edge_boot <- bootnet(analysis_data, nBoots = ", input$stability_bootstrap, ","),
                "                   default = 'EBICglasso', type = 'nonparametric')",
                "",
                "# 稳定性统计",
                "edge_stability_summary <- summary(edge_boot)",
                "print(edge_stability_summary)"
              )
              values$code_recorder <- record_actual_code(values$code_recorder, edge_stability_code, "edge_stability", "边稳定性分析")
              
              edge_boot <- bootnet(values$analysis_data, nBoots = input$stability_bootstrap, 
                                 default = "EBICglasso", type = "nonparametric")
              values$edge_stability <- edge_boot
              showNotification("边稳定性分析完成", type = "message")
            }, error = function(e) {
              showNotification(paste("边稳定性分析失败:", e$message), type = "error")
              values$edge_stability <- NULL
            })
          }
          
          incProgress(0.8, detail = "计算中心性稳定性...")
          
          if(input$run_centrality_stability) {
            tryCatch({
              # 记录中心性稳定性分析代码
              cent_stability_code <- c(
                "# ===== 中心性稳定性分析 Centrality Stability Analysis =====",
                "",
                paste0("# Bootstrap中心性稳定性分析 (", input$stability_bootstrap, "次重采样)"),
                paste0("cent_boot <- bootnet(analysis_data, nBoots = ", input$stability_bootstrap, ","),
                "                   default = 'EBICglasso', type = 'case',",
                "                   statistics = c('strength', 'closeness', 'betweenness'))",
                "",
                "# 中心性稳定性统计",
                "cent_stability_summary <- summary(cent_boot)",
                "print(cent_stability_summary)",
                "",
                "# CS系数计算（稳定性阈值）",
                "cs_coefficients <- corStability(cent_boot)",
                "print(cs_coefficients)"
              )
              values$code_recorder <- record_actual_code(values$code_recorder, cent_stability_code, "centrality_stability", "中心性稳定性分析")
              
              cent_boot <- bootnet(values$analysis_data, nBoots = input$stability_bootstrap,
                                 default = "EBICglasso", type = "case", 
                                 statistics = c("strength", "closeness", "betweenness"))
              values$centrality_stability <- cent_boot
              showNotification("中心性稳定性分析完成", type = "message")
            }, error = function(e) {
              showNotification(paste("中心性稳定性分析失败:", e$message), type = "error")
              values$centrality_stability <- NULL
            })
          }
          
          values$stability_result <- list(
            edge_stability = if(input$run_edge_stability) values$edge_stability else NULL,
            centrality_stability = if(input$run_centrality_stability) values$centrality_stability else NULL,
            bootstrap_n = input$stability_bootstrap
          )
          
          # 使用get_stability_plot生成稳定性分析辅助图表（对应主图Fig1-Fig2）
          tryCatch({
            timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
            
            # 设置工作目录到输出文件夹
            if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
              old_wd <- getwd()
              setwd(values$output_folder)
              on.exit(setwd(old_wd))
            }
            
            # 使用quickNet包进行完整的稳定性分析（对应主网络分析Fig1-Fig2）
            if(requireNamespace("quickNet", quietly = TRUE)) {
              
              # S1: 网络稳定性分析（对应Fig1主网络图的稳定性）
              if(!is.null(values$analysis_data)) {
                # 记录网络稳定性分析代码
                stability_analysis_code <- c(
                  "# ===== 网络稳定性分析 Network Stability Analysis =====",
                  "library(quickNet)",
                  "",
                  "# quickNet包的稳定性分析",
                  "sta_result <- Stability(analysis_data)",
                  "",
                  "# 生成稳定性图表",
                  paste0('timestamp <- "', timestamp, '"'),
                  'prefix <- paste0("SFig2_network_stability_", timestamp)',
                  'get_stability_plot(sta_result, prefix = prefix, width = 6, height = 4.5)',
                  'cat("网络稳定性分析完成，图表已保存\\n")'
                )
                values$code_recorder <- record_actual_code(values$code_recorder, stability_analysis_code, "stability_analysis", "quickNet网络稳定性分析")
                
                sta_result <- Stability(values$analysis_data)
                values$stability_complete <- sta_result
                
                # 使用get_stability_plot生成专业的稳定性图表
                s1_prefix <- paste0("S1_network_stability_", timestamp)
                get_stability_plot(sta_result, prefix = s1_prefix, width = 8, height = 6)
                
                values$network_stability_pdf <- paste0(s1_prefix, "_stability_plot.pdf")
                values$network_stability_csv <- paste0(s1_prefix, "_stability_data.csv")
                cat("S1网络稳定性图表已生成:", values$network_stability_pdf, "\n")
              }
              
              # S2: 中心性稳定性分析（对应Fig2中心性图的稳定性）
              if(!is.null(values$centrality_stability)) {
                s2_prefix <- paste0("S2_centrality_stability_", timestamp)
                
                # 生成中心性稳定性图
                pdf(paste0(s2_prefix, "_centrality_stability.pdf"), width = 8, height = 6)
                plot(values$centrality_stability, statistics = c("strength", "closeness", "betweenness"))
                dev.off()
                
                values$centrality_stability_pdf <- paste0(s2_prefix, "_centrality_stability.pdf")
                cat("S2中心性稳定性图表已生成:", values$centrality_stability_pdf, "\n")
                
                # S2相关的CS系数分析
                cs_pdf_file <- paste0(s2_prefix, "_cs_coefficient.pdf")
                pdf(cs_pdf_file, width = 8, height = 6)
                plot(values$centrality_stability, "strength")
                dev.off()
                
                values$cs_coefficient_pdf <- cs_pdf_file
                
                # 保存CS系数数据
                cs_data_file <- paste0(s2_prefix, "_cs_coefficient.csv")
                tryCatch({
                  cs_stats <- corStability(values$centrality_stability)
                  cs_df <- data.frame(
                    Statistic = names(cs_stats),
                    CS_Coefficient = as.numeric(cs_stats),
                    Interpretation = ifelse(as.numeric(cs_stats) > 0.5, "稳定 (>0.5)", 
                                          ifelse(as.numeric(cs_stats) > 0.25, "可接受 (0.25-0.5)", "不稳定 (<0.25)"))
                  )
                  write.csv(cs_df, cs_data_file, row.names = FALSE)
                  cat("S2 CS系数数据已保存:", cs_data_file, "\n")
                }, error = function(e) {
                  cat("保存CS系数数据失败:", e$message, "\n")
                })
              }
              
              # S3: 边稳定性分析（对应主网络边的稳定性，如果有的话）
              if(!is.null(values$edge_stability)) {
                s3_prefix <- paste0("S3_edge_stability_", timestamp)
                s3_pdf_file <- paste0(s3_prefix, "_edge_stability.pdf")
                
                pdf(s3_pdf_file, width = 8, height = 6)
                plot(values$edge_stability, labels = FALSE, order = "sample")
                dev.off()
                
                values$edge_stability_pdf <- s3_pdf_file
                cat("S3边稳定性图表已生成:", values$edge_stability_pdf, "\n")
              }
              
            } else {
              showNotification("需要quickNet包进行稳定性分析", type = "warning")
            }
            
          }, error = function(e) {
            cat("生成稳定性SFig失败:", e$message, "\n")
          })
          
          incProgress(1, detail = "稳定性分析完成!")
          showNotification("稳定性分析完成！", type = "message")
          
          # 更新完整脚本（包含稳定性分析）
          if(!is.null(values$code_recorder) && !is.null(values$output_folder)) {
            tryCatch({
              script_path <- file.path(values$output_folder, "NetworkAnalysis_Complete_Script.R")
              generate_complete_script(values$code_recorder, script_path)
              cat("📝 已更新完整脚本（包含稳定性分析）:", script_path, "\n")
            }, error = function(e) {
              cat("⚠️ 稳定性分析脚本更新失败:", e$message, "\n")
            })
          }
          
        } else {
          showNotification("需要bootnet包进行稳定性分析，请先安装", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("稳定性分析失败:", e$message), type = "error")
        values$stability_result <- NULL
      })
    })
  })
  
  # 稳定性图输出
  output$edge_stability_plot <- renderPlot({
    req(values$stability_result, values$stability_result$edge_stability)
    if(requireNamespace("bootnet", quietly = TRUE)) {
      plot(values$stability_result$edge_stability, labels = FALSE, order = "sample")
    }
  })
  
  output$centrality_stability_plot <- renderPlot({
    req(values$stability_result, values$stability_result$centrality_stability)
    if(requireNamespace("bootnet", quietly = TRUE)) {
      plot(values$stability_result$centrality_stability, statistics = c("strength", "closeness", "betweenness"))
    }
  })
  
  output$stability_summary <- renderText({
    req(values$stability_result)
    
    result <- paste0("稳定性分析摘要（辅助材料）\n",
                    "========================\n\n",
                    "Bootstrap次数: ", values$stability_result$bootstrap_n, "\n\n")
    
    # S1: 网络稳定性（对应Fig1主网络）
    if(!is.null(values$network_stability_pdf)) {
      result <- paste0(result, "✓ S1: 网络稳定性分析已完成\n")
      result <- paste0(result, "  📊 ", basename(values$network_stability_pdf), " (对应Fig1网络图)\n")
      if(!is.null(values$network_stability_csv)) {
        result <- paste0(result, "  📄 ", basename(values$network_stability_csv), "\n")
      }
    }
    
    # S2: 中心性稳定性（对应Fig2中心性图）
    if(!is.null(values$stability_result$centrality_stability)) {
      result <- paste0(result, "\n✓ S2: 中心性稳定性分析已完成\n")
      if(!is.null(values$centrality_stability_pdf)) {
        result <- paste0(result, "  📊 ", basename(values$centrality_stability_pdf), " (对应Fig2中心性图)\n")
      }
      if(!is.null(values$cs_coefficient_pdf)) {
        result <- paste0(result, "  📊 ", basename(values$cs_coefficient_pdf), " (CS系数)\n")
      }
    }
    
    # S3: 边稳定性（对应主网络边）
    if(!is.null(values$stability_result$edge_stability)) {
      result <- paste0(result, "\n✓ S3: 边稳定性分析已完成\n")
      if(!is.null(values$edge_stability_pdf)) {
        result <- paste0(result, "  📊 ", basename(values$edge_stability_pdf), " (网络边稳定性)\n")
      }
    }
    
    result <- paste0(result, "\n📁 文件组织说明:\n",
                    "   S1-S3: 对应Fig1-Fig3主图的稳定性分析\n",
                    "   网页显示实时图表，PDF用于论文发表\n",
                    "   所有辅助材料保存到results文件夹\n")
    
    result <- paste0(result, "\n📖 解读建议:\n",
                    "- 边的置信区间较窄表示边稳定\n",
                    "- 中心性指标的稳定性应大于0.25\n",
                    "- CS系数应大于0.5表示稳定\n",
                    "- S编号与Fig主图编号对应")
    
    return(result)
  })
  
  # =============================================================================
  # 组间网络差异比较服务器端逻辑
  # =============================================================================
  
  # 动态更新分组变量选择
  observe({
    if(!is.null(values$analysis_data) && !is.null(values$processed_data)) {
      # 获取分析数据中的数值型变量（用于数值分组）
      numeric_vars <- names(values$analysis_data)[sapply(values$analysis_data, is.numeric)]
      
      # 调试输出
      cat("更新分组变量选择:\n")
      cat("analysis_data变量:", names(values$analysis_data), "\n")
      cat("processed_data变量:", names(values$processed_data), "\n")
      cat("数值型变量:", numeric_vars, "\n")
      
      # 获取原始数据中的分类变量（用于分类分组）
      # 查找可能的分类变量：字符型、因子型，或值较少的数值型变量
      categorical_candidates <- c()
      for(col_name in names(values$processed_data)) {
        col_data <- values$processed_data[[col_name]]
        
        # 调试输出每个变量的情况
        if(col_name %in% c("Gender", "ID", "Age")) {
          cat("检查变量", col_name, ":\n")
          cat("  类型:", class(col_data), "\n")
          cat("  唯一值数量:", length(unique(col_data[!is.na(col_data)])), "\n")
          cat("  唯一值:", unique(col_data[!is.na(col_data)]), "\n")
          cat("  缺失值数量:", sum(is.na(col_data)), "\n")
        }
        
        # 字符型或因子型变量
        if(is.character(col_data) || is.factor(col_data)) {
          unique_count <- length(unique(col_data[!is.na(col_data)]))
          if(unique_count >= 2 && unique_count <= 10) {
            categorical_candidates <- c(categorical_candidates, col_name)
          }
        }
        # 值较少的数值型变量（可能是编码的分类变量）
        else if(is.numeric(col_data)) {
          unique_count <- length(unique(col_data[!is.na(col_data)]))
          if(unique_count >= 2 && unique_count <= 5) {
            categorical_candidates <- c(categorical_candidates, col_name)
          }
        }
      }
      
      cat("分类变量候选:", categorical_candidates, "\n")
      
      # 组合选项
      all_choices <- list()
      
      if(length(numeric_vars) > 0) {
        all_choices[["数值变量（分析数据）"]] <- setNames(numeric_vars, paste0("🔢 ", numeric_vars))
      }
      
      if(length(categorical_candidates) > 0) {
        all_choices[["分类变量（原始数据）"]] <- setNames(categorical_candidates, paste0("📊 ", categorical_candidates))
      }
      
      # 更新选择
      if(length(all_choices) > 0) {
        updateSelectInput(session, "group_variable", 
                         choices = all_choices,
                         selected = if(length(numeric_vars) > 0) numeric_vars[1] else categorical_candidates[1])
      }
    }
  })
  
  # 组间比较分析完成状态
  output$groupCompareComplete <- reactive({
    return(!is.null(values$group_compare_result))
  })
  outputOptions(output, "groupCompareComplete", suspendWhenHidden = FALSE)
  
  # 执行组间比较分析
  observeEvent(input$run_group_compare, {
    req(values$analysis_data, input$group_variable)
    
    if(is.null(values$network_result)) {
      showNotification("请先完成网络分析", type = "error")
      return()
    }
    
    withProgress(message = '正在进行组间比较分析...', value = 0, {
      
      incProgress(0.1, detail = "准备分组数据...")
      
      # 获取分组变量 - 如果是分类变量，从原始数据获取
      if(input$group_variable %in% names(values$analysis_data)) {
        group_var <- values$analysis_data[[input$group_variable]]
        data_source <- "analysis"
        cat("从analysis_data获取分组变量:", input$group_variable, "\n")
      } else if(input$group_variable %in% names(values$processed_data)) {
        group_var <- values$processed_data[[input$group_variable]]
        data_source <- "processed"
        cat("从processed_data获取分组变量:", input$group_variable, "\n")
      } else {
        cat("分组变量不存在 - input$group_variable:", input$group_variable, "\n")
        cat("analysis_data变量名:", names(values$analysis_data), "\n")
        cat("processed_data变量名:", names(values$processed_data), "\n")
        showNotification("未找到选择的分组变量", type = "error")
        return()
      }
      
      # 调试输出
      cat("分组变量值:", head(group_var, 20), "\n")
      cat("分组变量类型:", class(group_var), "\n")
      cat("分组变量长度:", length(group_var), "\n")
      cat("缺失值数量:", sum(is.na(group_var)), "\n")
      
      # 严格的数据验证
      if(all(is.na(group_var))) {
        showNotification("选择的分组变量全为缺失值", type = "error")
        return()
      }
      
      # 检查无穷值
      if(any(is.infinite(group_var), na.rm = TRUE)) {
        showNotification("分组变量包含无穷值，请检查数据质量", type = "error")
        return()
      }
      
      # 检查有效值数量 - 修复：字符型变量不需要is.finite检查
      if(is.numeric(group_var)) {
        valid_values <- group_var[!is.na(group_var) & is.finite(group_var)]
      } else {
        valid_values <- group_var[!is.na(group_var)]
      }
      
      cat("有效值数量:", length(valid_values), "\n")
      cat("有效值内容:", head(valid_values, 10), "\n")
      
      if(length(valid_values) < 20) {
        showNotification(paste0("分组变量有效值过少 (", length(valid_values), ")，建议至少20个有效值"), type = "error")
        return()
      }
      
      # 检查变异性（对于数值变量）
      if(is.numeric(valid_values) && sd(valid_values, na.rm = TRUE) == 0) {
        showNotification("分组变量没有变异性（所有值相同），无法进行分组", type = "error")
        return()
      }
      
      # 检查分类变量的唯一值数量
      if(is.character(valid_values) || is.factor(valid_values)) {
        unique_count <- length(unique(valid_values))
        cat("分类变量唯一值数量:", unique_count, "\n")
        cat("唯一值:", unique(valid_values), "\n")
        if(unique_count < 2) {
          showNotification("分类变量只有一个类别，无法进行分组比较", type = "error")
          return()
        }
      }
      
      # 根据选择的方法进行分组
      tryCatch({
        # 为了处理分组变量可能来自不同数据源的情况，需要确保索引一致性
        # 如果分组变量来自processed_data，需要找到对应的analysis_data行
        if(data_source == "processed" && nrow(values$processed_data) > nrow(values$analysis_data)) {
          # 如果processed_data比analysis_data行数多，需要匹配索引
          # 假设analysis_data是processed_data的子集，按行名或ID匹配
          if(all(rownames(values$analysis_data) %in% rownames(values$processed_data))) {
            group_var <- group_var[rownames(values$processed_data) %in% rownames(values$analysis_data)]
          } else {
            # 如果行名不匹配，按位置匹配（前n行）
            group_var <- group_var[1:nrow(values$analysis_data)]
          }
        }
        
        # 初始化threshold为NULL，对于不使用threshold的方法
        threshold <- NULL
        
        if(input$group_method == "median") {
          if(!is.numeric(group_var)) {
            showNotification("中位数分组只适用于数值变量，请选择'分类变量分组'方法", type = "error")
            return()
          }
          threshold <- median(valid_values)
          group_indices1 <- group_var < threshold & !is.na(group_var) & is.finite(group_var)
          group_indices2 <- group_var >= threshold & !is.na(group_var) & is.finite(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_低分组")
          group2_name <- paste0(input$group_variable, "_高分组")
        } else if(input$group_method == "mean") {
          threshold <- mean(valid_values)
          group_indices1 <- group_var < threshold & !is.na(group_var) & is.finite(group_var)
          group_indices2 <- group_var >= threshold & !is.na(group_var) & is.finite(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_低分组")
          group2_name <- paste0(input$group_variable, "_高分组")
        } else if(input$group_method == "tertile") {
          q1 <- quantile(valid_values, 0.33)
          q3 <- quantile(valid_values, 0.67)
          threshold <- paste0("Q1=", round(q1, 2), ", Q3=", round(q3, 2))  # 记录分位数信息
          group_indices1 <- group_var <= q1 & !is.na(group_var) & is.finite(group_var)
          group_indices2 <- group_var >= q3 & !is.na(group_var) & is.finite(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_低三分位")
          group2_name <- paste0(input$group_variable, "_高三分位")
        } else if(input$group_method == "extreme_27") {
          # 前后27%分组
          q27 <- quantile(valid_values, 0.27)
          q73 <- quantile(valid_values, 0.73)
          threshold <- paste0("Q27=", round(q27, 2), ", Q73=", round(q73, 2))  # 记录分位数信息
          group_indices1 <- group_var <= q27 & !is.na(group_var) & is.finite(group_var)
          group_indices2 <- group_var >= q73 & !is.na(group_var) & is.finite(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_低27%")
          group2_name <- paste0(input$group_variable, "_高27%")
        } else if(input$group_method == "categorical") {
          # 分类变量分组 - 先进行性别变量智能标准化
          group_var <- standardize_gender_variable(group_var, input$group_variable)
          
          unique_values <- unique(group_var[!is.na(group_var)])
          
          cat("分类变量分组调试:\n")
          cat("唯一值:", unique_values, "\n")
          cat("唯一值数量:", length(unique_values), "\n")
          
          if(length(unique_values) < 2) {
            showNotification("分类变量值少于2个，无法分组", type = "error")
            return()
          }
          
          if(length(unique_values) > 10) {
            showNotification("分类变量值过多（>10），建议选择数值变量", type = "error")
            return()
          }
          
          # 选择前两个最常见的类别
          value_counts <- table(group_var)
          sorted_values <- sort(value_counts, decreasing = TRUE)
          
          cat("值计数:", value_counts, "\n")
          cat("排序后的值:", names(sorted_values), "\n")
          
          if(length(sorted_values) >= 2) {
            value1 <- names(sorted_values)[1]
            value2 <- names(sorted_values)[2]
            threshold <- paste0(value1, " vs ", value2)  # 记录分类信息
            
            cat("选择的两个类别:", value1, "vs", value2, "\n")
            
            group_indices1 <- group_var == value1 & !is.na(group_var)
            group_indices2 <- group_var == value2 & !is.na(group_var)
            
            cat("组1索引数量:", sum(group_indices1), "\n")
            cat("组2索引数量:", sum(group_indices2), "\n")
            
            group1_data <- values$analysis_data[group_indices1, ]
            group2_data <- values$analysis_data[group_indices2, ]
            group1_name <- paste0(input$group_variable, "_", value1)
            group2_name <- paste0(input$group_variable, "_", value2)
            
            cat("组1数据行数:", nrow(group1_data), "\n")
            cat("组2数据行数:", nrow(group2_data), "\n")
          }
        } else if(input$group_method == "custom") {
          threshold <- input$custom_threshold
          group_indices1 <- group_var < threshold & !is.na(group_var) & is.finite(group_var)
          group_indices2 <- group_var >= threshold & !is.na(group_var) & is.finite(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_<", threshold)
          group2_name <- paste0(input$group_variable, "_>=", threshold)
        }
        
        # 检查分组结果
        if(nrow(group1_data) < 10 || nrow(group2_data) < 10) {
          showNotification("分组后样本量过小（每组至少需要10个案例）", type = "error")
          return()
        }
        
        incProgress(0.3, detail = paste0("组1: ", nrow(group1_data), " 案例, 组2: ", nrow(group2_data), " 案例"))
        
        # 数据清理：移除无限值和缺失值
        incProgress(0.4, detail = "清理数据...")
        
        # 清理group1_data
        group1_clean <- group1_data
        for(col in names(group1_clean)) {
          if(is.numeric(group1_clean[[col]])) {
            # 移除无限值和缺失值
            group1_clean[[col]][!is.finite(group1_clean[[col]])] <- NA
          }
        }
        # 移除包含任何缺失值的行
        group1_clean <- group1_clean[complete.cases(group1_clean), ]
        
        # 清理group2_data
        group2_clean <- group2_data
        for(col in names(group2_clean)) {
          if(is.numeric(group2_clean[[col]])) {
            # 移除无限值和缺失值
            group2_clean[[col]][!is.finite(group2_clean[[col]])] <- NA
          }
        }
        # 移除包含任何缺失值的行
        group2_clean <- group2_clean[complete.cases(group2_clean), ]
        
        # 检查清理后的样本量
        if(nrow(group1_clean) < 10 || nrow(group2_clean) < 10) {
          showNotification(paste0("数据清理后样本量过小（组1: ", nrow(group1_clean), ", 组2: ", nrow(group2_clean), "），每组至少需要10个完整案例"), type = "error")
          return()
        }
        
        cat("数据清理结果:\n")
        cat("组1: 原始", nrow(group1_data), "行 -> 清理后", nrow(group1_clean), "行\n")
        cat("组2: 原始", nrow(group2_data), "行 -> 清理后", nrow(group2_clean), "行\n")
        
        # 执行网络比较
        incProgress(0.5, detail = "执行置换检验...")
        
        if(requireNamespace("quickNet", quietly = TRUE)) {
          # 记录组别对比分析代码
          group_comparison_code <- c(
            "# ===== 组别网络对比分析 Group Network Comparison =====",
            "library(quickNet)",
            "",
            "# 分组数据准备",
            paste0("group_var <- '", input$temp_group_var, "'"),
            "group1_data <- analysis_data[analysis_data[[group_var]] == unique(analysis_data[[group_var]])[1], ]",
            "group2_data <- analysis_data[analysis_data[[group_var]] == unique(analysis_data[[group_var]])[2], ]",
            "",
            "# 移除分组变量",
            "group1_data <- group1_data[, !names(group1_data) %in% group_var]",
            "group2_data <- group2_data[, !names(group2_data) %in% group_var]",
            "",
            "# 确保完整案例",
            "group1_clean <- group1_data[complete.cases(group1_data), ]",
            "group2_clean <- group2_data[complete.cases(group2_data), ]",
            "",
            paste0("# 网络比较测试 (", input$permutation_n, "次置换)"),
            "compare_result <- NetCompare(",
            "  group1_clean, group2_clean,",
            paste0("  it = ", input$permutation_n, ","),
            paste0("  p.adjust.methods = '", input$p_adjust_method, "'"),
            ")",
            "",
            "# 输出比较结果",
            "print('全局强度不变性检验:')",
            "print(compare_result$glstrinv.pval)",
            "print('网络结构不变性检验:')",
            "print(compare_result$nwinv.pval)",
            "",
            "# 显著差异边",
            "if(!is.null(compare_result$diff_sig)) {",
            "  cat('显著差异边数量:', sum(compare_result$diff_sig != 0, na.rm = TRUE), '\\n')",
            "}"
          )
          values$code_recorder <- record_actual_code(values$code_recorder, group_comparison_code, "group_comparison", "组别网络对比分析")
          
          compare_result <- NetCompare(
            group1_clean, group2_clean,
            it = input$permutation_n,
            p.adjust.methods = input$p_adjust_method
          )
          
          incProgress(0.8, detail = "生成比较图和结果...")
          
          # 解析NetCompare结果结构，包含完整的NCT字段
          nct_result <- list(
            # 全局强度不变性检验
            glstrinv.real = compare_result$glstrinv.real,
            glstrinv.sep = compare_result$glstrinv.sep, 
            glstrinv.pval = compare_result$glstrinv.pval,
            glstrinv.perm = compare_result$glstrinv.perm,
            
            # 网络结构不变性检验
            nwinv.real = compare_result$nwinv.real,
            nwinv.pval = compare_result$nwinv.pval,
            nwinv.perm = compare_result$nwinv.perm,
            
            # 边不变性检验
            einv.real = compare_result$einv.real,
            einv.pvals = compare_result$einv.pvals,
            einv.perm = compare_result$einv.perm,
            
            # 差异显著性结果
            diff_sig = compare_result$diff_sig,
            edge_weight_p = compare_result$edge_weight_p
          )
          
          # 保存结果
          values$group_compare_result <- list(
            compare_result = compare_result,
            nct_result = nct_result,  # 添加结构化的NCT结果
            group1_data = group1_clean,  # 使用清理后的数据
            group2_data = group2_clean,  # 使用清理后的数据
            group1_name = group1_name,
            group2_name = group2_name,
            group_variable = input$group_variable,
            threshold = threshold,
            method = input$group_method,
            permutation_n = input$permutation_n,
            p_adjust_method = input$p_adjust_method
          )
          
          # 使用get_compare_plot生成网络比较的PDF图和CSV数据
          tryCatch({
            if(requireNamespace("quickNet", quietly = TRUE)) {
              timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
              
              # 简洁命名：Fig3
              prefix <- "Fig3"
              
              # 切换到输出文件夹
              if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
                old_wd <- getwd()
                setwd(values$output_folder)
                on.exit(setwd(old_wd))
              }
              
              # 需要为get_compare_plot提供网络对象，使用主网络分析结果
              network_obj <- values$network_result
              
              # 调用get_compare_plot生成PDF图（按照你的标准格式）
              quickNet::get_compare_plot(compare_result, network_obj, 
                             prefix = prefix, 
                             width = 6, height = 4.5)
              
              # 简洁命名的CSV文件
              diff_csv_filename <- paste0(prefix, "_diff.csv")
              pval_csv_filename <- paste0(prefix, "_pval.csv")
              
              # 保存文件路径
              values$compare_pdf_path <- file.path(values$output_folder, paste0(prefix, ".pdf"))
              values$compare_diff_csv_path <- file.path(values$output_folder, diff_csv_filename)
              values$compare_pval_csv_path <- file.path(values$output_folder, pval_csv_filename)
              
              # 按照tutorial标准格式生成CSV数据文件
              if(!is.null(compare_result$diff_sig)) {
                write.csv(data.frame(compare_result$diff_sig), 
                         values$compare_diff_csv_path, 
                         row.names = TRUE)
                cat("已保存差异网络CSV:", values$compare_diff_csv_path, "\n")
              }
              
              if(!is.null(compare_result$edge_weight_p)) {
                write.csv(data.frame(compare_result$edge_weight_p), 
                         values$compare_pval_csv_path, 
                         row.names = TRUE)
                cat("已保存P值矩阵CSV:", values$compare_pval_csv_path, "\n")
              }
              
              cat("已使用get_compare_plot生成网络比较结果\n")
              cat("PDF文件:", values$compare_pdf_path, "\n")
            }
          }, error = function(e) {
            cat("get_compare_plot调用失败:", e$message, "\n")
          })
          
          # 桥接网络比较分析（如果启用）
          if(!is.null(input$enable_bridge_compare) && input$enable_bridge_compare && 
             !is.null(values$bridge_result) && !is.null(values$bridge_groups)) {
            
            incProgress(0.9, detail = "进行桥接网络比较分析...")
            
            tryCatch({
              # 为两组数据分别进行桥接分析
              bridge_groups_template <- values$variable_groups  # 使用原始的变量分组
              
              # 组1桥接分析
              if(requireNamespace("quickNet", quietly = TRUE)) {
                # 构建组1的网络用于桥接分析
                group1_network <- safe_network_analysis(
                  data = group1_clean,  # 使用清理后的数据
                  threshold = input$threshold %||% 0.05,
                  edge_labels = FALSE,  # 桥接比较时简化显示
                  colors = values$colors  # 使用统一的配色
                )
                
                # 组1桥接分析
                group1_bridge_result <- Bridge(group1_network, communities = bridge_groups_template)
                group1_bridge_groups <- bridgeGroup(group1_bridge_result, bridge_groups_template, 
                                                   labels = NULL, n = input$bridge_n %||% 1, by_group = TRUE)
                
                # 构建组2的网络用于桥接分析
                group2_network <- safe_network_analysis(
                  data = group2_clean,  # 使用清理后的数据
                  threshold = input$threshold %||% 0.05,
                  edge_labels = FALSE,  # 桥接比较时简化显示
                  colors = values$colors  # 使用统一的配色
                )
                
                # 组2桥接分析
                group2_bridge_result <- Bridge(group2_network, communities = bridge_groups_template)
                group2_bridge_groups <- bridgeGroup(group2_bridge_result, bridge_groups_template, 
                                                   labels = NULL, n = input$bridge_n %||% 1, by_group = TRUE)
                
                # 比较桥接节点
                all_vars <- names(group1_bridge_groups)
                bridge_comparison <- data.frame(
                  Variable = all_vars,
                  Group1_BridgeStatus = group1_bridge_groups[all_vars],
                  Group2_BridgeStatus = group2_bridge_groups[all_vars],
                  stringsAsFactors = FALSE
                )
                
                # 识别桥接状态变化
                bridge_comparison$StatusChange <- ifelse(
                  bridge_comparison$Group1_BridgeStatus != bridge_comparison$Group2_BridgeStatus,
                  paste0(bridge_comparison$Group1_BridgeStatus, " → ", bridge_comparison$Group2_BridgeStatus),
                  "无变化"
                )
                
                # 统计桥接节点
                group1_bridges <- all_vars[group1_bridge_groups == "Bridge"]
                group2_bridges <- all_vars[group2_bridge_groups == "Bridge"]
                
                # 保存桥接比较结果
                values$bridge_compare_result <- list(
                  group1_bridge_result = group1_bridge_result,
                  group2_bridge_result = group2_bridge_result,
                  group1_bridge_groups = group1_bridge_groups,
                  group2_bridge_groups = group2_bridge_groups,
                  bridge_comparison = bridge_comparison,
                  group1_bridges = group1_bridges,
                  group2_bridges = group2_bridges,
                  group1_name = group1_name,
                  group2_name = group2_name
                )
                
                showNotification(paste0("桥接网络比较完成！组1: ", length(group1_bridges), " 个桥接节点，组2: ", length(group2_bridges), " 个桥接节点"), type = "message")
              }
            }, error = function(e) {
              showNotification(paste("桥接网络比较失败:", e$message), type = "warning")
              values$bridge_compare_result <- NULL
            })
          } else {
            # 如果没有启用桥接比较，清空相关结果
            values$bridge_compare_result <- NULL
          }
          
          incProgress(1, detail = "组间比较完成!")
          
          showNotification(paste0("组间比较分析完成！组1: ", nrow(group1_clean), " 案例，组2: ", nrow(group2_clean), " 案例"), type = "message")
          
          # 更新完整脚本（包含组别对比分析）
          if(!is.null(values$code_recorder) && !is.null(values$output_folder)) {
            tryCatch({
              script_path <- file.path(values$output_folder, "NetworkAnalysis_Complete_Script.R")
              generate_complete_script(values$code_recorder, script_path)
              cat("📝 已更新完整脚本（包含组别对比分析）:", script_path, "\n")
            }, error = function(e) {
              cat("⚠️ 组别对比分析脚本更新失败:", e$message, "\n")
            })
          }
          
        } else {
          showNotification("quickNet包不可用，无法进行组间比较", type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("组间比较分析失败:", e$message), type = "error")
        values$group_compare_result <- NULL
      })
    })
  })
  
  # 组间比较差异网络图
  output$group_compare_plot <- renderPlot({
    req(values$group_compare_result)
    
    # 显示PDF文件生成位置信息（如果存在），但仍显示比较图
    
    # 如果没有PDF文件，则直接显示比较图（与get_compare_plot相同参数）
    tryCatch({
      if(requireNamespace("quickNet", quietly = TRUE)) {
        quickNet::get_compare_plot(values$group_compare_result$compare_result, values$network_result)
      } else {
        plot.new()
        text(0.5, 0.5, "quickNet包不可用", cex = 1.5, col = "red")
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("比较图生成失败:", e$message), cex = 1.2, col = "red")
    })
  })
  
  # 移除了正边和负边差异图（只保留综合差异图）
  
  # 显著性检验表格
  output$group_compare_table <- DT::renderDataTable({
    req(values$group_compare_result)
    
    tryCatch({
      result <- values$group_compare_result$compare_result
      
      # 创建显著性检验结果表格，显示diff_sig和edge_weight_p矩阵
      sig_results <- NULL
      
      # 优先使用diff_sig和edge_weight_p矩阵
      if(!is.null(result$diff_sig) && !is.null(result$edge_weight_p)) {
        
        diff_matrix <- as.matrix(result$diff_sig)
        p_matrix <- as.matrix(result$edge_weight_p)
        
        # 获取变量名
        var_names <- rownames(diff_matrix)
        if(is.null(var_names)) var_names <- colnames(diff_matrix)
        if(is.null(var_names)) var_names <- paste0("V", 1:nrow(diff_matrix))
        
        # 创建边的标签和对应的差异值、p值
        edges <- c()
        differences <- c()
        p_values <- c()
        
        # 遍历上三角矩阵（避免重复）
        for(i in 1:(nrow(diff_matrix)-1)) {
          for(j in (i+1):ncol(diff_matrix)) {
            edge_name <- paste0(var_names[i], " -- ", var_names[j])
            edges <- c(edges, edge_name)
            differences <- c(differences, diff_matrix[i, j])
            p_values <- c(p_values, p_matrix[i, j])
          }
        }
        
        # 创建结果数据框
        sig_results <- data.frame(
          边连接 = edges,
          差异值 = round(differences, 4),
          P值 = round(p_values, 4),
          显著性 = ifelse(p_values < 0.05, "显著", "不显著"),
          效应大小 = ifelse(abs(differences) > 0.1, "大", 
                      ifelse(abs(differences) > 0.05, "中", "小")),
          stringsAsFactors = FALSE
        )
        
        # 按p值排序，显著的在前
        sig_results <- sig_results[order(sig_results$P值), ]
        
      } else {
        # 如果没有diff_sig和edge_weight_p，显示调试信息
        debug_info <- data.frame(
          字段名 = names(result) %||% "无字段",
          类型 = if(!is.null(result)) sapply(result, class) else "NULL",
          说明 = "NetCompare结果结构信息",
          stringsAsFactors = FALSE
        )
        sig_results <- debug_info
      }
      
      if(!is.null(sig_results) && nrow(sig_results) > 0) {
        DT::datatable(sig_results, 
                     options = list(pageLength = 15, scrollX = TRUE),
                     rownames = FALSE) %>%
          DT::formatStyle("显著性", 
                         backgroundColor = DT::styleEqual("显著", "#d4edda"))
      } else {
        # 显示调试信息
        debug_info <- data.frame(
          字段名 = names(result) %||% "无字段",
          类型 = if(!is.null(result)) class(result) else "NULL",
          说明 = "NetCompare结果结构信息",
          stringsAsFactors = FALSE
        )
        DT::datatable(debug_info, options = list(dom = 't'), rownames = FALSE)
      }
      
    }, error = function(e) {
      error_info <- data.frame(
        错误信息 = paste("表格生成失败:", e$message),
        建议 = "请检查NetCompare函数的输出格式",
        stringsAsFactors = FALSE
      )
      DT::datatable(error_info, options = list(dom = 't'), rownames = FALSE)
    })
  })
  
  # 组间统计表格
  output$group_stats_table <- DT::renderDataTable({
    req(values$group_compare_result)
    
    tryCatch({
      result <- values$group_compare_result
      
      # 创建组间统计表格
      basic_stats <- data.frame(
        统计项目 = c("分组变量", "分组方法", "分组阈值", "组1名称", "组1样本量", "组2名称", "组2样本量", 
                  "置换检验次数", "多重比较校正"),
        统计值 = c(
          result$group_variable,
          switch(result$method,
                "median" = "中位数分组",
                "mean" = "均值分组", 
                "tertile" = "三分位数分组",
                "extreme_27" = "前后27%分组",
                "categorical" = "分类变量分组",
                "custom" = "自定义阈值"),
          as.character(result$threshold),
          result$group1_name,
          as.character(nrow(result$group1_data)),
          result$group2_name,
          as.character(nrow(result$group2_data)),
          as.character(result$permutation_n),
          result$p_adjust_method
        ),
        stringsAsFactors = FALSE
      )
      
      # 添加diff_sig和edge_weight_p矩阵的统计信息
      if(!is.null(result$compare_result$diff_sig) && !is.null(result$compare_result$edge_weight_p)) {
        
        diff_matrix <- as.matrix(result$compare_result$diff_sig)
        p_matrix <- as.matrix(result$compare_result$edge_weight_p)
        
        # 计算上三角矩阵统计（避免重复计算）
        upper_tri_indices <- upper.tri(diff_matrix)
        diff_values <- diff_matrix[upper_tri_indices]
        p_values <- p_matrix[upper_tri_indices]
        
        # 统计显著边
        sig_count <- sum(p_values < 0.05, na.rm = TRUE)
        total_count <- length(p_values)
        
        # 统计效应大小
        large_effect <- sum(abs(diff_values) > 0.1, na.rm = TRUE)
        medium_effect <- sum(abs(diff_values) > 0.05 & abs(diff_values) <= 0.1, na.rm = TRUE)
        
        # 添加矩阵统计到表格
        result_stats <- data.frame(
          统计项目 = c("矩阵维度", "检验的边数", "显著差异边数", "显著差异比例", 
                      "大效应边数(|diff|>0.1)", "中效应边数(0.05<|diff|≤0.1)", 
                      "平均差异值", "最大绝对差异"),
          统计值 = c(
            paste0(nrow(diff_matrix), "×", ncol(diff_matrix)),
            as.character(total_count),
            as.character(sig_count),
            paste0(round(sig_count/total_count*100, 1), "%"),
            as.character(large_effect),
            as.character(medium_effect),
            round(mean(abs(diff_values), na.rm = TRUE), 4),
            round(max(abs(diff_values), na.rm = TRUE), 4)
          ),
          stringsAsFactors = FALSE
        )
        
        # 合并基本统计和结果统计
        final_stats <- rbind(basic_stats, result_stats)
      } else {
        final_stats <- basic_stats
      }
      
      # 返回DT表格
      DT::datatable(final_stats, 
                   options = list(pageLength = 20, scrollX = TRUE, dom = 't'),
                   rownames = FALSE) %>%
        DT::formatStyle(columns = c(1, 2), fontSize = '14px')
      
    }, error = function(e) {
      data.frame(错误 = paste("组间统计表格生成失败:", e$message))
    })
  })
  
  # P值矩阵表格
  output$p_value_matrix_table <- DT::renderDataTable({
    req(values$group_compare_result)
    
    tryCatch({
      result <- values$group_compare_result$compare_result
      
      if(!is.null(result$edge_weight_p)) {
        
        # 获取P值矩阵
        p_matrix <- as.matrix(result$edge_weight_p)
        
        # 获取变量名
        var_names <- rownames(p_matrix)
        if(is.null(var_names)) var_names <- colnames(p_matrix)
        if(is.null(var_names)) var_names <- paste0("变量", 1:nrow(p_matrix))
        
        # 创建带变量名的P值矩阵表格
        p_matrix_df <- as.data.frame(p_matrix)
        
        # 设置行名和列名
        rownames(p_matrix_df) <- var_names
        colnames(p_matrix_df) <- var_names
        
        # 添加行名作为第一列
        p_matrix_df <- data.frame(变量 = var_names, p_matrix_df, stringsAsFactors = FALSE)
        
        # 对数值列进行四舍五入
        numeric_cols <- sapply(p_matrix_df, is.numeric)
        p_matrix_df[numeric_cols] <- lapply(p_matrix_df[numeric_cols], function(x) round(x, 4))
        
        DT::datatable(p_matrix_df, 
                     options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"),
                     rownames = FALSE) %>%
          DT::formatStyle(columns = 2:ncol(p_matrix_df), 
                         backgroundColor = DT::styleInterval(c(0.01, 0.05), 
                                                           c("#d4edda", "#fff3cd", "#f8d7da")))
        
      } else {
        # 如果没有edge_weight_p矩阵，显示提示
        info_df <- data.frame(
          说明 = "暂无P值矩阵数据",
          建议 = "请确保NetCompare函数返回了edge_weight_p字段",
          stringsAsFactors = FALSE
        )
        DT::datatable(info_df, options = list(dom = 't'), rownames = FALSE)
      }
      
    }, error = function(e) {
      error_df <- data.frame(
        错误信息 = paste("P值矩阵显示失败:", e$message),
        stringsAsFactors = FALSE
      )
      DT::datatable(error_df, options = list(dom = 't'), rownames = FALSE)
    })
  })
  
  # 桥接网络比较输出
  # 桥接比较图
  output$bridge_compare_plot <- renderPlot({
    req(values$bridge_compare_result)
    
    tryCatch({
      result <- values$bridge_compare_result
      
      # 创建桥接节点比较的可视化
      group1_bridges <- result$group1_bridges
      group2_bridges <- result$group2_bridges
      all_vars <- names(result$group1_bridge_groups)
      
      # 创建比较矩阵
      comparison_data <- data.frame(
        Variable = all_vars,
        Group1 = ifelse(all_vars %in% group1_bridges, 1, 0),
        Group2 = ifelse(all_vars %in% group2_bridges, 1, 0),
        stringsAsFactors = FALSE
      )
      
      # 计算变化类型
      comparison_data$Change <- ifelse(
        comparison_data$Group1 == 1 & comparison_data$Group2 == 1, "两组都是桥接节点",
        ifelse(comparison_data$Group1 == 1 & comparison_data$Group2 == 0, paste0(result$group1_name, "独有"),
               ifelse(comparison_data$Group1 == 0 & comparison_data$Group2 == 1, paste0(result$group2_name, "独有"),
                      "两组都不是桥接节点"))
      )
      
      # 使用base R绘图
      if(requireNamespace("ggplot2", quietly = TRUE)) {
        # 如果有ggplot2，使用更好的可视化
        library(ggplot2, quietly = TRUE)
        
        # 准备数据用于ggplot
        plot_data <- comparison_data[comparison_data$Group1 == 1 | comparison_data$Group2 == 1, ]
        if(nrow(plot_data) > 0) {
          plot_data$Group1_status <- ifelse(plot_data$Group1 == 1, result$group1_name, "")
          plot_data$Group2_status <- ifelse(plot_data$Group2 == 1, result$group2_name, "")
          
          p <- ggplot(plot_data, aes(x = Variable)) +
            geom_point(aes(y = 1, color = result$group1_name), 
                      data = plot_data[plot_data$Group1 == 1, ], size = 4) +
            geom_point(aes(y = 0, color = result$group2_name), 
                      data = plot_data[plot_data$Group2 == 1, ], size = 4) +
            scale_color_manual(values = c("#2376b7", "#d2568c")) +
            scale_y_continuous(breaks = c(0, 1), labels = c(result$group2_name, result$group1_name)) +
            labs(title = "桥接节点组间比较", 
                 x = "变量", y = "组别", color = "桥接节点") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
          
          print(p)
        } else {
          plot.new()
          text(0.5, 0.5, "两组均无桥接节点", cex = 1.5, col = "gray")
        }
      } else {
        # 使用base R绘图
        bridge_vars <- comparison_data[comparison_data$Group1 == 1 | comparison_data$Group2 == 1, ]
        if(nrow(bridge_vars) > 0) {
          plot(1, type = "n", xlim = c(0, nrow(bridge_vars)+1), ylim = c(-0.5, 1.5),
               xlab = "桥接节点", ylab = "组别", main = "桥接节点组间比较")
          
          for(i in 1:nrow(bridge_vars)) {
            var_name <- bridge_vars$Variable[i]
            if(bridge_vars$Group1[i] == 1) {
              points(i, 1, pch = 16, col = "#2376b7", cex = 2)
            }
            if(bridge_vars$Group2[i] == 1) {
              points(i, 0, pch = 16, col = "#d2568c", cex = 2)
            }
            text(i, -0.3, var_name, srt = 45, adj = 1, cex = 0.8)
          }
          
          legend("topright", legend = c(result$group1_name, result$group2_name),
                 col = c("#2376b7", "#d2568c"), pch = 16)
        } else {
          plot.new()
          text(0.5, 0.5, "两组均无桥接节点", cex = 1.5, col = "gray")
        }
      }
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("桥接比较图生成失败:", e$message), cex = 1, col = "red")
    })
  })
  
  # 桥接比较统计信息
  output$bridge_compare_stats <- renderText({
    req(values$bridge_compare_result)
    
    tryCatch({
      result <- values$bridge_compare_result
      
      group1_bridges <- result$group1_bridges
      group2_bridges <- result$group2_bridges
      
      # 计算重叠和独有节点
      common_bridges <- intersect(group1_bridges, group2_bridges)
      group1_unique <- setdiff(group1_bridges, group2_bridges)
      group2_unique <- setdiff(group2_bridges, group1_bridges)
      
      # 格式化输出
      output_text <- paste0(
        "🌉 桥接节点比较统计\n",
        "=" * 40, "\n\n",
        "📊 基本统计：\n",
        "• ", result$group1_name, "：", length(group1_bridges), " 个桥接节点\n",
        "• ", result$group2_name, "：", length(group2_bridges), " 个桥接节点\n",
        "• 共同桥接节点：", length(common_bridges), " 个\n\n",
        
        "🔍 详细分析：\n"
      )
      
      if(length(common_bridges) > 0) {
        output_text <- paste0(output_text,
          "• 两组共同的桥接节点：\n  ", paste(common_bridges, collapse = ", "), "\n\n"
        )
      }
      
      if(length(group1_unique) > 0) {
        output_text <- paste0(output_text,
          "• ", result$group1_name, " 独有的桥接节点：\n  ", paste(group1_unique, collapse = ", "), "\n\n"
        )
      }
      
      if(length(group2_unique) > 0) {
        output_text <- paste0(output_text,
          "• ", result$group2_name, " 独有的桥接节点：\n  ", paste(group2_unique, collapse = ", "), "\n\n"
        )
      }
      
      # 添加重叠比例
      total_unique_bridges <- length(union(group1_bridges, group2_bridges))
      if(total_unique_bridges > 0) {
        overlap_pct <- round(length(common_bridges) / total_unique_bridges * 100, 1)
        output_text <- paste0(output_text,
          "📈 重叠度分析：\n",
          "• 桥接节点重叠比例：", overlap_pct, "%\n",
          "• 总计不重复桥接节点：", total_unique_bridges, " 个\n"
        )
      }
      
      return(output_text)
      
    }, error = function(e) {
      return(paste("桥接比较统计失败:", e$message))
    })
  })
  
  # 桥接差异表
  output$bridge_diff_table <- DT::renderDataTable({
    req(values$bridge_compare_result)
    
    tryCatch({
      result <- values$bridge_compare_result
      bridge_comparison <- result$bridge_comparison
      
      # 只显示有变化的变量或桥接节点
      interesting_vars <- bridge_comparison[
        bridge_comparison$StatusChange != "无变化" | 
        bridge_comparison$Group1_BridgeStatus == "Bridge" | 
        bridge_comparison$Group2_BridgeStatus == "Bridge", 
      ]
      
      if(nrow(interesting_vars) > 0) {
        # 重命名列
        names(interesting_vars) <- c("变量", paste0(result$group1_name, "_状态"), 
                                    paste0(result$group2_name, "_状态"), "状态变化")
        
        DT::datatable(interesting_vars, 
                     options = list(pageLength = 15, scrollX = TRUE),
                     rownames = FALSE) %>%
          DT::formatStyle(columns = "状态变化", 
                         backgroundColor = DT::styleEqual("无变化", "#f8f9fa"),
                         color = DT::styleEqual("无变化", "#6c757d"))
      } else {
        # 如果没有有趣的变化，显示所有变量
        names(bridge_comparison) <- c("变量", paste0(result$group1_name, "_状态"), 
                                    paste0(result$group2_name, "_状态"), "状态变化")
        
        DT::datatable(bridge_comparison, 
                     options = list(pageLength = 15, scrollX = TRUE),
                     rownames = FALSE)
      }
      
    }, error = function(e) {
      error_df <- data.frame(
        错误信息 = paste("桥接差异表生成失败:", e$message),
        stringsAsFactors = FALSE
      )
      DT::datatable(error_df, options = list(dom = 't'), rownames = FALSE)
    })
  })
  
  # 下载处理
  # 桥接网络图下载
  output$download_bridge_plot <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("bridge_network_plot_", timestamp, ".pdf")
    },
    content = function(file) {
      tryCatch({
        # 优先使用get_network_plot生成的桥接网络PDF文件
        if(!is.null(values$bridge_network_pdf_path) && file.exists(values$bridge_network_pdf_path)) {
          file.copy(values$bridge_network_pdf_path, file, overwrite = TRUE)
          cat("复制桥接网络PDF文件:", values$bridge_network_pdf_path, "->", file, "\n")
        } else {
          # 备用方案：重新生成PDF
          pdf(file, width = 6, height = 4.5)
          if(!is.null(values$bridge_network)) {
            plot(values$bridge_network)
          } else {
            plot.new()
            text(0.5, 0.5, "桥接网络结果不可用", cex = 1.5, col = "red")
          }
          dev.off()
        }
      }, error = function(e) {
        cat("保存桥接网络图失败:", e$message, "\n")
      })
    }
  )
  
  # 桥接中心性图下载
  output$download_bridge_centrality <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("bridge_centrality_plot_", timestamp, ".pdf")
    },
    content = function(file) {
      tryCatch({
        # 优先使用get_bridge_plot生成的桥接中心性PDF文件
        if(!is.null(values$bridge_pdf_path) && file.exists(values$bridge_pdf_path)) {
          file.copy(values$bridge_pdf_path, file, overwrite = TRUE)
          cat("复制桥接中心性PDF文件:", values$bridge_pdf_path, "->", file, "\n")
        } else {
          # 备用方案：重新生成PDF
          pdf(file, width = 6, height = 4.5)
          if(!is.null(values$bridge_result)) {
            get_bridge_plot(values$bridge_result)
          } else {
            plot.new()
            text(0.5, 0.5, "桥接中心性结果不可用", cex = 1.5, col = "red")
          }
          dev.off()
        }
      }, error = function(e) {
        cat("保存桥接中心性图失败:", e$message, "\n")
      })
    }
  )
  
  # 桥接分析数据下载
  output$download_bridge_data <- downloadHandler(
    filename = function() {
      paste0("bridge_analysis_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(!is.null(values$bridge_groups) && !is.null(values$bridge_result)) {
        # 创建桥接分析结果数据框
        bridge_df <- data.frame(
          Variable = names(values$bridge_groups),
          Group = values$bridge_groups,
          IsBridge = values$bridge_groups == "Bridge",
          stringsAsFactors = FALSE
        )
        
        # 如果有bridge_result的额外信息，可以添加
        if(is.list(values$bridge_result) && !is.null(values$bridge_result$bridge)) {
          # 尝试添加桥接强度等信息
          tryCatch({
            if(!is.null(values$bridge_result$bridge) && length(values$bridge_result$bridge) == nrow(bridge_df)) {
              bridge_df$BridgeStrength <- values$bridge_result$bridge
            }
          }, error = function(e) {
            # 如果添加失败，继续使用基础数据框
          })
        }
        
        write.csv(bridge_df, file, row.names = FALSE)
      } else {
        # 如果没有桥接结果，创建空文件
        empty_df <- data.frame(
          Message = "桥接网络分析未运行或无结果",
          stringsAsFactors = FALSE
        )
        write.csv(empty_df, file, row.names = FALSE)
      }
    }
  )
  
  # 网络比较图下载
  output$download_compare_plot <- downloadHandler(
    filename = function() {
      "Fig3.pdf"
    },
    content = function(file) {
      tryCatch({
        # 优先使用get_compare_plot生成的PDF文件
        if(!is.null(values$compare_pdf_path) && file.exists(values$compare_pdf_path)) {
          file.copy(values$compare_pdf_path, file, overwrite = TRUE)
          cat("复制网络比较PDF文件:", values$compare_pdf_path, "->", file, "\n")
        } else {
          # 备用方案：重新生成PDF
          pdf(file, width = 6, height = 4.5)
          if(!is.null(values$group_compare_result)) {
            quickNet::get_compare_plot(values$group_compare_result$compare_result, values$network_result)
          } else {
            plot.new()
            text(0.5, 0.5, "网络比较结果不可用", cex = 1.5, col = "red")
          }
          dev.off()
        }
      }, error = function(e) {
        cat("保存网络比较图失败:", e$message, "\n")
      })
    }
  )
  
  # 网络比较差异数据下载
  output$download_compare_diff <- downloadHandler(
    filename = function() {
      "Fig3_diff.csv"
    },
    content = function(file) {
      tryCatch({
        # 优先使用已生成的CSV文件
        if(!is.null(values$compare_diff_csv_path) && file.exists(values$compare_diff_csv_path)) {
          file.copy(values$compare_diff_csv_path, file, overwrite = TRUE)
          cat("复制差异数据CSV文件:", values$compare_diff_csv_path, "->", file, "\n")
        } else if(!is.null(values$group_compare_result$compare_result$diff_sig)) {
          # 备用方案：重新生成CSV
          write.csv(data.frame(values$group_compare_result$compare_result$diff_sig), 
                   file, row.names = TRUE)
          cat("重新生成差异数据CSV文件:", file, "\n")
        } else {
          # 如果没有数据，创建说明文件
          write("没有找到网络比较差异数据。请确保已运行网络比较分析。", file)
        }
      }, error = function(e) {
        cat("保存网络比较差异数据失败:", e$message, "\n")
        write(paste("保存差异数据时出错:", e$message), file)
      })
    }
  )
  
  # 网络比较P值数据下载
  output$download_compare_pval <- downloadHandler(
    filename = function() {
      "Fig3_pval.csv"
    },
    content = function(file) {
      tryCatch({
        # 优先使用已生成的CSV文件
        if(!is.null(values$compare_pval_csv_path) && file.exists(values$compare_pval_csv_path)) {
          file.copy(values$compare_pval_csv_path, file, overwrite = TRUE)
          cat("复制P值数据CSV文件:", values$compare_pval_csv_path, "->", file, "\n")
        } else if(!is.null(values$group_compare_result$compare_result$edge_weight_p)) {
          # 备用方案：重新生成CSV
          write.csv(data.frame(values$group_compare_result$compare_result$edge_weight_p), 
                   file, row.names = TRUE)
          cat("重新生成P值数据CSV文件:", file, "\n")
        } else {
          # 如果没有数据，创建说明文件
          write("没有找到网络比较P值数据。请确保已运行网络比较分析。", file)
        }
      }, error = function(e) {
        cat("保存网络比较P值数据失败:", e$message, "\n")
        write(paste("保存P值数据时出错:", e$message), file)
      })
    }
  )
  
  output$download_network_plot <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("network_plot_", timestamp, ".pdf")
    },
    content = function(file) {
      tryCatch({
        # 优先使用get_network_plot生成的PDF文件
        if(!is.null(values$network_pdf_path) && file.exists(values$network_pdf_path)) {
          file.copy(values$network_pdf_path, file, overwrite = TRUE)
          cat("复制网络PDF文件:", values$network_pdf_path, "->", file, "\n")
        } else {
          # 备用方案：重新生成PDF
          pdf(file, width = 6, height = 4.5)
          if(!is.null(values$network_result)) {
            plot(values$network_result)
          } else {
            plot.new()
            text(0.5, 0.5, "网络结果不可用", cex = 1.5, col = "red")
          }
          dev.off()
        }
      }, error = function(e) {
        cat("保存网络图失败:", e$message, "\n")
      })
    }
  )
  
  output$download_centrality_plot <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("centrality_plot_", timestamp, ".pdf")
    },
    content = function(file) {
      tryCatch({
        # 优先使用get_centrality_plot生成的PDF文件
        if(!is.null(values$centrality_pdf_path) && file.exists(values$centrality_pdf_path)) {
          file.copy(values$centrality_pdf_path, file, overwrite = TRUE)
          cat("复制中心性PDF文件:", values$centrality_pdf_path, "->", file, "\n")
        } else {
          # 备用方案：重新生成PDF
          pdf(file, width = 6, height = 4.5)
          if(!is.null(values$centrality_result)) {
            plot(get_centrality_plot(values$centrality_result))
          } else {
            plot.new()
            text(0.5, 0.5, "中心性结果不可用", cex = 1.5, col = "red")
          }
          dev.off()
        }
      }, error = function(e) {
        cat("保存中心性图失败:", e$message, "\n")
      })
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("analysis_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$processed_data, file, row.names = FALSE)
    }
  )
  
  # 下载分析代码
  output$download_code <- downloadHandler(
    filename = function() {
      paste0("network_analysis_code_", Sys.Date(), ".R")
    },
    content = function(file) {
      # 生成完整的分析代码
      analysis_code <- generate_analysis_code(values)
      writeLines(analysis_code, file, useBytes = TRUE)
    }
  )
  
  # 下载所有结果文件（ZIP压缩包）
  output$download_all_results <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("network_analysis_results_", timestamp, ".zip")
    },
    content = function(file) {
      tryCatch({
        if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
          # 获取输出文件夹中的所有文件
          files_to_zip <- list.files(values$output_folder, full.names = TRUE, recursive = TRUE)
          
          if(length(files_to_zip) > 0) {
            # 创建临时目录用于打包
            temp_dir <- tempdir()
            zip_dir <- file.path(temp_dir, "network_analysis_results")
            dir.create(zip_dir, showWarnings = FALSE, recursive = TRUE)
            
            # 复制所有文件到临时目录，保持原有结构
            for(src_file in files_to_zip) {
              rel_path <- gsub(paste0("^", values$output_folder, "/"), "", src_file)
              dest_file <- file.path(zip_dir, rel_path)
              dest_dir <- dirname(dest_file)
              dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
              file.copy(src_file, dest_file, overwrite = TRUE)
            }
            
            # 创建ZIP文件
            old_wd <- getwd()
            setwd(temp_dir)
            zip(file, "network_analysis_results", flags = "-r")
            setwd(old_wd)
            
            cat("已创建结果压缩包:", file, "\n")
            cat("包含文件数:", length(files_to_zip), "\n")
          } else {
            # 如果没有文件，创建一个说明文件
            write("没有找到分析结果文件。请确保已运行网络分析。", file)
            cat("警告：没有找到结果文件\n")
          }
        } else {
          # 如果输出文件夹不存在，创建一个说明文件
          write("输出文件夹不存在。请先运行网络分析生成结果。", file)
          cat("警告：输出文件夹不存在\n")
        }
      }, error = function(e) {
        cat("创建结果压缩包失败:", e$message, "\n")
        # 创建错误说明文件
        write(paste("创建压缩包时出错:", e$message), file)
      })
    }
  )
  
  # 下载示例数据
  output$download_example <- downloadHandler(
    filename = "example_data.csv",
    content = function(file) {
      # 创建示例数据
      set.seed(123)
      n <- 200
      
      example_data <- data.frame(
        # AUDIT量表（10题）
        AUDIT10_1 = sample(0:4, n, replace = TRUE),
        AUDIT10_2 = sample(0:4, n, replace = TRUE),
        AUDIT10_3 = sample(0:4, n, replace = TRUE),
        AUDIT10_4 = sample(0:4, n, replace = TRUE),
        AUDIT10_5 = sample(0:4, n, replace = TRUE),
        AUDIT10_6 = sample(0:4, n, replace = TRUE),
        AUDIT10_7 = sample(0:4, n, replace = TRUE),
        AUDIT10_8 = sample(0:4, n, replace = TRUE),
        AUDIT10_9 = sample(0:4, n, replace = TRUE),
        AUDIT10_10 = sample(0:4, n, replace = TRUE),
        
        # HRF量表（18题）
        HRF18_1 = sample(1:7, n, replace = TRUE),
        HRF18_2 = sample(1:7, n, replace = TRUE),
        HRF18_3 = sample(1:7, n, replace = TRUE),
        HRF18_4 = sample(1:7, n, replace = TRUE),
        HRF18_5 = sample(1:7, n, replace = TRUE),
        HRF18_6 = sample(1:7, n, replace = TRUE),
        HRF18_7 = sample(1:7, n, replace = TRUE),
        HRF18_8 = sample(1:7, n, replace = TRUE),
        HRF18_9 = sample(1:7, n, replace = TRUE),
        HRF18_10 = sample(1:7, n, replace = TRUE),
        HRF18_11 = sample(1:7, n, replace = TRUE),
        HRF18_12 = sample(1:7, n, replace = TRUE),
        HRF18_13 = sample(1:7, n, replace = TRUE),
        HRF18_14 = sample(1:7, n, replace = TRUE),
        HRF18_15 = sample(1:7, n, replace = TRUE),
        HRF18_16 = sample(1:7, n, replace = TRUE),
        HRF18_17 = sample(1:7, n, replace = TRUE),
        HRF18_18 = sample(1:7, n, replace = TRUE)
      )
      
      write.csv(example_data, file, row.names = FALSE)
    }
  )
  
  # 稳定性分析下载处理器
  output$download_stability <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("stability_report_", timestamp, ".zip")
    },
    content = function(file) {
      tryCatch({
        # 创建临时目录
        temp_dir <- tempdir()
        zip_dir <- file.path(temp_dir, "stability_analysis")
        dir.create(zip_dir, showWarnings = FALSE, recursive = TRUE)
        
        files_to_include <- c()
        
        # 边稳定性PDF
        if(!is.null(values$edge_stability_pdf) && file.exists(values$edge_stability_pdf)) {
          dest_file <- file.path(zip_dir, basename(values$edge_stability_pdf))
          file.copy(values$edge_stability_pdf, dest_file, overwrite = TRUE)
          files_to_include <- c(files_to_include, dest_file)
        }
        
        # 中心性稳定性PDF
        if(!is.null(values$centrality_stability_pdf) && file.exists(values$centrality_stability_pdf)) {
          dest_file <- file.path(zip_dir, basename(values$centrality_stability_pdf))
          file.copy(values$centrality_stability_pdf, dest_file, overwrite = TRUE)
          files_to_include <- c(files_to_include, dest_file)
        }
        
        # CS系数PDF
        if(!is.null(values$cs_coefficient_pdf) && file.exists(values$cs_coefficient_pdf)) {
          dest_file <- file.path(zip_dir, basename(values$cs_coefficient_pdf))
          file.copy(values$cs_coefficient_pdf, dest_file, overwrite = TRUE)
          files_to_include <- c(files_to_include, dest_file)
        }
        
        if(length(files_to_include) > 0) {
          # 创建ZIP文件
          old_wd <- getwd()
          setwd(temp_dir)
          zip(file, "stability_analysis", flags = "-r")
          setwd(old_wd)
          cat("已创建稳定性分析压缩包:", file, "\n")
        } else {
          write("没有找到稳定性分析结果文件。请先运行稳定性分析。", file)
        }
        
      }, error = function(e) {
        cat("创建稳定性分析压缩包失败:", e$message, "\n")
        write(paste("创建压缩包时出错:", e$message), file)
      })
    }
  )
  
  # 贝叶斯网络稳定性下载处理器
  output$download_bn_stability <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("bayesian_stability_", timestamp, ".pdf")
    },
    content = function(file) {
      tryCatch({
        # 如果贝叶斯网络有稳定性分析结果，使用那个PDF
        if(!is.null(values$bayesian_stability_pdf) && file.exists(values$bayesian_stability_pdf)) {
          file.copy(values$bayesian_stability_pdf, file, overwrite = TRUE)
        } else {
          # 否则创建一个说明文件
          pdf(file, width = 8, height = 6)
          plot.new()
          text(0.5, 0.5, "贝叶斯网络稳定性分析\n尚未运行或未生成结果", cex = 1.5, col = "gray")
          dev.off()
        }
      }, error = function(e) {
        cat("保存贝叶斯稳定性图失败:", e$message, "\n")
      })
    }
  )
  
  # =============================================================================
  # 变量选择页面的服务器端逻辑
  # =============================================================================
  
  # 动态生成高级量表选择器 - 用于变量选择页面  
  output$advanced_scale_selectors <- renderUI({
    # 检查是否有已计算的量表结果
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary) && 
       length(values$calculated_scales$summary) > 0) {
      
      scales_info <- values$calculated_scales$summary
      selectors <- list()
      
      # 数据源信息
      selectors[[1]] <- div(
        class = "alert alert-success",
        tags$strong("✅ 数据源: 已计算量表"),
        tags$p(paste0("正在使用变量构造页面的计算结果 (", length(scales_info), " 个量表)"))
      )
      
      # 定义总分变量模式（避免重复代码）
      total_patterns <- c("_Total$", "_mean$", "_sum$", "_weighted$", "_max$", "_cfa$", "_pca$", "_factor$", "_std$")
      
      for(scale_name in names(scales_info)) {
        scale_info <- scales_info[[scale_name]]
        is_manual <- !is.null(scale_info$is_manual) && scale_info$is_manual
        
        # 分析可用的变量层级
        total_vars_names <- scale_info$new_variables[sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
        subscale_vars <- scale_info$new_variables[!sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
        
        # 为不同类型的量表生成不同的选择选项
        choices <- list()
        
        if(is_manual) {
          # 手动规则：只有汇总层选项
          if(length(total_vars_names) > 0) {
            choices[["🎯 汇总层 (计算结果)"]] <- "summary"
            default_choice <- "summary"
            info_text <- paste0("手动计算变量: ", paste(total_vars_names, collapse = ", "))
          } else {
            # 如果没有识别的总分变量，显示所有变量
            choices[["🎯 汇总层 (全部变量)"]] <- "summary"
            default_choice <- "summary"
            info_text <- paste0("手动计算变量: ", paste(scale_info$new_variables, collapse = ", "))
          }
        } else {
          # 预配置量表：检查available_scales获取详细信息
          available_scale_info <- NULL
          if(!is.null(values$calculated_scales$available_scales) && 
             scale_name %in% names(values$calculated_scales$available_scales)) {
            available_scale_info <- values$calculated_scales$available_scales[[scale_name]]
          }
          
          if(!is.null(available_scale_info)) {
            # 汇总层：使用总分变量
            if(length(total_vars_names) > 0) {
              choices[["🎯 汇总层 (总分)"]] <- "summary"
            }
            
            # 子量表层：使用维度变量
            if(length(subscale_vars) > 1) {
              choices[[paste0("📊 子量表层 (", length(subscale_vars), "个维度)")]] <- "subscale"
            }
            
            # 条目层：使用原始条目（如果存在）
            if(!is.null(available_scale_info$existing_items) && length(available_scale_info$existing_items) > 0) {
              choices[[paste0("📝 条目层 (", length(available_scale_info$existing_items), "个条目)")]] <- "items"
            }
            
            default_choice <- if(scale_name %in% c("AUDIT", "PHQ", "GAD")) "summary" else if(length(subscale_vars) > 1) "subscale" else "summary"
            info_text <- paste0("新增变量: ", paste(scale_info$new_variables, collapse = ", "))
          } else {
            # 没有详细信息，使用基本选项
            choices[["🎯 汇总层 (总分)"]] <- "summary"
            if(length(scale_info$new_variables) > 1) {
              choices[["📊 子量表层"]] <- "subscale"
            }
            default_choice <- "summary"
            info_text <- paste0("新增变量: ", paste(scale_info$new_variables, collapse = ", "))
          }
        }
        
        # 确保至少有一个选项
        if(length(choices) == 0) {
          choices[["🎯 汇总层"]] <- "summary"
          default_choice <- "summary"
        }
        
        # 确保默认选择在可用选项中
        if(!default_choice %in% unlist(choices)) {
          default_choice <- unlist(choices)[1]
        }
        
        selectors[[length(selectors) + 1]] <- div(
          class = "panel panel-primary",
          style = "margin-bottom: 15px;",
          div(
            class = "panel-heading",
            h5(paste0(if(is_manual) "📝 " else "📊 ", scale_name)),
            div(style = "font-size: 0.85em; color: #777; margin-top: 5px;", info_text)
          ),
          div(
            class = "panel-body",
            selectInput(
              inputId = paste0("advanced_level_", scale_name),
              label = "选择分析层级：",
              choices = choices,
              selected = default_choice,
              width = "100%"
            ),
            div(
              style = "margin-top: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 4px; font-size: 0.9em;",
              paste0("覆盖率: ", round(scale_info$coverage_rate * 100, 1), "%", 
                     " (", scale_info$existing_items, "/", scale_info$total_items, " 题目)")
            )
          )
        )
      }
      
      return(tagList(selectors))
      
    } else {
      return(div(
        class = "alert alert-warning",
        tags$h4("📋 暂无可用量表"),
        tags$p("请先在", tags$strong("变量构造"), "页面计算量表，然后返回此处进行变量选择"),
        tags$hr(),
        tags$small("建议流程：上传数据 → 变量构造 → 变量选择 → 网络分析")
      ))
    }
  })
  
  # 确认变量选择
  
  # 辅助函数：获取变量统计信息
  get_variable_stats <- function(var_name, data) {
    if(is.null(data) || is.null(var_name) || !var_name %in% names(data)) {
      return(" [不存在]")
    }
    
    var_data <- data[[var_name]]
    total_obs <- length(var_data)
    missing_obs <- sum(is.na(var_data))
    valid_obs <- total_obs - missing_obs
    missing_pct <- round(missing_obs / total_obs * 100, 1)
    
    if(missing_obs == 0) {
      return(paste0(" [", valid_obs, " 完整]"))
    } else if(missing_pct < 5) {
      return(paste0(" [", valid_obs, "/", total_obs, "]"))
    } else if(missing_pct < 20) {
      return(paste0(" [", valid_obs, "/", total_obs, " ⚠️", missing_pct, "%缺失]"))
    } else {
      return(paste0(" [", valid_obs, "/", total_obs, " ❌", missing_pct, "%缺失]"))
    }
  }
  
  # 生成最终变量预览
  output$final_variables_preview <- renderText({
    # 检查是否有已计算的量表结果
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary) && 
       length(values$calculated_scales$summary) > 0) {
      
      # 定义总分变量模式（保持一致性）
      total_patterns <- c("_Total$", "_mean$", "_sum$", "_weighted$", "_max$", "_cfa$", "_pca$", "_factor$", "_std$")
      
      scales_info <- values$calculated_scales$summary
      preview_lines <- character(0)
      total_vars <- 0
      
      for(scale_name in names(scales_info)) {
        scale_info <- scales_info[[scale_name]]
        level_input_id <- paste0("advanced_level_", scale_name)
        selected_level <- input[[level_input_id]]
        
        if(is.null(selected_level)) selected_level <- "summary"
        
        is_manual <- !is.null(scale_info$is_manual) && scale_info$is_manual
        scale_icon <- if(is_manual) "📝" else "📊"
        
        if(selected_level == "summary") {
          # 汇总层：显示新增的变量
          if(is_manual) {
            # 手动计算变量的统计信息
            for(var_name in scale_info$new_variables) {
              var_stats <- get_variable_stats(var_name, values$processed_data)
              preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, ": ", var_name, var_stats, " (手动计算)"))
            }
            total_vars <- total_vars + length(scale_info$new_variables)
          } else {
            # 对于预配置量表，显示总分变量（支持新命名规则）
            total_vars_names <- scale_info$new_variables[sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
            if(length(total_vars_names) > 0) {
              # 显示变量类型
              var_type <- if(grepl("_mean$", total_vars_names[1])) "均值" else 
                         if(grepl("_sum$", total_vars_names[1])) "求和" else
                         if(grepl("_weighted$", total_vars_names[1])) "加权" else
                         if(grepl("_cfa$", total_vars_names[1])) "CFA" else
                         if(grepl("_pca$", total_vars_names[1])) "PCA" else "总分"
              var_stats <- get_variable_stats(total_vars_names[1], values$processed_data)
              preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, ": ", total_vars_names[1], var_stats, " (", var_type, ")"))
              total_vars <- total_vars + 1
            } else {
              var_stats <- get_variable_stats(scale_info$new_variables[1], values$processed_data)
              preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, ": ", scale_info$new_variables[1], var_stats, " (汇总)"))
              total_vars <- total_vars + 1
            }
          }
          
        } else if(selected_level == "subscale") {
          # 子量表层：显示非总分变量（排除所有总分模式）
          subscale_vars <- scale_info$new_variables[!sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
          if(length(subscale_vars) > 0) {
            preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, " (", length(subscale_vars), " 个维度):"))
            for(var_name in subscale_vars) {
              var_stats <- get_variable_stats(var_name, values$processed_data)
              preview_lines <- c(preview_lines, paste0("   • ", var_name, var_stats))
            }
            total_vars <- total_vars + length(subscale_vars)
          }
          
        } else if(selected_level == "items") {
          # 条目层：显示原始条目
          if(!is.null(values$calculated_scales$available_scales) && 
             scale_name %in% names(values$calculated_scales$available_scales)) {
            available_scale_info <- values$calculated_scales$available_scales[[scale_name]]
            if(!is.null(available_scale_info$existing_items)) {
              items <- available_scale_info$existing_items
              
              # 计算完整案例数（所有条目都不缺失的案例）
              if(!is.null(values$processed_data)) {
                available_items <- intersect(items, names(values$processed_data))
                if(length(available_items) > 0) {
                  items_data <- values$processed_data[, available_items, drop = FALSE]
                  complete_items_cases <- sum(complete.cases(items_data))
                  total_cases <- nrow(items_data)
                  items_stats <- paste0(" [完整: ", complete_items_cases, "/", total_cases, "]")
                } else {
                  items_stats <- " [数据缺失]"
                }
              } else {
                items_stats <- ""
              }
              
              preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, ": ", length(items), " 个条目", items_stats))
              
              # 显示前几个条目及其统计信息
              display_items <- head(items, 3)
              for(item_name in display_items) {
                item_stats <- get_variable_stats(item_name, values$processed_data)
                preview_lines <- c(preview_lines, paste0("   • ", item_name, item_stats))
              }
              if(length(items) > 3) {
                preview_lines <- c(preview_lines, paste0("   ... 还有 ", length(items) - 3, " 个条目"))
              }
              
              total_vars <- total_vars + length(items)
            }
          }
        }
        
        preview_lines <- c(preview_lines, "")
      }
      
      # 计算当前变量组合的完整案例数
      temp_analysis_data <- NULL
      complete_cases_info <- ""
      
      if(!is.null(values$processed_data)) {
        # 重新计算final_variables用于完整案例统计
        temp_final_variables <- character(0)
        for(scale_name in names(scales_info)) {
          scale_info <- scales_info[[scale_name]]
          level_input_id <- paste0("advanced_level_", scale_name)
          selected_level <- input[[level_input_id]]
          if(is.null(selected_level)) selected_level <- "summary"
          is_manual <- !is.null(scale_info$is_manual) && scale_info$is_manual
          
          if(selected_level == "summary") {
            if(is_manual) {
              temp_final_variables <- c(temp_final_variables, scale_info$new_variables)
            } else {
              total_vars_names <- scale_info$new_variables[sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
              if(length(total_vars_names) > 0) {
                temp_final_variables <- c(temp_final_variables, total_vars_names[1])
              } else {
                temp_final_variables <- c(temp_final_variables, scale_info$new_variables[1])
              }
            }
          } else if(selected_level == "subscale") {
            subscale_vars <- scale_info$new_variables[!sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
            temp_final_variables <- c(temp_final_variables, subscale_vars)
          } else if(selected_level == "items") {
            if(!is.null(values$calculated_scales$available_scales) && 
               scale_name %in% names(values$calculated_scales$available_scales)) {
              available_scale_info <- values$calculated_scales$available_scales[[scale_name]]
              if(!is.null(available_scale_info$existing_items)) {
                temp_final_variables <- c(temp_final_variables, available_scale_info$existing_items)
              }
            }
          }
        }
        
        # 检查这些变量的完整案例数
        if(length(temp_final_variables) > 0) {
          available_vars <- intersect(temp_final_variables, names(values$processed_data))
          if(length(available_vars) > 0) {
            temp_analysis_data <- values$processed_data[, available_vars, drop = FALSE]
            complete_cases <- sum(complete.cases(temp_analysis_data))
            total_cases <- nrow(temp_analysis_data)
            missing_vars <- setdiff(temp_final_variables, available_vars)
            
            # 生成完整案例信息
            if(complete_cases >= 50) {
              status_icon <- "✅"
              status_color <- ""
            } else if(complete_cases >= 20) {
              status_icon <- "⚠️"
              status_color <- ""
            } else {
              status_icon <- "❌"
              status_color <- ""
            }
            
            complete_cases_info <- paste0(
              "📊 数据质量评估:\n",
              "   ", status_icon, " 完整观测: ", complete_cases, " / ", total_cases, " (", round(complete_cases/total_cases*100, 1), "%)\n"
            )
            
            if(length(missing_vars) > 0) {
              complete_cases_info <- paste0(complete_cases_info,
                "   🔍 缺失变量: ", length(missing_vars), " 个 (", paste(head(missing_vars, 3), collapse = ", "), 
                if(length(missing_vars) > 3) "..." else "", ")\n"
              )
            }
            
            if(complete_cases < 20) {
              complete_cases_info <- paste0(complete_cases_info,
                "   💡 建议: 完整案例过少，考虑减少变量或检查数据质量\n"
              )
            } else if(complete_cases < 50) {
              complete_cases_info <- paste0(complete_cases_info,
                "   💡 建议: 完整案例较少，网络分析结果可能不够稳定\n"
              )
            }
            
            complete_cases_info <- paste0(complete_cases_info, "\n")
          }
        }
      }
      
      header <- paste0("🎯 网络分析将包含 ", total_vars, " 个变量\n",
                      "==========================================\n\n",
                      complete_cases_info)
      
      # 添加分组配色预览
      color_preview <- ""
      if(!is.null(values$variable_groups) && length(values$variable_groups) > 0) {
        color_preview <- "\n🎨 变量分组配色:\n"
        available_colors <- VIZ_CONFIG$colors$primary
        color_names <- c("绿色", "蓝色", "粉色", "黄色", "紫色", "浅粉", "浅蓝", "浅绿", "橙色", "淡紫")
        
        # 获取最终变量列表以生成索引
        if(total_vars > 0) {
          # 重新计算final_variables用于索引显示
          temp_final_variables <- character(0)
          for(scale_name in names(scales_info)) {
            scale_info <- scales_info[[scale_name]]
            level_input_id <- paste0("advanced_level_", scale_name)
            selected_level <- input[[level_input_id]]
            if(is.null(selected_level)) selected_level <- "summary"
            is_manual <- !is.null(scale_info$is_manual) && scale_info$is_manual
            
            if(selected_level == "summary") {
              if(is_manual) {
                temp_final_variables <- c(temp_final_variables, scale_info$new_variables)
              } else {
                total_vars_names <- scale_info$new_variables[sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
                if(length(total_vars_names) > 0) {
                  temp_final_variables <- c(temp_final_variables, total_vars_names[1])
                } else {
                  temp_final_variables <- c(temp_final_variables, scale_info$new_variables[1])
                }
              }
            } else if(selected_level == "subscale") {
              subscale_vars <- scale_info$new_variables[!sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
              temp_final_variables <- c(temp_final_variables, subscale_vars)
            } else if(selected_level == "items") {
              if(!is.null(values$calculated_scales$available_scales) && 
                 scale_name %in% names(values$calculated_scales$available_scales)) {
                available_scale_info <- values$calculated_scales$available_scales[[scale_name]]
                if(!is.null(available_scale_info$existing_items)) {
                  temp_final_variables <- c(temp_final_variables, available_scale_info$existing_items)
                }
              }
            }
          }
          
          # 显示分组及其变量索引
          for(i in seq_along(values$variable_groups)) {
            group_name <- names(values$variable_groups)[i]
            scales_in_group <- values$variable_groups[[i]]
            color_index <- ((i-1) %% length(available_colors)) + 1
            color_name <- if(color_index <= length(color_names)) color_names[color_index] else paste0("颜色", color_index)
            
            # 找到这个分组对应的变量索引
            group_indices <- c()
            for(scale_name in scales_in_group) {
              matching_indices <- which(
                temp_final_variables == scale_name |
                startsWith(temp_final_variables, paste0(scale_name, "_")) |
                grepl(paste0("_", scale_name, "_"), temp_final_variables) |
                endsWith(temp_final_variables, paste0("_", scale_name))
              )
              group_indices <- c(group_indices, matching_indices)
            }
            group_indices <- unique(sort(group_indices))
            
            if(length(group_indices) > 0) {
              indices_text <- if(length(group_indices) == 1) {
                as.character(group_indices)
              } else if(all(diff(group_indices) == 1)) {
                paste0(min(group_indices), ":", max(group_indices))
              } else {
                paste0("c(", paste(group_indices, collapse = ","), ")")
              }
              
              color_preview <- paste0(color_preview, 
                                    "  ", group_name, " (", color_name, ") = ", indices_text, " # ", 
                                    paste(scales_in_group, collapse = ", "), "\n")
            } else {
              color_preview <- paste0(color_preview, 
                                    "  ", group_name, " (", color_name, "): ", 
                                    paste(scales_in_group, collapse = ", "), " [未匹配到变量]\n")
            }
          }
        }
        
        color_preview <- paste0(color_preview, "\n格式类似: groups=list(组1=1, 组2=2:4, 组3=c(5:12))\n")
      }
      
      return(paste0(header, paste(preview_lines, collapse = "\n"), color_preview))
      
    } else if(!is.null(values$scales) && length(values$scales) > 0) {
      return(paste0("✅ 已检测到 ", length(values$scales), " 个量表\n\n⚠️ 请先在【变量构造】页面进行计算，然后返回此处选择变量"))
    } else {
      return("⚠️ 暂无可用变量，请先上传数据并完成变量构造")
    }
  })
  
  # 确认变量选择
  observeEvent(input$confirm_variables, {
    # 检查是否有已计算的量表结果
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary) && 
       length(values$calculated_scales$summary) > 0) {
      
      scales_info <- values$calculated_scales$summary
      final_variables <- character(0)
      scale_groups <- list()  # 用于构建groups参数
      current_index <- 1      # 当前变量索引
      
      # 定义总分变量模式（保持一致性）
      total_patterns <- c("_Total$", "_mean$", "_sum$", "_weighted$", "_max$", "_cfa$", "_pca$", "_factor$", "_std$")
      
      for(scale_name in names(scales_info)) {
        scale_info <- scales_info[[scale_name]]
        level_input_id <- paste0("advanced_level_", scale_name)
        selected_level <- input[[level_input_id]]
        
        if(is.null(selected_level)) selected_level <- "summary"
        
        is_manual <- !is.null(scale_info$is_manual) && scale_info$is_manual
        
        # 记录这个量表的起始索引
        scale_start_index <- current_index
        scale_variables <- character(0)
        
        if(selected_level == "summary") {
          # 汇总层：选择合适的变量
          if(is_manual) {
            # 手动规则：使用生成的变量
            scale_variables <- scale_info$new_variables
          } else {
            # 预配置量表：优先选择总分变量
            total_vars_names <- scale_info$new_variables[sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
            if(length(total_vars_names) > 0) {
              scale_variables <- total_vars_names[1]
            } else {
              scale_variables <- scale_info$new_variables[1]
            }
          }
          
        } else if(selected_level == "subscale") {
          # 子量表层：选择非总分变量（排除所有总分模式）
          scale_variables <- scale_info$new_variables[!sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
          
        } else if(selected_level == "items") {
          # 条目层：选择原始条目变量
          if(!is.null(values$calculated_scales$available_scales) && 
             scale_name %in% names(values$calculated_scales$available_scales)) {
            available_scale_info <- values$calculated_scales$available_scales[[scale_name]]
            if(!is.null(available_scale_info$existing_items)) {
              scale_variables <- available_scale_info$existing_items
            }
          }
        }
        
        # 添加到final_variables并记录groups信息
        if(length(scale_variables) > 0) {
          final_variables <- c(final_variables, scale_variables)
          
          # 计算这个量表的索引范围
          scale_end_index <- current_index + length(scale_variables) - 1
          if(length(scale_variables) == 1) {
            scale_groups[[scale_name]] <- scale_start_index
          } else {
            scale_groups[[scale_name]] <- scale_start_index:scale_end_index
          }
          
          # 更新当前索引
          current_index <- scale_end_index + 1
        }
      }
      
      # 确保选择的变量在数据中存在
      final_variables <- intersect(final_variables, names(values$processed_data))
      
      if(length(final_variables) == 0) {
        showNotification("没有选择到有效的变量，请检查设置", type = "error")
        return()
      }
      
      # 保存最终选择的变量用于网络分析
      values$analysis_data <- values$processed_data[, final_variables, drop = FALSE]
      values$final_variables <- final_variables  # 保存变量列表给温度分析使用
      values$variables_confirmed <- TRUE
      
      # 保存构建好的groups参数
      values$scale_groups <- scale_groups
      
      # 调试输出groups信息
      cat("=== 构建的Groups参数 ===\n")
      cat("变量列表:", paste(final_variables, collapse = ", "), "\n")
      for(scale_name in names(scale_groups)) {
        indices <- scale_groups[[scale_name]]
        if(length(indices) == 1) {
          cat("- ", scale_name, ": ", indices, "\n")
        } else {
          cat("- ", scale_name, ": ", min(indices), ":", max(indices), "\n")
        }
      }
      cat("Groups格式: list(", paste(sapply(names(scale_groups), function(x) {
        indices <- scale_groups[[x]]
        if(length(indices) == 1) {
          paste0(x, "=", indices)
        } else {
          paste0(x, "=", min(indices), ":", max(indices))
        }
      }), collapse = ", "), ")\n")
      cat("======================\n")
      
      showNotification(paste0("已确认选择 ", length(final_variables), " 个变量用于网络分析"), type = "message")
      
    } else {
      showNotification("请先在变量构造页面完成量表计算", type = "error")
    }
  })
  
  # 变量是否已选择的状态
  output$variablesSelected <- reactive({
    # 检查是否有已计算的量表结果
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary) && 
       length(values$calculated_scales$summary) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  outputOptions(output, "variablesSelected", suspendWhenHidden = FALSE)
  
  # 变量是否已确认的状态  
  output$variablesConfirmed <- reactive({
    return(!is.null(values$variables_confirmed) && values$variables_confirmed)
  })
  outputOptions(output, "variablesConfirmed", suspendWhenHidden = FALSE)
  
  # 检查是否有多个组别（用于显示桥接分析选项）
  output$hasMultipleGroups <- reactive({
    return(!is.null(values$variable_groups) && length(values$variable_groups) >= 2)
  })
  outputOptions(output, "hasMultipleGroups", suspendWhenHidden = FALSE)
  
  # 检查是否有桥接分析结果（用于显示桥接网络标签页）
  output$hasBridgeAnalysis <- reactive({
    return(!is.null(values$bridge_result) && !is.null(values$bridge_network))
  })
  outputOptions(output, "hasBridgeAnalysis", suspendWhenHidden = FALSE)
  
  # 检查是否有桥接比较结果
  output$hasBridgeCompareResult <- reactive({
    return(!is.null(values$bridge_compare_result))
  })
  outputOptions(output, "hasBridgeCompareResult", suspendWhenHidden = FALSE)
  
  # 重新选择变量
  observeEvent(input$reselect_variables, {
    values$variables_confirmed <- FALSE
    values$analysis_data <- NULL
    showNotification("已重置变量选择，请重新配置", type = "message")
  })
  
  # 变量分组配色UI
  output$variable_groups_ui <- renderUI({
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary) && 
       length(values$calculated_scales$summary) > 0) {
      
      scales_info <- values$calculated_scales$summary
      scale_names <- names(scales_info)
      
      # 默认分组：每个量表为一组
      if(is.null(values$variable_groups)) {
        values$variable_groups <- list()
        for(i in seq_along(scale_names)) {
          values$variable_groups[[scale_names[i]]] <- scale_names[i]
        }
      }
      
      # 获取可用颜色
      available_colors <- VIZ_CONFIG$colors$primary
      if(is.null(available_colors)) {
        available_colors <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A", 
                             "#FB9A99", "#A6CEE3", "#B2DF8A", "#FDBF6F", "#CAB2D6")
      }
      
      group_ui <- list()
      
      # 当前分组显示（可编辑）
      group_ui[[1]] <- div(
        style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 12px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          h6("当前分组配置：", style = "margin: 0; font-weight: bold;"),
          div(
            # 编辑模式状态显示
            if(!is.null(values$edit_mode) && values$edit_mode) {
              list(
                span("🔧 编辑模式", style = "font-size: 11px; color: #28a745; font-weight: bold; margin-right: 8px;"),
                actionButton("edit_groups_mode", "完成编辑", 
                            class = "btn-sm btn-success", 
                            style = "height: 28px; font-size: 11px;")
              )
            } else {
              actionButton("edit_groups_mode", "编辑分组", 
                          class = "btn-sm btn-outline-primary", 
                          style = "height: 28px; font-size: 11px;")
            },
            actionButton("reset_groups", "重置", 
                        class = "btn-sm btn-outline-secondary", 
                        style = "height: 28px; font-size: 11px; margin-left: 5px;")
          )
        ),
        # 编辑模式提示
        if(!is.null(values$edit_mode) && values$edit_mode) {
          div(
            class = "alert alert-warning",
            style = "padding: 8px 12px; margin-bottom: 10px; font-size: 12px;",
            tags$strong("📝 编辑模式："), "点击分组右上角的 × 按钮可删除分组，被删除分组中的量表会重新分配为单独的组。"
          )
        },
        
        div(id = "current_groups_display",
            if(!is.null(values$edit_mode) && values$edit_mode) {
              # 编辑模式：显示可删除的分组
              lapply(seq_along(values$variable_groups), function(i) {
                group_name <- names(values$variable_groups)[i]
                scales_in_group <- values$variable_groups[[i]]
                color <- available_colors[((i-1) %% length(available_colors)) + 1]
                
                div(
                  style = paste0("background-color: ", color, "; color: white; padding: 6px 10px; margin: 3px 2px; border-radius: 4px; display: inline-block; font-size: 12px; position: relative; cursor: pointer; border: 2px dashed rgba(255,255,255,0.5);"),
                  span(paste0(group_name, ": ", paste(scales_in_group, collapse = ", "))),
                  # 删除按钮
                  actionButton(paste0("delete_group_", group_name), "×", 
                              class = "btn-sm", 
                              style = "position: absolute; top: -8px; right: -8px; width: 20px; height: 20px; padding: 0; font-size: 12px; background: #dc3545; color: white; border: 2px solid white; border-radius: 50%; cursor: pointer; box-shadow: 0 2px 4px rgba(0,0,0,0.2);",
                              onclick = paste0("Shiny.setInputValue('delete_group_trigger', '", group_name, "', {priority: 'event'});"))
                )
              })
            } else {
              # 普通模式：只显示分组
              lapply(seq_along(values$variable_groups), function(i) {
                group_name <- names(values$variable_groups)[i]
                scales_in_group <- values$variable_groups[[i]]
                color <- available_colors[((i-1) %% length(available_colors)) + 1]
                
                div(
                  style = paste0("background-color: ", color, "; color: white; padding: 6px 10px; margin: 3px 2px; border-radius: 4px; display: inline-block; font-size: 12px;"),
                  paste0(group_name, ": ", paste(scales_in_group, collapse = ", "))
                )
              })
            }
        )
      )
      
      # 智能分组工具
      group_ui[[2]] <- div(
        style = "margin-top: 12px; padding: 12px; border: 1px solid #dee2e6; border-radius: 5px;",
        h6("🎨 智能分组工具：", style = "margin-bottom: 10px; color: #495057;"),
        
        # 快速分组按钮
        div(
          style = "margin-bottom: 10px;",
          h6("快速分组：", style = "font-size: 13px; margin-bottom: 5px;"),
          div(
            style = "display: flex; gap: 5px; flex-wrap: wrap;",
            actionButton("quick_all_one", "全部合并为一组", 
                        class = "btn-sm btn-outline-info", 
                        style = "font-size: 11px;"),
            actionButton("quick_each_one", "每个量表一组", 
                        class = "btn-sm btn-outline-info", 
                        style = "font-size: 11px;"),
            actionButton("quick_by_type", "按类型分组", 
                        class = "btn-sm btn-outline-info", 
                        style = "font-size: 11px;")
          )
        ),
        
        # 自定义分组
        div(
          h6("自定义分组：", style = "font-size: 13px; margin-bottom: 8px;"),
          div(
            style = "display: flex; gap: 8px; align-items: end; margin-bottom: 8px;",
            div(
              style = "flex: 1;",
              tags$label("选择量表：", style = "font-size: 12px; margin-bottom: 2px; display: block;"),
              selectInput("group_scales", NULL, 
                         choices = scale_names,
                         selected = NULL,
                         multiple = TRUE,
                         width = "100%")
            ),
            div(
              style = "width: 100px;",
              tags$label("组名：", style = "font-size: 12px; margin-bottom: 2px; display: block;"),
              textInput("group_name", NULL, 
                       placeholder = "如：认知组", 
                       width = "100%")
            ),
            actionButton("add_custom_group", "创建分组", 
                        class = "btn-sm btn-success",
                        style = "height: 34px; white-space: nowrap;")
          ),
          div(
            style = "font-size: 11px; color: #6c757d;",
            "💡 提示：选择多个量表可以将它们合并为一组，便于在网络图中识别"
          )
        )
      )
      
      return(group_ui)
    }
  })
  
  # 添加自定义分组（新版本）
  observeEvent(input$add_custom_group, {
    req(input$group_scales, input$group_name)
    
    if(nchar(trimws(input$group_name)) == 0) {
      showNotification("请输入组名", type = "warning")
      return()
    }
    
    group_name <- trimws(input$group_name)
    selected_scales <- input$group_scales
    
    # 检查是否有重复
    if(group_name %in% names(values$variable_groups)) {
      showNotification("组名已存在，请使用不同的组名", type = "warning")
      return()
    }
    
    # 从现有分组中移除这些量表
    for(existing_group in names(values$variable_groups)) {
      values$variable_groups[[existing_group]] <- values$variable_groups[[existing_group]][
        !values$variable_groups[[existing_group]] %in% selected_scales
      ]
    }
    
    # 移除空的分组
    values$variable_groups <- values$variable_groups[sapply(values$variable_groups, length) > 0]
    
    # 添加新分组
    values$variable_groups[[group_name]] <- selected_scales
    
    # 清空输入框
    updateSelectInput(session, "group_scales", selected = NULL)
    updateTextInput(session, "group_name", value = "")
    
    showNotification(paste0("已创建分组: ", group_name), type = "message")
  })
  
  # 保留旧版本的兼容性
  observeEvent(input$add_group, {
    req(input$group_scales, input$group_name)
    
    if(nchar(trimws(input$group_name)) == 0) {
      showNotification("请输入组名", type = "warning")
      return()
    }
    
    group_name <- trimws(input$group_name)
    selected_scales <- input$group_scales
    
    # 检查是否有重复
    if(group_name %in% names(values$variable_groups)) {
      showNotification("组名已存在，请使用不同的组名", type = "warning")
      return()
    }
    
    # 从现有分组中移除这些量表
    for(existing_group in names(values$variable_groups)) {
      values$variable_groups[[existing_group]] <- values$variable_groups[[existing_group]][
        !values$variable_groups[[existing_group]] %in% selected_scales
      ]
    }
    
    # 移除空的分组
    values$variable_groups <- values$variable_groups[sapply(values$variable_groups, length) > 0]
    
    # 添加新分组
    values$variable_groups[[group_name]] <- selected_scales
    
    # 清空输入框
    updateSelectInput(session, "group_scales", selected = NULL)
    updateTextInput(session, "group_name", value = "")
    
    showNotification(paste0("已创建分组: ", group_name), type = "message")
  })
  
  # 快速分组功能
  # 全部合并为一组
  observeEvent(input$quick_all_one, {
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary)) {
      scale_names <- names(values$calculated_scales$summary)
      values$variable_groups <- list("组1" = scale_names)
      showNotification("已将所有量表合并为一组", type = "message")
    }
  })
  
  # 每个量表一组
  observeEvent(input$quick_each_one, {
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary)) {
      scale_names <- names(values$calculated_scales$summary)
      values$variable_groups <- list()
      for(i in seq_along(scale_names)) {
        values$variable_groups[[paste0("组", i)]] <- scale_names[i]
      }
      showNotification("已重置为默认分组（每个量表一组）", type = "message")
    }
  })
  
  # 按类型分组（简单的启发式分组）
  observeEvent(input$quick_by_type, {
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary)) {
      scale_names <- names(values$calculated_scales$summary)
      
      # 简单的类型分组逻辑
      values$variable_groups <- list()
      mood_scales <- scale_names[grepl("PHQ|GAD|BDI|DASS", scale_names, ignore.case = TRUE)]
      substance_scales <- scale_names[grepl("AUDIT|FTND", scale_names, ignore.case = TRUE)]
      motivation_scales <- scale_names[grepl("HRF|motivation", scale_names, ignore.case = TRUE)]
      other_scales <- setdiff(scale_names, c(mood_scales, substance_scales, motivation_scales))
      
      group_counter <- 1
      if(length(mood_scales) > 0) {
        values$variable_groups[[paste0("情绪组")]] <- mood_scales
        group_counter <- group_counter + 1
      }
      if(length(substance_scales) > 0) {
        values$variable_groups[[paste0("物质组")]] <- substance_scales
        group_counter <- group_counter + 1
      }
      if(length(motivation_scales) > 0) {
        values$variable_groups[[paste0("动机组")]] <- motivation_scales
        group_counter <- group_counter + 1
      }
      if(length(other_scales) > 0) {
        values$variable_groups[[paste0("其他组")]] <- other_scales
      }
      
      showNotification(paste0("已按类型自动分组，共", length(values$variable_groups), "组"), type = "message")
    }
  })
  
  # 编辑分组模式切换
  observeEvent(input$edit_groups_mode, {
    if(is.null(values$edit_mode)) values$edit_mode <- FALSE
    values$edit_mode <- !values$edit_mode
    
    if(values$edit_mode) {
      showNotification("已进入编辑模式，点击×可删除分组", type = "message")
    } else {
      showNotification("已退出编辑模式", type = "message")
    }
  })
  
  # 删除分组
  observeEvent(input$delete_group_trigger, {
    req(input$delete_group_trigger)
    
    group_to_delete <- input$delete_group_trigger
    
    if(group_to_delete %in% names(values$variable_groups)) {
      # 获取被删除分组中的量表
      deleted_scales <- values$variable_groups[[group_to_delete]]
      
      # 删除分组
      values$variable_groups[[group_to_delete]] <- NULL
      
      # 将被删除分组中的量表重新分配为单独的组
      if(length(deleted_scales) > 0) {
        # 找到当前最大的组号
        existing_group_numbers <- as.numeric(gsub("组", "", names(values$variable_groups)[grepl("^组[0-9]+$", names(values$variable_groups))]))
        if(length(existing_group_numbers) > 0) {
          next_group_num <- max(existing_group_numbers) + 1
        } else {
          next_group_num <- 1
        }
        
        # 为每个被删除的量表创建新分组
        for(scale in deleted_scales) {
          values$variable_groups[[paste0("组", next_group_num)]] <- scale
          next_group_num <- next_group_num + 1
        }
      }
      
      showNotification(paste0("已删除分组: ", group_to_delete, "，其中的量表已重新分配"), type = "message")
    }
  })
  
  # 重置为默认分组
  observeEvent(input$reset_groups, {
    if(!is.null(values$calculated_scales) && !is.null(values$calculated_scales$summary)) {
      scale_names <- names(values$calculated_scales$summary)
      values$variable_groups <- list()
      for(i in seq_along(scale_names)) {
        values$variable_groups[[paste0("组", i)]] <- scale_names[i]
      }
      values$edit_mode <- FALSE  # 退出编辑模式
      showNotification("已重置为默认分组（每个量表一组）", type = "message")
    }
  })
  
  # =============================================================================
  # 贝叶斯网络分析服务器端逻辑
  # =============================================================================
  
  # 贝叶斯网络分析准备状态
  output$bayesianReady <- reactive({
    # 检查是否已确认变量选择且有分析数据
    if(!is.null(values$variables_confirmed) && values$variables_confirmed && 
       !is.null(values$analysis_data) && ncol(values$analysis_data) >= 2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  outputOptions(output, "bayesianReady", suspendWhenHidden = FALSE)
  
  # 贝叶斯网络分析完成状态
  bayesian_completed <- reactiveVal(FALSE)
  
  output$bayesianComplete <- reactive({
    return(bayesian_completed())
  })
  outputOptions(output, "bayesianComplete", suspendWhenHidden = FALSE)
  
  # 智能约束规则生成
  smart_constraints <- reactive({
    req(values$scales, input$constraint_mode == "smart", input$smart_constraints)
    
    generate_smart_constraints(
      data = values$processed_data,
      scales = values$scales,
      constraint_types = input$smart_constraints,
      inter_scale_strength = input$inter_scale_strength %||% 0.8
    )
  })
  
  # 手动约束规则解析
  manual_constraints <- reactive({
    if(input$constraint_mode != "manual") return(list(blacklist = NULL, whitelist = NULL))
    
    blacklist_parsed <- parse_manual_constraints(input$manual_blacklist)
    whitelist_parsed <- parse_manual_constraints(input$manual_whitelist)
    
    list(
      blacklist = blacklist_parsed$constraints,
      whitelist = whitelist_parsed$constraints,
      blacklist_errors = blacklist_parsed$invalid_lines,
      whitelist_errors = whitelist_parsed$invalid_lines
    )
  })
  
  # 最终约束规则
  final_bayesian_constraints <- reactive({
    if(input$constraint_mode == "smart") {
      return(smart_constraints())
    } else if(input$constraint_mode == "manual") {
      return(manual_constraints())
    } else {
      return(list(blacklist = NULL, whitelist = NULL))
    }
  })
  
  # 约束规则计数显示
  output$blacklist_count <- renderText({
    constraints <- final_bayesian_constraints()
    if(is.null(constraints$blacklist)) "0" else nrow(constraints$blacklist)
  })
  
  output$whitelist_count <- renderText({
    constraints <- final_bayesian_constraints()
    if(is.null(constraints$whitelist)) "0" else nrow(constraints$whitelist)
  })
  
  # 约束规则验证
  observeEvent(input$validate_blacklist, {
    req(input$manual_blacklist)
    
    parsed <- parse_manual_constraints(input$manual_blacklist)
    validation <- validate_constraints(parsed$constraints, names(values$processed_data))
    
    if(validation$valid) {
      output$blacklist_status <- renderText({
        paste("✅ 黑名单有效\n",
              "规则数量:", validation$stats$total_rules, "\n",
              "涉及变量:", validation$stats$unique_from + validation$stats$unique_to)
      })
    } else {
      output$blacklist_status <- renderText({
        paste("❌ 黑名单有误:\n", paste(validation$errors, collapse = "\n"))
      })
    }
  })
  
  observeEvent(input$validate_whitelist, {
    req(input$manual_whitelist)
    
    parsed <- parse_manual_constraints(input$manual_whitelist)
    validation <- validate_constraints(parsed$constraints, names(values$processed_data))
    
    if(validation$valid) {
      output$whitelist_status <- renderText({
        paste("✅ 白名单有效\n",
              "规则数量:", validation$stats$total_rules, "\n", 
              "涉及变量:", validation$stats$unique_from + validation$stats$unique_to)
      })
    } else {
      output$whitelist_status <- renderText({
        paste("❌ 白名单有误:\n", paste(validation$errors, collapse = "\n"))
      })
    }
  })
  
  # 贝叶斯网络分析执行
  observeEvent(input$run_bayesian, {
    # 检查变量选择是否已确认且分析数据已准备
    if(is.null(values$variables_confirmed) || !values$variables_confirmed) {
      showNotification("请先在变量选择页面确认变量选择", type = "error")
      return()
    }
    
    if(is.null(values$analysis_data) || ncol(values$analysis_data) < 2) {
      showNotification("贝叶斯分析需要至少2个变量", type = "error")
      return()
    }
    
    withProgress(message = '正在进行贝叶斯网络分析...', value = 0, {
      
      incProgress(0.1, detail = "准备数据和约束规则...")
      
      # 使用变量选择页面准备的分析数据
      analysis_data <- values$analysis_data
      n_vars <- ncol(analysis_data)
      
      # 检查贝叶斯网络分析的数据要求
      if(n_vars > 20) {
        showNotification(paste0("贝叶斯网络分析建议变量数不超过20个，当前有", n_vars, "个变量"), type = "warning")
      }
      
      # 检查数据质量
      complete_cases <- sum(complete.cases(analysis_data))
      if(complete_cases < 10) {
        showNotification(paste0("贝叶斯网络分析需要更多的完整案例（当前：", complete_cases, "）"), type = "error")
        return()
      }
      
      incProgress(0.2, detail = "生成约束规则...")
      
      # 获取约束规则
      constraints <- final_bayesian_constraints()
      
      incProgress(0.3, detail = "开始网络学习...")
      
      tryCatch({
        # 记录贝叶斯网络分析代码
        bayesian_code <- c(
          "# ===== 贝叶斯网络分析 Bayesian Network Analysis =====",
          "library(bnlearn)",
          "library(Rgraphviz)  # 网络可视化",
          "",
          "# 数据离散化（如果需要）",
          "# 贝叶斯网络通常需要离散数据",
          "discrete_data <- apply(analysis_data, 2, function(x) {",
          "  cut(x, breaks = 3, labels = c('Low', 'Medium', 'High'))",
          "})",
          "discrete_data <- as.data.frame(discrete_data)",
          "",
          paste0("# 贝叶斯网络学习算法: ", input$bn_algorithm),
          paste0("# 评分函数: ", input$score_function),
          paste0("# Bootstrap轮数: ", input$bootstrap_rounds),
          "",
          "# 约束设置",
          if(!is.null(constraints$blacklist) && nrow(constraints$blacklist) > 0) {
            paste0("blacklist <- data.frame(",
                   "from = c(", paste0("'", constraints$blacklist$from, "'", collapse = ", "), "),",
                   "to = c(", paste0("'", constraints$blacklist$to, "'", collapse = ", "), "))")
          } else {
            "blacklist <- NULL"
          },
          if(!is.null(constraints$whitelist) && nrow(constraints$whitelist) > 0) {
            paste0("whitelist <- data.frame(",
                   "from = c(", paste0("'", constraints$whitelist$from, "'", collapse = ", "), "),",
                   "to = c(", paste0("'", constraints$whitelist$to, "'", collapse = ", "), "))")
          } else {
            "whitelist <- NULL"
          },
          "",
          "# 学习网络结构",
          paste0("learned_network <- ", input$bn_algorithm, "(discrete_data"),
          if(!is.null(constraints$blacklist) && nrow(constraints$blacklist) > 0) ", blacklist = blacklist" else "",
          if(!is.null(constraints$whitelist) && nrow(constraints$whitelist) > 0) ", whitelist = whitelist" else "",
          ")",
          "",
          "# Bootstrap稳定性分析",
          paste0("boot_result <- boot.strength(discrete_data, R = ", input$bootstrap_rounds, ","),
          paste0("                           algorithm = '", input$bn_algorithm, "'"),
          if(!is.null(constraints$blacklist) && nrow(constraints$blacklist) > 0) ", blacklist = blacklist" else "",
          if(!is.null(constraints$whitelist) && nrow(constraints$whitelist) > 0) ", whitelist = whitelist" else "",
          ")",
          "",
          "# 平均网络",
          paste0("averaged_network <- averaged.network(boot_result, threshold = ", input$strength_threshold, ")"),
          "",
          "# 可视化",
          "graphviz.plot(learned_network, main = 'Learned Bayesian Network')",
          "strength.plot(averaged_network, boot_result, shape = 'ellipse')"
        )
        values$code_recorder <- record_actual_code(values$code_recorder, bayesian_code, "bayesian_analysis", "贝叶斯网络分析")
        
        # 执行贝叶斯网络分析
        values$bayesian_result <- conduct_likert_bayesian_analysis(
          data = analysis_data,
          algorithm = input$bn_algorithm,
          score = input$score_function,
          bootstrap_n = input$bootstrap_rounds,
          threshold = input$strength_threshold,
          direction_threshold = input$direction_threshold,
          blacklist = constraints$blacklist,
          whitelist = constraints$whitelist
        )
        
        # 自动保存贝叶斯网络分析结果
        if(!is.null(values$bayesian_result)) {
          # 创建边强度数据框
          if(!is.null(values$bayesian_result$stable_edges)) {
            edges_df <- values$bayesian_result$stable_edges
            edges_df <- edges_df[order(-edges_df$strength), ]
            
            # 保存贝叶斯分析结果
            auto_save_result("bayesian", 
                            result_object = values$bayesian_result,
                            data_frame = edges_df,
                            filename_prefix = "Fig5b_bayesian_network")
          }
        }
        
        incProgress(0.8, detail = "完成分析...")
        
        # 生成贝叶斯网络主图：Figure5a结构图和Figure5b平均网络图
        tryCatch({
          timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
          
          # 设置工作目录到输出文件夹
          if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
            old_wd <- getwd()
            setwd(values$output_folder)
            on.exit(setwd(old_wd))
            
            # 获取分组和颜色信息
            groups <- if(!is.null(values$variable_groups)) values$variable_groups else NULL
            colors <- if(!is.null(groups)) {
              unique_groups <- unique(groups)
              VIZ_CONFIG$colors$primary[1:length(unique_groups)]
            } else NULL
            
            # Figure5a: 贝叶斯网络结构图（无权重的学习图结构）
            figure5a_file <- paste0("Fig5a_bayesian_structure_", timestamp, ".pdf")
            pdf(figure5a_file, width = 8, height = 6)
            tryCatch({
              if(!is.null(values$bayesian_result) && !is.null(values$bayesian_result$learned_network)) {
                create_bayesian_network_plot(
                  bayesian_result = values$bayesian_result,
                  colors = values$network_group_colors,  # 使用组级别颜色
                  groups = values$network_groups_by_index,  # 使用正确的索引格式分组
                  title = "Bayesian Network Structure",
                  network_type = "structure"  # 仅显示结构，无权重
                )
              }
            }, error = function(e) {
              plot.new()
              text(0.5, 0.5, paste("贝叶斯结构图生成失败:", e$message), cex = 1.2, col = "red")
            })
            dev.off()
            values$bayesian_structure_pdf <- figure5a_file
            
            # Figure5b: 贝叶斯平均网络图（带颜色强度值的版本）
            figure5b_file <- paste0("Fig5b_bayesian_averaged_", timestamp, ".pdf")
            pdf(figure5b_file, width = 8, height = 6)
            tryCatch({
              if(!is.null(values$bayesian_result) && !is.null(values$bayesian_result$averaged_network)) {
                create_bayesian_network_plot(
                  bayesian_result = values$bayesian_result,
                  colors = values$network_group_colors,  # 使用组级别颜色
                  groups = values$network_groups_by_index,  # 使用正确的索引格式分组
                  title = "Averaged Bayesian Network",
                  network_type = "averaged"  # 显示平均网络，带权重强度
                )
              }
            }, error = function(e) {
              plot.new()
              text(0.5, 0.5, paste("贝叶斯平均图生成失败:", e$message), cex = 1.2, col = "red")
            })
            dev.off()
            values$bayesian_averaged_pdf <- figure5b_file
            
            # Figure5a对应的CSV: 贝叶斯网络结构数据
            figure5a_csv <- paste0("Fig5a_bayesian_structure_", timestamp, ".csv")
            if(!is.null(values$bayesian_result$learned_network)) {
              structure_data <- data.frame(
                From = character(0),
                To = character(0),
                stringsAsFactors = FALSE
              )
              
              # 提取有向边信息
              if(!is.null(values$bayesian_result$learned_network$arcs) && nrow(values$bayesian_result$learned_network$arcs) > 0) {
                structure_data <- data.frame(
                  From = values$bayesian_result$learned_network$arcs[, "from"],
                  To = values$bayesian_result$learned_network$arcs[, "to"],
                  stringsAsFactors = FALSE
                )
              }
              write.csv(structure_data, figure5a_csv, row.names = FALSE)
              values$bayesian_structure_csv <- file.path(values$output_folder, figure5a_csv)
            }
            
            # Figure5b对应的CSV: 贝叶斯网络强度数据
            figure5b_csv <- paste0("Fig5b_bayesian_averaged_", timestamp, ".csv")
            if(!is.null(values$bayesian_result$averaged_network)) {
              strength_data <- values$bayesian_result$averaged_network
              write.csv(strength_data, figure5b_csv, row.names = TRUE)
              values$bayesian_averaged_csv <- file.path(values$output_folder, figure5b_csv)
            }
            
            # 额外的评估指标CSV（补充材料）
            sfig_metrics_file <- paste0("SFig_bayesian_metrics_", timestamp, ".csv")
            metrics_df <- data.frame(
              Metric = c("Network Score", "BIC Score", "Log-likelihood", "Mean CV Loss", "SD CV Loss"),
              Value = c(
                values$bayesian_result$network_score,
                values$bayesian_result$bic_score,
                values$bayesian_result$loglik_score,
                values$bayesian_result$mean_cv_loss,
                values$bayesian_result$sd_cv_loss
              ),
              stringsAsFactors = FALSE
            )
            write.csv(metrics_df, sfig_metrics_file, row.names = FALSE)
            
            # 保存主要文件路径（保持兼容性）
            values$bayesian_pdf_path <- file.path(values$output_folder, figure5a_file)  # 主要使用结构图PDF
            values$bayesian_metrics_csv_path <- file.path(values$output_folder, sfig_metrics_file)
            
            cat("贝叶斯网络PDF:", values$bayesian_pdf_path, "\n")
            cat("贝叶斯评估指标CSV:", values$bayesian_metrics_csv_path, "\n")
          }
        }, error = function(e) {
          cat("贝叶斯网络PDF/CSV生成失败:", e$message, "\n")
        })
        
        bayesian_completed(TRUE)
        showNotification("贝叶斯网络分析完成！", type = "message")
        
        # 更新完整脚本（包含贝叶斯分析）
        if(!is.null(values$code_recorder) && !is.null(values$output_folder)) {
          tryCatch({
            script_path <- file.path(values$output_folder, "NetworkAnalysis_Complete_Script.R")
            generate_complete_script(values$code_recorder, script_path)
            cat("📝 已更新完整脚本（包含贝叶斯分析）:", script_path, "\n")
          }, error = function(e) {
            cat("⚠️ 贝叶斯分析脚本更新失败:", e$message, "\n")
          })
        }
        
      }, error = function(e) {
        showNotification(paste("贝叶斯网络分析失败:", e$message), type = "error")
        bayesian_completed(FALSE)
      })
    })
  })
  
  # 贝叶斯网络图输出 - 使用继承样式的可视化
  output$bayesian_network_plot <- renderPlot({
    req(values$bayesian_result)
    
    tryCatch({
      # 获取网络分析的可视化配置（使用组级别颜色）
      colors <- if(!is.null(values$network_group_colors)) values$network_group_colors else NULL
      groups <- if(!is.null(values$variable_groups)) values$variable_groups else NULL
      layout <- if(!is.null(values$network_result) && !is.null(values$network_result$layout)) {
        values$network_result$layout
      } else {
        "spring"
      }
      
      # 使用新的贝叶斯网络可视化函数
      bayesian_plot <- create_bayesian_network_plot(
        bayesian_result = values$bayesian_result,
        colors = values$network_group_colors,  # 使用组级别颜色
        groups = values$network_groups_by_index,  # 使用正确的索引格式分组
        layout = layout,
        title = "学习的贝叶斯网络结构"
      )
      
      if(!is.null(bayesian_plot)) {
        plot(bayesian_plot)
      } else {
        # 备用方案：使用bnlearn的默认可视化
        if(requireNamespace("bnlearn", quietly = TRUE) && requireNamespace("Rgraphviz", quietly = TRUE)) {
          bnlearn::graphviz.plot(values$bayesian_result$learned_network)
        } else {
          # 最基础的备用方案：使用igraph
          if(requireNamespace("igraph", quietly = TRUE)) {
            edges <- values$bayesian_result$stable_edges
            if(!is.null(edges) && nrow(edges) > 0) {
              g <- igraph::graph_from_data_frame(edges[, c("from", "to")], directed = TRUE)
              igraph::plot(g, vertex.size = 20, vertex.label.cex = 0.8,
                        edge.arrow.size = 0.5, layout = igraph::layout_with_fr)
            } else {
              plot.new()
              text(0.5, 0.5, "未发现稳定的边连接", cex = 1.5)
            }
          } else {
            plot.new()
            text(0.5, 0.5, "需要安装Rgraphviz或igraph包进行可视化", cex = 1.2)
          }
        }
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("绘图失败:", e$message), cex = 1.2)
    })
  })
  
  # 贝叶斯网络统计信息输出
  output$bayesian_network_stats <- renderText({
    req(values$bayesian_result)
    
    params <- values$bayesian_result$parameters
    
    paste0(
      "📊 网络结构统计\n",
      "====================\n",
      "算法: ", params$algorithm, "\n",
      "评分函数: ", params$score, "\n",
      "样本量: ", params$sample_size, "\n",
      "变量数: ", params$variable_count, "\n",
      "边数量: ", params$edge_count, "\n",
      "稳定边数: ", params$stable_edge_count, " (阈值≥", params$threshold, ")\n",
      "边密度: ", sprintf("%.2f%%", params$edge_density * 100), "\n",
      "Bootstrap轮数: ", params$bootstrap_n
    )
  })
  
  # 模型评分指标输出
  output$model_scores <- renderText({
    req(values$bayesian_result)
    
    paste0(
      "🎯 模型评估指标\n",
      "==================\n",
      "网络评分 (", values$bayesian_result$parameters$score, "): ", 
      sprintf("%.2f", values$bayesian_result$network_score), "\n",
      "BIC: ", sprintf("%.2f", values$bayesian_result$bic_score), "\n",
      "Log-likelihood: ", sprintf("%.2f", values$bayesian_result$loglik_score), "\n\n",
      "📊 交叉验证 (10-fold)\n",
      "平均损失: ", sprintf("%.4f", values$bayesian_result$mean_cv_loss), "\n",
      "标准差: ", sprintf("%.4f", values$bayesian_result$sd_cv_loss), "\n\n",
      "💡 解释：\n",
      "• BIC越小越好 (拟合与复杂性的平衡)\n",
      "• Log-likelihood越大越好 (拟合质量)\n",
      "• CV损失越小越好 (泛化能力)"
    )
  })
  
  # 稳定性指标输出
  output$stability_metrics <- renderText({
    req(values$bayesian_result)
    
    boot_result <- values$bayesian_result$bootstrap_result
    stable_edges <- values$bayesian_result$stable_edges
    params <- values$bayesian_result$parameters
    
    # 计算稳定性统计
    high_strength_edges <- nrow(boot_result[boot_result$strength >= 0.9, ])
    medium_strength_edges <- nrow(boot_result[boot_result$strength >= 0.7 & boot_result$strength < 0.9, ])
    low_strength_edges <- nrow(boot_result[boot_result$strength >= 0.5 & boot_result$strength < 0.7, ])
    
    high_direction_edges <- nrow(boot_result[boot_result$direction >= 0.8, ])
    
    paste0(
      "🔄 Bootstrap稳定性分析\n",
      "========================\n",
      "Bootstrap轮数: ", params$bootstrap_n, "\n",
      "强度阈值: ", params$threshold, "\n",
      "方向阈值: ", params$direction_threshold, "\n\n",
      "📊 边强度分布：\n",
      "• 高强度 (≥0.9): ", high_strength_edges, " 条\n",
      "• 中强度 (0.7-0.9): ", medium_strength_edges, " 条\n",
      "• 低强度 (0.5-0.7): ", low_strength_edges, " 条\n\n",
      "📈 方向稳定性：\n",
      "• 高方向性 (≥0.8): ", high_direction_edges, " 条\n\n",
      "✅ 稳定边 (同时满足强度和方向阈值)：\n",
      "• 总数: ", nrow(stable_edges), " 条\n",
      "• 占比: ", sprintf("%.1f%%", nrow(stable_edges) / nrow(boot_result) * 100)
    )
  })
  
  # 交叉验证图输出
  output$cv_plot <- renderPlot({
    req(values$bayesian_result, values$bayesian_result$cv_loss)
    
    cv_loss <- values$bayesian_result$cv_loss
    
    if(length(cv_loss) > 0) {
      par(mar = c(4, 4, 2, 1))
      plot(cv_loss, type = "o", pch = 19, col = "#4A90E2",
           main = "10-Fold 交叉验证损失",
           xlab = "Fold", ylab = "Log-likelihood Loss",
           ylim = c(min(cv_loss) * 0.95, max(cv_loss) * 1.05))
      
      # 添加平均线
      abline(h = mean(cv_loss), col = "#D0021B", lty = 2, lwd = 2)
      
      # 添加标准差区间
      mean_loss <- mean(cv_loss)
      sd_loss <- sd(cv_loss)
      abline(h = mean_loss + sd_loss, col = "#D0021B", lty = 3)
      abline(h = mean_loss - sd_loss, col = "#D0021B", lty = 3)
      
      legend("topright", 
             legend = c("CV Loss", "Mean", "±1 SD"),
             col = c("#4A90E2", "#D0021B", "#D0021B"),
             lty = c(1, 2, 3), pch = c(19, NA, NA),
             cex = 0.8)
    } else {
      plot.new()
      text(0.5, 0.5, "交叉验证结果不可用", cex = 1.2)
    }
  })
  
  # 特征值图输出
  output$eigenvalue_plot <- renderPlot({
    req(values$bayesian_result, values$bayesian_result$eigen_values)
    
    eigen_vals <- values$bayesian_result$eigen_values
    
    par(mar = c(4, 4, 2, 1))
    plot(eigen_vals, type = "b", pch = 19, col = "#4A90E2",
         main = "相关矩阵特征值分布",
         xlab = "成分", ylab = "特征值",
         ylim = c(0, max(eigen_vals) * 1.1))
    
    # 添加Kaiser准则线（特征值=1）
    abline(h = 1, col = "#D0021B", lty = 2, lwd = 2)
    
    # 标注大于1的特征值数量
    n_factors <- sum(eigen_vals > 1)
    text(length(eigen_vals) * 0.7, max(eigen_vals) * 0.9,
         paste("因子数 (特征值>1):", n_factors),
         col = "#D0021B", cex = 1.1, font = 2)
    
    legend("topright", 
           legend = c("特征值", "Kaiser准则"),
           col = c("#4A90E2", "#D0021B"),
           lty = c(1, 2), pch = c(19, NA),
           cex = 0.8)
  })
  
  # 平均网络图输出
  output$bayesian_averaged_plot <- renderPlot({
    req(values$bayesian_result, values$bayesian_result$averaged_network)
    
    tryCatch({
      # 使用与主网络相同的样式绘制平均网络
      colors <- if(!is.null(values$colors)) values$colors else NULL
      groups <- if(!is.null(values$variable_groups)) values$variable_groups else NULL
      layout <- if(!is.null(values$network_result) && !is.null(values$network_result$layout)) {
        values$network_result$layout
      } else {
        "spring"
      }
      
      averaged_plot <- create_bayesian_network_plot(
        bayesian_result = values$bayesian_result,
        colors = values$network_group_colors,  # 使用组级别颜色
        groups = values$network_groups_by_index,  # 使用正确的索引格式分组
        layout = layout,
        title = "Bootstrap平均网络 (稳定边)",
        network_type = "averaged"  # 指定为平均网络，显示权重强度
      )
      
      if(!is.null(averaged_plot)) {
        plot(averaged_plot)
      } else {
        plot.new()
        text(0.5, 0.5, "平均网络可视化失败", cex = 1.2, col = "red")
      }
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("平均网络绘制失败:", e$message), cex = 1, col = "red")
    })
  })
  
  # 网络比较输出
  output$network_comparison <- renderText({
    req(values$bayesian_result)
    
    # 如果有GLASSO网络结果，进行比较
    if(!is.null(values$network_result)) {
      paste0(
        "🔍 贝叶斯网络 vs GLASSO网络\n",
        "==============================\n",
        "贝叶斯网络边数: ", values$bayesian_result$parameters$edge_count, "\n",
        "GLASSO网络边数: ", "需要从network_result提取", "\n\n",
        "💡 主要差异：\n",
        "• 贝叶斯网络：有向边，表示因果关系\n",
        "• GLASSO网络：无向边，表示偏相关\n",
        "• 贝叶斯稳定边更保守，关注因果方向\n",
        "• GLASSO关注条件独立性"
      )
    } else {
      paste0(
        "🔍 网络比较\n",
        "============\n",
        "需要先完成GLASSO网络分析才能进行比较。\n\n",
        "💡 分析建议：\n",
        "1. 先进行网络分析 (GLASSO)\n",
        "2. 再进行贝叶斯分析\n",
        "3. 比较两种方法的发现"
      )
    }
  })
  
  # 参数拟合图输出 
  output$bn_fit_plot <- renderPlot({
    req(values$bayesian_result, values$bayesian_result$fitted_network)
    
    tryCatch({
      if(requireNamespace("bnlearn", quietly = TRUE)) {
        # 创建条件概率分布的可视化
        fitted_net <- values$bayesian_result$fitted_network
        
        # 选择一个有足够边的节点进行展示
        variable_names <- names(values$bayesian_result$data)
        
        if(length(variable_names) > 0) {
          # 使用第一个变量作为示例
          target_var <- variable_names[1]
          
          # 创建直方图显示拟合结果
          par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
          
          # 显示前4个变量的分布
          for(i in 1:min(4, length(variable_names))) {
            var_name <- variable_names[i]
            var_data <- values$bayesian_result$data[[var_name]]
            
            hist(var_data, 
                 main = paste("分布:", var_name),
                 xlab = var_name,
                 ylab = "频数",
                 col = "#4A90E2",
                 border = "white")
            
            # 添加正态拟合曲线
            if(is.numeric(var_data)) {
              x_seq <- seq(min(var_data), max(var_data), length.out = 100)
              normal_curve <- dnorm(x_seq, mean(var_data), sd(var_data)) * length(var_data) * diff(range(var_data)) / 30
              lines(x_seq, normal_curve, col = "#D0021B", lwd = 2)
            }
          }
          
          par(mfrow = c(1, 1))
        } else {
          plot.new()
          text(0.5, 0.5, "参数拟合结果不可用", cex = 1.2)
        }
      } else {
        plot.new()
        text(0.5, 0.5, "需要bnlearn包进行参数可视化", cex = 1.2)
      }
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("参数拟合可视化失败:", e$message), cex = 1, col = "red")
    })
  })
  
  # 残差分析输出
  output$residual_analysis <- renderText({
    req(values$bayesian_result)
    
    tryCatch({
      if(!is.null(values$bayesian_result$fitted_network)) {
        fitted_net <- values$bayesian_result$fitted_network
        data <- values$bayesian_result$data
        
        # 计算基本的拟合统计
        n_params <- sum(sapply(fitted_net, function(x) length(x$coefficients)))
        n_obs <- nrow(data)
        n_vars <- ncol(data)
        
        # 计算模型复杂度
        complexity <- n_params / (n_obs * n_vars)
        
        paste0(
          "🔧 参数拟合诊断\n",
          "==================\n",
          "观测数: ", n_obs, "\n",
          "变量数: ", n_vars, "\n", 
          "参数数: ", n_params, "\n",
          "模型复杂度: ", sprintf("%.3f", complexity), "\n\n",
          "💡 拟合质量评估：\n",
          if(complexity < 0.1) {
            "• 模型复杂度适中，拟合良好"
          } else if(complexity < 0.2) {
            "• 模型较复杂，可能存在过拟合风险"
          } else {
            "• 模型过于复杂，建议简化网络结构"
          }, "\n\n",
          "📊 建议：\n",
          "• 参数估计基于高斯假设\n",
          "• 适用于连续型心理量表数据\n", 
          "• 可用于预测和干预分析"
        )
      } else {
        "参数拟合结果不可用"
      }
      
    }, error = function(e) {
      paste("残差分析失败:", e$message)
    })
  })
  
  # 稳定性分析图输出
  output$bayesian_stability_plot <- renderPlot({
    req(values$bayesian_result)
    
    tryCatch({
      if(requireNamespace("bnlearn", quietly = TRUE)) {
        bnlearn::strength.plot(values$bayesian_result$averaged_network, 
                              values$bayesian_result$bootstrap_result, 
                              shape = "ellipse")
      } else {
        plot.new()
        text(0.5, 0.5, "需要bnlearn包进行稳定性可视化", cex = 1.2)
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("稳定性图绘制失败:", e$message), cex = 1.2)
    })
  })
  
  # 边强度表输出
  output$bayesian_edges_table <- DT::renderDataTable({
    req(values$bayesian_result)
    
    edges_data <- values$bayesian_result$stable_edges
    if(nrow(edges_data) > 0) {
      # 格式化数据表
      edges_data$strength <- round(edges_data$strength, 3)
      edges_data$direction <- round(edges_data$direction, 3)
      
      DT::datatable(
        edges_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        caption = "稳定边强度表 (强度 ≥ 0.85, 方向 ≥ 0.5)"
      )
    } else {
      DT::datatable(
        data.frame(信息 = "未发现达到阈值的稳定边连接"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # 分析报告输出
  output$bayesian_report <- renderUI({
    req(values$bayesian_result)
    
    HTML(generate_bayesian_report(values$bayesian_result, values$scales, values$network_result))
  })
  
  # 下载处理器
  output$download_bn_plot <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("bayesian_network_", timestamp, ".pdf")
    },
    content = function(file) {
      tryCatch({
        # 优先使用生成的PDF文件
        if(!is.null(values$bayesian_pdf_path) && file.exists(values$bayesian_pdf_path)) {
          file.copy(values$bayesian_pdf_path, file, overwrite = TRUE)
          cat("复制贝叶斯网络PDF文件:", values$bayesian_pdf_path, "->", file, "\n")
        } else {
          # 备用方案：重新生成PDF
          pdf(file, width = 8, height = 6)
          tryCatch({
            if(!is.null(values$bayesian_result)) {
              groups <- if(!is.null(values$variable_groups)) values$variable_groups else NULL
              colors <- if(!is.null(groups)) {
                unique_groups <- unique(groups)
                if(exists("VIZ_CONFIG")) VIZ_CONFIG$colors$primary[1:length(unique_groups)] else NULL
              } else NULL
              create_bayesian_network_plot(
                bayesian_result = values$bayesian_result,
                colors = values$network_group_colors,  # 使用组级别颜色
                groups = values$network_groups_by_index,  # 使用正确的索引格式分组
                title = "Bayesian Network Structure"
              )
            } else {
              plot.new()
              text(0.5, 0.5, "贝叶斯网络不可用", cex = 1.5, col = "red")
            }
          }, error = function(e) {
            plot.new()
            text(0.5, 0.5, paste("贝叶斯网络图生成失败:", e$message), cex = 1.2, col = "red")
          })
          dev.off()
        }
      }, error = function(e) {
        cat("保存贝叶斯网络图失败:", e$message, "\n")
      })
    }
  )
  
  output$download_bn_edges <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("bayesian_edges_", timestamp, ".csv")
    },
    content = function(file) {
      tryCatch({
        # 优先使用生成的CSV文件
        if(!is.null(values$bayesian_edges_csv_path) && file.exists(values$bayesian_edges_csv_path)) {
          file.copy(values$bayesian_edges_csv_path, file, overwrite = TRUE)
          cat("复制贝叶斯边数据CSV文件:", values$bayesian_edges_csv_path, "->", file, "\n")
        } else if(!is.null(values$bayesian_result$stable_edges)) {
          # 备用方案：重新生成CSV
          write.csv(values$bayesian_result$stable_edges, file, row.names = FALSE)
          cat("重新生成贝叶斯边数据CSV文件:", file, "\n")
        } else {
          # 如果没有数据，创建说明文件
          write("没有找到贝叶斯网络稳定边数据。请确保已运行贝叶斯网络分析。", file)
        }
      }, error = function(e) {
        cat("保存贝叶斯边数据失败:", e$message, "\n")
        write(paste("保存边数据时出错:", e$message), file)
      })
    }
  )
  
  # 贝叶斯评估指标下载
  output$download_bn_metrics <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("bayesian_metrics_", timestamp, ".csv")
    },
    content = function(file) {
      tryCatch({
        # 优先使用生成的CSV文件
        if(!is.null(values$bayesian_metrics_csv_path) && file.exists(values$bayesian_metrics_csv_path)) {
          file.copy(values$bayesian_metrics_csv_path, file, overwrite = TRUE)
          cat("复制贝叶斯评估指标CSV文件:", values$bayesian_metrics_csv_path, "->", file, "\n")
        } else if(!is.null(values$bayesian_result)) {
          # 备用方案：重新生成CSV
          metrics_df <- data.frame(
            Metric = c("Network Score", "BIC Score", "Log-likelihood", "Mean CV Loss", "SD CV Loss"),
            Value = c(
              values$bayesian_result$network_score,
              values$bayesian_result$bic_score,
              values$bayesian_result$loglik_score,
              values$bayesian_result$mean_cv_loss,
              values$bayesian_result$sd_cv_loss
            ),
            stringsAsFactors = FALSE
          )
          write.csv(metrics_df, file, row.names = FALSE)
          cat("重新生成贝叶斯评估指标CSV文件:", file, "\n")
        } else {
          # 如果没有数据，创建说明文件
          write("没有找到贝叶斯网络评估指标。请确保已运行贝叶斯网络分析。", file)
        }
      }, error = function(e) {
        cat("保存贝叶斯评估指标失败:", e$message, "\n")
        write(paste("保存评估指标时出错:", e$message), file)
      })
    }
  )
  
  output$download_bn_report <- downloadHandler(
    filename = function() {
      paste0("bayesian_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      report_content <- generate_bayesian_report(values$bayesian_result, values$scales, values$network_result)
      writeLines(report_content, file)
    }
  )
  
  # ==================== 网络温度分析下载处理器 ====================
  
  # 温度分析主要结果下载 - Fig4 PDF
  output$download_temp_plot <- downloadHandler(
    filename = "Fig4_network_temperature_comparison.pdf",
    content = function(file) {
      tryCatch({
        if(is.null(values$temperature_result) || !values$temperature_result$success) {
          # 创建空的PDF说明没有数据
          pdf(file, width = 8, height = 6)
          plot.new()
          text(0.5, 0.5, "温度分析未完成或失败", cex = 1.5, col = "red")
          dev.off()
          return()
        }
        
        pdf(file, width = 10, height = 8)
        
        result <- values$temperature_result
        # 检查是否为多组分析
        is_multigroup <- FALSE
        if(!is.null(result$models) && length(result$models) > 0) {
          first_model <- result$models[[1]]
          if(!is.null(first_model) && !is.null(first_model@parameters)) {
            params <- first_model@parameters
            beta_params <- params[params$matrix == "beta", ]
            groups <- unique(beta_params$group)
            is_multigroup <- length(groups) > 1
          }
        }
        
        if(is_multigroup) {
          # 多组分析：生成温度比较图
          models <- result$models
          model_names <- names(models)
          temp_data <- data.frame(Model = character(), Group = character(), Temperature = numeric(), stringsAsFactors = FALSE)
          
          for(model_name in model_names) {
            model <- models[[model_name]]
            params <- model@parameters
            beta_params <- params[params$matrix == "beta", ]
            
            if(nrow(beta_params) > 0) {
              groups <- unique(beta_params$group)
              for(group in groups) {
                group_betas <- beta_params[beta_params$group == group, "est"]
                if(length(group_betas) > 0) {
                  group_temp <- 1 / mean(group_betas, na.rm = TRUE)
                  temp_data <- rbind(temp_data, data.frame(
                    Model = model_name, 
                    Group = group, 
                    Temperature = group_temp,
                    stringsAsFactors = FALSE
                  ))
                }
              }
            }
          }
          
          if(nrow(temp_data) > 0) {
            # 重塑数据为矩阵格式
            temp_matrix <- reshape(temp_data, idvar = "Model", timevar = "Group", direction = "wide")
            rownames(temp_matrix) <- temp_matrix$Model
            temp_matrix <- temp_matrix[, -1, drop = FALSE]
            colnames(temp_matrix) <- gsub("Temperature.", "", colnames(temp_matrix))
            temp_matrix <- as.matrix(temp_matrix)
            
            # 创建分组比较的条形图
            par(mar = c(8, 6, 4, 8))
            colors <- c("#4285F4", "#EA4335", "#FBBC04", "#34A853")
            barplot(t(temp_matrix), 
                    beside = TRUE,
                    col = colors[1:ncol(temp_matrix)],
                    main = "网络温度组间比较 - Fig4\n(多组约束模型温度对比)",
                    xlab = "",
                    ylab = "网络温度 (T = 1/β)",
                    las = 2,
                    cex.names = 0.7,
                    cex.main = 1.2,
                    legend.text = colnames(temp_matrix),
                    args.legend = list(x = "topright", inset = c(-0.15, 0), cex = 0.8))
            mtext("约束模型类型", side = 1, line = 6, cex = 1)
          }
        } else {
          # 单组分析
          metrics <- result$metrics
          temperatures <- sapply(metrics, function(x) x$temperature)
          temperatures <- temperatures[!is.na(temperatures)]
          
          if(length(temperatures) > 0) {
            par(mar = c(5, 8, 4, 2))
            barplot(temperatures, 
                    names.arg = names(temperatures),
                    horiz = TRUE,
                    col = rainbow(length(temperatures), alpha = 0.7),
                    main = "网络温度模型比较 - Fig4",
                    xlab = "网络温度 (T = 1/β)",
                    las = 1,
                    cex.names = 0.8)
          }
        }
        
        dev.off()
        cat("Fig4 PDF保存成功:", file, "\n")
      }, error = function(e) {
        cat("Fig4 PDF生成失败:", e$message, "\n")
        # 生成错误说明PDF
        pdf(file, width = 8, height = 6)
        plot.new()
        text(0.5, 0.5, paste("PDF生成失败:", e$message), cex = 1.2, col = "red")
        dev.off()
      })
    }
  )
  
  # 温度分析结果数据下载 - CSV
  output$download_temp_results <- downloadHandler(
    filename = "Fig4_temperature_results.csv",
    content = function(file) {
      tryCatch({
        if(is.null(values$temperature_result) || !values$temperature_result$success) {
          write("温度分析未完成或失败", file)
          return()
        }
        
        result <- values$temperature_result
        
        # 检查是否为多组分析并提取数据
        if(!is.null(result$models) && length(result$models) > 0) {
          first_model <- result$models[[1]]
          params <- first_model@parameters
          beta_params <- params[params$matrix == "beta", ]
          groups <- unique(beta_params$group)
          is_multigroup <- length(groups) > 1
          
          if(is_multigroup) {
            # 多组分析：生成详细的温度数据表
            models <- result$models
            all_results <- list()
            
            for(model_name in names(models)) {
              model <- models[[model_name]]
              model_params <- model@parameters
              model_beta_params <- model_params[model_params$matrix == "beta", ]
              
              for(group in groups) {
                group_betas <- model_beta_params[model_beta_params$group == group, "est"]
                if(length(group_betas) > 0) {
                  group_temp <- 1 / mean(group_betas, na.rm = TRUE)
                  all_results[[length(all_results) + 1]] <- data.frame(
                    Model = model_name,
                    Group = group,
                    Beta_Mean = mean(group_betas, na.rm = TRUE),
                    Temperature = group_temp,
                    Model_Type = if(grepl("Dense", model_name)) "Dense" else "Sparse",
                    Constraint_Level = case_when(
                      grepl("Free", model_name) ~ "M1-M2: 自由模型",
                      grepl("Omega", model_name) & !grepl("Tau", model_name) ~ "M3-M4: 网络相等", 
                      grepl("OmegaTau", model_name) & !grepl("Beta", model_name) ~ "M5-M6: 网络+阈值相等",
                      grepl("OmegaTauBeta", model_name) ~ "M7-M8: 完全相等",
                      TRUE ~ "其他"
                    ),
                    stringsAsFactors = FALSE
                  )
                }
              }
            }
            
            results_df <- do.call(rbind, all_results)
            
            # 添加模型比较信息
            if(!is.null(result$comparison)) {
              comparison_info <- paste("最佳模型(BIC):", names(result$models)[1])
              results_df$Best_Model_Notes <- comparison_info
            }
            
            write.csv(results_df, file, row.names = FALSE)
            
          } else {
            # 单组分析
            metrics <- result$metrics
            single_results <- data.frame(
              Model = names(metrics),
              Temperature = sapply(metrics, function(x) x$temperature),
              stringsAsFactors = FALSE
            )
            write.csv(single_results, file, row.names = FALSE)
          }
        } else {
          write("无法提取温度分析结果数据", file)
        }
        
        cat("温度结果CSV保存成功:", file, "\n")
      }, error = function(e) {
        cat("温度结果CSV生成失败:", e$message, "\n")
        write(paste("数据导出失败:", e$message), file)
      })
    }
  )
  
  # 下载完整R脚本
  output$download_temp_script <- downloadHandler(
    filename = function() {
      paste0("NetworkTemperatureAnalysis_Script_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
    },
    content = function(file) {
      tryCatch({
        if(is.null(values$temperature_result) || 
           !values$temperature_result$success ||
           is.null(values$temperature_result$code_recorder)) {
          
          # 如果没有代码记录器，生成基本脚本
          basic_script <- c(
            "################################################################################",
            "##                    心理量表网络温度分析脚本                      ##",
            "##                 Psychology Network Temperature Analysis                    ##",
            "################################################################################",
            "",
            "# 分析未完成或代码记录不可用",
            "# Analysis not completed or code recorder unavailable",
            "",
            "# 请先运行完整的网络温度分析再下载脚本",
            "# Please complete the network temperature analysis first",
            "",
            'cat("请在NetworkApp中运行完整分析后再下载脚本\n")'
          )
          
          writeLines(basic_script, file)
          return()
        }
        
        # 使用代码记录器生成完整脚本
        code_recorder <- values$temperature_result$code_recorder
        script_content <- generate_complete_script(code_recorder)
        
        if(!is.null(script_content)) {
          writeLines(strsplit(script_content, "\n")[[1]], file)
          cat("📝 完整R脚本下载成功:", file, "\n")
        } else {
          writeLines(c("😭 脚本生成失败", "Script generation failed"), file)
        }
        
      }, error = function(e) {
        cat("⚠️ R脚本下载失败:", e$message, "\n")
        writeLines(c(
          "# R脚本生成错误 Script Generation Error",
          paste("# 错误信息 Error:", e$message),
          "",
          "# 请在NetworkApp中重新运行分析",
          "# Please re-run the analysis in NetworkApp"
        ), file)
      })
    }
  )
  
  # 网络温度分析相关响应式输出
  output$temp_group_var_selector <- renderUI({
    req(values$processed_data)
    
    # 获取可用的分类变量
    categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) 
      is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) <= 10))]
    
    if(length(categorical_vars) == 0) {
      return(div("暂无可用的分组变量"))
    }
    
    selectInput("temp_group_var", "选择分组变量",
               choices = setNames(categorical_vars, categorical_vars),
               selected = if("Gender" %in% categorical_vars) "Gender" else categorical_vars[1])
  })
  
  # 动态更新热图组别选择器和组别标题
  observe({
    if(!is.null(values$temperature_result) && values$temperature_result$success && 
       !is.null(values$temperature_result$is_multigroup) && values$temperature_result$is_multigroup) {
      
      # 从温度分析结果中提取组别信息
      tryCatch({
        models <- values$temperature_result$models
        if(length(models) > 0) {
          first_model <- models[[1]]
          params <- first_model@parameters
          beta_params <- params[params$matrix == "beta", ]
          
          if(nrow(beta_params) > 0) {
            groups <- unique(beta_params$group)
            group_choices <- setNames(groups, paste0("组别", seq_along(groups), " (", groups, ")"))
            
            
            # 动态更新组别标题
            if(length(groups) >= 1) {
              group1_name <- groups[1]
              output$group1_title_text <- renderText({
                paste0("👨 ", group1_name, " 网络图")
              })
            }
            
            if(length(groups) >= 2) {
              group2_name <- groups[2]
              output$group2_title_text <- renderText({
                paste0("👩 ", group2_name, " 网络图")
              })
            }
          }
        }
      }, error = function(e) {
        cat("更新热图组别选择器失败:", e$message, "\n")
      })
    }
  })
  
  # 温度分析状态输出
  output$temperatureAnalysisRunning <- reactive({
    !is.null(values$temperature_analysis_running) && values$temperature_analysis_running
  })
  outputOptions(output, "temperatureAnalysisRunning", suspendWhenHidden = FALSE)
  
  output$temperatureAnalysisComplete <- reactive({
    !is.null(values$temperature_result) && !is.null(values$temperature_result$success) && values$temperature_result$success
  })
  outputOptions(output, "temperatureAnalysisComplete", suspendWhenHidden = FALSE)
  
  output$temperatureAnalysisError <- reactive({
    !is.null(values$temperature_error)
  })
  outputOptions(output, "temperatureAnalysisError", suspendWhenHidden = FALSE)
  
  output$showTempVisualizations <- reactive({
    !is.null(values$temperature_result) && values$temperature_result$success
  })
  outputOptions(output, "showTempVisualizations", suspendWhenHidden = FALSE)
  
  # 网络温度分析按钮事件
  observeEvent(input$run_temperature_analysis, {
    
    cat("🖐️ 用户点击了网络温度分析按钮\n")
    
    # 检查基本条件
    if(is.null(values$processed_data)) {
      showNotification("⚠️ 请先上传数据", type = "error", duration = 5)
      return()
    }
    
    if(is.null(values$final_variables) || length(values$final_variables) == 0) {
      showNotification("⚠️ 请先在\"变量选择\"页面选择分析变量并点击\"确认选择\"", type = "error", duration = 10)
      return()
    }
    
    # 设置运行状态
    values$temperature_analysis_running <- TRUE
    values$temperature_error <- NULL
    values$temperature_result <- NULL
    
    # 获取参数
    selected_vars <- values$final_variables
    group_var <- NULL
    
    if(input$enable_grouping && !is.null(input$temp_group_var)) {
      group_var <- input$temp_group_var
      
      # 如果是年龄分组，需要创建年龄分组变量
      if(group_var == "Age" && !is.null(input$age_groups)) {
        age_groups <- input$age_groups
        age_data <- values$processed_data$Age
        age_breaks <- quantile(age_data, probs = seq(0, 1, length.out = age_groups + 1), na.rm = TRUE)
        values$processed_data$AgeGroup <- cut(age_data, breaks = age_breaks, 
                                            labels = paste0("Group", 1:age_groups), include.lowest = TRUE)
        group_var <- "AgeGroup"
        cat("创建年龄分组变量:", age_groups, "组\n")
      }
    }
    
    tryCatch({
      
      withProgress(message = '正在进行网络温度分析...', value = 0, {
        
        incProgress(0.1, detail = "准备数据...")
        
        # 初始化代码记录器
        code_recorder <- init_code_recorder()
        
        # 记录数据加载代码
        if(!is.null(values$upload_filepath)) {
          code_recorder <- record_data_loading(code_recorder, values$upload_filepath, values$sheet_name)
        }
        
        # 记录数据预处理代码
        code_recorder <- record_data_preprocessing(
          code_recorder, 
          selected_vars, 
          input$temp_binary_method, 
          input$temp_binary_encoding, 
          group_var
        )
        
        cat("🚀 调用network_temperature_analysis函数...\n")
        cat("参数概要:\n")
        cat("  - 数据行数:", nrow(values$processed_data), "\n")
        cat("  - 分析变量:", paste(selected_vars, collapse = ", "), "\n")
        cat("  - 分组变量:", ifelse(is.null(group_var), "无", group_var), "\n")
        cat("  - 二值化方法:", input$temp_binary_method, "\n")
        cat("  - 编码方式:", input$temp_binary_encoding, "\n")
        
        incProgress(0.2, detail = "运行温度分析...")
        
        temp_result <- network_temperature_analysis(
          data = values$processed_data,
          group_var = group_var,
          selected_vars = selected_vars,
          binary_transform = input$temp_binary_method,
          binary_encoding = input$temp_binary_encoding,
          binary_threshold = input$temp_binary_threshold,
          estimator = input$temp_estimator,
          alpha = input$temp_alpha
        )
        
        cat("✅ network_temperature_analysis函数返回正常\n")
        
        incProgress(0.8, detail = "生成结果...")
        
        # 记录网络温度分析代码
        code_recorder <- record_temperature_analysis(code_recorder, selected_vars, group_var)
        
        # 记录可视化代码
        code_recorder <- record_visualization(code_recorder, selected_vars, group_var)
        
        # 记录结果导出代码
        code_recorder <- record_exports(code_recorder, selected_vars)
        
        # 生成完整脚本并保存
        if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
          tryCatch({
            script_path <- file.path(values$output_folder, "NetworkTemperatureAnalysis_Script.R")
            generate_complete_script(code_recorder, script_path)
            cat("📝 已生成完整R脚本:", script_path, "\n")
            
            # 保存代码记录器到结果中
            temp_result$code_recorder <- code_recorder
            temp_result$generated_script_path <- script_path
          }, error = function(e) {
            cat("⚠️ 脚本生成失败:", e$message, "\n")
          })
        }
        
        values$temperature_result <- temp_result
        values$temperature_analysis_running <- FALSE
        
        # 调试信息
        cat("🔍 温度分析结果保存完成\n")
        cat("结果success状态:", temp_result$success, "\n")
        cat("temperatureAnalysisComplete应该为:", !is.null(values$temperature_result) && !is.null(values$temperature_result$success) && values$temperature_result$success, "\n")
        
        incProgress(1, detail = "完成!")
        
        showNotification("✅ 网络温度分析完成！请查看下方结果", type = "message", duration = 8)
        
        # 更新完整脚本（包含温度分析）
        if(!is.null(values$code_recorder) && !is.null(values$output_folder)) {
          tryCatch({
            script_path <- file.path(values$output_folder, "NetworkAnalysis_Complete_Script.R")
            generate_complete_script(values$code_recorder, script_path)
            cat("📝 已更新完整脚本（包含温度分析）:", script_path, "\n")
          }, error = function(e) {
            cat("⚠️ 温度分析脚本更新失败:", e$message, "\n")
          })
        }
        
      })
      
    }, error = function(e) {
      cat("❌ 网络温度分析失败:", e$message, "\n")
      values$temperature_error <- e$message
      values$temperature_analysis_running <- FALSE
      showNotification(paste("❌ 分析失败:", e$message), type = "error", duration = 10)
    })
  })
  
  # 温度分析摘要输出
  
  # 温度分析报告输出
  output$temperature_analysis_report <- renderUI({
    
    cat("🖥️ 渲染温度分析报告...\n")
    cat("  temperature_result是否为NULL:", is.null(values$temperature_result), "\n")
    
    if(is.null(values$temperature_result)) {
      cat("  显示默认占位内容\n")
      return(div(class = "text-center", style = "padding: 30px;",
        icon("chart-bar", class = "fa-2x text-muted"), br(), br(),
        h5("分析报告将在完成后显示", class = "text-muted")
      ))
    }
    
    cat("  temperature_result存在，success状态:", values$temperature_result$success, "\n")
    
    if(!values$temperature_result$success) {
      return(div(class = "alert alert-danger", "分析失败，请检查参数设置"))
    }
    
    result <- values$temperature_result
    
    # 生成详细的温度分析解释
    html_content <- paste0(
      "<div class='panel panel-info'>",
      "<div class='panel-heading'><h4>🌡️ 网络温度分析结果解释</h4></div>",
      "<div class='panel-body'>",
      
      "<h5>📊 <strong>分析概述</strong></h5>",
      "<p>网络温度 (T = 1/β) 反映了网络的稳定性和连接强度。温度越低，网络越稳定；温度越高，网络连接越不稳定。</p>",
      
      "<h5>🔬 <strong>8个模型的约束层级</strong></h5>",
      "<ul>",
      "<li><strong>M1-M2 (自由模型)</strong>: 每组独立估计所有参数，包括连接、阈值和温度</li>",
      "<li><strong>M3-M4 (网络相等)</strong>: 两组共享相同的连接结构，但阈值和温度独立</li>",
      "<li><strong>M5-M6 (网络+阈值相等)</strong>: 两组共享连接结构和阈值，但温度独立</li>",
      "<li><strong>M7-M8 (完全相等)</strong>: 两组共享所有参数，包括温度</li>",
      "</ul>",
      
      "<h5>📈 <strong>Dense vs Sparse策略</strong></h5>",
      "<p><strong>Dense模型</strong>: 保留所有可能的连接；<strong>Sparse模型</strong>: 通过统计检验剪枝，保留显著连接。</p>",
      
      if(!is.null(result$models) && length(result$models) > 0) {
        models <- result$models
        first_model <- models[[1]]
        params <- first_model@parameters
        beta_params <- params[params$matrix == "beta", ]
        groups <- unique(beta_params$group)
        
        # 使用与汇总信息一致的最佳模型选择逻辑
        selected_best_model <- if(!is.null(result$comparison) && !is.null(result$comparison$best_model)) {
          result$comparison$best_model
        } else {
          names(models)[1]  # 退尾方案
        }
        
        if(length(groups) > 1) {
          paste0(
            "<h5>🏆 <strong>最佳模型选择</strong></h5>",
            "<p>基于BIC准则，最佳模型为: <strong>", selected_best_model, "</strong></p>",
            
            "<h5>🌡️ <strong>组间温度对比</strong></h5>",
            "<p>以下温度比较图展示了 <strong>", groups[1], "</strong> 组和 <strong>", groups[2], "</strong> 组在不同约束条件下的温度变化：</p>",
            "<ul>",
            "<li>如果两组温度相近，说明网络稳定性相似</li>",
            "<li>如果某个约束层级下温度差异显著，说明该层级的参数存在组间差异</li>",
            "<li>M7-M8模型温度相同是因为强制约束两组使用相同温度参数</li>",
            "</ul>"
          )
        } else {
          "<h5>🏆 <strong>单组分析结果</strong></h5><p>当前为单组网络温度分析。</p>"
        }
      } else {
        ""
      },
      
      "<h5>💡 <strong>临床意义</strong></h5>",
      "<p>网络温度分析有助于理解不同群体的心理网络稳定性差异，为精准干预提供科学依据。温度较低的网络可能需要更强的干预才能产生变化。</p>",
      
      "</div></div>"
    )
    
    HTML(html_content)
  })
  
  # 温度比较图
  output$temp_comparison_plot <- renderPlot({
    
    cat("🖥️ 渲染温度比较图...\n")
    cat("  temperature_result是否为NULL:", is.null(values$temperature_result), "\n")
    
    if(is.null(values$temperature_result)) {
      cat("  显示默认图表\n")
      plot.new()
      text(0.5, 0.5, "点击\"开始网络温度分析\"运行分析", cex = 1.2, col = "gray")
      return()
    }
    
    cat("  temperature_result存在，success状态:", values$temperature_result$success, "\n")
    
    if(!values$temperature_result$success) {
      plot.new()
      text(0.5, 0.5, "分析失败", cex = 1.5, col = "red")
      return()
    }
    
    result <- values$temperature_result
    metrics <- result$metrics
    
    if(length(metrics) == 0) {
      plot.new()
      text(0.5, 0.5, "无可视化数据", cex = 1.5, col = "gray")
      return()
    }
    
    # 无论是否为多组分析，都显示统一的温度比较结果
    # 多组分析中分组信息已经包含在模型拟合中，结果为统一的综合模型
    temperatures <- sapply(metrics, function(x) x$temperature)
    temperatures <- temperatures[!is.na(temperatures)]
    
    if(length(temperatures) == 0) {
      plot.new()
      text(0.5, 0.5, "温度数据不可用", cex = 1.5, col = "gray")
      return()
    }
    
    # 创建温度比较条形图，缩短模型名称
    # 缩短模型名称以便显示
    short_names <- names(temperatures)
    short_names <- gsub("_Free_Dense", "_Free", short_names)
    short_names <- gsub("_Free_Sparse", "_Sparse", short_names)
    short_names <- gsub("_Equal_Dense", "_Dense", short_names)
    short_names <- gsub("_Equal_Sparse", "_Sparse", short_names)
    short_names <- gsub("Omega_", "ω_", short_names)
    short_names <- gsub("Tau_", "τ_", short_names)
    short_names <- gsub("Beta_", "β_", short_names)
    
    par(mar = c(5, 10, 4, 2))  # 增加左边距以适应模型名
    bars <- barplot(temperatures, 
                    names.arg = short_names,
                    horiz = TRUE,
                    col = rainbow(length(temperatures), alpha = 0.7),
                    main = "Network Temperature Comparison\n(8 Constraint Models)",
                    xlab = "Temperature (T = 1/β)",
                    las = 1,
                    cex.names = 0.7)  # 略微减小字体
    
    # 添加数值标签
    text(temperatures + max(temperatures) * 0.02, 
         bars, 
         round(temperatures, 3), 
         pos = 4, cex = 0.8)
    
    # 保存Fig4a到结果文件夹
    if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
      tryCatch({
        timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
        fig4a_path <- file.path(values$output_folder, "Fig4a_temperature_comparison.pdf")
        
        pdf(fig4a_path, width = 10, height = 6)  # 增加宽度以适应模型名
        
        # 缩短模型名称
        short_names <- names(temperatures)
        short_names <- gsub("_Free_Dense", "_Free", short_names)
        short_names <- gsub("_Free_Sparse", "_Sparse", short_names)
        short_names <- gsub("_Equal_Dense", "_Dense", short_names)
        short_names <- gsub("_Equal_Sparse", "_Sparse", short_names)
        short_names <- gsub("Omega_", "ω_", short_names)
        short_names <- gsub("Tau_", "τ_", short_names)
        short_names <- gsub("Beta_", "β_", short_names)
        
        par(mar = c(5, 10, 4, 2))
        bars <- barplot(temperatures, 
                        names.arg = short_names,
                        horiz = TRUE,
                        col = rainbow(length(temperatures), alpha = 0.7),
                        main = "Network Temperature Comparison\n(8 Constraint Models)",
                        xlab = "Temperature (T = 1/β)",
                        las = 1,
                        cex.names = 0.7)
        text(temperatures + max(temperatures) * 0.02, bars, round(temperatures, 3), pos = 4, cex = 0.8)
        dev.off()
        
        cat("  已保存Fig4a:", fig4a_path, "\n")
        
        # 同时导出CSV文件，包含所有模型的网络指标
        tryCatch({
          if(!is.null(result$metrics) && length(result$metrics) > 0) {
            # 准备数据框
            metrics_df <- data.frame()
            
            for(model_name in names(result$metrics)) {
              metric <- result$metrics[[model_name]]
              row_data <- data.frame(
                Model = model_name,
                Temperature = round(metric$temperature %||% NA, 4),
                Global_Strength = round(metric$global_strength %||% NA, 4),
                Network_Density = round(metric$density %||% NA, 4),
                Connectivity = round(metric$connectivity %||% NA, 4),
                Network_Entropy = round(metric$entropy %||% NA, 4),
                Clustering_Coefficient = round(metric$clustering %||% NA, 4),
                Number_of_Nodes = metric$n_nodes %||% NA,
                Model_AIC = round(metric$AIC %||% NA, 2),
                Model_BIC = round(metric$BIC %||% NA, 2),
                CFI = round(metric$CFI %||% NA, 4),
                RMSEA = round(metric$RMSEA %||% NA, 4),
                stringsAsFactors = FALSE
              )
              metrics_df <- rbind(metrics_df, row_data)
            }
            
            # 保存CSV文件
            csv_path <- file.path(values$output_folder, "Fig4_temperature_network_metrics.csv")
            write.csv(metrics_df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
            cat("  已导出CSV数据:", csv_path, "\n")
          }
        }, error = function(e) {
          cat("  CSV导出失败:", e$message, "\n")
        })
        
      }, error = function(e) {
        cat("  Fig4a保存失败:", e$message, "\n")
      })
    }
  })
  
  # 网络热图
  output$temp_network_heatmap <- renderPlot({
    
    cat("🖥️ 渲染网络热图...\n")
    cat("  temperature_result是否为NULL:", is.null(values$temperature_result), "\n")
    
    if(is.null(values$temperature_result)) {
      cat("  显示默认热图\n")
      plot.new()
      text(0.5, 0.5, "点击\"开始网络温度分析\"运行分析", cex = 1.2, col = "gray")
      return()
    }
    
    if(!values$temperature_result$success) {
      cat("  分析失败，显示错误信息\n")
      plot.new()
      text(0.5, 0.5, "分析失败", cex = 1.5, col = "red")
      return()
    }
    
    cat("  temperature_result存在且成功，继续渲染...\n")
    
    result <- values$temperature_result
    
    # 计算并显示症状协方差矩阵热图 (参考calculate_temperature.R 第166-174行)
    tryCatch({
      # 获取分析数据来计算协方差矩阵
      if(is.null(values$processed_data) || is.null(values$final_variables)) {
        plot.new()
        text(0.5, 0.5, "缺少数据或变量信息", cex = 1.5, col = "gray")
        return()
      }
      
      # 提取分析变量的数据
      analysis_data <- values$processed_data[, values$final_variables, drop = FALSE]
      
      # 移除缺失值
      analysis_data <- na.omit(analysis_data)
      
      if(nrow(analysis_data) == 0) {
        plot.new()
        text(0.5, 0.5, "数据为空", cex = 1.5, col = "gray")
        return()
      }
      
      # 计算协方差矩阵 (参考calculate_temperature.R)
      cov_matrix <- cov(analysis_data, use = "complete.obs")
      
      if(is.null(cov_matrix) || !is.matrix(cov_matrix)) {
        plot.new()
        text(0.5, 0.5, "协方差矩阵计算失败", cex = 1.5, col = "gray")
        return()
      }
      
      # 设置变量名称标签
      var_names <- values$final_variables
      rownames(cov_matrix) <- var_names
      colnames(cov_matrix) <- var_names
      
      cat("  热图 - 使用症状协方差矩阵 (", nrow(cov_matrix), "x", ncol(cov_matrix), ")\n")
      
      # 创建症状协方差热图 (参考calculate_temperature.R风格)
      if(requireNamespace("viridis", quietly = TRUE)) {
        # 使用基础R heatmap with viridis colors (参考calculate_temperature.R第170-174行)
        par(mar = c(8, 8, 4, 2))
        heatmap(cov_matrix, 
                symm = TRUE,
                col = viridis::cividis(100),
                Rowv = NA,
                main = "Symptom Covariance Matrix Heatmap",
                cexRow = 0.7,
                cexCol = 0.7,
                margins = c(10, 10))
      } else {
        # 备用热图
        par(mar = c(8, 8, 4, 2))
        heatmap(cov_matrix, 
                main = "Symptom Covariance Matrix Heatmap",
                col = heat.colors(20),
                scale = "none",
                symm = TRUE,
                cexRow = 0.7, 
                cexCol = 0.7,
                margins = c(10, 10))
      }
      
      # 保存Fig4b到结果文件夹
      if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
        tryCatch({
          timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
          fig4b_path <- file.path(values$output_folder, "Fig4b_temperature_heatmap.pdf")
          
          pdf(fig4b_path, width = 10, height = 8)
          if(requireNamespace("viridis", quietly = TRUE)) {
            par(mar = c(8, 8, 4, 2))
            heatmap(cov_matrix, 
                    symm = TRUE,
                    col = viridis::plasma(100),
                    Rowv = NA,
                    main = "Symptom Covariance Matrix Heatmap",
                    cexRow = 0.7,
                    cexCol = 0.7,
                    margins = c(10, 10))
          } else {
            par(mar = c(8, 8, 4, 2))
            heatmap(cov_matrix, 
                    main = "Symptom Covariance Matrix Heatmap",
                    col = heat.colors(20),
                    scale = "none",
                    symm = TRUE,
                    cexRow = 0.7, 
                    cexCol = 0.7,
                    margins = c(10, 10))
          }
          dev.off()
          
          cat("  已保存Fig4b:", fig4b_path, "\n")
        }, error = function(e) {
          cat("  Fig4b保存失败:", e$message, "\n")
        })
      }
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("热图生成失败:", e$message), cex = 1.2, col = "red")
    })
  })
  
  # 组别1网络图
  output$temp_group1_network <- renderPlot({
    cat("🖥️ 渲染组别1网络图...\n")
    
    # 检查基本条件
    if(is.null(values$temperature_result) || !values$temperature_result$success ||
       is.null(values$final_variables) || is.null(values$processed_data)) {
      plot.new()
      text(0.5, 0.5, "需要完成变量选择和温度分析", cex = 1.2, col = "gray")
      return()
    }
    
    tryCatch({
      # 检查是否为多组分析
      models <- values$temperature_result$models
      if(is.null(models) || length(models) == 0) {
        plot.new()
        text(0.5, 0.5, "无模型数据", cex = 1.2, col = "gray")
        return()
      }
      
      # 从模型参数中获取组别信息
      first_model <- models[[1]]
      params <- first_model@parameters
      beta_params <- params[params$matrix == "beta", ]
      groups <- unique(beta_params$group)
      is_multigroup <- length(groups) > 1
      
      if(!is_multigroup) {
        plot.new()
        text(0.5, 0.5, "仅在多组分析时显示", cex = 1.2, col = "gray")
        return()
      }
      
      group1_name <- groups[1]
      cat("  组别1网络图 - 组别名称:", group1_name, "\n")
      
      # 获取分组变量名称
      group_var <- NULL
      if(!is.null(values$temperature_result$parameters)) {
        group_var <- values$temperature_result$parameters$group_var
      }
      
      if(is.null(group_var)) {
        plot.new()
        text(0.5, 0.5, "未找到分组变量", cex = 1.2, col = "orange")
        return()
      }
      
      # 从原始数据中提取组别1的数据子集
      full_data <- values$processed_data
      group1_data <- full_data[full_data[[group_var]] == group1_name, ]
      
      if(nrow(group1_data) == 0) {
        plot.new()
        text(0.5, 0.5, paste("组别", group1_name, "无数据"), cex = 1.2, col = "orange")
        return()
      }
      
      # 提取分析变量
      analysis_vars <- values$final_variables
      group1_analysis_data <- group1_data[, analysis_vars, drop = FALSE]
      
      cat("  组别1数据维度:", nrow(group1_analysis_data), "x", ncol(group1_analysis_data), "\n")
      
      # 使用quickNet进行网络分析 - 继承主网络的配置
      network_args <- list(
        data = group1_analysis_data,
        threshold = input$threshold %||% 0.05,
        edge.labels = TRUE,
        layout = values$layout %||% "spring"  # 继承主网络的layout
      )
      
      # 继承节点分组和颜色配置
      if(!is.null(values$variable_groups) && !is.null(values$network_group_colors)) {
        cat("  继承分组配置: groups =", length(values$variable_groups), "个组别\n")
        cat("  继承颜色配置: colors =", length(values$network_group_colors), "种颜色\n")
        
        # 确保格式正确
        network_args$groups <- values$variable_groups
        network_args$colors <- values$network_group_colors
        
        # 调试信息
        cat("  groups类型:", class(network_args$groups), "\n")
        cat("  colors类型:", class(network_args$colors), "\n")
        
        # 转换为正确格式
        if(is.factor(network_args$groups)) {
          network_args$groups <- as.character(network_args$groups)
        }
        if(!is.character(network_args$colors)) {
          network_args$colors <- as.character(network_args$colors)
        }
      } else {
        cat("  未找到分组配色信息，使用默认设置\n")
      }
      
      # 运行quickNet分析
      cat("  运行quickNet分析...\n")
      cat("  最终参数:", paste(names(network_args), collapse = ", "), "\n")
      group1_network <- do.call(quickNet::quickNet, network_args)
      
      # 保存组别1网络结果（用于导出）
      values$group1_network_result <- group1_network
      
      # 使用get_network_plot保存FigS4a结果
      tryCatch({
        if(requireNamespace("quickNet", quietly = TRUE) && !is.null(values$output_folder)) {
          timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
          prefix <- paste0("FigS4a_group1_network_", timestamp)
          
          # 切换到输出文件夹
          old_wd <- getwd()
          setwd(values$output_folder)
          on.exit(setwd(old_wd))
          
          # 调用get_network_plot保存图片
          get_network_plot(group1_network, 
                         prefix = prefix, 
                         width = 6, height = 4.5)
          
          cat("  已保存组别1网络图:", file.path(values$output_folder, paste0(prefix, "_network_plot.pdf")), "\n")
        }
      }, error = function(e) {
        cat("  FigS4a保存失败:", e$message, "\n")
      })
      
      # 直接绘制网络图
      plot(group1_network, main = paste0(group1_name, " 网络图"))
      
    }, error = function(e) {
      cat("组别1网络图错误:", e$message, "\n")
      plot.new()
      text(0.5, 0.5, paste("组别1网络图失败:", e$message), cex = 1, col = "red")
    })
  })
  
  # 组别2网络图
  output$temp_group2_network <- renderPlot({
    cat("🖥️ 渲染组别2网络图...\n")
    
    # 检查基本条件
    if(is.null(values$temperature_result) || !values$temperature_result$success ||
       is.null(values$final_variables) || is.null(values$processed_data)) {
      plot.new()
      text(0.5, 0.5, "需要完成变量选择和温度分析", cex = 1.2, col = "gray")
      return()
    }
    
    tryCatch({
      # 检查是否为多组分析
      models <- values$temperature_result$models
      if(is.null(models) || length(models) == 0) {
        plot.new()
        text(0.5, 0.5, "无模型数据", cex = 1.2, col = "gray")
        return()
      }
      
      # 从模型参数中获取组别信息
      first_model <- models[[1]]
      params <- first_model@parameters
      beta_params <- params[params$matrix == "beta", ]
      groups <- unique(beta_params$group)
      is_multigroup <- length(groups) > 1
      
      if(!is_multigroup) {
        plot.new()
        text(0.5, 0.5, "仅在多组分析时显示", cex = 1.2, col = "gray")
        return()
      }
      
      # 获取第二个组别的名称
      group2_name <- if(length(groups) >= 2) groups[2] else "组别2"
      cat("  组别2网络图 - 组别名称:", group2_name, "\n")
      
      # 获取分组变量
      group_var <- values$temperature_result$parameters$group_var
      if(is.null(group_var)) {
        plot.new()
        text(0.5, 0.5, "未找到分组变量", cex = 1.2, col = "orange")
        return()
      }
      
      # 从主数据集中提取组别2的数据
      full_data <- values$processed_data
      group2_data <- full_data[full_data[[group_var]] == group2_name, ]
      
      if(nrow(group2_data) == 0) {
        plot.new()
        text(0.5, 0.5, paste("组别", group2_name, "无数据"), cex = 1.2, col = "orange")
        return()
      }
      
      # 提取分析变量
      analysis_vars <- values$final_variables
      group2_analysis_data <- group2_data[, analysis_vars, drop = FALSE]
      
      cat("  组别2数据维度:", nrow(group2_analysis_data), "x", ncol(group2_analysis_data), "\n")
      
      # 使用quickNet进行网络分析 - 继承主网络的配置
      network_args <- list(
        data = group2_analysis_data,
        threshold = input$threshold %||% 0.05,
        edge.labels = TRUE,
        layout = values$layout %||% "spring"  # 继承主网络的layout
      )
      
      # 继承节点分组和颜色配置
      if(!is.null(values$variable_groups) && !is.null(values$network_group_colors)) {
        cat("  继承分组配置: groups =", length(values$variable_groups), "个组别\n")
        cat("  继承颜色配置: colors =", length(values$network_group_colors), "种颜色\n")
        
        # 确保格式正确
        network_args$groups <- values$variable_groups
        network_args$colors <- values$network_group_colors
        
        # 调试信息
        cat("  groups类型:", class(network_args$groups), "\n")
        cat("  colors类型:", class(network_args$colors), "\n")
        
        # 转换为正确格式
        if(is.factor(network_args$groups)) {
          network_args$groups <- as.character(network_args$groups)
        }
        if(!is.character(network_args$colors)) {
          network_args$colors <- as.character(network_args$colors)
        }
      } else {
        cat("  未找到分组配色信息，使用默认设置\n")
      }
      
      # 运行quickNet分析
      cat("  运行quickNet分析...\n")
      cat("  最终参数:", paste(names(network_args), collapse = ", "), "\n")
      group2_network <- do.call(quickNet::quickNet, network_args)
      
      # 保存组别2网络结果（用于导出）
      values$group2_network_result <- group2_network
      
      # 使用get_network_plot保存FigS4b结果
      tryCatch({
        if(requireNamespace("quickNet", quietly = TRUE) && !is.null(values$output_folder)) {
          timestamp <- values$upload_timestamp %||% format(Sys.time(), "%Y%m%d_%H%M%S")
          prefix <- paste0("FigS4b_group2_network_", timestamp)
          
          # 切换到输出文件夹
          old_wd <- getwd()
          setwd(values$output_folder)
          on.exit(setwd(old_wd))
          
          # 调用get_network_plot保存图片
          get_network_plot(group2_network, 
                         prefix = prefix, 
                         width = 6, height = 4.5)
          
          cat("  已保存组别2网络图:", file.path(values$output_folder, paste0(prefix, "_network_plot.pdf")), "\n")
        }
      }, error = function(e) {
        cat("  FigS4b保存失败:", e$message, "\n")
      })
      
      # 直接绘制网络图（quickNet已经处理了可视化）
      plot(group2_network, main = paste0(group2_name, " 网络图"))
      
    }, error = function(e) {
      cat("组别2网络图错误:", e$message, "\n")
      plot.new()
      text(0.5, 0.5, paste("组别2网络图失败:", e$message), cex = 1, col = "red")
    })
  })
  
  # 组别网络图下载处理器
  output$download_group1_network <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("FigS4a_group1_network_", timestamp, ".pdf")
    },
    content = function(file) {
      tryCatch({
        # 优先使用get_network_plot生成的PDF文件
        expected_path <- file.path(values$output_folder, 
                                  paste0("FigS4a_group1_network_", values$upload_timestamp, "_network_plot.pdf"))
        if(!is.null(expected_path) && file.exists(expected_path)) {
          file.copy(expected_path, file)
        } else if(!is.null(values$group1_network_result)) {
          # 备用：重新生成PDF
          pdf(file, width = 8, height = 6)
          plot(values$group1_network_result, main = "组别1网络图")
          dev.off()
        } else {
          stop("组别1网络图数据不可用")
        }
      }, error = function(e) {
        showNotification(paste("下载失败:", e$message), type = "error")
        # 创建错误文档
        pdf(file, width = 8, height = 6)
        plot.new()
        text(0.5, 0.5, paste("下载失败:", e$message), cex = 1.5)
        dev.off()
      })
    }
  )
  
  output$download_group2_network <- downloadHandler(
    filename = function() {
      timestamp <- if(!is.null(values$upload_timestamp)) values$upload_timestamp else format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("FigS4b_group2_network_", timestamp, ".pdf")
    },
    content = function(file) {
      tryCatch({
        # 优先使用get_network_plot生成的PDF文件
        expected_path <- file.path(values$output_folder, 
                                  paste0("FigS4b_group2_network_", values$upload_timestamp, "_network_plot.pdf"))
        if(!is.null(expected_path) && file.exists(expected_path)) {
          file.copy(expected_path, file)
        } else if(!is.null(values$group2_network_result)) {
          # 备用：重新生成PDF
          pdf(file, width = 8, height = 6)
          plot(values$group2_network_result, main = "组别2网络图")
          dev.off()
        } else {
          stop("组别2网络图数据不可用")
        }
      }, error = function(e) {
        showNotification(paste("下载失败:", e$message), type = "error")
        # 创建错误文档
        pdf(file, width = 8, height = 6)
        plot.new()
        text(0.5, 0.5, paste("下载失败:", e$message), cex = 1.5)
        dev.off()
      })
    }
  )

  # 修正temp_analysis_summary以匹配实际数据结构
  output$temp_analysis_summary <- renderUI({
    req(values$temperature_result)
    
    if(!values$temperature_result$success) {
      return(div(class = "alert alert-danger", "分析失败"))
    }
    
    result <- values$temperature_result
    params <- result$parameters
    metrics <- result$metrics
    
    # 提取温度统计信息
    temperatures <- sapply(metrics, function(x) x$temperature)
    temperatures <- temperatures[!is.na(temperatures)]
    
    # 计算温度统计量
    temp_stats <- if(length(temperatures) > 0) {
      list(
        mean = mean(temperatures, na.rm = TRUE),
        min = min(temperatures, na.rm = TRUE),
        max = max(temperatures, na.rm = TRUE),
        range = max(temperatures, na.rm = TRUE) - min(temperatures, na.rm = TRUE)
      )
    } else { NULL }
    
    # 提取模型比较信息
    best_model_info <- ""
    best_model_name <- ""
    if(!is.null(result$comparison) && !is.null(result$comparison$best_model)) {
      best_model <- result$comparison$best_model
      best_model_name <- best_model  # 保存最佳模型名称
      if(!is.null(metrics) && !is.null(metrics[[best_model]])) {
        best_temp <- metrics[[best_model]]$temperature
        if(!is.null(best_temp) && !is.na(best_temp)) {
          best_model_info <- paste0(best_model, " (T = ", round(best_temp, 4), ")")
        } else {
          best_model_info <- best_model
        }
      } else {
        best_model_info <- best_model
      }
    }
    
    # 基本信息显示
    tags$div(
      tags$h4("📊 温度分析汇总", style = "color: #337ab7; margin-bottom: 15px;"),
      
      # 基本信息
      tags$div(class = "row",
        tags$div(class = "col-md-6",
          tags$p(tags$strong("✅ 分析状态："), tags$span("完成", style = "color: green;")),
          tags$p(tags$strong("📊 分析类型："), 
                 ifelse(is.null(params$group_var), "单组网络温度分析", "多组网络温度比较")),
          tags$p(tags$strong("🔧 二值化方法："), params$binary_transform),
          tags$p(tags$strong("📝 编码格式："), params$binary_encoding)
        ),
        tags$div(class = "col-md-6",
          tags$p(tags$strong("📋 模型数量："), length(result$models)),
          tags$p(tags$strong("🎯 分析变量："), length(values$final_variables)),
          tags$p(tags$strong("👥 样本数量："), nrow(values$processed_data)),
          if(!is.null(best_model_info) && best_model_info != "") {
            tags$p(tags$strong("🏆 最佳模型："), best_model_info)
          } else {
            tags$p(tags$strong("🏆 最佳模型："), "待确定")
          }
        )
      ),
      
      # 网络结构指标
      if(!is.null(metrics) && length(metrics) > 0) {
        # 从最佳模型提取网络指标
        best_metrics <- NULL
        if(!is.null(result$comparison) && !is.null(result$comparison$best_model) && !is.null(metrics)) {
          best_model_idx <- result$comparison$best_model
          if(!is.null(metrics[[best_model_idx]])) {
            best_metrics <- metrics[[best_model_idx]]
          }
        }
        
        # 如果没有找到最佳模型的指标，使用第一个可用的
        if(is.null(best_metrics) && !is.null(metrics) && length(metrics) > 0) {
          for(i in seq_along(metrics)) {
            if(!is.null(metrics[[i]])) {
              best_metrics <- metrics[[i]]
              break
            }
          }
        }
        
        if(!is.null(best_metrics)) {
          tags$div(
            tags$h5("🌐 网络结构指标 (最佳模型)", style = "color: #d9534f; margin-top: 20px;"),
            tags$div(class = "row",
              tags$div(class = "col-md-3",
                tags$p(tags$strong("网络温度："), 
                       tags$code(if(!is.null(best_metrics$temperature) && !is.na(best_metrics$temperature)) round(best_metrics$temperature, 4) else "N/A"))
              ),
              tags$div(class = "col-md-3",
                tags$p(tags$strong("全局强度："), 
                       tags$code(if(!is.null(best_metrics$global_strength) && !is.na(best_metrics$global_strength)) round(best_metrics$global_strength, 4) else "N/A"))
              ),
              tags$div(class = "col-md-3",
                tags$p(tags$strong("网络密度："), 
                       tags$code(if(!is.null(best_metrics$density) && !is.na(best_metrics$density)) round(best_metrics$density, 4) else "N/A"))
              ),
              tags$div(class = "col-md-3",
                tags$p(tags$strong("连接度："), 
                       tags$code(if(!is.null(best_metrics$connectivity) && !is.na(best_metrics$connectivity)) round(best_metrics$connectivity, 4) else "N/A"))
              )
            ),
            tags$div(class = "row", style = "margin-top: 10px;",
              tags$div(class = "col-md-3",
                tags$p(tags$strong("网络熵："), 
                       tags$code(if(!is.null(best_metrics$entropy) && !is.na(best_metrics$entropy)) round(best_metrics$entropy, 4) else "N/A"))
              ),
              tags$div(class = "col-md-3",
                tags$p(tags$strong("聚类系数："), 
                       tags$code(if(!is.null(best_metrics$clustering) && !is.na(best_metrics$clustering)) round(best_metrics$clustering, 4) else "N/A"))
              ),
              tags$div(class = "col-md-3",
                tags$p(tags$strong("节点数："), 
                       tags$code(if(!is.null(best_metrics$n_nodes) && !is.na(best_metrics$n_nodes)) best_metrics$n_nodes else "N/A"))
              ),
              tags$div(class = "col-md-3",
                tags$p(tags$strong("模型BIC："), 
                       tags$code(if(!is.null(best_metrics$BIC) && !is.na(best_metrics$BIC)) round(best_metrics$BIC, 2) else "N/A"))
              )
            )
          )
        }
      },
      
      # 模型拟合信息
      tags$div(
        tags$h5("📈 模型拟合信息", style = "color: #5cb85c; margin-top: 20px;"),
        tags$p("• M1-M2: 自由参数模型（所有参数独立估计）"),
        tags$p("• M3-M4: 网络结构等同模型（ω参数相等）"),  
        tags$p("• M5-M6: 网络结构+阈值等同模型（ω+τ参数相等）"),
        tags$p("• M7-M8: 全参数等同模型（ω+τ+β参数相等）"),
        tags$p(tags$em("注：偶数模型为稀疏版本，奇数模型为密集版本"))
      )
    )
  })
  
  # =============================================================================
  # 样本量计算模块
  # =============================================================================
  
  # 网络特征信息输出
  output$network_features_info <- renderText({
    if(!is.null(values$network_result) && !is.null(values$analysis_data)) {
      tryCatch({
        features <- extract_network_features(values$network_result)
        
        # 安全地获取数据信息
        n_subjects <- if(!is.null(values$analysis_data)) nrow(values$analysis_data) else "未知"
        
        complete_rate <- if(!is.null(values$analysis_data)) {
          round(sum(complete.cases(values$analysis_data))/nrow(values$analysis_data)*100, 1)
        } else {
          "未知"
        }
        
        paste0(
          "节点数量: ", features$nodes, "\n",
          "网络密度: ", round(features$density, 3), "\n", 
          "当前样本量: ", n_subjects, "\n",
          "数据完整性: ", complete_rate, "%\n",
          "网络类型: ", if(!is.null(values$network_result)) class(values$network_result)[1] else "未知"
        )
      }, error = function(e) {
        paste0(
          "网络特征提取遇到问题\n",
          "错误信息: ", e$message, "\n",
          "请检查网络分析结果是否正常"
        )
      })
    } else {
      "请先完成网络分析\n\n说明：样本量计算需要基于\n已完成的网络分析结果"
    }
  })
  
  # 调试信息输出
  output$debug_network_info <- renderText({
    if(!is.null(values$network_result)) {
      debug_network_structure(values$network_result)
    } else {
      "网络分析结果不可用"
    }
  })
  
  # 样本量计算按钮观察器
  observeEvent(input$run_sample_size, {
    req(values$network_result, values$analysis_data)
    
    # 检查powerly包是否可用
    if(!requireNamespace("powerly", quietly = TRUE)) {
      showNotification("错误：需要安装powerly包\n请运行: install.packages('powerly')", 
                      type = "error", duration = 10)
      return()
    }
    
    # 显示计算开始信息
    showNotification("⏳ 开始样本量计算，这可能需要几分钟...", 
                    type = "message", duration = 5)
    
    # 执行样本量计算
    tryCatch({
      # 从网络结果提取特征
      features <- extract_network_features(values$network_result)
      
      # 调用简化的样本量计算函数
      result <- calculate_sample_size(
        network_result = values$network_result,
        nodes = features$nodes,
        density = features$density,
        range_lower = input$sample_range_lower %||% 300,
        range_upper = input$sample_range_upper %||% 2000,
        cores = input$powerly_cores %||% 2
      )
      
      if(!is.null(result)) {
        values$sample_size_result <- result
        values$sample_size_complete <- TRUE
        
        # 立即添加样本量计算代码到完整脚本
        if(!is.null(values$code_recorder) && !is.null(values$output_folder)) {
          tryCatch({
            # 加载样本量代码生成器
            source('sample_size_code_gen.R')
            
            # 生成样本量计算代码
            sample_size_code <- generate_sample_size_code(
              features$nodes, 
              features$density,
              input$sample_range_lower %||% 300,
              input$sample_range_upper %||% 2000,
              input$powerly_cores %||% 2
            )
            
            # 添加到代码记录器
            old_length <- length(values$code_recorder)
            values$code_recorder <- c(values$code_recorder, sample_size_code)
            new_length <- length(values$code_recorder)
            
            cat("📋 代码记录器更新: ", old_length, " -> ", new_length, " 行\n")
            cat("📊 样本量代码行数:", length(sample_size_code), "\n")
            
            # 更新完整脚本
            script_path <- file.path(values$output_folder, "NetworkAnalysis_Complete_Script.R")
            generate_complete_script(values$code_recorder, script_path)
            cat("📝 已更新完整脚本（包含样本量计算）:", script_path, "\n")
          }, error = function(e) {
            cat("⚠️ 样本量计算脚本更新失败:", e$message, "\n")
          })
        }
        
        # 导出PDF文件到当前的结果文件夹
        tryCatch({
          # 使用当前网络分析的输出文件夹
          output_dir <- if(!is.null(values$output_folder) && dir.exists(values$output_folder)) {
            values$output_folder
          } else {
            getwd()  # 如果没有结果文件夹，使用当前目录
          }
          
          exported_files <- export_powerly_plots(
            powerly_result = result, 
            output_dir = output_dir
          )
          
          if(length(exported_files) > 0) {
            showNotification(paste0("✅ 样本量计算完成！在 ", basename(output_dir), " 中导出了", length(exported_files), "个PDF文件"), 
                            type = "message", duration = 8)
          } else {
            showNotification("✅ 样本量计算完成！（PDF导出失败）", type = "warning", duration = 5)
          }
        }, error = function(e) {
          cat("PDF导出失败:", e$message, "\n")
          showNotification(paste0("✅ 样本量计算完成！（PDF导出错误: ", e$message, "）"), type = "warning", duration = 8)
        })
      } else {
        showNotification("❌ 样本量计算失败，请检查参数设置", 
                        type = "error", duration = 5)
      }
      
    }, error = function(e) {
      cat("样本量计算错误:", e$message, "\n")
      showNotification(paste("计算错误:", e$message), 
                      type = "error", duration = 8)
    })
  })
  
  # 样本量分析完成状态
  output$sampleSizeComplete <- reactive({
    !is.null(values$sample_size_complete) && values$sample_size_complete
  })
  outputOptions(output, "sampleSizeComplete", suspendWhenHidden = FALSE)
  
  # 样本量分析结果摘要
  output$sample_size_summary <- renderText({
    if(!is.null(values$sample_size_result)) {
      result <- values$sample_size_result
      
      # 安全地获取推荐样本量（支持confidence interval格式）
      recommendation <- tryCatch({
        if(!is.null(result$recommendation)) {
          if(is.numeric(result$recommendation) && length(result$recommendation) > 1) {
            # 置信区间格式
            paste0("2.5% = ", round(result$recommendation[1]), " | ", 
                   "50% = ", round(result$recommendation[2]), " | ", 
                   "97.5% = ", round(result$recommendation[3]))
          } else {
            as.character(result$recommendation)
          }
        } else {
          "无法确定"
        }
      }, error = function(e) "无法确定")
      
      # 安全地获取网络信息
      network_info <- if(!is.null(result$network_info)) result$network_info else list()
      nodes <- if(!is.null(network_info$nodes)) network_info$nodes else "未知"
      density <- if(!is.null(network_info$density)) round(network_info$density, 3) else "未知"
      analysis_date <- if(!is.null(network_info$analysis_date)) {
        format(network_info$analysis_date, "%Y-%m-%d %H:%M")
      } else {
        "未知"
      }
      
      paste0(
        "🎯 推荐样本量: ", recommendation, "\n",
        "📊 网络节点数: ", nodes, "\n",
        "🔗 网络密度: ", density, "\n",
        "📅 分析时间: ", analysis_date
      )
    } else {
      "样本量分析未完成"
    }
  })
  
  # 样本量推荐简要版
  output$sample_size_recommendation <- renderText({
    if(!is.null(values$sample_size_result)) {
      result <- values$sample_size_result
      
      # 安全地获取推荐样本量（单个数值）
      recommendation <- tryCatch({
        if(!is.null(result$recommendation)) {
          rec <- result$recommendation
          if(is.numeric(rec) && length(rec) > 1) {
            rec[length(rec) %/% 2 + 1]  # 取中间值
          } else {
            as.numeric(rec)[1]
          }
        } else {
          NULL
        }
      }, error = function(e) NULL)
      
      if(!is.null(recommendation) && is.numeric(recommendation) && length(recommendation) == 1) {
        interpretation <- interpret_sample_size(recommendation)
        paste0(
          "推荐样本量: ", round(recommendation), "\n",
          "质量评价: ", interpretation
        )
      } else {
        "无法确定推荐样本量"
      }
    } else {
      "请先运行样本量分析"
    }
  })
  
  # 研究设计建议
  output$research_design_suggestions <- renderUI({
    if(!is.null(values$sample_size_result)) {
      result <- values$sample_size_result
      
      # 安全地获取推荐样本量（单个数值）
      recommendation <- tryCatch({
        if(!is.null(result$recommendation)) {
          rec <- result$recommendation
          if(is.numeric(rec) && length(rec) > 1) {
            rec[length(rec) %/% 2 + 1]  # 取中间值
          } else {
            as.numeric(rec)[1]
          }
        } else {
          NULL
        }
      }, error = function(e) NULL)
      
      if(!is.null(recommendation) && is.numeric(recommendation) && length(recommendation) == 1 && !is.na(recommendation)) {
        # 计算建议范围
        rec_rounded <- round(recommendation)
        conservative_n <- ceiling(rec_rounded * 1.2)
        minimum_n <- ceiling(rec_rounded * 0.8)
        
        tagList(
          tags$p(tags$strong("样本量建议：")),
          tags$ul(
            tags$li(paste0("最小样本量: ", minimum_n, " (80%功效)")),
            tags$li(paste0("推荐样本量: ", rec_rounded, " (目标功效)")),
            tags$li(paste0("保守样本量: ", conservative_n, " (20%缓冲)"))
          ),
          tags$p(tags$strong("研究设计考虑：")),
          tags$ul(
            tags$li("建议预留20-30%的样本量以应对数据缺失"),
            tags$li("对于多时点研究，需要考虑随访流失率"),
            tags$li("如需进行亚组分析，每个亚组至少需要推荐样本量的50%")
          )
        )
      } else {
        tags$p("样本量推荐不可用")
      }
    } else {
      tags$p("请先完成样本量分析")
    }
  })
  
  # Step 1 可视化
  output$powerly_step1_plot <- renderPlot({
    req(values$sample_size_result)
    if(!is.null(values$sample_size_result) && 
       requireNamespace("powerly", quietly = TRUE)) {
      tryCatch({
        # 使用原始结果进行绘图
        original_result <- if(!is.null(values$sample_size_result$original_result)) {
          values$sample_size_result$original_result
        } else {
          values$sample_size_result
        }
        plot(original_result, step = 1)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Step 1 图表失败:", e$message), cex = 1, col = "red")
      })
    }
  })
  
  # Step 2 可视化
  output$powerly_step2_plot <- renderPlot({
    req(values$sample_size_result)
    if(!is.null(values$sample_size_result) && 
       requireNamespace("powerly", quietly = TRUE)) {
      tryCatch({
        # 使用原始结果进行绘图
        original_result <- if(!is.null(values$sample_size_result$original_result)) {
          values$sample_size_result$original_result
        } else {
          values$sample_size_result
        }
        plot(original_result, step = 2)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Step 2 图表失败:", e$message), cex = 1, col = "red")
      })
    }
  })
  
  # Step 3 可视化
  output$powerly_step3_plot <- renderPlot({
    req(values$sample_size_result)
    if(!is.null(values$sample_size_result) && 
       requireNamespace("powerly", quietly = TRUE)) {
      tryCatch({
        # 使用原始结果进行绘图
        original_result <- if(!is.null(values$sample_size_result$original_result)) {
          values$sample_size_result$original_result
        } else {
          values$sample_size_result
        }
        plot(original_result, step = 3)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Step 3 图表失败:", e$message), cex = 1, col = "red")
      })
    }
  })
  
  # 详细报告
  output$detailed_sample_report <- renderText({
    if(!is.null(values$sample_size_result)) {
      generate_sample_size_report(values$sample_size_result)
    } else {
      "详细报告不可用"
    }
  })
  
  # 样本量报告下载
  output$download_sample_size <- downloadHandler(
    filename = function() {
      paste0("sample_size_analysis_", Sys.Date(), ".html")
    },
    content = function(file) {
      if(!is.null(values$sample_size_result)) {
        # 生成HTML报告
        report_content <- generate_sample_size_report(values$sample_size_result)
        
        # 转换为HTML格式
        html_content <- paste0(
          "<!DOCTYPE html><html><head>",
          "<meta charset='UTF-8'>",
          "<title>样本量分析报告</title>",
          "<style>",
          "body { font-family: Arial, sans-serif; margin: 40px; }",
          "h1 { color: #2c3e50; }",
          "h2 { color: #3498db; border-bottom: 2px solid #3498db; }",
          "pre { background-color: #f8f9fa; padding: 10px; border-radius: 5px; }",
          "</style>",
          "</head><body>",
          "<h1>样本量分析报告</h1>",
          gsub("\n", "<br>", report_content),
          "</body></html>"
        )
        
        writeLines(html_content, file, useBytes = TRUE)
      }
    }
  )
  
  # 首页"开始分析"按钮观察器
  observeEvent(input$start_analysis, {
    updateTabItems(session, "sidebar", "upload")
    showNotification("欢迎开始您的网络分析！请先上传数据文件。", 
                    type = "message", duration = 3)
  })
}

# =============================================================================
# 运行应用
# =============================================================================

shinyApp(ui = ui, server = server)
