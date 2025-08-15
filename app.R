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
  safe_network_analysis <<- function(data, threshold = 0.05, edge_labels = TRUE, colors = NULL, ...) { 
    if(requireNamespace("quickNet", quietly = TRUE)) {
      return(quickNet::quickNet(data, threshold = threshold, edge.labels = edge_labels, ...))
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
      menuItem("数据上传", tabName = "upload", icon = icon("upload")),
      menuItem("变量构造", tabName = "construct", icon = icon("calculator")),
      menuItem("变量选择", tabName = "variables", icon = icon("check-square")),
      menuItem("网络分析", tabName = "analysis", icon = icon("project-diagram")),
      menuItem("贝叶斯网络", tabName = "bayesian", icon = icon("brain")),
      menuItem("稳定性分析", tabName = "stability", icon = icon("chart-line")),
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
              
              checkboxInput("show_edge_labels", "显示边权重", TRUE),
              helpText("在网络图上显示具体的相关系数"),
              
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
            title = "网络图", status = "success", solidHeader = TRUE, width = 9,
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
                tabPanel("显著性检验", DT::dataTableOutput("group_compare_table")),
                tabPanel("组间统计", DT::dataTableOutput("group_stats_table"))
              )
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
                         "PC Algorithm" = "pc"
                       ), selected = "hc"),
            
            selectInput("score_function", "评分函数", 
                       choices = list(
                         "BGe (贝叶斯高斯)" = "bge",
                         "BIC (贝叶斯信息准则)" = "bic"
                       ), selected = "bge"),
            
            numericInput("bootstrap_rounds", "Bootstrap轮数",
                        value = 1000, min = 500, max = 5000, step = 500),
            
            numericInput("strength_threshold", "边强度阈值", 
                        value = 0.85, min = 0.5, max = 1.0, step = 0.05),
            
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
                tabPanel("网络结构", plotOutput("bayesian_network_plot", height = "600px")),
                tabPanel("稳定性分析", plotOutput("bayesian_stability_plot", height = "600px")),
                tabPanel("边强度表", DT::dataTableOutput("bayesian_edges_table")),
                tabPanel("分析报告", uiOutput("bayesian_report"))
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
      
      # 结果下载页面
      tabItem(
        tabName = "download",
        fluidRow(
          box(
            title = "下载选项", status = "primary", solidHeader = TRUE, width = 12,
            
            conditionalPanel(
              condition = "output.analysisComplete",
              h4("可下载文件："),
              br(),
              downloadButton("download_network_plot", "下载网络图 (PNG)", class = "btn-primary"),
              br(), br(),
              downloadButton("download_centrality_plot", "下载中心性图 (PNG)", class = "btn-primary"),
              br(), br(),
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
    bayesian_result = NULL
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
        showNotification("完整案例太少，无法进行网络分析", type = "error")
        return()
      }
      
      if(complete_cases / total_cases < 0.5) {
        showNotification(paste("缺失数据较多，完整案例只有", complete_cases, "/", total_cases), type = "warning")
      }
      
      incProgress(0.3, detail = "构建网络...")
      
      # 使用安全的网络分析函数
      tryCatch({
        colors <- VIZ_CONFIG$colors$primary[1:min(n_vars, length(VIZ_CONFIG$colors$primary))]
        
        values$network_result <- safe_network_analysis(
          data = values$analysis_data,
          threshold = input$threshold %||% 0.05,
          edge_labels = input$show_edge_labels %||% TRUE,
          colors = colors
        )
        
        incProgress(0.7, detail = "计算中心性指标...")
        
        # 中心性分析
        tryCatch({
          if(requireNamespace("quickNet", quietly = TRUE)) {
            values$centrality_result <- Centrality(values$network_result)
          } else {
            showNotification("quickNet包不可用，跳过中心性计算", type = "warning")
            values$centrality_result <- NULL
          }
        }, error = function(e) {
          showNotification(paste("中心性计算失败:", e$message), type = "warning")
          values$centrality_result <- NULL
        })
        
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
    
    # 确保plot正确显示
    tryCatch({
      plot(values$network_result)
    }, error = function(e) {
      # 如果plot函数失败，尝试直接输出对象
      values$network_result
    })
  })
  
  # 中心性图输出
  output$centrality_plot <- renderPlot({
    req(values$centrality_result)
    
    get_centrality_plot(values$centrality_result)
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
            edge_boot <- bootnet(values$analysis_data, nBoots = input$stability_bootstrap, 
                               default = "EBICglasso", type = "nonparametric")
            values$edge_stability <- edge_boot
          }
          
          incProgress(0.8, detail = "计算中心性稳定性...")
          
          if(input$run_centrality_stability) {
            cent_boot <- bootnet(values$analysis_data, nBoots = input$stability_bootstrap,
                               default = "EBICglasso", type = "case", 
                               statistics = c("strength", "closeness", "betweenness"))
            values$centrality_stability <- cent_boot
          }
          
          values$stability_result <- list(
            edge_stability = if(input$run_edge_stability) values$edge_stability else NULL,
            centrality_stability = if(input$run_centrality_stability) values$centrality_stability else NULL,
            bootstrap_n = input$stability_bootstrap
          )
          
          incProgress(1, detail = "稳定性分析完成!")
          showNotification("稳定性分析完成！", type = "message")
          
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
    
    result <- paste0("稳定性分析摘要\n",
                    "================\n\n",
                    "Bootstrap次数: ", values$stability_result$bootstrap_n, "\n\n")
    
    if(!is.null(values$stability_result$edge_stability)) {
      result <- paste0(result, "✓ 边稳定性分析已完成\n")
    }
    
    if(!is.null(values$stability_result$centrality_stability)) {
      result <- paste0(result, "✓ 中心性稳定性分析已完成\n")
    }
    
    result <- paste0(result, "\n建议:\n",
                    "- 边的置信区间较窄表示边稳定\n",
                    "- 中心性指标的稳定性应大于0.25\n",
                    "- CS系数应大于0.5表示稳定")
    
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
      
      # 获取原始数据中的分类变量（用于分类分组）
      # 查找可能的分类变量：字符型、因子型，或值较少的数值型变量
      categorical_candidates <- c()
      for(col_name in names(values$processed_data)) {
        col_data <- values$processed_data[[col_name]]
        
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
      } else if(input$group_variable %in% names(values$processed_data)) {
        group_var <- values$processed_data[[input$group_variable]]
        data_source <- "processed"
      } else {
        showNotification("未找到选择的分组变量", type = "error")
        return()
      }
      
      if(all(is.na(group_var))) {
        showNotification("选择的分组变量全为缺失值", type = "error")
        return()
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
          threshold <- median(group_var, na.rm = TRUE)
          group_indices1 <- group_var < threshold & !is.na(group_var)
          group_indices2 <- group_var >= threshold & !is.na(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_低分组")
          group2_name <- paste0(input$group_variable, "_高分组")
        } else if(input$group_method == "mean") {
          threshold <- mean(group_var, na.rm = TRUE)
          group_indices1 <- group_var < threshold & !is.na(group_var)
          group_indices2 <- group_var >= threshold & !is.na(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_低分组")
          group2_name <- paste0(input$group_variable, "_高分组")
        } else if(input$group_method == "tertile") {
          q1 <- quantile(group_var, 0.33, na.rm = TRUE)
          q3 <- quantile(group_var, 0.67, na.rm = TRUE)
          threshold <- paste0("Q1=", round(q1, 2), ", Q3=", round(q3, 2))  # 记录分位数信息
          group_indices1 <- group_var <= q1 & !is.na(group_var)
          group_indices2 <- group_var >= q3 & !is.na(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_低三分位")
          group2_name <- paste0(input$group_variable, "_高三分位")
        } else if(input$group_method == "extreme_27") {
          # 前后27%分组
          q27 <- quantile(group_var, 0.27, na.rm = TRUE)
          q73 <- quantile(group_var, 0.73, na.rm = TRUE)
          threshold <- paste0("Q27=", round(q27, 2), ", Q73=", round(q73, 2))  # 记录分位数信息
          group_indices1 <- group_var <= q27 & !is.na(group_var)
          group_indices2 <- group_var >= q73 & !is.na(group_var)
          group1_data <- values$analysis_data[group_indices1, ]
          group2_data <- values$analysis_data[group_indices2, ]
          group1_name <- paste0(input$group_variable, "_低27%")
          group2_name <- paste0(input$group_variable, "_高27%")
        } else if(input$group_method == "categorical") {
          # 分类变量分组
          unique_values <- unique(group_var[!is.na(group_var)])
          
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
          
          if(length(sorted_values) >= 2) {
            value1 <- names(sorted_values)[1]
            value2 <- names(sorted_values)[2]
            threshold <- paste0(value1, " vs ", value2)  # 记录分类信息
            
            group_indices1 <- group_var == value1 & !is.na(group_var)
            group_indices2 <- group_var == value2 & !is.na(group_var)
            group1_data <- values$analysis_data[group_indices1, ]
            group2_data <- values$analysis_data[group_indices2, ]
            group1_name <- paste0(input$group_variable, "_", value1)
            group2_name <- paste0(input$group_variable, "_", value2)
          }
        } else if(input$group_method == "custom") {
          threshold <- input$custom_threshold
          group_indices1 <- group_var < threshold & !is.na(group_var)
          group_indices2 <- group_var >= threshold & !is.na(group_var)
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
        
        # 执行网络比较
        incProgress(0.5, detail = "执行置换检验...")
        
        if(requireNamespace("quickNet", quietly = TRUE)) {
          compare_result <- NetCompare(
            group1_data, group2_data,
            it = input$permutation_n,
            p.adjust.methods = input$p_adjust_method
          )
          
          incProgress(0.8, detail = "生成比较图...")
          
          # 保存结果
          values$group_compare_result <- list(
            compare_result = compare_result,
            group1_data = group1_data,
            group2_data = group2_data,
            group1_name = group1_name,
            group2_name = group2_name,
            group_variable = input$group_variable,
            threshold = threshold,
            method = input$group_method,
            permutation_n = input$permutation_n,
            p_adjust_method = input$p_adjust_method
          )
          
          incProgress(1, detail = "组间比较完成!")
          
          showNotification(paste0("组间比较分析完成！组1: ", nrow(group1_data), " 案例，组2: ", nrow(group2_data), " 案例"), type = "message")
          
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
    
    tryCatch({
      if(requireNamespace("quickNet", quietly = TRUE)) {
        get_compare_plot(values$group_compare_result$compare_result, values$network_result)
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
      
      # 调试：检查result的结构
      cat("NetCompare结果结构:\n")
      cat("字段名:", names(result), "\n")
      if(!is.null(result)) {
        cat("结果类型:", class(result), "\n")
      }
      
      # 尝试多种可能的字段名来提取显著性检验结果
      sig_results <- NULL
      
      # 方法1：检查标准字段
      if(!is.null(result$p.values) && !is.null(result$difference)) {
        sig_results <- data.frame(
          边 = names(result$p.values),
          原始p值 = round(result$p.values, 4),
          校正p值 = round(result$p.adjust %||% result$p.values, 4),
          显著性 = ifelse((result$p.adjust %||% result$p.values) < 0.05, "显著", "不显著"),
          差异值 = round(result$difference, 4),
          stringsAsFactors = FALSE
        )
      }
      # 方法2：检查pval字段
      else if(!is.null(result$pval) && !is.null(result$difference)) {
        sig_results <- data.frame(
          边 = names(result$pval),
          原始p值 = round(result$pval, 4),
          校正p值 = round(result$p.adjust %||% result$pval, 4),
          显著性 = ifelse((result$p.adjust %||% result$pval) < 0.05, "显著", "不显著"),
          差异值 = round(result$difference, 4),
          stringsAsFactors = FALSE
        )
      }
      # 方法3：检查p字段
      else if(!is.null(result$p) && !is.null(result$difference)) {
        sig_results <- data.frame(
          边 = names(result$p),
          原始p值 = round(result$p, 4),
          校正p值 = round(result$p.adjust %||% result$p, 4),
          显著性 = ifelse((result$p.adjust %||% result$p) < 0.05, "显著", "不显著"),
          差异值 = round(result$difference, 4),
          stringsAsFactors = FALSE
        )
      }
      # 方法4：如果result本身是数据框
      else if(is.data.frame(result)) {
        # 尝试从数据框中提取
        p_col <- NULL
        diff_col <- NULL
        
        if("p.value" %in% names(result)) p_col <- "p.value"
        else if("pval" %in% names(result)) p_col <- "pval"
        else if("p" %in% names(result)) p_col <- "p"
        
        if("difference" %in% names(result)) diff_col <- "difference"
        else if("diff" %in% names(result)) diff_col <- "diff"
        
        if(!is.null(p_col) && !is.null(diff_col)) {
          sig_results <- data.frame(
            边 = rownames(result) %||% paste0("边", 1:nrow(result)),
            原始p值 = round(result[[p_col]], 4),
            校正p值 = round(result[["p.adjust"]] %||% result[[p_col]], 4),
            显著性 = ifelse((result[["p.adjust"]] %||% result[[p_col]]) < 0.05, "显著", "不显著"),
            差异值 = round(result[[diff_col]], 4),
            stringsAsFactors = FALSE
          )
        }
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
      
      # 添加显著性统计
      if(!is.null(result$compare_result$p.adjust)) {
        sig_count <- sum(result$compare_result$p.adjust < 0.05, na.rm = TRUE)
        total_count <- length(result$compare_result$p.adjust)
        
        # 添加结果统计到表格
        result_stats <- data.frame(
          统计项目 = c("检验的边数", "显著差异边数", "显著差异比例"),
          统计值 = c(
            as.character(total_count),
            as.character(sig_count),
            paste0(round(sig_count/total_count*100, 1), "%")
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
  
  # 下载处理
  output$download_network_plot <- downloadHandler(
    filename = function() {
      paste0("network_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600, res = 100)
      print(values$network_result)
      dev.off()
    }
  )
  
  output$download_centrality_plot <- downloadHandler(
    filename = function() {
      paste0("centrality_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600, res = 100)
      print(get_centrality_plot(values$centrality_result))
      dev.off()
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
            preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, ": ", paste(scale_info$new_variables, collapse = ", "), " (手动计算)"))
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
              preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, ": ", total_vars_names[1], " (", var_type, ")"))
              total_vars <- total_vars + 1
            } else {
              preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, ": ", scale_info$new_variables[1], " (汇总)"))
              total_vars <- total_vars + 1
            }
          }
          
        } else if(selected_level == "subscale") {
          # 子量表层：显示非总分变量（排除所有总分模式）
          subscale_vars <- scale_info$new_variables[!sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
          if(length(subscale_vars) > 0) {
            preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, " (", length(subscale_vars), " 个维度):"))
            for(var_name in subscale_vars) {
              preview_lines <- c(preview_lines, paste0("   • ", var_name))
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
              preview_lines <- c(preview_lines, paste0(scale_icon, " ", scale_name, ": ", length(items), " 个条目"))
              preview_lines <- c(preview_lines, paste0("   ", paste(head(items, 3), collapse = ", "), 
                                                      if(length(items) > 3) "..." else ""))
              total_vars <- total_vars + length(items)
            }
          }
        }
        
        preview_lines <- c(preview_lines, "")
      }
      
      header <- paste0("🎯 网络分析将包含 ", total_vars, " 个变量\n",
                      "==========================================\n\n")
      
      return(paste0(header, paste(preview_lines, collapse = "\n")))
      
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
      
      # 定义总分变量模式（保持一致性）
      total_patterns <- c("_Total$", "_mean$", "_sum$", "_weighted$", "_max$", "_cfa$", "_pca$", "_factor$", "_std$")
      
      for(scale_name in names(scales_info)) {
        scale_info <- scales_info[[scale_name]]
        level_input_id <- paste0("advanced_level_", scale_name)
        selected_level <- input[[level_input_id]]
        
        if(is.null(selected_level)) selected_level <- "summary"
        
        is_manual <- !is.null(scale_info$is_manual) && scale_info$is_manual
        
        if(selected_level == "summary") {
          # 汇总层：选择合适的变量
          if(is_manual) {
            # 手动规则：使用生成的变量
            final_variables <- c(final_variables, scale_info$new_variables)
          } else {
            # 预配置量表：优先选择总分变量
            total_vars_names <- scale_info$new_variables[sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
            if(length(total_vars_names) > 0) {
              final_variables <- c(final_variables, total_vars_names[1])
            } else {
              final_variables <- c(final_variables, scale_info$new_variables[1])
            }
          }
          
        } else if(selected_level == "subscale") {
          # 子量表层：选择非总分变量（排除所有总分模式）
          subscale_vars <- scale_info$new_variables[!sapply(scale_info$new_variables, function(x) any(sapply(total_patterns, function(p) grepl(p, x))))]
          final_variables <- c(final_variables, subscale_vars)
          
        } else if(selected_level == "items") {
          # 条目层：选择原始条目变量
          if(!is.null(values$calculated_scales$available_scales) && 
             scale_name %in% names(values$calculated_scales$available_scales)) {
            available_scale_info <- values$calculated_scales$available_scales[[scale_name]]
            if(!is.null(available_scale_info$existing_items)) {
              final_variables <- c(final_variables, available_scale_info$existing_items)
            }
          }
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
      values$variables_confirmed <- TRUE
      
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
  
  # 重新选择变量
  observeEvent(input$reselect_variables, {
    values$variables_confirmed <- FALSE
    values$analysis_data <- NULL
    showNotification("已重置变量选择，请重新配置", type = "message")
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
        # 执行贝叶斯网络分析
        values$bayesian_result <- conduct_likert_bayesian_analysis(
          data = analysis_data,
          algorithm = input$bn_algorithm,
          score = input$score_function,
          bootstrap_n = input$bootstrap_rounds,
          threshold = input$strength_threshold,
          blacklist = constraints$blacklist,
          whitelist = constraints$whitelist
        )
        
        incProgress(0.8, detail = "完成分析...")
        
        bayesian_completed(TRUE)
        showNotification("贝叶斯网络分析完成！", type = "message")
        
      }, error = function(e) {
        showNotification(paste("贝叶斯网络分析失败:", e$message), type = "error")
        bayesian_completed(FALSE)
      })
    })
  })
  
  # 贝叶斯网络图输出
  output$bayesian_network_plot <- renderPlot({
    req(values$bayesian_result)
    
    tryCatch({
      # 使用bnlearn的绘图功能
      if(requireNamespace("bnlearn", quietly = TRUE) && requireNamespace("Rgraphviz", quietly = TRUE)) {
        bnlearn::graphviz.plot(values$bayesian_result$averaged_network)
      } else {
        # 备用方案：使用igraph
        if(requireNamespace("igraph", quietly = TRUE)) {
          # 转换为igraph格式并绘图
          edges <- values$bayesian_result$stable_edges
          if(nrow(edges) > 0) {
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
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("绘图失败:", e$message), cex = 1.2)
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
      paste0("bayesian_network_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      if(requireNamespace("bnlearn", quietly = TRUE) && requireNamespace("Rgraphviz", quietly = TRUE)) {
        bnlearn::graphviz.plot(values$bayesian_result$averaged_network)
      } else {
        plot.new()
        text(0.5, 0.5, "需要安装Rgraphviz包", cex = 2)
      }
      dev.off()
    }
  )
  
  output$download_bn_edges <- downloadHandler(
    filename = function() {
      paste0("bayesian_edges_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$bayesian_result$stable_edges, file, row.names = FALSE)
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
}

# =============================================================================
# 运行应用
# =============================================================================

shinyApp(ui = ui, server = server)