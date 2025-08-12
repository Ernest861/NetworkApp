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
      menuItem("变量选择", tabName = "variables", icon = icon("check-square")),
      menuItem("网络分析", tabName = "analysis", icon = icon("project-diagram")),
      menuItem("贝叶斯网络", tabName = "bayesian", icon = icon("brain")),
      menuItem("稳定性分析", tabName = "stability", icon = icon("chart-line")),
      menuItem("结果下载", tabName = "download", icon = icon("download")),
      menuItem("使用说明", tabName = "help", icon = icon("question-circle"))
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
              
              h4("数据统计："),
              verbatimTextOutput("data_statistics")
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
                  tags$p("在'变量选择'页面配置分析变量", class = "text-muted"))
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
    analysis_data = NULL
  )
  
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
        values$raw_data <- dplyr::select_if(values$raw_data, is.numeric)
        incProgress(0.3, detail = "验证数据质量...")
        
        # 数据验证 (使用增强版包含李克特量表检测)
        values$validation_result <- validate_likert_data(values$raw_data)
        
        if(values$validation_result$valid) {
          incProgress(0.5, detail = "解析量表结构...")
          
          # 解析量表结构
          values$scales <- parse_scale_structure_advanced(values$raw_data)
          
          incProgress(0.7, detail = "计算维度得分...")
          
          # 数据预处理
          processed_raw <- preprocess_data(values$raw_data, input$remove_outliers)
          
          # 计算量表得分
          values$processed_data <- compute_scale_scores_advanced(processed_raw, values$scales)
          
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
  
  output$scalesDetected <- reactive({
    return(!is.null(values$scales) && length(values$scales) > 0)
  })
  outputOptions(output, "scalesDetected", suspendWhenHidden = FALSE)
  
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
    req(values$processed_data, values$scales, values$validation_result)
    
    # 检查数据有效性
    if(!values$validation_result$valid) {
      showNotification("数据验证失败，无法进行分析", type = "error")
      return()
    }
    
    withProgress(message = '正在进行网络分析...', value = 0, {
      
      # 根据每个量表的层级选择构建分析数据
      analysis_vars <- c()
      
      for(scale_name in names(values$scales)) {
        scale_info <- values$scales[[scale_name]]
        
        # 优先使用高级选择器的设置，如果没有则使用基本选择器
        advanced_level_input_id <- paste0("advanced_level_", scale_name)
        basic_level_input_id <- paste0("level_", scale_name)
        
        selected_level <- input[[advanced_level_input_id]]
        if(is.null(selected_level)) {
          selected_level <- input[[basic_level_input_id]]
        }
        
        if(is.null(selected_level)) {
          selected_level <- "summary"  # 默认值
        }
        
        if(selected_level == "summary") {
          # 汇总层：使用维度得分（通常是总分）
          subscale_names <- names(scale_info$subscales)
          if(length(subscale_names) == 1) {
            analysis_vars <- c(analysis_vars, subscale_names[1])
          } else {
            # 如果有多个维度，选择总分或第一个
            total_subscale <- subscale_names[grep("Total", subscale_names)]
            if(length(total_subscale) > 0) {
              analysis_vars <- c(analysis_vars, total_subscale[1])
            } else {
              analysis_vars <- c(analysis_vars, subscale_names[1])
            }
          }
          
        } else if(selected_level == "subscale") {
          # 子量表层：使用各维度得分
          subscale_names <- names(scale_info$subscales)
          # 排除总分，只要维度得分
          dimension_subscales <- subscale_names[!grepl("Total", subscale_names)]
          if(length(dimension_subscales) > 0) {
            analysis_vars <- c(analysis_vars, dimension_subscales)
          } else {
            analysis_vars <- c(analysis_vars, subscale_names)
          }
          
        } else {
          # 条目层：使用原始条目
          items <- scale_info$items
          analysis_vars <- c(analysis_vars, items)
        }
      }
      
      # 构建最终分析数据
      available_vars <- intersect(analysis_vars, names(values$processed_data))
      if(length(available_vars) == 0) {
        # 如果processed_data中没有，尝试从raw_data获取
        available_vars <- intersect(analysis_vars, names(values$raw_data))
        if(length(available_vars) > 0) {
          values$analysis_data <- values$raw_data[, available_vars, drop = FALSE]
        }
      } else {
        values$analysis_data <- values$processed_data[, available_vars, drop = FALSE]
      }
      
      # 总体变量数建议
      if(ncol(values$analysis_data) > 30) {
        showNotification(paste("当前选择了", ncol(values$analysis_data), "个变量，建议控制在30个以内以获得更好的可视化效果"), type = "warning")
      }
      
      incProgress(0.2, detail = "准备分析数据...")
      
      if(ncol(values$analysis_data) < 2) {
        showNotification("可用变量不足，请检查数据结构", type = "error")
        return()
      }
      
      incProgress(0.3, detail = "构建网络...")
      
      # 使用安全的网络分析函数
      tryCatch({
        colors <- VIZ_CONFIG$colors$primary[1:min(ncol(values$analysis_data), length(VIZ_CONFIG$colors$primary))]
        
        values$network_result <- safe_network_analysis(
          data = values$analysis_data,
          threshold = input$threshold,
          edge_labels = input$show_edge_labels,
          colors = colors
        )
        
        incProgress(0.5, detail = "计算中心性指标...")
        
        # 中心性分析
        if(requireNamespace("quickNet", quietly = TRUE)) {
          values$centrality_result <- Centrality(values$network_result)
        }
        
        incProgress(1, detail = "网络分析完成!")
        
        showNotification("网络分析完成！", type = "message")
        
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
    req(values$scales)
    
    if(length(values$scales) == 0) {
      return(div(
        class = "alert alert-info",
        tags$h4("📋 暂无可用量表"),
        tags$p("请先在\"数据上传\"页面上传数据文件")
      ))
    }
    
    selectors <- list()
    
    for(scale_name in names(values$scales)) {
      scale_info <- values$scales[[scale_name]]
      
      # 构建详细的选项说明
      choices <- list()
      choice_descriptions <- list()
      
      # 汇总层选项
      total_subscales <- names(scale_info$subscales)[grepl("Total", names(scale_info$subscales))]
      if(length(total_subscales) > 0) {
        choices[["🎯 汇总层 (总分)"]] <- "summary"
        choice_descriptions[["summary"]] <- paste0("使用 ", total_subscales[1], " 作为该量表的总体得分")
      } else if(length(scale_info$subscales) == 1) {
        choices[["🎯 汇总层 (总分)"]] <- "summary"
        choice_descriptions[["summary"]] <- paste0("使用 ", names(scale_info$subscales)[1], " 作为总体得分")
      } else {
        choices[["🎯 汇总层 (第一维度)"]] <- "summary"
        choice_descriptions[["summary"]] <- paste0("使用 ", names(scale_info$subscales)[1], " 作为代表性得分")
      }
      
      # 子量表层选项  
      dimension_subscales <- names(scale_info$subscales)[!grepl("Total", names(scale_info$subscales))]
      if(length(dimension_subscales) > 1) {
        choices[[paste0("📊 子量表层 (", length(dimension_subscales), "个维度)")]] <- "subscale"
        choice_descriptions[["subscale"]] <- paste0("使用各维度得分: ", paste(dimension_subscales, collapse = ", "))
      }
      
      # 条目层选项
      item_count <- length(scale_info$items)
      choices[[paste0("📝 条目层 (", item_count, "个题目)")]] <- "items"
      if(item_count <= 10) {
        choice_descriptions[["items"]] <- paste0("使用所有 ", item_count, " 个原始题目")
      } else {
        choice_descriptions[["items"]] <- paste0("使用所有 ", item_count, " 个题目 (", 
                                               paste(head(scale_info$items, 3), collapse = ", "), 
                                               if(item_count > 3) "..." else "", ")")
      }
      
      # 智能默认选择
      default_value <- if(scale_name %in% c("AUDIT", "PHQ", "GAD")) {
        "summary"
      } else if(scale_name == "HRF" && item_count <= 18) {
        "items"
      } else if(length(dimension_subscales) > 1) {
        "subscale"
      } else {
        "summary"
      }
      
      if(!default_value %in% unlist(choices)) default_value <- unlist(choices)[1]
      
      # 创建卡片式选择器
      selectors[[scale_name]] <- div(
        class = "panel panel-default",
        style = "margin-bottom: 15px;",
        div(
          class = "panel-heading",
          style = "background-color: #f8f9fa; border-color: #dee2e6;",
          tags$h4(
            class = "panel-title",
            style = "margin: 0; color: #495057;",
            paste0("📊 ", scale_info$name)
          ),
          div(
            style = "font-size: 0.85em; color: #6c757d; margin-top: 5px;",
            paste0("包含 ", length(scale_info$items), " 个条目，", length(scale_info$subscales), " 个维度")
          )
        ),
        div(
          class = "panel-body",
          selectInput(
            inputId = paste0("advanced_level_", scale_name),
            label = "选择分析层级：",
            choices = choices,
            selected = default_value,
            width = "100%"
          ),
          div(
            id = paste0("choice_desc_", scale_name),
            style = "margin-top: 10px; padding: 8px; background-color: #e9f7ef; border-radius: 4px; font-size: 0.9em;",
            textOutput(paste0("choice_description_", scale_name))
          )
        )
      )
    }
    
    return(tagList(selectors))
  })
  
  # 为每个量表动态生成选择说明
  observe({
    req(values$scales)
    for(scale_name in names(values$scales)) {
      local({
        local_scale_name <- scale_name
        scale_info <- values$scales[[local_scale_name]]
        
        output[[paste0("choice_description_", local_scale_name)]] <- renderText({
          level_input_id <- paste0("advanced_level_", local_scale_name)
          selected_level <- input[[level_input_id]]
          
          if(is.null(selected_level)) return("")
          
          if(selected_level == "summary") {
            total_subscales <- names(scale_info$subscales)[grepl("Total", names(scale_info$subscales))]
            if(length(total_subscales) > 0) {
              return(paste0("✓ 将使用 ", total_subscales[1], " 作为该量表的代表变量"))
            } else {
              return(paste0("✓ 将使用 ", names(scale_info$subscales)[1], " 作为该量表的代表变量"))
            }
            
          } else if(selected_level == "subscale") {
            dimension_subscales <- names(scale_info$subscales)[!grepl("Total", names(scale_info$subscales))]
            return(paste0("✓ 将使用 ", length(dimension_subscales), " 个维度变量: ", 
                         paste(dimension_subscales, collapse = ", ")))
            
          } else {
            item_count <- length(scale_info$items)
            return(paste0("✓ 将使用所有 ", item_count, " 个条目变量"))
          }
        })
      })
    }
  })
  
  # 生成最终变量预览
  output$final_variables_preview <- renderText({
    req(values$scales)
    
    if(length(values$scales) == 0) {
      return("未检测到量表结构")
    }
    
    preview_lines <- c()
    total_vars <- 0
    
    for(scale_name in names(values$scales)) {
      scale_info <- values$scales[[scale_name]]
      level_input_id <- paste0("advanced_level_", scale_name)
      selected_level <- input[[level_input_id]]
      
      if(is.null(selected_level)) selected_level <- "summary"
      
      if(selected_level == "summary") {
        total_subscales <- names(scale_info$subscales)[grepl("Total", names(scale_info$subscales))]
        if(length(total_subscales) > 0) {
          var_name <- total_subscales[1]
        } else {
          var_name <- names(scale_info$subscales)[1]
        }
        preview_lines <- c(preview_lines, paste0("📊 ", scale_info$name, ": ", var_name, " (汇总层)"))
        total_vars <- total_vars + 1
        
      } else if(selected_level == "subscale") {
        dimension_subscales <- names(scale_info$subscales)[!grepl("Total", names(scale_info$subscales))]
        if(length(dimension_subscales) > 0) {
          preview_lines <- c(preview_lines, paste0("📊 ", scale_info$name, ":"))
          for(sub_name in dimension_subscales) {
            preview_lines <- c(preview_lines, paste0("   • ", sub_name))
          }
          total_vars <- total_vars + length(dimension_subscales)
        }
        
      } else {
        items <- scale_info$items
        preview_lines <- c(preview_lines, paste0("📊 ", scale_info$name, ": ", length(items), " 个条目"))
        preview_lines <- c(preview_lines, paste0("   ", paste(head(items, 5), collapse = ", "), 
                                                if(length(items) > 5) "..." else ""))
        total_vars <- total_vars + length(items)
      }
      
      preview_lines <- c(preview_lines, "")
    }
    
    header <- paste0("🎯 网络分析将包含 ", total_vars, " 个变量\n",
                    "==========================================\n\n")
    
    return(paste0(header, paste(preview_lines, collapse = "\n")))
  })
  
  # 变量是否已选择的状态
  output$variablesSelected <- reactive({
    req(values$scales)
    return(length(values$scales) > 0)
  })
  outputOptions(output, "variablesSelected", suspendWhenHidden = FALSE)
  
  # 变量是否已确认的状态  
  variables_confirmed <- reactiveVal(FALSE)
  
  output$variablesConfirmed <- reactive({
    return(variables_confirmed())
  })
  outputOptions(output, "variablesConfirmed", suspendWhenHidden = FALSE)
  
  # 确认变量选择
  observeEvent(input$confirm_variables, {
    req(values$scales, length(values$scales) > 0)
    
    # 计算总变量数进行验证
    total_vars <- 0
    for(scale_name in names(values$scales)) {
      level_input_id <- paste0("advanced_level_", scale_name)
      selected_level <- input[[level_input_id]]
      if(is.null(selected_level)) selected_level <- "summary"
      
      if(selected_level == "summary") {
        total_vars <- total_vars + 1
      } else if(selected_level == "subscale") {
        scale_info <- values$scales[[scale_name]]
        dimension_subscales <- names(scale_info$subscales)[!grepl("Total", names(scale_info$subscales))]
        total_vars <- total_vars + length(dimension_subscales)
      } else {
        scale_info <- values$scales[[scale_name]]
        total_vars <- total_vars + length(scale_info$items)
      }
    }
    
    if(total_vars < 2) {
      showNotification("至少需要选择2个变量才能进行网络分析", type = "error")
      return()
    }
    
    if(total_vars > 50) {
      showNotification("变量数过多，可能影响分析效果，建议控制在50个以内", type = "warning")
    }
    
    variables_confirmed(TRUE)
    showNotification(paste0("变量选择已确认！共选择了 ", total_vars, " 个变量"), type = "message")
  })
  
  # 重新选择变量
  observeEvent(input$reselect_variables, {
    variables_confirmed(FALSE)
    showNotification("已重置变量选择，请重新配置", type = "message")
  })
  
  # =============================================================================
  # 贝叶斯网络分析服务器端逻辑
  # =============================================================================
  
  # 贝叶斯网络分析准备状态
  output$bayesianReady <- reactive({
    req(values$validation_result)
    return(values$validation_result$bayesian_ready)
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
    req(values$processed_data, values$validation_result$bayesian_ready)
    
    withProgress(message = '正在进行贝叶斯网络分析...', value = 0, {
      
      incProgress(0.1, detail = "准备数据和约束规则...")
      
      # 获取最终分析数据
      analysis_data <- NULL
      if(variables_confirmed()) {
        # 使用高级变量选择的结果构建分析数据
        analysis_vars <- c()
        
        for(scale_name in names(values$scales)) {
          scale_info <- values$scales[[scale_name]]
          level_input_id <- paste0("advanced_level_", scale_name)
          selected_level <- input[[level_input_id]]
          
          if(is.null(selected_level)) selected_level <- "summary"
          
          if(selected_level == "summary") {
            total_subscales <- names(scale_info$subscales)[grepl("Total", names(scale_info$subscales))]
            if(length(total_subscales) > 0) {
              analysis_vars <- c(analysis_vars, total_subscales[1])
            } else {
              analysis_vars <- c(analysis_vars, names(scale_info$subscales)[1])
            }
          } else if(selected_level == "subscale") {
            dimension_subscales <- names(scale_info$subscales)[!grepl("Total", names(scale_info$subscales))]
            analysis_vars <- c(analysis_vars, dimension_subscales)
          } else {
            analysis_vars <- c(analysis_vars, scale_info$items)
          }
        }
        
        # 构建分析数据
        available_vars <- intersect(analysis_vars, names(values$processed_data))
        if(length(available_vars) == 0) {
          available_vars <- intersect(analysis_vars, names(values$raw_data))
          analysis_data <- values$raw_data[, available_vars, drop = FALSE]
        } else {
          analysis_data <- values$processed_data[, available_vars, drop = FALSE]
        }
      } else {
        # 使用所有可用的数值列
        analysis_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
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
    
    HTML(generate_bayesian_report(values$bayesian_result))
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
      report_content <- generate_bayesian_report(values$bayesian_result)
      writeLines(report_content, file)
    }
  )
}

# =============================================================================
# 运行应用
# =============================================================================

shinyApp(ui = ui, server = server)