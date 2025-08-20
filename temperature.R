# =============================================================================
# 网络温度分析独立模块
# 完整的UI和服务器端代码
# =============================================================================

# =============================================================================
# 1. UI组件 - 网络温度分析页面
# =============================================================================

temperature_ui <- function() {
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
                          class = "btn-success btn-sm")
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
    
    # 结果展示区
    conditionalPanel(
      condition = "output.temperatureAnalysisComplete",
      
      # 分析报告
      fluidRow(
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
        
        # 网络热图
        box(
          title = "🔥 网络连接热图", status = "warning", solidHeader = TRUE, width = 6,
          conditionalPanel(
            condition = "input.enable_grouping",
            selectInput("heatmap_group_select", "选择显示组别", choices = NULL)
          ),
          plotOutput("temp_network_heatmap"),
          br(),
          downloadButton("download_temp_heatmap", "下载热图", class = "btn-warning btn-sm")
        )
      )
    )
  )
}

# =============================================================================
# 2. 服务器端逻辑
# =============================================================================

temperature_server <- function(input, output, session, values) {
  
  # 分组变量选择器
  output$temp_group_var_selector <- renderUI({
    req(values$processed_data)
    
    # 检测分类变量
    categorical_vars <- names(values$processed_data)[
      sapply(values$processed_data, function(x) 
        is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) <= 10))]
    
    selectInput("temp_group_var", "选择分组变量",
               choices = c("Gender" = "Gender", "Age" = "Age", categorical_vars),
               selected = if("Gender" %in% categorical_vars) "Gender" else categorical_vars[1])
  })
  
  # 分析状态响应式变量
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
  
  # 网络温度分析主要事件处理
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
        
        values$temperature_result <- temp_result
        values$temperature_analysis_running <- FALSE
        
        # 调试信息
        cat("🔍 温度分析结果保存完成\n")
        cat("结果success状态:", temp_result$success, "\n")
        cat("temperatureAnalysisComplete应该为:", !is.null(values$temperature_result) && !is.null(values$temperature_result$success) && values$temperature_result$success, "\n")
        
        incProgress(1, detail = "完成!")
        
        showNotification("✅ 网络温度分析完成！请查看下方结果", type = "message", duration = 8)
        
      })
      
    }, error = function(e) {
      cat("❌ 网络温度分析失败:", e$message, "\n")
      values$temperature_error <- e$message
      values$temperature_analysis_running <- FALSE
      showNotification(paste("❌ 分析失败:", e$message), type = "error", duration = 10)
    })
  })
  
  # 结果输出函数
  
  # 分析摘要
  output$temp_analysis_summary <- renderUI({
    req(values$temperature_result)
    
    if(!values$temperature_result$success) {
      return(div(class = "alert alert-danger", "分析失败"))
    }
    
    result <- values$temperature_result
    params <- result$parameters
    
    # 基本信息显示
    tags$div(
      tags$p(tags$strong("✅ 分析状态："), "完成"),
      tags$p(tags$strong("📊 分析类型："), 
             ifelse(is.null(params$group_var), "单组网络温度分析", "多组网络温度比较")),
      tags$p(tags$strong("🔧 二值化方法："), params$binary_transform),
      tags$p(tags$strong("📝 编码格式："), params$binary_encoding),
      tags$p(tags$strong("📋 模型数量："), length(result$models)),
      if(!is.null(result$comparison$best_model)) {
        tags$p(tags$strong("🏆 最佳模型："), result$comparison$best_model)
      }
    )
  })
  
  # 分析报告
  output$temperature_analysis_report <- renderUI({
    req(values$temperature_result)
    
    if(!values$temperature_result$success) {
      return(div(class = "alert alert-danger", "分析失败，请检查参数设置"))
    }
    
    result <- values$temperature_result
    
    # 使用分析函数返回的summary
    HTML(result$summary)
  })
  
  # 温度比较图
  output$temp_comparison_plot <- renderPlot({
    req(values$temperature_result)
    req(values$temperature_result$success)
    
    result <- values$temperature_result
    metrics <- result$metrics
    
    if(length(metrics) == 0) {
      plot.new()
      text(0.5, 0.5, "无可视化数据", cex = 1.5, col = "gray")
      return()
    }
    
    # 提取温度数据
    model_names <- names(metrics)
    temperatures <- sapply(metrics, function(x) x$temperature)
    temperatures <- temperatures[!is.na(temperatures)]
    
    if(length(temperatures) == 0) {
      plot.new()
      text(0.5, 0.5, "温度数据不可用", cex = 1.5, col = "gray")
      return()
    }
    
    # 创建温度比较条形图
    par(mar = c(5, 8, 4, 2))
    barplot(temperatures, 
            names.arg = names(temperatures),
            horiz = TRUE,
            col = rainbow(length(temperatures), alpha = 0.7),
            main = "网络温度比较",
            xlab = "温度值 (T = 1/β)",
            las = 1,
            cex.names = 0.8)
    
    # 添加数值标签
    text(temperatures + max(temperatures) * 0.02, 
         seq_along(temperatures), 
         round(temperatures, 3), 
         pos = 4, cex = 0.8)
  })
  
  # 网络热图
  output$temp_network_heatmap <- renderPlot({
    req(values$temperature_result)
    req(values$temperature_result$success)
    
    result <- values$temperature_result
    
    # 尝试从模型中提取omega矩阵
    tryCatch({
      models <- result$models
      
      if(length(models) == 0) {
        plot.new()
        text(0.5, 0.5, "无模型数据", cex = 1.5, col = "gray")
        return()
      }
      
      # 选择第一个可用模型
      first_model <- models[[1]]
      
      # 提取omega矩阵
      omega_matrix <- psychonetrics::getmatrix(first_model, "omega")
      
      if(is.list(omega_matrix)) {
        omega_matrix <- omega_matrix[[1]]  # 多组情况取第一组
      }
      
      if(is.null(omega_matrix) || !is.matrix(omega_matrix)) {
        plot.new()
        text(0.5, 0.5, "矩阵提取失败", cex = 1.5, col = "gray")
        return()
      }
      
      # 创建热图
      if(requireNamespace("corrplot", quietly = TRUE)) {
        corrplot::corrplot(omega_matrix, 
                          method = "color",
                          type = "upper",
                          order = "hclust",
                          title = "网络连接强度热图",
                          tl.cex = 0.8,
                          tl.col = "black")
      } else {
        # 使用基础R绘制热图
        heatmap(omega_matrix, 
                main = "网络连接强度热图",
                col = heat.colors(20),
                scale = "none")
      }
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("热图生成失败:", e$message), cex = 1.2, col = "red")
    })
  })
}

# =============================================================================
# 3. 核心分析函数 (来自utils.R)
# =============================================================================

# 主函数：网络温度分析
network_temperature_analysis <- function(data, 
                                       group_var = NULL,
                                       selected_vars,
                                       binary_transform = "median",
                                       binary_encoding = "01", 
                                       binary_threshold = NULL,
                                       estimator = "ML",
                                       alpha = 0.05) {
  
  cat("🌡️ 开始网络温度分析...\n")
  
  tryCatch({
    
    # 1. 数据准备
    cat("📊 步骤1: 数据准备和二值化...\n")
    binary_data <- prepare_binary_data(data, selected_vars, binary_transform, binary_encoding, binary_threshold)
    
    # 2. 构建Ising模型
    cat("🔧 步骤2: 构建Ising模型...\n")
    ising_models <- fit_ising_models(binary_data, group_var, selected_vars, estimator)
    
    # 3. 计算网络指标
    cat("📈 步骤3: 计算网络温度和全局指标...\n")
    network_metrics <- extract_network_metrics(ising_models)
    
    # 4. 模型比较
    cat("⚖️ 步骤4: 模型比较和选择...\n")
    model_comparison <- compare_ising_models(ising_models)
    
    # 5. 生成结果摘要
    cat("📋 步骤5: 生成分析结果摘要...\n")
    analysis_summary <- generate_temperature_summary(network_metrics, model_comparison, group_var)
    
    result <- list(
      success = TRUE,
      binary_data = binary_data,
      models = ising_models,
      metrics = network_metrics,
      comparison = model_comparison,
      summary = analysis_summary,
      parameters = list(
        group_var = group_var,
        selected_vars = selected_vars,
        binary_transform = binary_transform,
        binary_encoding = binary_encoding,
        binary_threshold = binary_threshold,
        estimator = estimator,
        alpha = alpha
      )
    )
    
    cat("✅ 网络温度分析完成！\n")
    return(result)
    
  }, error = function(e) {
    cat("❌ 网络温度分析失败:", e$message, "\n")
    return(list(
      success = FALSE,
      error = e$message,
      parameters = list(
        group_var = group_var,
        selected_vars = selected_vars,
        binary_transform = binary_transform,
        binary_encoding = binary_encoding
      )
    ))
  })
}

# 数据二值化函数（两层设计）
prepare_binary_data <- function(data, vars, transform = "median", encoding = "01", threshold = NULL) {
  
  cat("🔄 数据二值化: 方法=", transform, ", 编码=", encoding, "\n")
  
  analysis_data <- data[, vars, drop = FALSE]
  
  # 第一层：转换为0/1
  for(var in vars) {
    var_data <- analysis_data[[var]]
    
    if(transform == "median") {
      threshold_val <- median(var_data, na.rm = TRUE)
      binary_var <- ifelse(var_data > threshold_val, 1, 0)
    } else if(transform == "mean") {
      threshold_val <- mean(var_data, na.rm = TRUE)
      binary_var <- ifelse(var_data > threshold_val, 1, 0)
    } else if(transform == "custom" && !is.null(threshold)) {
      binary_var <- ifelse(var_data > threshold, 1, 0)
    } else if(transform == "normalize") {
      normalized <- (var_data - min(var_data, na.rm = TRUE)) / (max(var_data, na.rm = TRUE) - min(var_data, na.rm = TRUE))
      binary_var <- ifelse(normalized > 0.5, 1, 0)
    } else if(transform == "keep") {
      # 假设数据已经是0/1
      binary_var <- var_data
    } else {
      # 默认使用中位数
      threshold_val <- median(var_data, na.rm = TRUE)
      binary_var <- ifelse(var_data > threshold_val, 1, 0)
    }
    
    analysis_data[[var]] <- binary_var
  }
  
  # 第二层：编码格式转换
  if(encoding == "neg11") {
    # 转换0/1为-1/1
    for(var in vars) {
      analysis_data[[var]] <- ifelse(analysis_data[[var]] == 0, -1, 1)
    }
    cat("📊 编码转换: 0→-1, 1→1\n")
  } else {
    cat("📊 保持0/1编码\n")
  }
  
  # 检查数据质量
  complete_cases <- complete.cases(analysis_data)
  n_complete <- sum(complete_cases)
  
  cat("📋 二值化完成 - 完整观测:", n_complete, "/", nrow(analysis_data), "\n")
  
  if(n_complete < 30) {
    warning("⚠️ 完整观测数量过少，可能影响分析结果")
  }
  
  return(analysis_data[complete_cases, , drop = FALSE])
}

# Ising模型拟合函数
fit_ising_models <- function(data, group_var = NULL, selected_vars, estimator = "ML") {
  
  if(!requireNamespace("psychonetrics", quietly = TRUE)) {
    stop("需要安装psychonetrics包：install.packages('psychonetrics')")
  }
  
  models <- list()
  
  if(is.null(group_var)) {
    # 单组分析
    cat("🔧 拟合单组Ising模型...\n")
    
    # 基础模型（参考zTemperature.R的简化版本）
    base_model <- psychonetrics::Ising(
      data = data,
      vars = selected_vars,
      estimator = estimator
    )
    
    models$single_free <- base_model %>% psychonetrics::runmodel()
    
    cat("✅ 单组模型拟合完成\n")
    
  } else {
    # 多组分析
    cat("🔧 拟合多组Ising模型 (分组变量:", group_var, ")...\n")
    
    # 基础多组模型
    base_model <- psychonetrics::Ising(
      data = data,
      vars = selected_vars,
      groups = group_var,
      estimator = estimator
    )
    
    # 4种约束层级（参考zTemperature.R）
    models$free <- base_model %>% psychonetrics::runmodel()
    models$omega_equal <- base_model %>% psychonetrics::groupequal("omega") %>% psychonetrics::runmodel()
    models$omega_tau_equal <- base_model %>% psychonetrics::groupequal("omega") %>% 
                             psychonetrics::groupequal("tau") %>% psychonetrics::runmodel()
    models$omega_tau_beta_equal <- base_model %>% psychonetrics::groupequal("omega") %>% 
                                  psychonetrics::groupequal("tau") %>% 
                                  psychonetrics::groupequal("beta") %>% psychonetrics::runmodel()
    
    cat("✅ 多组模型拟合完成 (4个约束层级)\n")
  }
  
  return(models)
}

# 网络指标提取函数
extract_network_metrics <- function(models) {
  
  metrics <- list()
  
  for(model_name in names(models)) {
    model <- models[[model_name]]
    
    tryCatch({
      # 提取参数
      params <- psychonetrics::parameters(model)
      
      # 计算温度 T = 1/β
      beta_params <- params[params$par == "beta", "est"]
      if(length(beta_params) > 0) {
        temperature <- 1 / mean(beta_params, na.rm = TRUE)
      } else {
        temperature <- NA
      }
      
      # 计算连接度
      omega_matrix <- psychonetrics::getmatrix(model, "omega")
      if(is.list(omega_matrix)) {
        # 多组情况，取第一组
        omega_matrix <- omega_matrix[[1]]
      }
      connectivity <- sum(abs(omega_matrix[upper.tri(omega_matrix)]))
      
      # 计算密度
      n_nodes <- nrow(omega_matrix)
      max_edges <- n_nodes * (n_nodes - 1) / 2
      density <- sum(omega_matrix[upper.tri(omega_matrix)] != 0) / max_edges
      
      # 拟合指标
      fit_info <- psychonetrics::fit(model)
      
      metrics[[model_name]] <- list(
        temperature = temperature,
        connectivity = connectivity,
        density = density,
        AIC = fit_info$AIC,
        BIC = fit_info$BIC,
        CFI = fit_info$CFI %||% NA,
        RMSEA = fit_info$RMSEA %||% NA
      )
      
    }, error = function(e) {
      cat("⚠️ 提取模型", model_name, "指标时出错:", e$message, "\n")
      metrics[[model_name]] <- list(
        temperature = NA,
        connectivity = NA,
        density = NA,
        AIC = NA,
        BIC = NA,
        CFI = NA,
        RMSEA = NA
      )
    })
  }
  
  return(metrics)
}

# 模型比较函数
compare_ising_models <- function(models) {
  
  if(length(models) <= 1) {
    return(list(best_model = names(models)[1], comparison_table = NULL))
  }
  
  tryCatch({
    # 使用psychonetrics的compare函数
    comparison_result <- psychonetrics::compare(models)
    
    # 提取比较表
    comparison_table <- comparison_result$fitTable
    
    # 找到BIC最低的模型
    best_model_idx <- which.min(comparison_table$BIC)
    best_model <- rownames(comparison_table)[best_model_idx]
    
    cat("🏆 最佳模型 (基于BIC):", best_model, "\n")
    
    return(list(
      best_model = best_model,
      comparison_table = comparison_table,
      comparison_result = comparison_result
    ))
    
  }, error = function(e) {
    cat("⚠️ 模型比较失败:", e$message, "\n")
    
    # 手动计算AIC/BIC比较
    aic_values <- sapply(models, function(m) psychonetrics::fit(m)$AIC)
    bic_values <- sapply(models, function(m) psychonetrics::fit(m)$BIC)
    
    best_model <- names(which.min(bic_values))
    
    comparison_table <- data.frame(
      Model = names(models),
      AIC = aic_values,
      BIC = bic_values,
      stringsAsFactors = FALSE
    )
    
    return(list(
      best_model = best_model,
      comparison_table = comparison_table,
      comparison_result = NULL
    ))
  })
}

# 生成分析摘要
generate_temperature_summary <- function(metrics, comparison, group_var) {
  
  summary_lines <- c()
  
  summary_lines <- c(summary_lines, 
    "<h3>🌡️ 网络温度分析结果摘要</h3>",
    paste0("<p><strong>分析类型：</strong>", 
           ifelse(is.null(group_var), "单组网络分析", paste0("多组网络分析 (分组变量: ", group_var, ")"))),
    paste0("<strong>分析模型数量：</strong>", length(metrics), "</p>")
  )
  
  if(!is.null(comparison$best_model)) {
    summary_lines <- c(summary_lines,
      paste0("<p><strong>🏆 最佳模型：</strong>", comparison$best_model, " (基于BIC准则)</p>")
    )
    
    # 最佳模型的关键指标
    best_metrics <- metrics[[comparison$best_model]]
    if(!is.null(best_metrics)) {
      summary_lines <- c(summary_lines,
        "<h4>🔍 最佳模型关键指标：</h4>",
        "<ul>",
        paste0("<li><strong>网络温度：</strong>", round(best_metrics$temperature, 3)),
        paste0("<li><strong>连接强度：</strong>", round(best_metrics$connectivity, 3)),
        paste0("<li><strong>网络密度：</strong>", round(best_metrics$density, 3)),
        paste0("<li><strong>BIC：</strong>", round(best_metrics$BIC, 2)),
        "</ul>"
      )
    }
  }
  
  # 模型比较表
  if(!is.null(comparison$comparison_table)) {
    summary_lines <- c(summary_lines,
      "<h4>📊 模型比较表：</h4>",
      "<p>以下为所有拟合模型的比较结果（按BIC排序）：</p>"
    )
  }
  
  summary_lines <- c(summary_lines,
    "<h4>💡 结果解释：</h4>",
    "<ul>",
    "<li><strong>网络温度：</strong>反映网络稳定性，温度越高表示网络越不稳定</li>",
    "<li><strong>连接强度：</strong>网络中所有边权重的绝对值之和</li>",
    "<li><strong>网络密度：</strong>实际连接数与可能最大连接数的比值</li>",
    "<li><strong>BIC准则：</strong>模型选择指标，值越小表示模型越优</li>",
    "</ul>"
  )
  
  return(paste(summary_lines, collapse = "\n"))
}

# 辅助函数：处理NULL值
`%||%` <- function(x, y) if(is.null(x)) y else x

# =============================================================================
# 使用示例
# =============================================================================

# 在主应用中集成：
# 1. 在UI中添加：temperature_ui()
# 2. 在server中调用：temperature_server(input, output, session, values)

cat("✅ 网络温度分析模块加载完成\n")
cat("📁 文件位置: temperature.R\n")
cat("🔧 主要组件: temperature_ui(), temperature_server(), network_temperature_analysis()\n")