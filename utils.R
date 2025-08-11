# =============================================================================
# 辅助函数库 - Utility Functions
# 用于数据处理、验证和分析的辅助函数
# =============================================================================

source("config.R")

# =============================================================================
# 数据解析和验证函数
# =============================================================================

#' 智能识别量表结构
#' @param data 输入数据框
#' @return 识别到的量表列表
parse_scale_structure_advanced <- function(data) {
  col_names <- names(data)
  detected_scales <- list()
  
  # 遍历配置文件中的量表定义
  for(scale_key in names(SCALE_CONFIGS)) {
    config <- SCALE_CONFIGS[[scale_key]]
    
    # 使用正则表达式匹配列名
    matching_cols <- grep(config$pattern, col_names, value = TRUE)
    
    if(length(matching_cols) >= 3) {  # 至少3个题目才算有效量表
      detected_scales[[scale_key]] <- list(
        name = config$name,
        name_en = config$name_en,
        items = matching_cols,
        subscales = list(),
        scoring = config$scoring,
        item_range = config$item_range,
        description = config$description
      )
      
      # 添加维度信息
      for(subscale_name in names(config$subscales)) {
        subscale_config <- config$subscales[[subscale_name]]
        valid_items <- intersect(subscale_config$items, matching_cols)
        
        if(length(valid_items) > 0) {
          detected_scales[[scale_key]]$subscales[[subscale_name]] <- list(
            items = valid_items,
            description = subscale_config$description
          )
        }
      }
    }
  }
  
  # 如果没有匹配的配置，尝试自动检测
  if(length(detected_scales) == 0) {
    detected_scales <- auto_detect_scales(col_names)
  }
  
  return(detected_scales)
}

#' 自动检测未知量表结构
#' @param col_names 列名向量
#' @return 检测到的量表结构
auto_detect_scales <- function(col_names) {
  detected <- list()
  
  # 提取前缀模式
  patterns <- unique(gsub("_.*$", "", col_names))
  patterns <- patterns[nchar(patterns) >= 2]  # 至少2个字符的前缀
  
  for(pattern in patterns) {
    matching_cols <- grep(paste0("^", pattern, "_"), col_names, value = TRUE)
    
    if(length(matching_cols) >= 3) {
      total_name <- paste0(pattern, "_Total")
      detected[[pattern]] <- list(
        name = paste0(pattern, " 量表"),
        name_en = paste0(pattern, " Scale"),
        items = matching_cols,
        subscales = setNames(list(list(
          items = matching_cols,
          description = "总分"
        )), total_name),
        scoring = "mean",  # 默认均值
        item_range = c(1, 7),  # 默认1-7分
        description = "自动识别的量表"
      )
    }
  }
  
  return(detected)
}

#' 数据验证函数
#' @param data 输入数据
#' @return 验证结果列表
validate_data <- function(data) {
  validation_result <- list(
    valid = TRUE,
    warnings = c(),
    errors = c(),
    statistics = list()
  )
  
  n_subjects <- nrow(data)
  n_variables <- ncol(data)
  
  # 检查样本量
  if(n_subjects < VALIDATION_RULES$min_subjects) {
    validation_result$errors <- c(validation_result$errors, 
                                 paste0("样本量过少：", n_subjects, " < ", VALIDATION_RULES$min_subjects))
    validation_result$valid <- FALSE
  }
  
  if(n_subjects > VALIDATION_RULES$max_subjects) {
    validation_result$warnings <- c(validation_result$warnings,
                                   paste0("样本量较大：", n_subjects, "，分析可能需要较长时间"))
  }
  
  # 检查缺失值
  missing_rates <- colSums(is.na(data)) / n_subjects
  problematic_vars <- names(missing_rates)[missing_rates > VALIDATION_RULES$max_missing_per_variable]
  
  if(length(problematic_vars) > 0) {
    validation_result$warnings <- c(validation_result$warnings,
                                   paste0("以下变量缺失值过多：", paste(problematic_vars, collapse = ", ")))
  }
  
  # 整体完整率
  complete_cases <- sum(complete.cases(data))
  complete_rate <- complete_cases / n_subjects
  
  if(complete_rate < VALIDATION_RULES$min_valid_rate) {
    validation_result$errors <- c(validation_result$errors,
                                 paste0("数据完整率过低：", round(complete_rate * 100, 1), "%"))
    validation_result$valid <- FALSE
  }
  
  # 统计信息
  validation_result$statistics <- list(
    n_subjects = n_subjects,
    n_variables = n_variables,
    complete_cases = complete_cases,
    complete_rate = complete_rate,
    missing_rates = missing_rates
  )
  
  return(validation_result)
}

# =============================================================================
# 数据处理函数
# =============================================================================

#' 计算量表得分
#' @param data 原始数据
#' @param scales 量表配置
#' @return 包含计算得分的数据框
compute_scale_scores_advanced <- function(data, scales) {
  result_data <- data
  
  for(scale_name in names(scales)) {
    scale_info <- scales[[scale_name]]
    
    # 计算各维度得分
    for(subscale_name in names(scale_info$subscales)) {
      subscale_info <- scale_info$subscales[[subscale_name]]
      items <- subscale_info$items
      valid_items <- intersect(items, names(data))
      
      if(length(valid_items) >= 2) {  # 至少2个题目
        subscale_data <- data[, valid_items, drop = FALSE]
        
        # 根据计分方式计算得分
        if(scale_info$scoring == "sum") {
          result_data[[subscale_name]] <- rowSums(subscale_data, na.rm = TRUE)
        } else {
          result_data[[subscale_name]] <- rowMeans(subscale_data, na.rm = TRUE)
        }
      }
    }
  }
  
  return(result_data)
}

#' 数据预处理
#' @param data 原始数据
#' @param remove_outliers 是否移除异常值
#' @return 预处理后的数据
preprocess_data <- function(data, remove_outliers = FALSE) {
  processed_data <- data
  
  # --- FIX STARTS HERE ---
  # Identify all columns that are not obviously character-based (like ID or Gender)
  # This is a safer approach than just converting everything.
  # For this app, we assume most columns are scale items and should be numeric.
  # A more robust solution might pass in a list of columns to convert.
  
  # Get all column names
  all_cols <- names(processed_data)
  
  # You can specify columns to exclude from conversion, e.g., ID or group columns
  # For now, we will attempt to convert all, as the analysis functions need numbers.
  
  # 更安全的数据类型转换
  # 只转换明确是量表条目的列（通过命名模式识别）
  scale_patterns <- c("^AUDIT", "^HRF", "^PHQ", "^GAD", "^BDI", "^DASS", "^IAT", "^FTND")
  
  for (col in all_cols) {
    # 检查是否是量表条目列
    is_scale_item <- any(sapply(scale_patterns, function(pattern) grepl(pattern, col)))
    
    if(is_scale_item) {
      original_class <- class(processed_data[[col]])
      if(original_class == "character" || original_class == "factor") {
        # 尝试转换为数值，但更加小心
        numeric_version <- suppressWarnings(as.numeric(as.character(processed_data[[col]])))
        # 只有在转换成功且不全是NA时才替换
        if(!all(is.na(numeric_version))) {
          processed_data[[col]] <- numeric_version
        }
      }
    }
  }
  # --- FIX ENDS HERE ---
  
  # Now, continue with outlier processing on the cleaned numeric data
  numeric_cols <- sapply(processed_data, is.numeric)
  
  if(remove_outliers) {
    for(col in names(processed_data)[numeric_cols]) {
      if(!is.null(processed_data[[col]]) && all(!is.na(processed_data[[col]]))) {
        if(VALIDATION_RULES$outlier_detection$method == "iqr") {
          Q1 <- quantile(processed_data[[col]], 0.25, na.rm = TRUE)
          Q3 <- quantile(processed_data[[col]], 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1
          
          lower_bound <- Q1 - 1.5 * IQR
          upper_bound <- Q3 + 1.5 * IQR
          
          processed_data[[col]][processed_data[[col]] < lower_bound | 
                                  processed_data[[col]] > upper_bound] <- NA
        }
      }
    }
  }
  
  return(processed_data)
}


# =============================================================================
# 网络分析函数
# =============================================================================

#' 安全的网络分析函数
#' @param data 分析数据
#' @param threshold 阈值
#' @param edge_labels 是否显示边标签
#' @param colors 颜色配置
#' @return 网络分析结果
safe_network_analysis <- function(data, threshold = 0.05, edge_labels = TRUE, colors = NULL) {
  
  # 数据检查
  data <- na.omit(data)
  
  if(nrow(data) < 30) {
    stop("样本量不足：需要至少30个有效观测值")
  }
  
  if(ncol(data) < 2) {
    stop("变量不足：需要至少2个变量")
  }
  
  # 检查变量方差
  var_check <- sapply(data, function(x) var(x, na.rm = TRUE))
  zero_var_cols <- names(var_check)[var_check == 0 | is.na(var_check)]
  
  if(length(zero_var_cols) > 0) {
    warning("以下变量方差为0，已移除：", paste(zero_var_cols, collapse = ", "))
    data <- data[, !names(data) %in% zero_var_cols, drop = FALSE]
  }
  
  # 设置颜色
  if(is.null(colors)) {
    colors <- VIZ_CONFIG$colors$primary[1:min(ncol(data), length(VIZ_CONFIG$colors$primary))]
  }
  
  # 执行网络分析
  tryCatch({
    network_result <- quickNet(data,
                              threshold = threshold,
                              edge.labels = edge_labels,
                              posCol = VIZ_CONFIG$colors$positive_edges,
                              negCol = VIZ_CONFIG$colors$negative_edges,
                              color = colors)
    return(network_result)
  }, error = function(e) {
    stop(paste("网络分析失败：", e$message))
  })
}

#' 生成网络分析报告
#' @param network_result 网络分析结果
#' @param centrality_result 中心性结果
#' @param data_info 数据信息
#' @return HTML报告字符串
generate_network_report <- function(network_result, centrality_result = NULL, data_info = NULL) {
  
  report_html <- paste0(
    "<h2>", REPORT_CONFIG$title, "</h2>",
    "<p><strong>生成时间：</strong>", Sys.time(), "</p>",
    "<hr>",
    
    "<h3>", REPORT_CONFIG$sections$data_overview, "</h3>"
  )
  
  if(!is.null(data_info)) {
    report_html <- paste0(report_html,
      "<ul>",
      "<li><strong>样本量：</strong>", data_info$n_subjects, "</li>",
      "<li><strong>变量数：</strong>", data_info$n_variables, "</li>",
      "<li><strong>数据完整率：</strong>", round(data_info$complete_rate * 100, 1), "%</li>",
      "</ul>"
    )
  }
  
  report_html <- paste0(report_html,
    "<h3>", REPORT_CONFIG$sections$network_structure, "</h3>",
    "<p>网络图显示了变量间的关系结构。边的粗细表示关系强度，颜色表示关系方向（蓝色为正相关，红色为负相关）。</p>",
    
    "<h3>", REPORT_CONFIG$sections$centrality_analysis, "</h3>",
    "<p>中心性分析识别网络中的核心节点：</p>",
    "<ul>",
    "<li><strong>Strength（强度）：</strong>节点的直接连接强度</li>",
    "<li><strong>Closeness（接近性）：</strong>节点影响整个网络的能力</li>",
    "<li><strong>Betweenness（介数）：</strong>节点作为桥梁的重要性</li>",
    "</ul>",
    
    "<h3>", REPORT_CONFIG$sections$interpretation, "</h3>",
    "<p>网络分析结果可用于：</p>",
    "<ul>",
    "<li>识别核心症状或特征</li>",
    "<li>理解变量间的条件依赖关系</li>",
    "<li>指导干预策略的制定</li>",
    "<li>探索潜在的因果关系</li>",
    "</ul>"
  )
  
  return(report_html)
}

# =============================================================================
# 可视化增强函数
# =============================================================================

#' 创建增强版中心性图
#' @param centrality_result 中心性结果
#' @param title 图表标题
#' @return ggplot对象
create_enhanced_centrality_plot <- function(centrality_result, title = "中心性分析") {
  if(is.null(centrality_result)) {
    return(NULL)
  }
  
  # 提取中心性数据
  cent_data <- centrality_result$centralityPlot$data
  
  # 创建图表
  p <- ggplot(cent_data, aes(x = value, y = node, fill = measure)) +
    geom_col(position = "dodge", alpha = 0.8) +
    facet_wrap(~ measure, scales = "free_x", ncol = 3) +
    scale_fill_manual(values = VIZ_CONFIG$colors$primary[1:3]) +
    labs(title = title, x = "标准化得分", y = "节点") +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
  
  return(p)
}

#' 导出分析结果
#' @param network_result 网络结果
#' @param centrality_result 中心性结果
#' @param output_dir 输出目录
#' @param prefix 文件前缀
export_analysis_results <- function(network_result, centrality_result = NULL, 
                                   output_dir = ".", prefix = "network") {
  
  # 创建输出目录
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 导出网络图
  png_file <- file.path(output_dir, paste0(prefix, "_network.png"))
  png(png_file, width = VIZ_CONFIG$plot_params$width, 
      height = VIZ_CONFIG$plot_params$height, res = VIZ_CONFIG$plot_params$dpi)
  print(network_result)
  dev.off()
  
  # 导出中心性图
  if(!is.null(centrality_result)) {
    cent_file <- file.path(output_dir, paste0(prefix, "_centrality.png"))
    png(cent_file, width = VIZ_CONFIG$plot_params$width,
        height = VIZ_CONFIG$plot_params$height, res = VIZ_CONFIG$plot_params$dpi)
    print(get_centrality_plot(centrality_result))
    dev.off()
  }
  
  # 返回导出的文件列表
  files <- c(png_file)
  if(!is.null(centrality_result)) {
    files <- c(files, cent_file)
  }
  
  return(files)
}