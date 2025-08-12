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

# =============================================================================
# 贝叶斯网络分析函数
# =============================================================================

#' 增强的数据验证 - 包含李克特量表检测
#' @param data 输入数据
#' @return 包含李克特量表信息的验证结果
validate_likert_data <- function(data) {
  validation_result <- validate_data(data)  # 调用原有验证
  
  # 添加李克特量表特异性检验
  validation_result$likert_info <- list()
  validation_result$bayesian_ready <- FALSE
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  for(col_name in numeric_cols) {
    if(sum(!is.na(data[[col_name]])) < 10) next  # 跳过缺失值太多的列
    
    unique_vals <- sort(unique(data[[col_name]], na.rm = TRUE))
    
    # 检查是否为李克特量表特征
    if(length(unique_vals) >= 2 && length(unique_vals) <= 10) {
      is_sequential <- all(diff(unique_vals) == 1)
      min_val <- min(unique_vals)
      max_val <- max(unique_vals)
      
      validation_result$likert_info[[col_name]] <- list(
        range = c(min_val, max_val),
        levels = length(unique_vals), 
        is_likert = is_sequential && min_val %in% c(0, 1),
        values = unique_vals
      )
      
      # 警告非标准李克特量表
      if(!is_sequential) {
        validation_result$warnings <- c(validation_result$warnings,
          paste0(col_name, ": 数值不连续，可能影响贝叶斯分析"))
      }
      
      if(!min_val %in% c(0, 1)) {
        validation_result$warnings <- c(validation_result$warnings,
          paste0(col_name, ": 起始值不是0或1，建议检查编码"))
      }
    }
  }
  
  # 评估贝叶斯网络分析适用性
  likert_vars <- length(validation_result$likert_info)
  if(likert_vars >= BAYESIAN_PARAMS$defaults$min_variables) {
    validation_result$bayesian_ready <- TRUE
  } else {
    validation_result$errors <- c(validation_result$errors,
      paste0("贝叶斯网络分析至少需要", BAYESIAN_PARAMS$defaults$min_variables, 
             "个李克特变量，当前只有", likert_vars, "个"))
  }
  
  return(validation_result)
}

#' 智能生成李克特量表的约束规则
#' @param data 数据框  
#' @param scales 识别的量表结构
#' @param constraint_types 约束类型向量
#' @param inter_scale_strength 量表间约束强度
#' @return 黑白名单列表
generate_smart_constraints <- function(data, scales, constraint_types, inter_scale_strength = 0.8) {
  
  blacklist <- NULL
  whitelist <- NULL
  var_names <- names(data)
  
  # 1. 量表间理论约束
  if("inter_scale" %in% constraint_types) {
    
    # 应用配置的理论约束
    for(constraint_name in names(BAYESIAN_PARAMS$theoretical_constraints)) {
      constraint <- BAYESIAN_PARAMS$theoretical_constraints[[constraint_name]]
      
      if(constraint$type == "blacklist") {
        from_vars <- var_names[grepl(constraint$from_pattern, var_names)]
        to_vars <- var_names[grepl(constraint$to_pattern, var_names)]
        
        if(length(from_vars) > 0 && length(to_vars) > 0) {
          constraint_pairs <- expand.grid(from = from_vars, to = to_vars, stringsAsFactors = FALSE)
          blacklist <- rbind(blacklist, constraint_pairs)
        }
      }
    }
  }
  
  # 2. 同量表内远程约束
  if("intra_scale_distant" %in% constraint_types) {
    for(scale_name in names(scales)) {
      scale_vars <- scales[[scale_name]]$items
      scale_vars <- intersect(scale_vars, var_names)  # 只保留实际存在的变量
      
      if(length(scale_vars) > 6) {
        # 禁止距离超过3个位置的题目直接连接
        for(i in 1:(length(scale_vars)-4)) {
          distant_vars <- scale_vars[(i+4):length(scale_vars)]
          distant_constraints <- expand.grid(from = scale_vars[i], to = distant_vars, stringsAsFactors = FALSE)
          blacklist <- rbind(blacklist, distant_constraints)
          # 双向约束
          distant_constraints_rev <- expand.grid(from = distant_vars, to = scale_vars[i], stringsAsFactors = FALSE)
          blacklist <- rbind(blacklist, distant_constraints_rev)
        }
      }
    }
  }
  
  # 3. 时序逻辑约束
  if("temporal_logic" %in% constraint_types) {
    for(scale_name in names(scales)) {
      scale_vars <- scales[[scale_name]]$items
      scale_vars <- intersect(scale_vars, var_names)
      
      # 提取题目编号
      item_numbers <- as.numeric(gsub(".*_", "", scale_vars))
      if(!any(is.na(item_numbers)) && length(item_numbers) > 1) {
        # 按编号排序
        sorted_indices <- order(item_numbers)
        sorted_vars <- scale_vars[sorted_indices] 
        
        # 禁止后面的题目影响前面的题目
        for(i in 1:(length(sorted_vars)-1)) {
          for(j in (i+1):length(sorted_vars)) {
            temporal_constraint <- data.frame(from = sorted_vars[j], to = sorted_vars[i], stringsAsFactors = FALSE)
            blacklist <- rbind(blacklist, temporal_constraint)
          }
        }
      }
    }
  }
  
  # 4. 维度内聚约束（白名单）
  if("dimension_cohesion" %in% constraint_types) {
    for(scale_name in names(scales)) {
      subscales <- scales[[scale_name]]$subscales
      
      if(length(subscales) > 1) {
        for(subscale_name in names(subscales)) {
          subscale_items <- subscales[[subscale_name]]$items
          subscale_items <- intersect(subscale_items, var_names)
          
          if(length(subscale_items) >= 2) {
            # 同维度题目间相邻连接
            for(i in 1:(length(subscale_items)-1)) {
              cohesion_constraint <- data.frame(from = subscale_items[i], 
                                               to = subscale_items[i+1], 
                                               stringsAsFactors = FALSE)
              whitelist <- rbind(whitelist, cohesion_constraint)
            }
          }
        }
      }
    }
  }
  
  return(list(
    blacklist = blacklist,
    whitelist = whitelist,
    summary = list(
      blacklist_rules = ifelse(is.null(blacklist), 0, nrow(blacklist)),
      whitelist_rules = ifelse(is.null(whitelist), 0, nrow(whitelist)),
      constraint_types = constraint_types
    )
  ))
}

#' 解析手动输入的约束规则
#' @param constraint_text 约束规则文本
#' @return 解析结果列表
parse_manual_constraints <- function(constraint_text) {
  if(is.null(constraint_text) || nchar(trimws(constraint_text)) == 0) {
    return(list(constraints = NULL, invalid_lines = c(), valid_count = 0))
  }
  
  lines <- strsplit(constraint_text, "\n")[[1]]
  lines <- trimws(lines[nchar(trimws(lines)) > 0])  # 移除空行
  
  constraints <- NULL
  invalid_lines <- c()
  
  for(i in seq_along(lines)) {
    parts <- strsplit(lines[i], ",")[[1]]
    if(length(parts) == 2) {
      from_var <- trimws(parts[1])
      to_var <- trimws(parts[2])
      
      if(nchar(from_var) > 0 && nchar(to_var) > 0) {
        constraints <- rbind(constraints, data.frame(from = from_var, to = to_var, stringsAsFactors = FALSE))
      } else {
        invalid_lines <- c(invalid_lines, i)
      }
    } else {
      invalid_lines <- c(invalid_lines, i)
    }
  }
  
  return(list(
    constraints = constraints,
    invalid_lines = invalid_lines,
    valid_count = ifelse(is.null(constraints), 0, nrow(constraints))
  ))
}

#' 验证约束规则的有效性
#' @param constraints 约束规则数据框
#' @param available_vars 可用变量列表
#' @return 验证结果列表
validate_constraints <- function(constraints, available_vars) {
  if(is.null(constraints) || nrow(constraints) == 0) {
    return(list(valid = TRUE, errors = c(), warnings = c(), stats = list(total_rules = 0)))
  }
  
  errors <- c()
  warnings <- c()
  
  # 检查变量是否存在
  missing_from <- setdiff(constraints$from, available_vars)
  missing_to <- setdiff(constraints$to, available_vars)
  
  if(length(missing_from) > 0) {
    errors <- c(errors, paste("未找到变量 (from):", paste(missing_from, collapse = ", ")))
  }
  
  if(length(missing_to) > 0) {
    errors <- c(errors, paste("未找到变量 (to):", paste(missing_to, collapse = ", ")))
  }
  
  # 检查自循环
  self_loops <- constraints$from == constraints$to
  if(any(self_loops)) {
    warnings <- c(warnings, paste("发现自循环:", paste(constraints$from[self_loops], collapse = ", ")))
  }
  
  # 检查重复规则
  duplicate_rules <- duplicated(constraints)
  if(any(duplicate_rules)) {
    warnings <- c(warnings, paste("发现重复规则:", sum(duplicate_rules), "个"))
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    stats = list(
      total_rules = nrow(constraints),
      unique_from = length(unique(constraints$from)),
      unique_to = length(unique(constraints$to)),
      self_loops = sum(self_loops),
      duplicates = sum(duplicate_rules)
    )
  ))
}

#' 李克特量表专用贝叶斯网络分析
#' @param data 预处理后的李克特量表数据
#' @param algorithm 学习算法
#' @param score 评分函数  
#' @param bootstrap_n Bootstrap轮数
#' @param threshold 强度阈值
#' @param blacklist 黑名单约束
#' @param whitelist 白名单约束
#' @return 贝叶斯网络分析结果
conduct_likert_bayesian_analysis <- function(data, 
                                           algorithm = "hc",
                                           score = "bge",
                                           bootstrap_n = 1000,
                                           threshold = 0.85,
                                           blacklist = NULL,
                                           whitelist = NULL) {
  
  # 检查bnlearn包
  if(!requireNamespace("bnlearn", quietly = TRUE)) {
    stop("贝叶斯网络分析需要bnlearn包。请安装: install.packages('bnlearn')")
  }
  
  # 确保数据为数值型并移除缺失值
  numeric_data <- data[sapply(data, is.numeric)]
  
  # 将所有integer转换为numeric (bnlearn要求)
  for(col in names(numeric_data)) {
    if(is.integer(numeric_data[[col]])) {
      numeric_data[[col]] <- as.numeric(numeric_data[[col]])
    }
  }
  
  numeric_data <- na.omit(numeric_data)
  
  if(nrow(numeric_data) < 30) {
    stop("贝叶斯网络分析需要至少30个完整观测值")
  }
  
  if(ncol(numeric_data) < 3) {
    stop("贝叶斯网络分析需要至少3个变量")
  }
  
  # 准备算法参数
  algo_args <- list(score = score)
  if(!is.null(blacklist)) {
    algo_args$blacklist <- blacklist
  }
  if(!is.null(whitelist)) {
    algo_args$whitelist <- whitelist
  }
  
  # 学习网络结构
  tryCatch({
    if(algorithm == "hc") {
      learned_net <- bnlearn::hc(numeric_data, score = score, blacklist = blacklist, whitelist = whitelist)
    } else if(algorithm == "tabu") {
      learned_net <- bnlearn::tabu(numeric_data, score = score, blacklist = blacklist, whitelist = whitelist)
    } else if(algorithm == "pc") {
      learned_net <- bnlearn::pc.stable(numeric_data, blacklist = blacklist, whitelist = whitelist)
    } else {
      learned_net <- bnlearn::gs(numeric_data, blacklist = blacklist, whitelist = whitelist)
    }
    
    # Bootstrap稳定性分析
    boot_result <- bnlearn::boot.strength(numeric_data,
                                         R = bootstrap_n,
                                         algorithm = algorithm,
                                         algorithm.args = algo_args)
    
    # 筛选稳定边
    stable_edges <- boot_result[boot_result$strength >= threshold & 
                               boot_result$direction >= 0.5, ]
    
    # 创建平均网络
    avg_network <- bnlearn::averaged.network(boot_result, threshold = threshold)
    
    # 网络评估
    network_score <- bnlearn::score(learned_net, numeric_data, type = score)
    
    return(list(
      learned_network = learned_net,
      averaged_network = avg_network,
      bootstrap_result = boot_result,
      stable_edges = stable_edges,
      network_score = network_score,
      blacklist = blacklist,
      whitelist = whitelist,
      parameters = list(
        algorithm = algorithm,
        score = score,
        bootstrap_n = bootstrap_n,
        threshold = threshold,
        sample_size = nrow(numeric_data),
        variable_count = ncol(numeric_data)
      )
    ))
    
  }, error = function(e) {
    stop(paste("贝叶斯网络分析失败:", e$message))
  })
}

#' 生成贝叶斯网络分析报告
#' @param bayesian_result 贝叶斯网络分析结果
#' @return HTML格式的报告字符串
generate_bayesian_report <- function(bayesian_result) {
  
  params <- bayesian_result$parameters
  stable_count <- nrow(bayesian_result$stable_edges)
  total_possible_edges <- params$variable_count * (params$variable_count - 1)
  
  report_html <- paste0(
    "<h2>🧠 贝叶斯网络分析报告</h2>",
    "<p><strong>生成时间：</strong>", Sys.time(), "</p>",
    "<hr>",
    
    "<h3>📊 分析参数</h3>",
    "<ul>",
    "<li><strong>学习算法：</strong>", BAYESIAN_PARAMS$algorithms[[params$algorithm]], "</li>",
    "<li><strong>评分函数：</strong>", BAYESIAN_PARAMS$score_functions[[params$score]], "</li>",
    "<li><strong>Bootstrap轮数：</strong>", params$bootstrap_n, "</li>",
    "<li><strong>强度阈值：</strong>", params$threshold, "</li>",
    "<li><strong>样本量：</strong>", params$sample_size, "</li>",
    "<li><strong>变量数：</strong>", params$variable_count, "</li>",
    "</ul>",
    
    "<h3>🔗 网络结构</h3>",
    "<ul>",
    "<li><strong>稳定边数量：</strong>", stable_count, " / ", total_possible_edges, " 可能的边</li>",
    "<li><strong>网络密度：</strong>", round(stable_count / total_possible_edges * 100, 2), "%</li>",
    "<li><strong>网络得分：</strong>", round(bayesian_result$network_score, 3), "</li>",
    "</ul>"
  )
  
  # 约束信息
  if(!is.null(bayesian_result$blacklist) || !is.null(bayesian_result$whitelist)) {
    report_html <- paste0(report_html,
      "<h3>⚖️ 约束规则</h3>",
      "<ul>"
    )
    
    if(!is.null(bayesian_result$blacklist)) {
      report_html <- paste0(report_html,
        "<li><strong>黑名单规则：</strong>", nrow(bayesian_result$blacklist), " 个禁止连接</li>"
      )
    }
    
    if(!is.null(bayesian_result$whitelist)) {
      report_html <- paste0(report_html,
        "<li><strong>白名单规则：</strong>", nrow(bayesian_result$whitelist), " 个强制连接</li>"
      )
    }
    
    report_html <- paste0(report_html, "</ul>")
  }
  
  # 解释和建议
  report_html <- paste0(report_html,
    "<h3>📈 结果解释</h3>",
    "<p>贝叶斯网络分析识别了变量间的<strong>有向因果关系</strong>，不同于无向网络分析：</p>",
    "<ul>",
    "<li><strong>有向边</strong>表示可能的因果关系方向</li>",
    "<li><strong>边强度</strong>反映关系的稳定性和可信度</li>",
    "<li><strong>网络密度</strong>显示变量间连接的紧密程度</li>",
    "</ul>",
    
    "<h3>💡 应用建议</h3>",
    "<ul>",
    "<li>关注强度 ≥ 0.85 的边，这些关系最为稳定</li>",
    "<li>结合理论知识解释因果关系的合理性</li>",
    "<li>可与无向网络结果对比，获得更全面的理解</li>",
    "</ul>"
  )
  
  return(report_html)
}