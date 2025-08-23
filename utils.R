# =============================================================================
# 辅助函数库 - Utility Functions
# 用于数据处理、验证和分析的辅助函数
# =============================================================================

source("config.R")
source("story_generator.R")
source("scale_calculator.R")

# =============================================================================
# 数据预处理函数
# =============================================================================

#' 标准化性别变量
#' 支持多种性别变量格式：1/2, 1/0, male/female, 男/女等
#' @param gender_var 性别变量向量
#' @param var_name 变量名（用于智能识别）
#' @return 标准化后的性别变量（1=男性，2=女性）
standardize_gender_variable <- function(gender_var, var_name = "") {
  if(is.null(gender_var) || length(gender_var) == 0) {
    return(gender_var)
  }
  
  # 检查是否为性别相关变量
  var_name_lower <- tolower(var_name)
  is_gender_var <- any(grepl("gender|sex|性别|gender|male|female", var_name_lower, ignore.case = TRUE))
  
  # 如果不是性别变量，直接返回原变量
  if(!is_gender_var) {
    return(gender_var)
  }
  
  cat("检测到性别变量:", var_name, "\n")
  
  # 移除缺失值进行分析
  valid_values <- gender_var[!is.na(gender_var)]
  unique_vals <- unique(valid_values)
  
  cat("原始唯一值:", paste(unique_vals, collapse = ", "), "\n")
  
  # 创建标准化后的变量
  standardized_var <- gender_var
  
  # 如果是字符型变量
  if(is.character(gender_var) || is.factor(gender_var)) {
    gender_char <- as.character(gender_var)
    gender_lower <- tolower(gender_char)
    
    # 男性的各种表示
    male_patterns <- c("male", "m", "man", "boy", "男", "男性", "1")
    # 女性的各种表示  
    female_patterns <- c("female", "f", "woman", "girl", "女", "女性", "2")
    
    for(pattern in male_patterns) {
      standardized_var[grepl(pattern, gender_lower, fixed = TRUE)] <- 1
    }
    for(pattern in female_patterns) {
      standardized_var[grepl(pattern, gender_lower, fixed = TRUE)] <- 2
    }
    
    # 转换为数值型
    standardized_var <- as.numeric(standardized_var)
    
  } else if(is.numeric(gender_var)) {
    # 数值型变量处理
    if(length(unique_vals) == 2) {
      sorted_vals <- sort(unique_vals)
      
      # 处理0/1编码 -> 1/2编码
      if(all(sorted_vals == c(0, 1))) {
        cat("检测到0/1编码，转换为1/2编码\n")
        standardized_var[gender_var == 0] <- 1  # 0 -> 1 (男性)
        standardized_var[gender_var == 1] <- 2  # 1 -> 2 (女性)
      }
      # 处理1/2编码（保持不变）
      else if(all(sorted_vals == c(1, 2))) {
        cat("检测到1/2编码，保持不变\n")
        # 保持原样
      }
      # 其他数值编码，映射到最小值=1，最大值=2
      else {
        cat("检测到其他数值编码，映射为1/2\n")
        min_val <- min(sorted_vals)
        max_val <- max(sorted_vals)
        standardized_var[gender_var == min_val] <- 1
        standardized_var[gender_var == max_val] <- 2
      }
    }
  }
  
  # 验证结果
  final_unique <- unique(standardized_var[!is.na(standardized_var)])
  cat("标准化后唯一值:", paste(final_unique, collapse = ", "), "\n")
  
  # 确保只有1和2两个值
  if(!all(final_unique %in% c(1, 2))) {
    warning("性别变量标准化后仍有异常值，可能影响分组分析")
  }
  
  return(standardized_var)
}

# =============================================================================
# 统一网络配置函数
# =============================================================================

#' 创建统一的网络参数配置
#' @param stored_colors 保存的颜色信息
#' @param stored_groups 保存的分组信息  
#' @param stored_layout 保存的布局信息
#' @param network_type 网络类型
#' @return 统一的网络参数配置
create_unified_network_params <- function(stored_colors = NULL, stored_groups = NULL, stored_layout = NULL, network_type = "main") {
  # 统一配色方案
  unified_colors <- if(!is.null(stored_colors)) {
    stored_colors
  } else {
    c("#1ba784","#63bbd0","#f87599","#fed71a",
      "#d1c2d3","#304fb0","#c6dfc8","#a8456b","#2486b9",
      "#e16c96","#fc8c23","#280c1c",
      "#fbb957","#de1c31","#ee3f4d",
      "#c0c4c3","#c6e6e8",
      "#12a182","#eb3c70","#eaad1a","#45b787","#d11a2d",
      "#eea08c","#cfccc9",
      "#2b1216","#61649f","#93b5cf","#c4cbcf",
      "#c4d7d6","#248067","#fbda41","#f1f0ed")
  }
  
  # 统一边颜色
  unified_edge_colors <- list(
    posCol = c("#2376b7", "#134857"),
    negCol = c("#d2568c", "#62102e")
  )
  
  return(list(
    colors = unified_colors,
    groups = stored_groups,
    layout = stored_layout,
    edge_colors = unified_edge_colors
  ))
}

# =============================================================================
# 贝叶斯网络可视化函数
# =============================================================================

#' 创建贝叶斯网络图
#' @param bayesian_result 贝叶斯分析结果
#' @param colors 颜色配置
#' @param groups 分组信息
#' @param layout 布局信息
#' @param title 图标题
#' @param network_type 网络类型
#' @return 贝叶斯网络图
create_bayesian_network_plot <- function(bayesian_result, colors = NULL, groups = NULL, layout = NULL, title = "Bayesian Network", network_type = "structure") {
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
  
  zposCol <- c("#2376b7","#134857") 
  znegCol <- c("#d2568c","#62102e")
  
  # 构建基础参数 - 严格按照您的模板
  args <- list(
    threshold = 0.05,
    edge.labels = TRUE,
    posCol = zposCol,
    negCol = znegCol,
    color = if(!is.null(colors)) colors else zcolor,
    legend = TRUE,
    legend.cex = 0.4,
    vsize = 6,
    esize = 5,
    asize = 5,
    edge.label.cex = 1,
    title = title
  )
  
  # 添加分组信息
  if(!is.null(groups)) {
    # 将分组信息转换为变量对应的组名
    variable_names <- names(bayesian_result$data)
    group_assignment <- rep("未分组", length(variable_names))
    names(group_assignment) <- variable_names
    
    # 为每个变量分配组名
    for(group_name in names(groups)) {
      scales_in_group <- groups[[group_name]]
      for(scale_name in scales_in_group) {
        # 找到属于这个量表的变量
        matching_vars <- variable_names[
          startsWith(variable_names, paste0(scale_name, "_")) |
          grepl(paste0("_", scale_name, "_"), variable_names) |
          endsWith(variable_names, paste0("_", scale_name)) |
          variable_names == scale_name
        ]
        group_assignment[matching_vars] <- group_name
      }
    }
    args$groups <- group_assignment
  }
  
  if(network_type == "averaged") {
    # 平均网络：显示权重强度
    if(!is.null(bayesian_result$stable_edges) && nrow(bayesian_result$stable_edges) > 0) {
      # 构建邻接矩阵
      variable_names <- names(bayesian_result$data)
      n_vars <- length(variable_names)
      adj_matrix <- matrix(0, n_vars, n_vars)
      rownames(adj_matrix) <- colnames(adj_matrix) <- variable_names
      
      stable_edges <- bayesian_result$stable_edges
      for(i in 1:nrow(stable_edges)) {
        from_var <- stable_edges$from[i]
        to_var <- stable_edges$to[i]
        strength <- stable_edges$strength[i]
        
        from_idx <- which(variable_names == from_var)
        to_idx <- which(variable_names == to_var)
        if(length(from_idx) > 0 && length(to_idx) > 0) {
          adj_matrix[from_idx, to_idx] <- strength
        }
      }
      
      args$data <- adj_matrix
      args$input <- "adjacency"
    }
  } else {
    # 结构网络：仅显示连接
    args$data <- bayesian_result$data
  }
  
  # 调用quickNet
  network_result <- do.call(quickNet::quickNet, args)
  return(network_result)
}

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

#' 简化的数据验证函数 - 只检查缺失值和数值类型
#' @param data 输入数据
#' @return 验证结果列表
validate_data <- function(data) {
  validation_result <- list(
    valid = TRUE,
    warnings = c(),
    errors = c(),
    statistics = list(),
    data_types = list()
  )
  
  n_subjects <- nrow(data)
  n_variables <- ncol(data)
  
  # 基本数据检查
  if(n_subjects == 0) {
    validation_result$errors <- c(validation_result$errors, "数据为空")
    validation_result$valid <- FALSE
    return(validation_result)
  }
  
  if(n_variables == 0) {
    validation_result$errors <- c(validation_result$errors, "没有变量列")
    validation_result$valid <- FALSE
    return(validation_result)
  }
  
  # 检查缺失值
  missing_rates <- colSums(is.na(data)) / n_subjects
  high_missing_vars <- names(missing_rates)[missing_rates > 0.5]
  
  if(length(high_missing_vars) > 0) {
    validation_result$warnings <- c(validation_result$warnings,
                                   paste0("以下变量缺失值超过50%：", paste(high_missing_vars, collapse = ", ")))
  }
  
  # 整体完整率
  complete_cases <- sum(complete.cases(data))
  complete_rate <- complete_cases / n_subjects
  
  # 检查数据类型并尝试转换
  numeric_conversion_summary <- list()
  converted_vars <- character(0)
  
  for(col_name in names(data)) {
    col_data <- data[[col_name]]
    original_type <- class(col_data)[1]
    
    # 跳过明显的ID列和字符列
    if(grepl("^(id|ID|uuid|UUID|name|Name|gender|Gender|city|City)", col_name, ignore.case = TRUE)) {
      numeric_conversion_summary[[col_name]] <- list(
        original_type = original_type,
        converted = FALSE,
        reason = "ID或分类变量"
      )
      next
    }
    
    # 尝试数值转换
    if(original_type %in% c("character", "factor")) {
      tryCatch({
        numeric_version <- as.numeric(as.character(col_data))
        non_na_converted <- sum(!is.na(numeric_version))
        non_na_original <- sum(!is.na(col_data))
        
        # 如果转换后非空值的数量相同或接近，则认为转换成功
        if(non_na_converted >= non_na_original * 0.8) {
          data[[col_name]] <- numeric_version
          converted_vars <- c(converted_vars, col_name)
          
          numeric_conversion_summary[[col_name]] <- list(
            original_type = original_type,
            converted = TRUE,
            success_rate = non_na_converted / non_na_original
          )
        } else {
          numeric_conversion_summary[[col_name]] <- list(
            original_type = original_type,
            converted = FALSE,
            reason = "转换失败率过高"
          )
        }
      }, error = function(e) {
        numeric_conversion_summary[[col_name]] <- list(
          original_type = original_type,
          converted = FALSE,
          reason = paste("转换错误:", e$message)
        )
      })
    } else if(original_type == "integer") {
      # 整数转换为数值
      data[[col_name]] <- as.numeric(col_data)
      converted_vars <- c(converted_vars, col_name)
      
      numeric_conversion_summary[[col_name]] <- list(
        original_type = original_type,
        converted = TRUE,
        reason = "整数转数值"
      )
    } else {
      numeric_conversion_summary[[col_name]] <- list(
        original_type = original_type,
        converted = FALSE,
        reason = "已是数值类型或无需转换"
      )
    }
  }
  
  # 汇总转换信息
  if(length(converted_vars) > 0) {
    validation_result$warnings <- c(validation_result$warnings,
                                   paste0("已自动转换", length(converted_vars), "个变量为数值类型"))
  }
  
  # 统计信息
  validation_result$statistics <- list(
    n_subjects = n_subjects,
    n_variables = n_variables,
    complete_cases = complete_cases,
    complete_rate = complete_rate,
    missing_rates = missing_rates,
    converted_variables = length(converted_vars)
  )
  
  validation_result$data_types <- numeric_conversion_summary
  validation_result$processed_data <- data  # 返回处理后的数据
  
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
safe_network_analysis <- function(data, threshold = 0.05, edge_labels = TRUE, colors = NULL, groups = NULL, shape = NULL, title = NULL, layout = NULL, vsize = NULL, ...) {
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
  
  zposCol <- c("#2376b7","#134857") 
  znegCol <- c("#d2568c","#62102e")
  
  # 构建quickNet参数 - 严格按照您的模板
  args <- list(
    data = data,
    threshold = threshold,
    edge.labels = edge_labels,
    posCol = zposCol,
    negCol = znegCol,
    color = if(!is.null(colors)) colors else zcolor,
    legend = TRUE,
    legend.cex = 0.4,
    vsize = if(!is.null(vsize)) vsize else 6,
    esize = 5,
    asize = 5,
    edge.label.cex = 1
  )
  
  # 添加其他参数
  if(!is.null(groups)) args$groups <- groups
  if(!is.null(shape)) args$shape <- shape
  if(!is.null(title)) args$title <- title
  if(!is.null(layout)) args$layout <- layout
  
  # 处理估计方法参数
  dots <- list(...)
  if("estimator" %in% names(dots)) {
    estimator <- dots$estimator
    if(estimator == "mgm") {
      # MGM特殊处理：调整参数以提高边选择敏感性
      args$method <- "mgm"
      args$gamma <- 0.0  # 降低gamma以提高敏感性
      cat("🔧 使用MGM方法，调整参数以提高边检测敏感性\n")
    } else if(estimator == "EBICglasso") {
      # EBICglasso是默认方法
      args$method <- "EBICglasso"
    } else {
      args$method <- estimator
    }
    # 移除estimator参数，避免传递给quickNet
    dots$estimator <- NULL
  }
  
  args <- c(args, dots)
  
  # 调用quickNet
  tryCatch({
    network_result <- do.call(quickNet::quickNet, args)
    
    # 检查是否获得了空网络
    if(!is.null(network_result) && 
       is.list(network_result) && 
       !is.null(network_result$graph) && 
       !is.null(network_result$graph$adjacency)) {
      
      # 计算网络边数
      adj_matrix <- network_result$graph$adjacency
      n_edges <- sum(adj_matrix != 0) / 2  # 无向网络，除以2
      
      if(n_edges == 0) {
        cat("⚠️ 警告：检测到空网络（无边），这可能导致后续分析问题\n")
        cat("建议：\n")
        cat("  - 降低阈值参数 (threshold < 0.05)\n")
        cat("  - 检查数据相关性是否过低\n") 
        cat("  - 考虑增加样本量\n")
        if("method" %in% names(args) && args$method == "mgm") {
          cat("  - 对于MGM方法，可以尝试调整gamma参数\n")
        }
      } else {
        cat("✅ 网络构建成功，包含", n_edges, "条边\n")
      }
    }
    
    return(network_result)
    
  }, error = function(e) {
    cat("❌ 网络分析失败:", e$message, "\n")
    
    # 为MGM提供更具体的错误处理建议
    if("method" %in% names(args) && args$method == "mgm") {
      cat("MGM分析失败可能的原因：\n")
      cat("  - 数据包含无穷值或缺失值\n")
      cat("  - 数据类型不符合MGM要求\n")
      cat("  - 样本量相对于变量数过小\n")
      cat("  - 尝试切换到EBICglasso方法\n")
    }
    
    # 重新抛出原始错误
    stop(e$message)
  })
  return(network_result)
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
# 统一网络可视化配置函数
# =============================================================================

#' 创建统一的网络可视化参数
#' @param stored_colors 保存的配色方案
#' @param stored_groups 保存的分组信息
#' @param stored_layout 保存的布局信息
#' @param network_type 网络类型 ("main", "bridge", "compare", "bayesian")
#' @return 统一的可视化参数列表
create_unified_network_params <- function(stored_colors = NULL, stored_groups = NULL, stored_layout = NULL, network_type = "main") {
  
  # 统一的配色方案
  unified_colors <- if(!is.null(stored_colors)) {
    stored_colors
  } else {
    # 使用标准配色 - 参考用户提供的zcolor
    c("#1ba784","#63bbd0","#f87599","#fed71a",
      "#d1c2d3","#304fb0","#c6dfc8","#a8456b","#2486b9",
      "#e16c96","#fc8c23","#280c1c",
      "#fbb957","#de1c31","#ee3f4d",
      "#c0c4c3","#c6e6e8",
      "#12a182","#eb3c70","#eaad1a","#45b787","#d11a2d",
      "#eea08c","#cfccc9",
      "#2b1216","#61649f","#93b5cf","#c4cbcf",
      "#c4d7d6","#248067","#fbda41","#f1f0ed")
  }
  
  # 统一的qgraph参数
  unified_params <- list(
    colors = unified_colors,
    groups = stored_groups,
    layout = stored_layout,
    
    # 统一的边颜色（参考用户代码）
    posCol = c("#2376b7", "#134857"),
    negCol = c("#d2568c", "#62102e"),
    
    # 统一的显示参数
    label.cex = 1.1,
    vsize = 6,
    esize = 5,
    asize = 5,
    edge.label.cex = 1,
    legend = TRUE,
    legend.cex = 0.4,
    GLratio = 7,
    layoutOffset = c(0.03, 0)
  )
  
  # 根据网络类型调整特定参数
  if(network_type == "bridge") {
    # 桥接网络的特殊配置
    unified_params$vsize <- 6
    unified_params$edge.labels <- TRUE
  } else if(network_type == "bayesian") {
    # 贝叶斯网络的特殊配置
    unified_params$directed <- TRUE
    unified_params$arrows <- TRUE
  }
  
  return(unified_params)
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

#' 获取中心性图
#' @param centrality_result 中心性结果
#' @return ggplot对象或plot输出
get_centrality_plot <- function(centrality_result, prefix = "centrality", path = ".", device = "pdf", width = 6, height = 4.5, ...) {
  if(is.null(centrality_result)) {
    return(NULL)
  }
  
  # 尝试使用quickNet包的内置绘图功能
  tryCatch({
    if(requireNamespace("quickNet", quietly = TRUE)) {
      # 检查centrality_result的结构
      if(is.list(centrality_result)) {
        # 如果是列表结构，检查是否有centralityPlot组件
        if("centralityPlot" %in% names(centrality_result)) {
          # 使用已存在的centralityPlot
          if(inherits(centrality_result$centralityPlot, "ggplot")) {
            print(centrality_result$centralityPlot)
          } else {
            # 如果centralityPlot不是ggplot对象，尝试直接绘制
            plot(centrality_result$centralityPlot)
          }
        } else if("centrality" %in% names(centrality_result)) {
          # 如果有centrality数据，使用quickNet绘制
          quickNet::centralityPlot(centrality_result$centrality)
        } else {
          # 尝试直接将结果传递给centralityPlot
          quickNet::centralityPlot(centrality_result)
        }
      } else if(inherits(centrality_result, c("qgraph", "bn.strength", "data.frame"))) {
        # 直接使用quickNet的centrality绘图函数
        quickNet::centralityPlot(centrality_result)
      } else {
        # 尝试直接绘制
        plot(centrality_result)
      }
    } else {
      # 备用方案：创建简单的中心性图
      create_enhanced_centrality_plot(centrality_result)
    }
  }, error = function(e) {
    # 如果quickNet方法失败，尝试使用备用方案
    tryCatch({
      create_enhanced_centrality_plot(centrality_result)
    }, error = function(e2) {
      # 最后的备用方案：显示错误信息并尝试基础绘图
      cat("中心性图生成失败，尝试基础绘图\n")
      cat("错误信息:", e$message, "\n")
      cat("中心性结果结构:", str(centrality_result), "\n")
      
      # 尝试基础绘图
      if(is.list(centrality_result) && length(centrality_result) > 0) {
        plot.new()
        text(0.5, 0.6, "中心性图", cex = 1.5, font = 2)
        text(0.5, 0.4, paste("包含", length(centrality_result), "个组件"), cex = 1.2)
        text(0.5, 0.2, "请检查数据格式", cex = 1, col = "orange")
      } else {
        plot.new()
        text(0.5, 0.5, paste("中心性图生成失败:", e$message), cex = 1.2, col = "red")
      }
    })
  })
}

#' 为Web显示的中心性图函数
#' @param centrality_result 中心性结果
#' @return 无返回值，直接绘制图形
plot_centrality_for_display <- function(centrality_result) {
  quickNet::get_centrality_plot(centrality_result)
}

#' 获取组间比较图
#' @param compare_result NetCompare结果
#' @param reference_network 参考网络对象
#' @param plot_type 图类型："all", "positive", "negative"
#' @return 组间比较图
get_compare_plot <- function(compare_result, reference_network, plot_type = "all") {
  if(is.null(compare_result)) {
    return(NULL)
  }
  
  tryCatch({
    if(requireNamespace("quickNet", quietly = TRUE)) {
      # 检查可用的绘图函数
      available_functions <- ls("package:quickNet")
      
      if("get_compare_plot" %in% available_functions) {
        # 使用get_compare_plot函数
        if(plot_type == "positive") {
          quickNet::get_compare_plot(compare_result, reference_network, 
                                    prefix = "pos", width = 6, height = 4.5, 
                                    plot_positive = TRUE, plot_negative = FALSE)
        } else if(plot_type == "negative") {
          quickNet::get_compare_plot(compare_result, reference_network,
                                    prefix = "neg", width = 6, height = 4.5,
                                    plot_positive = FALSE, plot_negative = TRUE)
        } else {
          quickNet::get_compare_plot(compare_result, reference_network,
                                    prefix = "diff", width = 6, height = 4.5)
        }
      } else if("plot_comparison" %in% available_functions) {
        # 尝试使用plot_comparison函数
        quickNet::plot_comparison(compare_result, reference_network)
      } else if("plotDifference" %in% available_functions) {
        # 尝试使用plotDifference函数
        quickNet::plotDifference(compare_result)
      } else {
        # 如果没有找到合适的函数，使用备用方案
        cat("未找到quickNet组间比较绘图函数，使用备用方案\n")
        create_simple_compare_plot(compare_result, plot_type)
      }
    } else {
      # 备用方案：创建简单的差异图
      create_simple_compare_plot(compare_result, plot_type)
    }
  }, error = function(e) {
    # 如果quickNet方法失败，使用备用方案
    cat("quickNet绘图失败:", e$message, "\n")
    tryCatch({
      create_simple_compare_plot(compare_result, plot_type)
    }, error = function(e2) {
      # 最后的备用方案：显示错误信息
      plot.new()
      text(0.5, 0.5, paste("组间比较图生成失败:", e$message), cex = 1.2, col = "red")
    })
  })
}

#' 创建简单的组间比较图（备用方案）
#' @param compare_result NetCompare结果
#' @param plot_type 图类型
create_simple_compare_plot <- function(compare_result, plot_type = "all") {
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "需要ggplot2包来生成比较图", cex = 1.2, col = "red")
    return()
  }
  
  library(ggplot2, quietly = TRUE)
  
  # 检查compare_result的结构
  cat("比较结果结构:", str(compare_result), "\n")
  
  # 尝试多种数据提取方式
  diff_data <- NULL
  
  if(is.list(compare_result)) {
    # 方式1：检查standard NetCompare输出格式
    if(!is.null(compare_result$difference) && !is.null(compare_result$p.adjust)) {
      diff_data <- data.frame(
        edge = names(compare_result$difference),
        difference = compare_result$difference,
        p_value = compare_result$p.adjust,
        significant = compare_result$p.adjust < 0.05,
        stringsAsFactors = FALSE
      )
    }
    # 方式2：检查是否有pval字段
    else if(!is.null(compare_result$difference) && !is.null(compare_result$pval)) {
      diff_data <- data.frame(
        edge = names(compare_result$difference),
        difference = compare_result$difference,
        p_value = compare_result$pval,
        significant = compare_result$pval < 0.05,
        stringsAsFactors = FALSE
      )
    }
    # 方式3：检查是否有直接的结果矩阵
    else if(!is.null(compare_result$result)) {
      result_df <- compare_result$result
      if("difference" %in% names(result_df) && "p_value" %in% names(result_df)) {
        diff_data <- data.frame(
          edge = rownames(result_df),
          difference = result_df$difference,
          p_value = result_df$p_value,
          significant = result_df$p_value < 0.05,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if(!is.null(diff_data) && nrow(diff_data) > 0) {
    # 根据plot_type过滤数据
    if(plot_type == "positive") {
      diff_data <- diff_data[diff_data$difference > 0, ]
    } else if(plot_type == "negative") {
      diff_data <- diff_data[diff_data$difference < 0, ]
    }
    
    if(nrow(diff_data) > 0) {
      # 创建简单的柱状图，使用与网络图一致的颜色
      diff_data$color_type <- ifelse(diff_data$difference > 0, "positive", "negative")
      
      p <- ggplot(diff_data, aes(x = reorder(edge, difference), y = difference, 
                                fill = color_type, alpha = significant)) +
        geom_col() +
        scale_fill_manual(values = c("positive" = "#2376b7", "negative" = "#d2568c"),
                         labels = c("positive" = "组1>组2", "negative" = "组2>组1")) +
        scale_alpha_manual(values = c("FALSE" = 0.5, "TRUE" = 1.0),
                          labels = c("FALSE" = "不显著", "TRUE" = "显著")) +
        labs(title = paste0("组间网络差异"),
             x = "网络边", y = "差异值 (组1 - 组2)",
             fill = "差异方向", alpha = "显著性") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
      
      print(p)
    } else {
      plot.new()
      text(0.5, 0.5, paste0("没有", plot_type, "类型的差异数据"), cex = 1.2, col = "orange")
    }
  } else {
    # 显示比较结果的基本信息
    plot.new()
    if(is.list(compare_result)) {
      available_fields <- names(compare_result)
      text(0.5, 0.7, "组间比较结果", cex = 1.5, font = 2)
      text(0.5, 0.5, paste("可用字段:", paste(available_fields, collapse = ", ")), cex = 1)
      text(0.5, 0.3, "请检查NetCompare结果格式", cex = 1, col = "orange")
    } else {
      text(0.5, 0.5, "比较结果数据格式不支持", cex = 1.2, col = "red")
    }
  }
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
                                           score = "bge",  # 默认使用BGe评分，适合连续数据
                                           bootstrap_n = 1000,
                                           threshold = 0.85,
                                           direction_threshold = 0.5,
                                           blacklist = NULL,
                                           whitelist = NULL) {
  
  # 检查必要的包
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
  
  # 检查数据类型并调整评分函数
  # 心理量表数据通常是连续/序数数据，需要使用Gaussian评分函数
  continuous_scores <- c("bge", "loglik-g", "aic-g", "bic-g")
  discrete_scores <- c("bic", "aic", "loglik", "k2", "bdj")
  
  if(score %in% discrete_scores) {
    cat("警告：检测到连续数据，将", score, "调整为适合连续数据的BGe评分\n")
    score <- "bge"
  }
  n_vars <- ncol(numeric_data)
  n_obs <- nrow(numeric_data)
  
  if(n_obs < 30) {
    stop("贝叶斯网络分析需要至少30个完整观测值")
  }
  
  if(n_vars < 3) {
    stop("贝叶斯网络分析需要至少3个变量")
  }
  
  # 数据质量诊断
  cat("贝叶斯网络分析数据诊断:\n")
  cat("样本量:", n_obs, "\n")
  cat("变量数:", n_vars, "\n")
  cat("理论最大边数:", (n_vars * (n_vars - 1) / 2), "\n")
  
  # 检查数据相关性
  cor_matrix <- cor(numeric_data)
  eigen_values <- eigen(cor_matrix)$values
  cat("特征值(前5个):", head(eigen_values, 5), "\n")
  
  # 准备算法参数
  algo_args <- list()
  if(algorithm %in% c("hc", "tabu")) {
    algo_args$score <- score
  }
  if(!is.null(blacklist)) {
    algo_args$blacklist <- blacklist
  }
  if(!is.null(whitelist)) {
    algo_args$whitelist <- whitelist
  }
  
  # 学习网络结构 - 扩展算法选择
  tryCatch({
    cat("使用", algorithm, "算法学习网络结构...\n")
    
    if(algorithm == "hc") {
      learned_net <- bnlearn::hc(numeric_data, score = score, blacklist = blacklist, whitelist = whitelist)
    } else if(algorithm == "tabu") {
      learned_net <- bnlearn::tabu(numeric_data, score = score, blacklist = blacklist, whitelist = whitelist)
    } else if(algorithm == "pc") {
      learned_net <- bnlearn::pc.stable(numeric_data, blacklist = blacklist, whitelist = whitelist)
    } else if(algorithm == "iamb") {
      learned_net <- bnlearn::iamb(numeric_data, blacklist = blacklist, whitelist = whitelist)
    } else if(algorithm == "iamb.fdr") {
      learned_net <- bnlearn::iamb.fdr(numeric_data, blacklist = blacklist, whitelist = whitelist)
    } else if(algorithm == "mmhc") {
      learned_net <- bnlearn::mmhc(numeric_data, blacklist = blacklist, whitelist = whitelist)
    } else if(algorithm == "rsmax2") {
      learned_net <- bnlearn::rsmax2(numeric_data, blacklist = blacklist, whitelist = whitelist)
    } else {
      learned_net <- bnlearn::gs(numeric_data, blacklist = blacklist, whitelist = whitelist)
    }
    
    cat("学习得到", nrow(learned_net$arcs), "条边\n")
    
    # 参数拟合 - 使用bn.fit估计条件概率分布
    cat("进行参数估计...\n")
    bn_fitted <- bnlearn::bn.fit(learned_net, numeric_data)
    
    # 计算模型指标 - 根据数据类型选择合适的评分函数
    network_score <- bnlearn::score(learned_net, numeric_data, type = score)
    
    # 对于连续数据，使用BIC-G (Gaussian BIC)或其他适合的评分
    tryCatch({
      # 尝试计算BIC，如果失败则使用BGe评分
      bic_score <- bnlearn::score(learned_net, numeric_data, type = "bic-g")
    }, error = function(e) {
      # 如果BIC-G也不可用，则使用BGe评分作为替代
      bic_score <- bnlearn::score(learned_net, numeric_data, type = "bge")
      cat("注意：使用BGe评分替代BIC（连续数据）\n")
    })
    
    # 对数似然评分（适用于连续数据）
    loglik_score <- bnlearn::score(learned_net, numeric_data, type = "loglik-g")
    
    cat("网络评分 -", score, ":", round(network_score, 2), "\n")
    cat("BIC:", round(bic_score, 2), "\n")
    cat("Log-likelihood:", round(loglik_score, 2), "\n")
    
    # Bootstrap稳定性分析
    cat("进行Bootstrap稳定性分析(", bootstrap_n, "轮)...\n")
    boot_result <- bnlearn::boot.strength(numeric_data,
                                         R = bootstrap_n,
                                         algorithm = algorithm,
                                         algorithm.args = algo_args,
                                         debug = FALSE)
    
    # 筛选稳定边 - 使用更严格的标准
    stable_edges <- boot_result[boot_result$strength >= threshold & 
                               boot_result$direction >= direction_threshold, ]
    
    cat("稳定边数量:", nrow(stable_edges), "/", nrow(boot_result), "\n")
    
    # 创建平均网络
    avg_network <- bnlearn::averaged.network(boot_result, threshold = threshold)
    
    # 交叉验证评估 - 使用适合连续数据的损失函数
    cat("进行交叉验证...\n")
    cv_result <- bnlearn::bn.cv(numeric_data, learned_net, loss = "logl-g", k = 10, debug = FALSE)
    cv_loss <- sapply(cv_result, function(x) x$loss)
    mean_cv_loss <- mean(cv_loss)
    sd_cv_loss <- sd(cv_loss)
    
    cat("交叉验证损失:", round(mean_cv_loss, 4), "±", round(sd_cv_loss, 4), "\n")
    
    # 创建强度图数据（用于qgraph可视化）
    strength_plot_data <- NULL
    if(requireNamespace("bnlearn", quietly = TRUE)) {
      tryCatch({
        strength_plot_data <- bnlearn::strength.plot(avg_network, boot_result, shape = "ellipse", render = FALSE)
      }, error = function(e) {
        cat("强度图生成失败:", e$message, "\n")
      })
    }
    
    return(list(
      # 网络结构
      learned_network = learned_net,
      averaged_network = avg_network,
      fitted_network = bn_fitted,
      
      # Bootstrap结果
      bootstrap_result = boot_result,
      stable_edges = stable_edges,
      strength_plot_data = strength_plot_data,
      
      # 模型评估指标
      network_score = network_score,
      bic_score = bic_score,
      loglik_score = loglik_score,
      
      # 交叉验证结果
      cv_result = cv_result,
      cv_loss = cv_loss,
      mean_cv_loss = mean_cv_loss,
      sd_cv_loss = sd_cv_loss,
      
      # 约束规则
      blacklist = blacklist,
      whitelist = whitelist,
      
      # 参数和诊断信息
      parameters = list(
        algorithm = algorithm,
        score = score,
        bootstrap_n = bootstrap_n,
        threshold = threshold,
        direction_threshold = direction_threshold,
        sample_size = n_obs,
        variable_count = n_vars,
        edge_count = nrow(learned_net$arcs),
        stable_edge_count = nrow(stable_edges),
        edge_density = nrow(learned_net$arcs) / (n_vars * (n_vars - 1) / 2)
      ),
      
      # 原始数据和诊断
      data = numeric_data,
      correlation_matrix = cor_matrix,
      eigen_values = eigen_values
    ))
    
  }, error = function(e) {
    stop(paste("贝叶斯网络分析失败:", e$message))
  })
}

#' 创建继承网络分析样式的贝叶斯网络可视化
#' @param bayesian_result 贝叶斯分析结果
#' @param colors 颜色配置（继承自网络分析）
#' @param groups 变量分组（继承自网络分析）
#' @param layout 布局（继承自网络分析）
#' @param title 图标题
#' @return qgraph对象
create_bayesian_network_plot <- function(bayesian_result, 
                                        colors = NULL, 
                                        groups = NULL, 
                                        layout = NULL,
                                        title = "贝叶斯网络结构",
                                        network_type = "structure") {
  
  # 检查必要的包
  if(!requireNamespace("qgraph", quietly = TRUE)) {
    stop("需要qgraph包进行可视化")
  }
  
  # 根据network_type选择不同的网络数据
  if(network_type == "averaged" && !is.null(bayesian_result$averaged_network)) {
    # Figure5b: 使用平均网络（带权重强度值）
    network_to_plot <- bayesian_result$averaged_network
    use_weights <- TRUE
  } else {
    # Figure5a: 使用学习网络结构（无权重）
    network_to_plot <- bayesian_result$learned_network
    use_weights <- FALSE
  }
  
  # 获取变量名
  variable_names <- names(bayesian_result$data)
  n_vars <- length(variable_names)
  
  # 构建邻接矩阵
  adj_matrix <- matrix(0, nrow = n_vars, ncol = n_vars)
  rownames(adj_matrix) <- colnames(adj_matrix) <- variable_names
  
  if(use_weights && network_type == "averaged") {
    # Figure5b: 平均网络，显示权重强度
    # 使用稳定边构建有向无环图
    if(!is.null(bayesian_result$stable_edges) && nrow(bayesian_result$stable_edges) > 0) {
      stable_edges <- bayesian_result$stable_edges
      for(i in 1:nrow(stable_edges)) {
        from_var <- stable_edges$from[i]
        to_var <- stable_edges$to[i]
        strength <- stable_edges$strength[i]  # 使用稳定边的强度
        
        from_idx <- which(variable_names == from_var)
        to_idx <- which(variable_names == to_var)
        if(length(from_idx) > 0 && length(to_idx) > 0) {
          adj_matrix[from_idx, to_idx] <- strength
        }
      }
    } else if(!is.null(network_to_plot$arcs)) {
      # 备用方案：使用平均网络的边结构
      arcs <- network_to_plot$arcs
      for(i in 1:nrow(arcs)) {
        from_idx <- which(variable_names == arcs[i, "from"])
        to_idx <- which(variable_names == arcs[i, "to"])
        if(length(from_idx) > 0 && length(to_idx) > 0) {
          # 对于平均网络，权重设为1（因为没有强度信息）
          adj_matrix[from_idx, to_idx] <- 1
        }
      }
    }
  } else {
    # Figure5a: 结构图，只显示连边结构（无权重）
    if(!is.null(network_to_plot$arcs) && nrow(network_to_plot$arcs) > 0) {
      arcs <- network_to_plot$arcs
      for(i in 1:nrow(arcs)) {
        from_idx <- which(variable_names == arcs[i, "from"])
        to_idx <- which(variable_names == arcs[i, "to"])
        if(length(from_idx) > 0 && length(to_idx) > 0) {
          # 结构图只显示连边，不显示权重（统一设为1）
          adj_matrix[from_idx, to_idx] <- 1
        }
      }
    } else if(!is.null(bayesian_result$stable_edges) && nrow(bayesian_result$stable_edges) > 0) {
      # 备用方案：使用稳定边但不显示权重
      edges <- bayesian_result$stable_edges
      for(i in 1:nrow(edges)) {
        from_idx <- which(variable_names == edges$from[i])
        to_idx <- which(variable_names == edges$to[i])
        if(length(from_idx) > 0 && length(to_idx) > 0) {
          adj_matrix[from_idx, to_idx] <- 1
        }
      }
    }
  }
  
  # 设置默认颜色（如果没有提供）
  if(is.null(colors)) {
    if(exists("VIZ_CONFIG") && !is.null(VIZ_CONFIG$colors$primary)) {
      colors <- VIZ_CONFIG$colors$primary[1:min(n_vars, length(VIZ_CONFIG$colors$primary))]
    } else {
      colors <- rainbow(n_vars)
    }
  }
  
  # 创建qgraph可视化
  tryCatch({
    qgraph_obj <- qgraph::qgraph(
      adj_matrix,
      layout = layout,
      labels = variable_names,
      groups = groups,
      color = colors,
      directed = TRUE,  # 贝叶斯网络是有向图
      arrows = TRUE,
      edge.labels = (network_type == "averaged"),  # 平均网络显示权重，结构图不显示
      edge.label.cex = 0.8,
      vsize = 8,
      esize = 5,
      asize = 5,
      title = title,
      legend = !is.null(groups),
      legend.cex = 0.4,
      
      # 边的颜色设置
      posCol = if(exists("VIZ_CONFIG")) VIZ_CONFIG$colors$positive_edges else "#4A90E2",
      negCol = if(exists("VIZ_CONFIG")) VIZ_CONFIG$colors$negative_edges else "#D0021B",
      
      # 布局参数
      repulsion = 0.8,
      
      # 阈值设置
      threshold = 0.01,  # 显示微弱的边
      
      # 标签设置
      label.cex = 1.1,
      label.color = "black"
    )
    
    return(qgraph_obj)
    
  }, error = function(e) {
    cat("qgraph可视化失败:", e$message, "\n")
    
    # 备用方案：使用基础的plot
    if(requireNamespace("igraph", quietly = TRUE)) {
      # 转换为igraph格式
      edges_df <- data.frame(
        from = rep(variable_names, each = n_vars),
        to = rep(variable_names, n_vars),
        weight = as.vector(adj_matrix)
      )
      edges_df <- edges_df[edges_df$weight > 0, ]
      
      if(nrow(edges_df) > 0) {
        g <- igraph::graph_from_data_frame(edges_df, directed = TRUE, vertices = variable_names)
        plot(g, 
             vertex.size = 20, 
             vertex.label.cex = 0.8,
             edge.arrow.size = 0.5,
             main = title)
        return(g)
      }
    }
    
    # 最基础的备用方案
    plot.new()
    text(0.5, 0.5, paste("贝叶斯网络可视化失败:\n", e$message), cex = 1.2, col = "red")
    return(NULL)
  })
}

#' 生成贝叶斯网络分析报告
#' @param bayesian_result 贝叶斯网络分析结果
#' @param detected_scales 检测到的量表信息（可选）
#' @param network_result 网络分析结果（可选）
#' @return HTML格式的报告字符串
generate_bayesian_report <- function(bayesian_result, detected_scales = NULL, network_result = NULL) {
  
  params <- bayesian_result$parameters
  stable_count <- nrow(bayesian_result$stable_edges)
  total_possible_edges <- params$variable_count * (params$variable_count - 1)
  
  # 生成智能故事（如果有量表信息）
  smart_story <- ""
  if (!is.null(detected_scales)) {
    tryCatch({
      smart_story <- generate_smart_story(detected_scales, network_result, bayesian_result)
    }, error = function(e) {
      smart_story <- ""  # 如果故事生成失败，使用空字符串
    })
  }
  
  report_html <- paste0(
    "<h2>🧠 贝叶斯网络分析报告</h2>",
    "<p><strong>生成时间：</strong>", Sys.time(), "</p>",
    "<hr>",
    
    # 智能故事（如果生成成功）
    if (nchar(smart_story) > 0) smart_story else "",
    
    # 第三步故事引导
    "<div class='alert alert-success'>",
    "<h4>📝 侦探故事 - 第三步：整理证据链</h4>",
    "<p>恭喜！您已经完成了完整的心理网络探索之旅：</p>",
    "<ol>",
    "<li><strong>🔍 发现线索</strong>：网络分析揭示了变量间的相关模式</li>",
    "<li><strong>🧠 推理方向</strong>：贝叶斯分析推断出因果关系方向</li>", 
    "<li><strong>📖 构建理论</strong>：现在可以整理出完整的理论故事</li>",
    "</ol>",
    "<p><strong>💡 如何解读结果：</strong>关注<strong>稳定边</strong>（强度≥0.85），",
    "这些代表最可靠的因果关系！</p>",
    "</div>",
    
    # 添加具体案例故事
    "<div class='panel panel-info'>",
    "<div class='panel-heading'><h5>🔍 案例故事：酒精使用与心理健康</h5></div>",
    "<div class='panel-body'>",
    "<p><strong>研究发现的可能故事线：</strong></p>",
    "<ul>",
    "<li><strong>恐惧动机 → 酒精使用：</strong>恐惧和焦虑驱动个体通过酒精来应对压力</li>",
    "<li><strong>酒精使用 → 抑郁症状：</strong>长期酒精使用导致情绪调节能力下降</li>",
    "<li><strong>习惯动机 ← 酒精使用：</strong>重复使用酒精形成习惯性动机模式</li>",
    "</ul>",
    "<p class='text-muted'><em>注意：这只是假设性解释，具体结果需要基于您的实际数据！</em></p>",
    "</div>",
    "</div>",
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

# =============================================================================
# 分析代码生成和记录功能
# =============================================================================

#' 生成完整的分析代码
#' @param analysis_params 分析参数列表
#' @param data_info 数据信息
#' @param variable_selection 变量选择信息
#' @param network_params 网络分析参数
#' @param stability_params 稳定性分析参数
#' @param group_compare_params 组间比较参数（可选）
#' @return 完整的R代码字符串
generate_analysis_code <- function(analysis_params, data_info = NULL, variable_selection = NULL, 
                                 network_params = NULL, stability_params = NULL, 
                                 group_compare_params = NULL) {
  
  # 生成代码头部
  code_lines <- c(
    "# =============================================================================",
    "# 心理量表网络分析 - 自动生成代码",
    paste("# 生成时间:", Sys.time()),
    "# =============================================================================",
    "",
    "# 加载必要的包",
    "library(dplyr)",
    "library(ggplot2)",
    "library(bootnet)",
    "library(qgraph)",
    "",
    "# 如果需要quickNet包，请先安装:",
    "# devtools::install_github('LeiGuo0812/quickNet')",
    "library(quickNet)",
    "",
    "# =============================================================================",
    "# 1. 数据加载和预处理",
    "# =============================================================================",
    ""
  )
  
  # 数据加载部分
  if (!is.null(data_info)) {
    code_lines <- c(code_lines,
      paste("# 原始数据包含", data_info$n_subjects, "个样本,", data_info$n_variables, "个变量"),
      "# 请将您的数据文件路径替换为实际路径",
      "data <- read.csv('your_data_file.csv', stringsAsFactors = FALSE)",
      "",
      "# 数据基本信息",
      paste("# 样本量:", data_info$n_subjects),
      paste("# 变量数:", data_info$n_variables),
      if (!is.null(data_info$missing_rate)) paste("# 缺失率:", round(data_info$missing_rate * 100, 1), "%") else "",
      ""
    )
  }
  
  # 变量选择部分
  if (!is.null(variable_selection)) {
    code_lines <- c(code_lines,
      "# =============================================================================",
      "# 2. 变量选择和筛选",
      "# =============================================================================",
      ""
    )
    
    # 为每个量表生成变量选择代码
    for (scale_name in names(variable_selection)) {
      scale_info <- variable_selection[[scale_name]]
      code_lines <- c(code_lines,
        paste("# 量表:", scale_name),
        paste("# 选择层级:", scale_info$level),
        paste(scale_name, "_variables <- c(", paste0("'", scale_info$variables, "'", collapse = ", "), ")"),
        ""
      )
    }
    
    # 合并所有变量
    code_lines <- c(code_lines,
      "# 合并所有分析变量",
      "analysis_variables <- c(",
      paste("  ", paste(names(variable_selection), "_variables", sep = "", collapse = ",\n  ")),
      ")",
      "",
      "# 提取分析数据",
      "analysis_data <- data[, analysis_variables]",
      "",
      "# 检查数据完整性",
      "cat('最终分析变量数:', length(analysis_variables), '\\n')",
      "cat('数据维度:', dim(analysis_data), '\\n')",
      "cat('缺失值统计:\\n')",
      "print(colSums(is.na(analysis_data)))",
      ""
    )
  }
  
  # 网络分析部分
  if (!is.null(network_params)) {
    code_lines <- c(code_lines,
      "# =============================================================================",
      "# 3. 网络分析",
      "# =============================================================================",
      "",
      "# 网络估计参数",
      paste("network_method <- '", network_params$method %||% "EBICglasso", "'", sep = ""),
      paste("tuning_param <- ", network_params$tuning %||% 0.5),
      paste("threshold <- ", network_params$threshold %||% 0.05),
      "",
      "# 估计网络结构",
      "if (network_method == 'EBICglasso') {",
      "  # EBIC高斯石墨模型",
      "  network_result <- estimateNetwork(analysis_data, ",
      "                                   default = 'EBICglasso',",
      "                                   tuning = tuning_param)",
      "} else if (network_method == 'quickNet') {",
      "  # quickNet方法",
      "  network_result <- quickNet(analysis_data, method = 'glasso')",
      "}",
      "",
      "# 网络可视化",
      "plot(network_result, ",
      "     layout = 'spring',",
      "     theme = 'colorblind',",
      "     title = '心理量表网络分析')",
      "",
      "# 中心性分析",
      "centrality_result <- centralityPlot(network_result, ",
      "                                   include = c('Strength', 'Closeness', 'Betweenness'))",
      ""
    )
  }
  
  # 稳定性分析部分
  if (!is.null(stability_params)) {
    code_lines <- c(code_lines,
      "# =============================================================================",
      "# 4. 稳定性分析",
      "# =============================================================================",
      "",
      paste("# Bootstrap参数"),
      paste("bootstrap_n <- ", stability_params$bootstrap_n %||% 1000),
      paste("bootstrap_type <- c('", paste(stability_params$bootstrap_type %||% c("nonparametric", "case"), collapse = "', '"), "')", sep = ""),
      "",
      "# 运行Bootstrap稳定性分析",
      "stability_result <- bootnet(network_result,",
      "                           nBoots = bootstrap_n,",
      "                           type = bootstrap_type)",
      "",
      "# 边稳定性检验",
      "plot(stability_result, ",
      "     labels = FALSE, ",
      "     order = 'sample')",
      "",
      "# 中心性稳定性检验", 
      "stability_centrality <- bootnet(network_result,",
      "                               nBoots = bootstrap_n,",
      "                               type = 'case')",
      "",
      "plot(stability_centrality, ",
      "     statistics = c('strength', 'closeness', 'betweenness'))",
      "",
      "# 稳定性系数计算",
      "corStability(stability_centrality)",
      ""
    )
  }
  
  # 组间比较部分
  if (!is.null(group_compare_params)) {
    code_lines <- c(code_lines,
      "# =============================================================================",
      "# 5. 组间比较分析",
      "# =============================================================================",
      "",
      paste("# 分组变量:", group_compare_params$group_var),
      paste("# 分组方法:", group_compare_params$method %||% "split"),
      "",
      "# 准备分组数据",
      if (group_compare_params$method == "split") {
        paste("split_value <- ", group_compare_params$split_value %||% "median(data[[group_compare_params$group_var]], na.rm = TRUE)")
      } else {
        paste("group_levels <- c('", paste(group_compare_params$group_levels, collapse = "', '"), "')", sep = "")
      },
      "",
      "# 分组网络估计",
      "if (network_method == 'EBICglasso') {",
      "  group1_data <- analysis_data[group_condition_1, ]",
      "  group2_data <- analysis_data[group_condition_2, ]",
      "  ",
      "  group1_network <- estimateNetwork(group1_data, default = 'EBICglasso')",
      "  group2_network <- estimateNetwork(group2_data, default = 'EBICglasso')",
      "}",
      "",
      "# 网络比较分析",
      "library(NetworkComparisonTest)  # 需要安装此包",
      "comparison_result <- NCT(group1_data, group2_data, ",
      "                        it = 1000,  # 置换次数",
      "                        test.edges = TRUE,",
      "                        edges = 'all')",
      "",
      "# 查看比较结果",
      "summary(comparison_result)",
      "",
      "# 差异网络可视化",
      "diff_network <- comparison_result$einv.pvals < 0.05",
      "qgraph(diff_network, ",
      "       layout = 'spring',",
      "       title = '组间差异网络')",
      ""
    )
  }
  
  # 结果保存部分
  code_lines <- c(code_lines,
    "# =============================================================================",
    "# 6. 结果保存",
    "# =============================================================================",
    "",
    "# 保存网络对象",
    "saveRDS(network_result, 'network_result.rds')",
    "",
    "# 保存稳定性结果",
    if (!is.null(stability_params)) "saveRDS(stability_result, 'stability_result.rds')" else "",
    "",
    "# 保存组间比较结果", 
    if (!is.null(group_compare_params)) "saveRDS(comparison_result, 'comparison_result.rds')" else "",
    "",
    "# 导出网络图",
    "png('network_plot.png', width = 800, height = 600, res = 300)",
    "plot(network_result, layout = 'spring', theme = 'colorblind')",
    "dev.off()",
    "",
    "# 导出中心性图",
    "png('centrality_plot.png', width = 800, height = 600, res = 300)", 
    "centralityPlot(network_result)",
    "dev.off()",
    "",
    "cat('分析完成！结果已保存到当前工作目录。\\n')",
    "",
    "# =============================================================================",
    "# 代码结束",
    "# ============================================================================="
  )
  
  # 合并所有代码行
  full_code <- paste(code_lines, collapse = "\n")
  
  return(full_code)
}

# =============================================================================
# 网络温度分析模块
# 参考 network_temperature-main/zTemperature.R 实现
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
    binary_data <- prepare_binary_data(data, selected_vars, binary_transform, binary_encoding, binary_threshold, group_var)
    
    # 2. 构建Ising模型
    cat("🔧 步骤2: 构建Ising模型...\n")
    ising_models <- fit_ising_models(binary_data, group_var, selected_vars, estimator)
    
    # 3. 计算网络指标
    cat("📈 步骤3: 计算网络温度和全局指标...\n")
    network_metrics <- tryCatch({
      extract_network_metrics(ising_models)
    }, error = function(e) {
      cat("❌ 步骤3失败:", e$message, "\n")
      stop("步骤3: extract_network_metrics失败 - ", e$message)
    })
    cat("✅ 步骤3完成\n")
    
    # 4. 模型比较
    cat("⚖️ 步骤4: 模型比较和选择...\n")
    model_comparison <- tryCatch({
      compare_ising_models(ising_models)
    }, error = function(e) {
      cat("❌ 步骤4失败:", e$message, "\n")
      stop("步骤4: compare_ising_models失败 - ", e$message)
    })
    cat("✅ 步骤4完成\n")
    
    # 5. 生成结果摘要
    cat("📋 步骤5: 生成分析结果摘要...\n")
    analysis_summary <- tryCatch({
      generate_temperature_summary(network_metrics, model_comparison, group_var)
    }, error = function(e) {
      cat("❌ 步骤5失败:", e$message, "\n")
      stop("步骤5: generate_temperature_summary失败 - ", e$message)
    })
    cat("✅ 步骤5完成\n")
    
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
prepare_binary_data <- function(data, vars, transform = "median", encoding = "01", threshold = NULL, group_var = NULL) {
  
  cat("🔄 数据二值化: 方法=", transform, ", 编码=", encoding, "\n")
  
  # 确保包含分组变量（如果有）
  if(!is.null(group_var) && group_var %in% names(data)) {
    analysis_data <- data[, c(vars, group_var), drop = FALSE]
    cat("📊 保留分组变量:", group_var, "\n")
  } else {
    analysis_data <- data[, vars, drop = FALSE]
  }
  
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
    # 单组分析 - 简化版本，只构建基础模型和稀疏版本
    cat("🔧 拟合单组Ising模型...\n")
    
    # 详细调试信息
    cat("  调试信息:\n")
    cat("    数据维度:", dim(data), "\n")
    cat("    选择变量:", paste(selected_vars, collapse = ", "), "\n")
    cat("    变量长度:", length(selected_vars), "\n")
    cat("    estimator:", estimator, "\n")
    
    # 检查数据和变量
    if(length(selected_vars) == 0) {
      stop("选择变量为空")
    }
    
    if(any(is.na(selected_vars)) || any(selected_vars == "")) {
      stop("选择变量包含NA或空值")
    }
    
    if(!all(selected_vars %in% names(data))) {
      missing_vars <- selected_vars[!selected_vars %in% names(data)]
      stop("数据中缺少变量: ", paste(missing_vars, collapse = ", "))
    }
    
    # 基础模型 - 使用简单的调用方式（参考测试脚本成功的方法）
    base_model <- tryCatch({
      psychonetrics::Ising(
        data = data,
        vars = selected_vars,
        estimator = estimator
      )
    }, error = function(e) {
      cat("  Ising模型构建失败:", e$message, "\n")
      stop("Ising模型构建失败: ", e$message)
    })
    
    tryCatch({
      # Dense模型（完整模型）
      cat("  构建Dense模型...\n")
      models$Dense <- base_model %>% psychonetrics::runmodel()
      
      # Sparse模型（修剪非显著边）
      cat("  构建Sparse模型...\n")
      models$Sparse <- base_model %>% 
        psychonetrics::prune(alpha=0.05) %>% 
        psychonetrics::stepup(alpha=0.05) %>% 
        psychonetrics::runmodel()
      
      cat("✅ 单组模型拟合完成 (2个模型)\n")
      
    }, error = function(e) {
      cat("  警告: 稀疏模型拟合失败，仅使用Dense模型:", e$message, "\n")
      models$Dense <- base_model %>% psychonetrics::runmodel()
      cat("✅ 单组模型拟合完成 (1个模型)\n")
    })
    
  } else {
    # 多组分析 - 实现完整的8模型（4约束层级 × 2密度策略）
    cat("🔧 拟合多组Ising模型 (分组变量:", group_var, ")...\n")
    
    # 基础多组模型
    base_model <- psychonetrics::Ising(
      data = data,
      vars = selected_vars,
      groups = group_var,
      estimator = estimator
    )
    
    # 定义拟合函数（Dense + Sparse）
    fit_dense_sparse <- function(model, label) {
      cat("  构建", label, "模型组...\n")
      tryCatch({
        dense <- model %>% psychonetrics::runmodel()
        sparse <- model %>% psychonetrics::prune(alpha=0.05) %>% 
                 psychonetrics::stepup(alpha=0.05) %>% psychonetrics::runmodel()
        list(dense = dense, sparse = sparse)
      }, error = function(e) {
        cat("    警告:", label, "稀疏模型失败，仅使用Dense模型:", e$message, "\n")
        dense <- model %>% psychonetrics::runmodel()
        list(dense = dense, sparse = NULL)
      })
    }
    
    # 4种约束层级（参考calculate_temperature.R）
    cat("  第1层级: 所有参数自由 (Free)...\n")
    free_models <- fit_dense_sparse(base_model, "Free")
    models$M1_Free_Dense <- free_models$dense
    if(!is.null(free_models$sparse)) models$M2_Free_Sparse <- free_models$sparse
    
    cat("  第2层级: 网络结构相等 (Omega Equal)...\n")
    omega_models <- fit_dense_sparse(
      base_model %>% psychonetrics::groupequal("omega"), "OmegaEqual"
    )
    models$M3_Omega_Dense <- omega_models$dense
    if(!is.null(omega_models$sparse)) models$M4_Omega_Sparse <- omega_models$sparse
    
    cat("  第3层级: 网络结构+阈值相等 (Omega+Tau Equal)...\n")
    omega_tau_models <- fit_dense_sparse(
      base_model %>% psychonetrics::groupequal("omega") %>% psychonetrics::groupequal("tau"), 
      "OmegaTauEqual"
    )
    models$M5_OmegaTau_Dense <- omega_tau_models$dense
    if(!is.null(omega_tau_models$sparse)) models$M6_OmegaTau_Sparse <- omega_tau_models$sparse
    
    cat("  第4层级: 所有参数相等 (Omega+Tau+Beta Equal)...\n")
    omega_tau_beta_models <- fit_dense_sparse(
      base_model %>% psychonetrics::groupequal("omega") %>% 
      psychonetrics::groupequal("tau") %>% psychonetrics::groupequal("beta"), 
      "OmegaTauBetaEqual"
    )
    models$M7_OmegaTauBeta_Dense <- omega_tau_beta_models$dense
    if(!is.null(omega_tau_beta_models$sparse)) models$M8_OmegaTauBeta_Sparse <- omega_tau_beta_models$sparse
    
    actual_model_count <- length(models)
    cat("✅ 多组模型拟合完成 (", actual_model_count, "个模型)\n")
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
      
      # 计算温度 T = 1/β (参考zTemperature.R第84行)
      cat("🔍 调试模型", model_name, "的网络指标计算...\n")
      
      # 检查各种可能的列名和方法提取beta参数
      beta_params <- c()
      
      if("matrix" %in% names(params)) {
        # 方法1: 使用matrix列 (参考calculate_temperature.R第178行)
        beta_params <- params[params$matrix == "beta", "est"]
        cat("  找到beta参数数量:", length(beta_params), "\n")
      }
      
      if(length(beta_params) == 0 && "param" %in% names(params)) {
        # 方法2: 使用param列
        beta_params <- params[params$param == "beta", "est"]
        cat("  找到beta参数数量:", length(beta_params), "\n")
      }
      
      if(length(beta_params) == 0 && "par" %in% names(params)) {
        # 方法3: 使用par列
        beta_params <- params[params$par == "beta", "est"]
        cat("  找到beta参数数量:", length(beta_params), "\n")
      }
      
      # 提取omega(边权重)和tau(阈值)矩阵用于计算其他网络指标
      omega_matrices <- NULL
      tau_matrices <- NULL
      
      tryCatch({
        omega_matrices <- psychonetrics::getmatrix(model, "omega")
        tau_matrices <- psychonetrics::getmatrix(model, "tau")
      }, error = function(e) {
        cat("  提取omega/tau矩阵失败:", e$message, "\n")
      })
      
      if(length(beta_params) > 0) {
        # 处理beta参数可能是list的情况
        if(is.list(beta_params)) {
          cat("  beta参数是list，尝试转换为数值\n")
          beta_values <- tryCatch({
            as.numeric(unlist(beta_params))
          }, error = function(e) {
            cat("    转换失败:", e$message, "\n")
            return(NA)
          })
        } else {
          beta_values <- as.numeric(beta_params)
        }
        
        cat("  转换后的beta值:", beta_values, "\n")
        
        if(length(beta_values) > 0 && !all(is.na(beta_values))) {
          beta_mean <- mean(beta_values, na.rm = TRUE)
          if(!is.na(beta_mean) && beta_mean != 0) {
            temperature <- 1 / beta_mean
            cat("  计算温度成功: 1/", beta_mean, "=", temperature, "\n")
          } else {
            temperature <- NA
            cat("  beta均值无效或为0，设置温度为NA\n")
          }
        } else {
          temperature <- NA
          cat("  所有beta值均为NA，设置温度为NA\n")
        }
      } else {
        temperature <- NA
        cat("  未找到beta参数，设置温度为NA\n")
      }
      
      # 计算连接度
      if(is.list(omega_matrices)) {
        # 多组情况，取第一组
        omega_matrix <- omega_matrices[[1]]
      } else {
        omega_matrix <- omega_matrices
      }
      
      connectivity <- NA
      density <- NA
      global_strength <- NA
      entropy <- NA
      clustering <- NA
      
      if(!is.null(omega_matrix) && is.matrix(omega_matrix)) {
        # 计算连接度 (参考temperature_sensitivity.R)
        connectivity <- sum(abs(omega_matrix[upper.tri(omega_matrix)]))
        
        # 计算密度
        n_nodes <- nrow(omega_matrix)
        max_edges <- n_nodes * (n_nodes - 1) / 2
        density <- sum(omega_matrix[upper.tri(omega_matrix)] != 0) / max_edges
        
        # 计算全局强度 (参考temperature_sensitivity.R 第88-96行)
        global_strength <- sum(abs(omega_matrix)) / 2  # 除以2避免重复计算
        
        # 计算网络熵 (如果IsingSampler包可用)
        if(requireNamespace("IsingSampler", quietly = TRUE) && 
           !is.null(tau_matrices) && length(beta_values) > 0) {
          tryCatch({
            tau_matrix <- if(is.list(tau_matrices)) tau_matrices[[1]] else tau_matrices
            if(is.matrix(tau_matrix) || is.vector(tau_matrix)) {
              # 确保omega矩阵对称
              if (!isSymmetric(omega_matrix)) {
                omega_matrix <- (omega_matrix + t(omega_matrix)) / 2
              }
              # 使用平均beta值计算熵
              entropy <- IsingSampler::IsingEntrophy(
                graph = omega_matrix, 
                thresholds = as.vector(tau_matrix), 
                beta = mean(beta_values, na.rm = TRUE)
              )
            }
          }, error = function(e) {
            cat("  计算网络熵失败:", e$message, "\n")
          })
        }
        
        # 计算聚类系数 (如果igraph包可用)
        if(requireNamespace("igraph", quietly = TRUE)) {
          tryCatch({
            # 创建igraph对象
            g <- igraph::graph_from_adjacency_matrix(
              abs(omega_matrix) > 0.01,  # 设置边的阈值
              mode = "undirected"
            )
            clustering <- igraph::transitivity(g, type = "global")
          }, error = function(e) {
            cat("  计算聚类系数失败:", e$message, "\n")
          })
        }
      }
      
      # 拟合指标 - 使用更直接的方法
      model_BIC <- NA
      model_AIC <- NA
      model_CFI <- NA
      model_RMSEA <- NA
      
      # 方法1: 直接从模型对象获取拟合指标
      tryCatch({
        cat("  调试: 检查model@fitmeasures可用指标:", paste(names(model@fitmeasures), collapse = ", "), "\n")
        
        # 尝试多种大小写组合
        bic_names <- c("BIC", "bic", "Bic")
        aic_names <- c("AIC", "aic", "Aic")
        cfi_names <- c("CFI", "cfi", "Cfi")
        rmsea_names <- c("RMSEA", "rmsea", "Rmsea")
        
        for(name in bic_names) {
          if(name %in% names(model@fitmeasures) && is.na(model_BIC)) {
            model_BIC <- model@fitmeasures[[name]]
            cat("  找到BIC (", name, "):", model_BIC, "\n")
            break
          }
        }
        
        for(name in aic_names) {
          if(name %in% names(model@fitmeasures) && is.na(model_AIC)) {
            model_AIC <- model@fitmeasures[[name]]
            cat("  找到AIC (", name, "):", model_AIC, "\n")
            break
          }
        }
        
        for(name in cfi_names) {
          if(name %in% names(model@fitmeasures) && is.na(model_CFI)) {
            model_CFI <- model@fitmeasures[[name]]
            cat("  找到CFI (", name, "):", model_CFI, "\n")
            break
          }
        }
        
        for(name in rmsea_names) {
          if(name %in% names(model@fitmeasures) && is.na(model_RMSEA)) {
            model_RMSEA <- model@fitmeasures[[name]]
            cat("  找到RMSEA (", name, "):", model_RMSEA, "\n")
            break
          }
        }
      }, error = function(e) {
        cat("  从 model@fitmeasures提取失败:", e$message, "\n")
      })
      
      # 方法2: 使用fit函数
      if(is.na(model_BIC) || is.na(model_AIC)) {
        tryCatch({
          fit_info <- psychonetrics::fit(model)
          if(!is.null(fit_info)) {
            if(is.na(model_BIC) && "BIC" %in% names(fit_info)) {
              model_BIC <- fit_info$BIC
            }
            if(is.na(model_AIC) && "AIC" %in% names(fit_info)) {
              model_AIC <- fit_info$AIC
            }
            if(is.na(model_CFI) && "CFI" %in% names(fit_info)) {
              model_CFI <- fit_info$CFI
            }
            if(is.na(model_RMSEA) && "RMSEA" %in% names(fit_info)) {
              model_RMSEA <- fit_info$RMSEA
            }
          }
        }, error = function(e) {
          cat("  使用fit函数提取失败:", e$message, "\n")
        })
      }
      
      # 方法3: 使用logLik和AIC/BIC函数直接计算
      if(is.na(model_BIC) || is.na(model_AIC)) {
        cat("  尝试直接使用AIC/BIC函数...\n")
        tryCatch({
          if(is.na(model_AIC)) {
            model_AIC <- AIC(model)
            cat("    使用AIC()函数得到:", model_AIC, "\n")
          }
          if(is.na(model_BIC)) {
            model_BIC <- BIC(model)
            cat("    使用BIC()函数得到:", model_BIC, "\n")
          }
        }, error = function(e) {
          cat("  直接使用AIC/BIC函数失败:", e$message, "\n")
        })
      }
      
      # 方法4: 尝试使用比较结果（如果有的话）
      if(is.na(model_BIC) || is.na(model_AIC)) {
        cat("  BIC和AIC仍然为NA，尝试最后一种方法...\n")
        tryCatch({
          # 尝试使用比较函数的结果
          temp_compare <- psychonetrics::compare(model)
          if(!is.null(temp_compare)) {
            cat("    compare结果列名:", paste(names(temp_compare), collapse = ", "), "\n")
            if(is.na(model_BIC) && "BIC" %in% names(temp_compare)) {
              model_BIC <- temp_compare$BIC[1]
              cat("    从compare结果找到BIC:", model_BIC, "\n")
            }
            if(is.na(model_AIC) && "AIC" %in% names(temp_compare)) {
              model_AIC <- temp_compare$AIC[1]
              cat("    从compare结果找到AIC:", model_AIC, "\n")
            }
          }
        }, error = function(e) {
          cat("  使用compare方法失败:", e$message, "\n")
        })
      }
      
      cat("  最终BIC值:", model_BIC, ", AIC值:", model_AIC, "\n")
      
      metrics[[model_name]] <- list(
        temperature = temperature,
        connectivity = connectivity,
        density = density,
        global_strength = global_strength,
        entropy = entropy,
        clustering = clustering,
        AIC = model_AIC,
        BIC = model_BIC,
        CFI = model_CFI,
        RMSEA = model_RMSEA,
        n_nodes = if(!is.null(omega_matrix)) nrow(omega_matrix) else NA
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
    # 使用psychonetrics的compare函数 - 需要传递named arguments而不是list
    cat("  调用psychonetrics::compare，模型数量:", length(models), "\n")
    cat("  模型名称:", paste(names(models), collapse = ", "), "\n")
    
    # 使用do.call将list转换为named arguments
    comparison_result <- do.call(psychonetrics::compare, models)
    
    cat("  compare调用成功，结果类型:", class(comparison_result), "\n")
    cat("  结果结构:", paste(names(comparison_result), collapse = ", "), "\n")
    
    # 提取比较表 - psychonetrics::compare直接返回比较表
    comparison_table <- comparison_result
    
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
    cat("  错误详细信息:", toString(e), "\n")
    
    # 手动计算AIC/BIC比较 - 使用改进的提取方法
    cat("  使用手动方法计算模型比较...\n")
    
    # 定义BIC提取函数
    get_model_BIC <- function(model) {
      bic_val <- NA
      tryCatch({
        bic_val <- BIC(model)
      }, error = function(e) {
        tryCatch({
          if("BIC" %in% names(model@fitmeasures)) {
            bic_val <- model@fitmeasures$BIC
          }
        }, error = function(e2) {
          tryCatch({
            fit_info <- psychonetrics::fit(model)
            if(!is.null(fit_info) && "BIC" %in% names(fit_info)) {
              bic_val <- fit_info$BIC
            }
          }, error = function(e3) {})
        })
      })
      return(bic_val)
    }
    
    # 定义AIC提取函数
    get_model_AIC <- function(model) {
      aic_val <- NA
      tryCatch({
        aic_val <- AIC(model)
      }, error = function(e) {
        tryCatch({
          if("AIC" %in% names(model@fitmeasures)) {
            aic_val <- model@fitmeasures$AIC
          }
        }, error = function(e2) {
          tryCatch({
            fit_info <- psychonetrics::fit(model)
            if(!is.null(fit_info) && "AIC" %in% names(fit_info)) {
              aic_val <- fit_info$AIC
            }
          }, error = function(e3) {})
        })
      })
      return(aic_val)
    }
    
    aic_values <- sapply(models, get_model_AIC)
    bic_values <- sapply(models, get_model_BIC)
    
    cat("  AIC值:", paste(names(aic_values), "=", round(aic_values, 2), collapse = ", "), "\n")
    cat("  BIC值:", paste(names(bic_values), "=", round(bic_values, 2), collapse = ", "), "\n")
    
    if(all(is.na(bic_values))) {
      cat("    所有BIC值都是NA，返回第一个模型...\n")
      return(list(best_model = names(models)[1], comparison_table = NULL))
    }
    
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
        paste0("<li><strong>网络温度：</strong>", ifelse(is.na(best_metrics$temperature), "无法计算", round(best_metrics$temperature, 3))),
        paste0("<li><strong>连接强度：</strong>", ifelse(is.na(best_metrics$connectivity), "无法计算", round(best_metrics$connectivity, 3))),
        paste0("<li><strong>网络密度：</strong>", ifelse(is.na(best_metrics$density), "无法计算", round(best_metrics$density, 3))),
        paste0("<li><strong>BIC：</strong>", ifelse(is.na(best_metrics$BIC), "无法计算", round(best_metrics$BIC, 2))),
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
`%||%` <- function(x, y) if(is.null(x)) y else x################################################################################
########################## 代码生成和导出功能 ############################## 
################################################################################

# 初始化代码记录器 - 按执行顺序记录
init_code_recorder <- function() {
  list(
    # 主要代码记录 - 按时间顺序
    execution_steps = list(),  # 新增：按执行顺序记录的步骤
    step_counter = 0,          # 新增：步骤计数器
    
    # 传统分类记录 - 保留向后兼容
    data_loading = c(),
    data_preprocessing = c(),
    network_analysis = c(),
    temperature_analysis = c(),
    visualization = c(),
    exports = c(),
    parameters = list(),
    session_info = list(
      timestamp = Sys.time(),
      user_selections = list()
    )
  )
}

# 添加代码记录 - 同时记录到分类和执行步骤
add_code_record <- function(recorder, section, code_lines, description = "") {
  if(is.null(recorder)) recorder <- init_code_recorder()
  
  # 添加时间戳和描述
  timestamp <- format(Sys.time(), "%H:%M:%S")
  if(description != "") {
    header_line <- paste0("# ", description, " [", timestamp, "]")
    code_lines <- c(header_line, code_lines)
  }
  
  # 传统分类记录（保留向后兼容）
  if(section %in% names(recorder)) {
    recorder[[section]] <- c(recorder[[section]], "", code_lines)
  } else {
    recorder[[section]] <- code_lines
  }
  
  # 新增：按执行顺序记录步骤
  recorder$step_counter <- recorder$step_counter + 1
  step_info <- list(
    step_number = recorder$step_counter,
    timestamp = Sys.time(),
    section = section,
    description = description,
    code = code_lines
  )
  recorder$execution_steps[[recorder$step_counter]] <- step_info
  
  return(recorder)
}

# 记录数据加载代码
record_data_loading <- function(recorder, file_path, sheet_name = NULL) {
  code_lines <- c(
    "# ===== 数据加载 Data Loading =====",
    "library(readxl)",
    "library(dplyr)",
    "library(psychonetrics)  # 网络温度分析",
    ""
  )
  
  if(!is.null(sheet_name)) {
    code_lines <- c(code_lines,
      paste0('raw_data <- readxl::read_excel("', basename(file_path), '", sheet = "', sheet_name, '")')
    )
  } else {
    code_lines <- c(code_lines,
      paste0('raw_data <- readxl::read_excel("', basename(file_path), '")')
    )
  }
  
  code_lines <- c(code_lines,
    "print(dim(raw_data))",
    "print(head(raw_data, 3))"
  )
  
  add_code_record(recorder, "data_loading", code_lines, "数据加载阶段")
}

# 记录数据预处理代码  
record_data_preprocessing <- function(recorder, final_variables, binary_transform, binary_encoding, group_var = NULL) {
  code_lines <- c(
    "# ===== 数据预处理 Data Preprocessing =====",
    "",
    "# 选择分析变量",
    paste0('analysis_vars <- c(', paste0('"', final_variables, '"', collapse = ', '), ')'),
    ""
  )
  
  if(!is.null(group_var)) {
    code_lines <- c(code_lines,
      "# 添加分组变量",
      paste0('group_var <- "', group_var, '"'),
      'analysis_data <- raw_data[, c(analysis_vars, group_var)]',
      ""
    )
  } else {
    code_lines <- c(code_lines,
      'analysis_data <- raw_data[, analysis_vars]',
      ""
    )
  }
  
  code_lines <- c(code_lines,
    "# 处理缺失值",
    'analysis_data <- analysis_data[complete.cases(analysis_data), ]',
    'print(paste("最终数据行数:", nrow(analysis_data)))',
    ""
  )
  
  # 添加二值化代码
  if(binary_transform != "无") {
    code_lines <- c(code_lines,
      paste0('# 二值化方法: ', binary_transform),
      'for(var in analysis_vars) {',
      '  if(is.numeric(analysis_data[[var]])) {'    
    )
    
    if(binary_transform == "中位数分割") {
      code_lines <- c(code_lines,
        '    median_val <- median(analysis_data[[var]], na.rm = TRUE)',
        '    analysis_data[[var]] <- ifelse(analysis_data[[var]] > median_val, 1, 0)'
      )
    } else if(binary_transform == "均值分割") {
      code_lines <- c(code_lines,
        '    mean_val <- mean(analysis_data[[var]], na.rm = TRUE)', 
        '    analysis_data[[var]] <- ifelse(analysis_data[[var]] > mean_val, 1, 0)'
      )
    }
    
    code_lines <- c(code_lines,
      '  }',
      '}',
      ""
    )
    
    # 添加编码转换
    if(binary_encoding == "-1/1编码") {
      code_lines <- c(code_lines,
        '# 转换为-1/1编码',
        'for(var in analysis_vars) {',
        '  analysis_data[[var]] <- ifelse(analysis_data[[var]] == 0, -1, 1)',
        '}',
        ""
      )
    }
  }
  
  add_code_record(recorder, "data_preprocessing", code_lines, "数据预处理阶段")
}

# 实时记录实际执行的代码
record_actual_code <- function(recorder, code_lines, section_name, description = NULL) {
  if(is.null(recorder)) {
    recorder <- init_code_recorder()
  }
  
  # 确保描述不为空
  final_description <- description %||% section_name
  
  # 直接使用add_code_record，避免重复添加时间戳
  return(add_code_record(recorder, section_name, code_lines, final_description))
}

# 记录网络分析代码
record_network_analysis <- function(recorder, final_variables, threshold = 0.05, groups = NULL, estimator = "EBICglasso") {
  code_lines <- c(
    "# ===== 网络分析 Network Analysis =====",
    "library(quickNet)   # 网络建构包",
    "library(bootnet)    # 稳定性分析",
    "library(qgraph)     # 网络可视化",
    "",
    "# 数据准备",
    "analysis_data <- analysis_data[complete.cases(analysis_data), ]",
    'print(paste("完整样本数量:", nrow(analysis_data)))',
    'print(paste("分析变量数量:", ncol(analysis_data)))',
    "",
    "# 网络建构参数",
    paste0('threshold <- ', threshold),
    paste0('estimator <- "', estimator, '"'),
    ""
  )
  
  if(!is.null(groups) && length(groups) > 1) {
    code_lines <- c(code_lines,
      "# 分组信息设置",
      paste0('groups <- c(', paste(sapply(1:length(final_variables), function(i) {
        group_idx <- which(sapply(groups, function(g) i %in% g))[1]
        if(is.na(group_idx)) 1 else group_idx
      }), collapse = ', '), ')'),
      paste0('group_colors <- c(', paste0('"', c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")[1:length(groups)], '"', collapse = ', '), ')'),
      ""
    )
  }
  
  # 根据估计方法生成不同的代码
  if(estimator == "EBICglasso") {
    code_lines <- c(code_lines,
      "# 使用quickNet进行网络分析（默认EBICglasso）",
      "network_result <- quickNet(",
      "  data = analysis_data,",
      paste0('  threshold = ', threshold, ','),
      "  layout = 'spring',",
      "  edge.labels = TRUE,",
      "  theme = 'colorblind'"
    )
  } else {
    code_lines <- c(code_lines,
      paste0("# 使用bootnet进行网络估计（", estimator, "）"),
      paste0("net_estimate <- estimateNetwork(analysis_data, default = '", estimator, "', threshold = TRUE)"),
      "",
      "# 使用qgraph进行可视化",
      "network_result <- qgraph(",
      "  net_estimate$graph,",
      "  layout = 'spring',",
      "  edge.labels = TRUE,",
      paste0('  threshold = ', threshold, ','),
      "  theme = 'colorblind'"
    )
  }
  
  # 添加分组参数（如果有的话）
  if(!is.null(groups) && length(groups) > 1) {
    code_lines <- c(code_lines,
      "  , groups = groups,",
      "  color = group_colors"
    )
  }
  
  # 结束函数调用
  code_lines <- c(code_lines,
    ")",
    "",
    "# 显示网络结果",
    "print(network_result)",
    "",
    "# 中心性分析",
    "centrality_result <- Centrality(network_result)",
    "print(centrality_result)",
    "",
    "# 专业网络图输出（PDF格式）",
    "get_network_plot(network_result, prefix = 'Fig1_network', width = 6, height = 4.5)",
    "cat('已生成网络图: Fig1_network_network_plot.pdf\\n')",
    "",
    "# 中心性可视化（PDF格式）", 
    "get_centrality_plot(centrality_result, prefix = 'Fig2_centrality', width = 8, height = 6)",
    "cat('已生成中心性图: Fig2_centrality_centrality_plot.pdf\\n')"
  )
  
  add_code_record(recorder, "network_analysis", code_lines, "网络分析阶段")
}

# 记录网络温度分析代码
record_temperature_analysis <- function(recorder, final_variables, group_var = NULL) {
  code_lines <- c(
    "# ===== 网络温度分析 Network Temperature Analysis =====",
    "library(psychonetrics)",
    "library(IsingSampler)  # 网络熵计算",
    "library(igraph)       # 网络指标计算",
    ""
  )
  
  if(!is.null(group_var)) {
    code_lines <- c(code_lines,
      "# 多组Ising模型建构 (8个约束层级)",
      paste0('vars <- c(', paste0('"', final_variables, '"', collapse = ', '), ')'),
      "",
      "# M1: 所有参数自由（稠密）",
      paste0('model1 <- Ising(analysis_data, vars = vars, groups = "', group_var, '") %>% runmodel'),
      "# M2: 所有参数自由（稀疏）", 
      'model2 <- model1 %>% prune(alpha = 0.05) %>% stepup(alpha = 0.05)',
      "",
      "# M3: 网络结构相等（稠密）",
      'model3 <- model1 %>% groupequal("omega") %>% runmodel',
      "# M4: 网络结构相等（稀疏）",
      'model4 <- model3 %>% prune(alpha = 0.05) %>% stepup(mi = "mi_equal", alpha = 0.05)',
      "",
      "# M5: 网络结构+阈值相等（稠密）",
      'model5 <- model3 %>% groupequal("tau") %>% runmodel',
      "# M6: 网络结构+阈值相等（稀疏）",
      'model6 <- model5 %>% prune(alpha = 0.05) %>% stepup(mi = "mi_equal", alpha = 0.05)',
      "",
      "# M7: 所有参数相等（稠密）",
      'model7 <- model5 %>% groupequal("beta") %>% runmodel',
      "# M8: 所有参数相等（稀疏）",
      'model8 <- model7 %>% prune(alpha = 0.05) %>% stepup(mi = "mi_equal", alpha = 0.05)',
      "",
      "# 模型列表",
      'models <- list(',
      '  "M1_Free_Dense" = model1,',
      '  "M2_Free_Sparse" = model2,', 
      '  "M3_Omega_Dense" = model3,',
      '  "M4_Omega_Sparse" = model4,',
      '  "M5_OmegaTau_Dense" = model5,',
      '  "M6_OmegaTau_Sparse" = model6,',
      '  "M7_OmegaTauBeta_Dense" = model7,',
      '  "M8_OmegaTauBeta_Sparse" = model8',
      ')'
    )
  } else {
    code_lines <- c(code_lines,
      "# 单组Ising模型建构",
      paste0('vars <- c(', paste0('"', final_variables, '"', collapse = ', '), ')'),
      "",
      "# 稠密和稀疏模型",
      'model1 <- Ising(analysis_data, vars = vars) %>% runmodel',
      'model2 <- model1 %>% prune(alpha = 0.05) %>% stepup(alpha = 0.05)',
      "",
      'models <- list(',
      '  "Dense_Model" = model1,',
      '  "Sparse_Model" = model2',
      ')'
    )
  }
  
  code_lines <- c(code_lines,
    "",
    "# 模型比较",
    'comparison_result <- psychonetrics::compare('
  )
  
  if(!is.null(group_var)) {
    model_refs <- paste0('models[[', 1:8, ']]', collapse = ', ')
    code_lines <- c(code_lines, paste0('  ', model_refs))
  } else {
    code_lines <- c(code_lines, '  models[[1]], models[[2]]')
  }
  
  code_lines <- c(code_lines,
    ') %>% arrange(BIC)',
    'print(comparison_result)',
    ""
  )
  
  add_code_record(recorder, "temperature_analysis", code_lines, "网络温度分析阶段")
}

# 记录可视化代码
record_visualization <- function(recorder, final_variables, group_var = NULL) {
  code_lines <- c(
    "# ===== 网络可视化 Network Visualization =====",
    "library(ggplot2)",
    "library(viridis)",
    "",
    "# 选择最佳模型",
    'best_model_idx <- which.min(comparison_result$BIC)',
    'best_model <- models[[best_model_idx]]',
    'cat("最佳模型:", names(models)[best_model_idx], "\\n")',
    "",
    "# 提取网络温度",
    'temperatures <- sapply(models, function(m) {',
    '  beta_params <- psychonetrics::parameters(m)',
    '  beta_values <- beta_params[beta_params$matrix == "beta", "est"]',
    '  1 / mean(beta_values, na.rm = TRUE)',
    '})',
    'print(temperatures)',
    ""
  )
  
  if(!is.null(group_var)) {
    code_lines <- c(code_lines,
      "# 温度比较条形图",
      'temperature_df <- data.frame(',
      '  Model = names(temperatures),',
      '  Temperature = temperatures',
      ')',
      '',
      '# 缩短模型名称以便显示',
      'temperature_df$Model <- gsub("_Free_Dense", "_Free", temperature_df$Model)',
      'temperature_df$Model <- gsub("_Free_Sparse", "_Sparse", temperature_df$Model)',
      'temperature_df$Model <- gsub("_Equal_Dense", "_Dense", temperature_df$Model)',
      'temperature_df$Model <- gsub("_Equal_Sparse", "_Sparse", temperature_df$Model)',
      '',
      'pdf("Fig4a_temperature_comparison.pdf", width = 10, height = 6)',
      'par(mar = c(5, 10, 4, 2))',
      'barplot(temperature_df$Temperature,',
      '        names.arg = temperature_df$Model,',
      '        horiz = TRUE,',
      '        col = rainbow(nrow(temperature_df), alpha = 0.7),',
      '        main = "Network Temperature Comparison\\n(8 Constraint Models)",',
      '        xlab = "Temperature (T = 1/β)",',
      '        las = 1,',
      '        cex.names = 0.7)',
      'dev.off()'
    )
  }
  
  code_lines <- c(code_lines,
    "",
    "# 网络热图 (症状协方差矩阵)",
    'covariance_matrix <- cov(analysis_data[, vars], use = "complete.obs")',
    'rownames(covariance_matrix) <- vars',
    'colnames(covariance_matrix) <- vars',
    '',
    'pdf("Fig4b_temperature_heatmap.pdf", width = 8, height = 6)',
    'heatmap(covariance_matrix,',
    '        symm = TRUE,', 
    '        col = viridis::plasma(100),',
    '        Rowv = NA,',
    '        main = "Symptom Covariance Matrix")',
    'dev.off()'
  )
  
  add_code_record(recorder, "visualization", code_lines, "网络可视化阶段")
}

# 记录结果导出代码
record_exports <- function(recorder, final_variables) {
  code_lines <- c(
    "# ===== 结果导出 Results Export =====",
    "",
    "# 提取网络指标",
    'extract_network_metrics <- function(models) {',
    '  metrics <- list()',
    '  for(model_name in names(models)) {',
    '    model <- models[[model_name]]',
    '    ',
    '    # 计算温度',
    '    params <- psychonetrics::parameters(model)',
    '    beta_params <- params[params$matrix == "beta", "est"]',
    '    temperature <- 1 / mean(beta_params, na.rm = TRUE)',
    '    ',
    '    # 提取网络矩阵',
    '    omega_matrices <- psychonetrics::getmatrix(model, "omega")',
    '    omega_matrix <- if(is.list(omega_matrices)) omega_matrices[[1]] else omega_matrices',
    '    ',
    '    # 计算网络指标',
    '    connectivity <- sum(abs(omega_matrix[upper.tri(omega_matrix)]))',
    '    n_nodes <- nrow(omega_matrix)',
    '    max_edges <- n_nodes * (n_nodes - 1) / 2',
    '    density <- sum(omega_matrix[upper.tri(omega_matrix)] != 0) / max_edges',
    '    global_strength <- sum(abs(omega_matrix)) / 2',
    '    ',
    '    # 拟合指标',
    '    tryCatch({',
    '      model_AIC <- AIC(model)',
    '      model_BIC <- BIC(model)',
    '    }, error = function(e) {',
    '      model_AIC <- NA',
    '      model_BIC <- NA',
    '    })',
    '    ',
    '    metrics[[model_name]] <- list(',
    '      temperature = temperature,',
    '      connectivity = connectivity,',
    '      density = density,',
    '      global_strength = global_strength,', 
    '      n_nodes = n_nodes,',
    '      AIC = model_AIC,',
    '      BIC = model_BIC',
    '    )',
    '  }',
    '  return(metrics)',
    '}',
    '',
    '# 提取指标',
    'network_metrics <- extract_network_metrics(models)',
    '',
    '# 生成CSV表格',
    'metrics_df <- data.frame()',
    'for(model_name in names(network_metrics)) {',
    '  metric <- network_metrics[[model_name]]',
    '  row_data <- data.frame(',
    '    Model = model_name,',
    '    Temperature = round(metric$temperature, 4),',
    '    Global_Strength = round(metric$global_strength, 4),',
    '    Network_Density = round(metric$density, 4),',
    '    Connectivity = round(metric$connectivity, 4),',
    '    Number_of_Nodes = metric$n_nodes,',
    '    Model_AIC = round(metric$AIC, 2),',
    '    Model_BIC = round(metric$BIC, 2)',
    '  )',
    '  metrics_df <- rbind(metrics_df, row_data)',
    '}',
    '',
    'write.csv(metrics_df, "Fig4_temperature_network_metrics.csv", row.names = FALSE)',
    'print(metrics_df)'
  )
  
  add_code_record(recorder, "exports", code_lines, "结果导出阶段")
}

# 生成完整R脚本 - 按用户实际执行步骤顺序
# 确保数据定义的完整性
ensure_data_definition <- function(code_lines) {
  # 检查是否包含数据加载和定义
  has_data_loading <- any(grepl("read|readxl|raw_data", code_lines))
  has_analysis_data_def <- any(grepl("analysis_data\\s*<-", code_lines))
  
  if(!has_data_loading) {
    # 添加数据加载提醒
    data_loading_reminder <- c(
      "# 请在此处添加数据加载代码",
      "# raw_data <- readxl::read_excel('your_data_file.xlsx')",
      ""
    )
    code_lines <- c(data_loading_reminder, code_lines)
  }
  
  if(!has_analysis_data_def && any(grepl("analysis_vars", code_lines))) {
    # 在 analysis_vars 定义后添加 analysis_data 定义
    var_line_idx <- which(grepl("analysis_vars", code_lines))[1]
    if(!is.na(var_line_idx)) {
      data_def_lines <- c(
        "",
        "analysis_data <- raw_data[, analysis_vars]",
        ""
      )
      # 在变量定义后插入数据定义
      code_lines <- c(
        code_lines[1:var_line_idx],
        data_def_lines,
        code_lines[(var_line_idx + 1):length(code_lines)]
      )
    }
  }
  
  # 特殊处理：修正 analysis_data 的循环引用问题
  # 例如：analysis_data <- analysis_data[complete.cases(analysis_data), ]
  problematic_lines <- grep("analysis_data\\s*<-\\s*analysis_data\\[", code_lines)
  if(length(problematic_lines) > 0 && any(grepl("analysis_vars", code_lines))) {
    for(line_idx in problematic_lines) {
      # 在这个问题行之前插入正确的数据定义
      if(line_idx > 1) {
        # 查找前面是否已经有数据定义
        previous_lines <- code_lines[1:(line_idx-1)]
        if(!any(grepl("analysis_data\\s*<-\\s*raw_data", previous_lines))) {
          # 在问题行之前插入正确的定义
          code_lines <- c(
            code_lines[1:(line_idx-1)],
            "analysis_data <- raw_data[, analysis_vars]",
            "",
            code_lines[line_idx:length(code_lines)]
          )
          break  # 只插入一次
        }
      }
    }
  }
  
  # 如果仍然有 analysis_data 使用但没有定义，进行替换修正
  uses_analysis_data <- any(grepl("analysis_data\\[", code_lines))
  still_no_def <- !any(grepl("analysis_data\\s*<-\\s*raw_data", code_lines))  # 查找正确的定义
  
  if(uses_analysis_data && still_no_def && any(grepl("analysis_vars", code_lines))) {
    # 查找第一个使用 analysis_data 的地方，在之前插入定义
    first_use_idx <- which(grepl("analysis_data\\[", code_lines))[1]
    if(!is.na(first_use_idx)) {
      # 在第一次使用前插入定义
      code_lines <- c(
        code_lines[1:(first_use_idx-1)],
        "analysis_data <- raw_data[, analysis_vars]",
        "",
        code_lines[first_use_idx:length(code_lines)]
      )
    }
  }
  
  return(code_lines)
}

generate_complete_script <- function(recorder, output_path = NULL) {
  if(is.null(recorder)) return(NULL)
  
  # 检测分析类型
  analysis_type <- detect_analysis_type(recorder)
  
  # 脚本头部
  header_lines <- generate_script_header(analysis_type)
  
  # 组合所有代码部分
  all_lines <- c(header_lines)
  
  # 优先使用execution_steps（按实际执行顺序）
  if(!is.null(recorder$execution_steps) && length(recorder$execution_steps) > 0) {
    cat("📋 使用实际执行步骤生成脚本，共", length(recorder$execution_steps), "个步骤\n")
    
    # 跟踪已添加的代码段，避免重复
    added_sections <- list()
    
    for(i in seq_along(recorder$execution_steps)) {
      step <- recorder$execution_steps[[i]]
      
      # 生成步骤唯一标识符
      step_id <- paste0(step$section, "_", step$description)
      
      # 跳过重复的步骤（例如重复的中心性分析）
      if(step_id %in% names(added_sections)) {
        cat("⚠️ 跳过重复步骤:", step$description, "\n")
        next
      }
      
      # 添加步骤分隔符
      step_header <- paste0("\n# ======== 步骤 ", i, ": ", step$description, " ========")
      step_timestamp <- paste0("# 执行时间: ", format(step$timestamp, "%Y-%m-%d %H:%M:%S"))
      
      all_lines <- c(all_lines, "", step_header, step_timestamp, "")
      
      # 添加实际代码
      if(length(step$code) > 0) {
        # 过滤掉重复的时间戳行和空行
        clean_code <- step$code[!grepl("^# .* \\[\\d{2}:\\d{2}:\\d{2}\\]", step$code)]
        clean_code <- clean_code[nzchar(clean_code)]  # 移除空行
        
        # 特殊处理：确保数据定义的完整性
        if(step$section == "data_preprocessing" && any(grepl("analysis_vars", clean_code))) {
          # 确保包含完整的数据定义
          clean_code <- ensure_data_definition(clean_code)
        }
        
        all_lines <- c(all_lines, clean_code, "")
      }
      
      # 记录已添加的步骤
      added_sections[[step_id]] <- TRUE
    }
  } else {
    # 降级到传统分类方式（向后兼容）
    cat("⚠️ 未找到执行步骤记录，使用传统分类方式生成脚本\n")
    all_lines <- c(all_lines, generate_legacy_script_body(recorder))
  }
  
  # 添加脚本尾部
  footer_lines <- generate_script_footer(analysis_type)
  all_lines <- c(all_lines, footer_lines)
  
  # 保存或返回
  if(!is.null(output_path)) {
    writeLines(all_lines, output_path, useBytes = TRUE)
    cat("📝 脚本已保存到:", output_path, "\n")
    return(output_path)
  } else {
    return(paste(all_lines, collapse = "\n"))
  }
}

# 检测分析类型
detect_analysis_type <- function(recorder) {
  if(is.null(recorder)) return("网络分析")
  
  # 检查不同类型的分析
  if(!is.null(recorder$temperature_analysis) && length(recorder$temperature_analysis) > 0) {
    return("网络温度分析")
  } else if(!is.null(recorder$bayesian_analysis) && length(recorder$bayesian_analysis) > 0) {
    return("贝叶斯网络分析")
  } else if(!is.null(recorder$bridge_analysis) && length(recorder$bridge_analysis) > 0) {
    return("桥接网络分析")
  } else {
    return("心理量表网络分析")
  }
}

# 生成脚本头部
generate_script_header <- function(analysis_type) {
  c(
    "################################################################################",
    paste0("##                    ", analysis_type, "脚本                           ##"),
    paste0("##                 Psychology Network Analysis Script                    ##"),
    "################################################################################",
    "##", 
    paste0("## 生成时间 Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "## 由NetworkApp自动生成 Auto-generated by NetworkApp",
    "## 此脚本按用户实际执行步骤顺序生成 Generated following actual execution order",
    "##",
    "## 使用说明 Instructions:", 
    "## 1. 确保安装所需R包 Install required packages",
    "## 2. 设置工作目录 Set working directory",
    "## 3. 更新数据文件路径 Update data file path",
    "## 4. 运行脚本 Execute script",
    "##",
    "################################################################################",
    "",
    "# 清理环境 Clear environment",
    "rm(list = ls())",
    "",
    "# 设置工作目录 Set working directory",
    "# setwd('/path/to/your/working/directory')",
    "",
    "# 检查并安装所需包 Check and install required packages",
    "required_packages <- c('readxl', 'dplyr', 'quickNet', 'bootnet', 'qgraph', 'ggplot2')",
    "for(pkg in required_packages) {",
    "  if(!require(pkg, character.only = TRUE)) {",
    "    install.packages(pkg)",
    "    library(pkg, character.only = TRUE)",
    "  }",
    "}",
    "",
    "# 特殊包安装 Special packages installation",
    "# if(!require(quickNet)) devtools::install_github('LeiGuo0812/quickNet')",
    ""
  )
}

# 生成脚本尾部
generate_script_footer <- function(analysis_type) {
  c(
    "",
    "################################################################################",
    "##                              脚本结束                                ##",
    "##                           Script Complete                               ##",
    "################################################################################",
    "",
    "# 显示完成信息",
    paste0('cat("\\n=== ', analysis_type, '完成 Analysis Complete ===\\n")'),
    'cat("\\n请检查生成的文件 Please check the generated files.\\n")'
  )
}

# 传统分类方式生成脚本主体（向后兼容）
generate_legacy_script_body <- function(recorder) {
  all_sections <- names(recorder)
  
  # 定义sections的逻辑顺序
  section_order <- c(
    "data_loading", 
    "data_preprocessing", 
    "network_analysis", 
    "bridge_analysis", 
    "bridge_visualization",
    "stability_analysis",      # quickNet的Stability()函数
    "edge_stability", 
    "centrality_stability", 
    "stability_visualization",
    "group_comparison",
    "bayesian_analysis", 
    "temperature_analysis", 
    "visualization", 
    "exports"
  )
  
  script_lines <- c()
  added_content <- list()  # 跟踪已添加的内容，避免重复
  
  # 按顺序处理已存在的sections
  for(section in section_order) {
    if(section %in% all_sections && length(recorder[[section]]) > 0) {
      section_code <- recorder[[section]]
      
      # 去除重复的时间戳和空行
      clean_code <- section_code[!grepl("^# .* \\[\\d{2}:\\d{2}:\\d{2}\\]", section_code)]
      clean_code <- clean_code[nzchar(clean_code)]
      
      # 检查内容是否已添加（基于代码内容的哈希）
      code_content <- paste(clean_code, collapse = "\n")
      # 使用简单的字符串哈希，避免依赖digest包
      content_hash <- paste0(section, "_", nchar(code_content), "_", substr(code_content, 1, 50))
      
      if(!content_hash %in% names(added_content)) {
        # 确保数据定义的完整性
        if(section == "data_preprocessing") {
          clean_code <- ensure_data_definition(clean_code)
        }
        
        script_lines <- c(script_lines, 
                         paste0("\n# ", toupper(gsub("_", " ", section)), " 阶段"),
                         clean_code, "")
        added_content[[content_hash]] <- section
      } else {
        cat("⚠️ 跳过重复的", section, "内容\n")
      }
    }
  }
  
  # 处理不在预定义顺序中的其他sections，但排除内部sections
  remaining_sections <- setdiff(all_sections, c(section_order, "parameters", "session_info", "execution_steps", "step_counter"))
  for(section in remaining_sections) {
    if(length(recorder[[section]]) > 0) {
      script_lines <- c(script_lines, recorder[[section]], "")
    }
  }
  
  return(script_lines)
}

# =============================================================================
# 样本量计算函数 (powerly)
# =============================================================================

#' 基于网络特征计算推荐样本量
#' @param network_result 网络分析结果对象（来自quickNet或qgraph）
#' @param nodes 网络节点数量
#' @param density 网络密度（可选，如果提供network_result会自动计算）
#' @param measure 性能测量类型 ("sen", "spe", "mcc", "rho")
#' @param statistic 统计指标 ("power")
#' @param measure_value 目标性能测量值 (0.6)
#' @param statistic_value 目标统计指标值 (0.8)
#' @param preset 预设配置 ("balanced", "conservative", "exploratory")
#' @param ... 其他powerly参数
#' @return powerly分析结果对象
calculate_sample_size <- function(network_result = NULL, 
                                nodes = NULL, 
                                density = NULL,
                                range_lower = 300,
                                range_upper = 2000,
                                cores = 2,
                                ...) {
  
  # 检查powerly包是否可用
  if(!requireNamespace("powerly", quietly = TRUE)) {
    stop("需要安装powerly包: install.packages('powerly')")
  }
  
  # 从网络结果中提取特征
  if(!is.null(network_result)) {
    network_features <- extract_network_features(network_result)
    if(is.null(nodes)) nodes <- network_features$nodes
    if(is.null(density)) density <- network_features$density
  }
  
  # 验证输入参数
  if(is.null(nodes) || is.null(density)) {
    stop("必须提供网络节点数和密度信息")
  }
  
  # 记录分析参数
  cat("🔬 样本量分析参数:\n")
  cat("   节点数:", nodes, "\n")
  cat("   网络密度:", round(density, 3), "\n")
  cat("   样本量搜索范围:", range_lower, "-", range_upper, "\n")
  cat("   并行核心数:", cores, "\n\n")
  
  # 执行样本量计算
  tryCatch({
    cat("⏳ 正在进行样本量计算，这可能需要几分钟...\n")
    
    # 简化的powerly参数，基于用户示例
    powerly_params <- list(
      range_lower = range_lower,
      range_upper = range_upper,
      nodes = nodes,
      density = density,
      cores = cores
    )
    
    cat("📋 Powerly参数:\n")
    cat("  - 节点数:", powerly_params$nodes, "\n")
    cat("  - 密度:", powerly_params$density, "\n")
    cat("  - 样本量范围:", powerly_params$range_lower, "-", powerly_params$range_upper, "\n")
    
    result <- do.call(powerly::powerly, powerly_params)
    
    cat("✅ 样本量计算完成!\n")
    
    # 创建一个安全的结果对象，避免修改R6环境
    safe_result <- list(
      # 复制powerly的关键信息
      recommendation = result$recommendation,
      converged = result$converged,
      iterations = result$iterations,
      step_1 = result$step_1,
      step_2 = result$step_2, 
      step_3 = result$step_3,
      # 添加我们的网络信息
      network_info = list(
        nodes = nodes,
        density = density,
        analysis_date = Sys.time()
      ),
      # 保存原始powerly结果以便绘图
      original_result = result
    )
    
    # 获取推荐样本量（confidence interval format）
    recommendation <- tryCatch({
      if(!is.null(result$recommendation)) {
        # 如果是向量，格式化为置信区间
        if(is.numeric(result$recommendation) && length(result$recommendation) > 1) {
          paste0("2.5% = ", round(result$recommendation[1]), " | ", 
                 "50% = ", round(result$recommendation[2]), " | ", 
                 "97.5% = ", round(result$recommendation[3]))
        } else {
          result$recommendation
        }
      } else {
        "无法确定"
      }
    }, error = function(e) "无法确定")
    
    cat("📊 推荐样本量:", recommendation, "\n\n")
    
    return(safe_result)
    
  }, error = function(e) {
    cat("❌ 样本量计算失败:", e$message, "\n")
    cat("💡 可能的原因:\n")
    cat("  - powerly包未正确安装\n")
    cat("  - 网络参数不合理\n")
    cat("  - 计算资源不足\n")
    return(NULL)
  })
}

#' 简单导出powerly三步可视化为PDF（按用户示例）
#' @param powerly_result powerly分析结果
#' @param output_dir 输出目录
#' @return 导出的文件路径列表
export_powerly_plots <- function(powerly_result, output_dir) {
  
  if(is.null(powerly_result)) {
    cat("❌ powerly结果为空\n")
    return(NULL)
  }
  
  # 提取原始powerly结果（如果是我们的安全对象）
  results <- if(!is.null(powerly_result$original_result)) {
    powerly_result$original_result
  } else {
    powerly_result
  }
  
  # 确保输出目录存在
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  exported_files <- c()
  
  # 按用户示例的简单方式导出三个步骤
  for(step in 1:3) {
    tryCatch({
      
      # 按FigS6a, FigS6b, FigS6c命名
      filename <- file.path(output_dir, paste0("FigS6", letters[step], "_samplesize", step, ".pdf"))
      
      cat("📊 导出Step", step, ":", basename(filename), "\n")
      
      # 简单的PDF导出，按用户示例
      pdf(filename, height = 8, width = 12)
      plot(results, step = step)
      dev.off()
      
      exported_files <- c(exported_files, filename)
      cat("✅ 成功: ", basename(filename), "\n")
      
    }, error = function(e) {
      cat("❌ Step", step, "失败:", e$message, "\n")
      # 安全关闭PDF设备
      if(dev.cur() > 1) dev.off()
    })
  }
  
  if(length(exported_files) > 0) {
    cat("\n🎉 共导出", length(exported_files), "个PDF文件\n")
  }
  
  return(exported_files)
}

#' 从网络分析结果中提取特征
#' @param network_result 网络分析结果
#' @return 包含节点数和密度的列表
extract_network_features <- function(network_result) {
  if(is.null(network_result)) {
    return(list(nodes = NULL, density = NULL))
  }
  
  nodes <- NULL
  density <- NULL
  
  tryCatch({
    # 处理不同类型的网络对象
    if(inherits(network_result, "qgraph")) {
      # qgraph对象特殊处理
      cat("🔍 处理qgraph对象...\n")
      
      # 首先尝试从layout获取节点数
      if(!is.null(network_result$layout) && is.matrix(network_result$layout)) {
        nodes <- nrow(network_result$layout)
        cat("从layout获取节点数:", nodes, "\n")
      }
      
      # 尝试从graphData的bootnetResult获取网络矩阵
      if(!is.null(network_result$graphData) && inherits(network_result$graphData, "bootnetResult")) {
        bootnet_obj <- network_result$graphData
        if(!is.null(bootnet_obj$graph) && is.matrix(bootnet_obj$graph)) {
          adj_matrix <- bootnet_obj$graph
          if(is.null(nodes)) nodes <- ncol(adj_matrix)
          
          # 显示实际的网络矩阵信息
          cat("📋 网络矩阵详情:\n")
          cat("- 矩阵维度:", dim(adj_matrix), "\n")
          cat("- 矩阵类型:", class(adj_matrix), "\n")
          cat("- 非零元素数量:", sum(adj_matrix != 0), "\n")
          cat("- 矩阵范围:", range(adj_matrix), "\n")
          
          # 显示矩阵内容（如果不太大）
          if(nrow(adj_matrix) <= 10) {
            cat("- 矩阵内容:\n")
            print(adj_matrix)
          }
          
          density <- calculate_network_density(adj_matrix)
          cat("从bootnetResult获取密度:", density, "\n")
        } else if(!is.null(bootnet_obj$sampleTable)) {
          # 从sampleTable构建网络矩阵
          sample_table <- bootnet_obj$sampleTable
          if(is.data.frame(sample_table) && nrow(sample_table) > 0) {
            if(is.null(nodes)) {
              # 从node1和node2列推断节点数
              max_node <- max(c(sample_table$node1, sample_table$node2), na.rm = TRUE)
              nodes <- max_node
            }
            # 计算有效边数
            valid_edges <- sum(sample_table$value != 0, na.rm = TRUE)
            max_edges <- nodes * (nodes - 1) / 2
            density <- if(max_edges > 0) valid_edges / max_edges else 0
            cat("从sampleTable计算密度:", density, "\n")
          }
        }
      }
      
      # 如果上述方法都失败，尝试从Edgelist
      if((is.null(nodes) || is.null(density)) && !is.null(network_result$Edgelist)) {
        edgelist <- network_result$Edgelist
        if(is.list(edgelist) && length(edgelist) > 0) {
          # 如果Edgelist是一个列表，尝试提取信息
          if(!is.null(edgelist$from) && !is.null(edgelist$to)) {
            if(is.null(nodes)) {
              nodes <- max(c(edgelist$from, edgelist$to), na.rm = TRUE)
            }
            if(!is.null(edgelist$weight)) {
              valid_edges <- sum(edgelist$weight != 0, na.rm = TRUE)
              max_edges <- nodes * (nodes - 1) / 2
              density <- if(max_edges > 0) valid_edges / max_edges else 0
            }
          }
        } else if(is.data.frame(edgelist) && nrow(edgelist) > 0) {
          if(is.null(nodes)) {
            nodes <- max(c(edgelist$from, edgelist$to), na.rm = TRUE)
          }
          if("weight" %in% names(edgelist)) {
            valid_edges <- sum(edgelist$weight != 0, na.rm = TRUE)
            max_edges <- nodes * (nodes - 1) / 2
            density <- if(max_edges > 0) valid_edges / max_edges else 0
          }
        }
      }
    } else if(is.matrix(network_result)) {
      # 直接的邻接矩阵
      if(nrow(network_result) == ncol(network_result)) {
        nodes <- ncol(network_result)
        density <- calculate_network_density(network_result)
      }
    } else if(is.list(network_result)) {
      # quickNet结果对象或其他列表对象
      if(!is.null(network_result$graph) && is.matrix(network_result$graph)) {
        nodes <- ncol(network_result$graph)
        density <- calculate_network_density(network_result$graph)
      } else if(!is.null(network_result$network) && is.matrix(network_result$network)) {
        nodes <- ncol(network_result$network)
        density <- calculate_network_density(network_result$network)
      } else if(!is.null(network_result$adjacency) && is.matrix(network_result$adjacency)) {
        nodes <- ncol(network_result$adjacency)
        density <- calculate_network_density(network_result$adjacency)
      } else {
        # 尝试从第一个矩阵元素提取
        matrix_elements <- network_result[sapply(network_result, is.matrix)]
        if(length(matrix_elements) > 0) {
          first_matrix <- matrix_elements[[1]]
          if(nrow(first_matrix) == ncol(first_matrix)) {
            nodes <- ncol(first_matrix)
            density <- calculate_network_density(first_matrix)
          }
        }
      }
    } else if(is.data.frame(network_result)) {
      # 数据框（可能是原始数据）
      nodes <- ncol(network_result)
      density <- 0.4  # 默认中等密度
    }
    
    # 如果仍然没有提取到有效信息，使用调试信息
    if(is.null(nodes) || is.null(density)) {
      cat("⚠️ 网络特征提取失败，网络对象结构:\n")
      if(is.list(network_result)) {
        cat("对象类型: list, 元素名称:", names(network_result), "\n")
        for(name in names(network_result)) {
          element <- network_result[[name]]
          cat("  ", name, ": ", class(element), 
              if(is.matrix(element)) paste0(" [", nrow(element), "x", ncol(element), "]") else "",
              "\n")
        }
      } else {
        cat("对象类型:", class(network_result), "\n")
        if(is.matrix(network_result)) {
          cat("矩阵维度:", dim(network_result), "\n")
        }
      }
      
      # 提供默认值
      nodes <- 10  # 默认节点数
      density <- 0.4  # 默认密度
    }
    
  }, error = function(e) {
    cat("❌ 网络特征提取错误:", e$message, "\n")
    # 提供默认值
    nodes <- 10
    density <- 0.4
  })
  
  # 确保返回值的有效性
  if(is.null(nodes) || !is.numeric(nodes) || nodes <= 0) {
    nodes <- 10
  }
  if(is.null(density) || !is.numeric(density) || density <= 0 || density > 1) {
    density <- 0.4
  }
  
  return(list(nodes = as.integer(nodes), density = as.numeric(density)))
}

#' 计算网络密度
#' @param adj_matrix 邻接矩阵
#' @return 网络密度值
calculate_network_density <- function(adj_matrix) {
  if(is.null(adj_matrix) || !is.matrix(adj_matrix)) {
    cat("⚠️ 邻接矩阵无效，使用默认密度\n")
    return(0.4)  # 默认值
  }
  
  tryCatch({
    # 确保矩阵是方阵
    if(nrow(adj_matrix) != ncol(adj_matrix)) {
      cat("⚠️ 邻接矩阵不是方阵:", dim(adj_matrix), "\n")
      return(0.4)
    }
    
    n <- nrow(adj_matrix)
    if(n <= 1) {
      cat("⚠️ 节点数量过少:", n, "\n")
      return(0.4)
    }
    
    # 创建矩阵副本以避免修改原矩阵
    temp_matrix <- as.matrix(adj_matrix)
    
    # 移除对角线
    diag(temp_matrix) <- 0
    
    # 计算非零边的数量（考虑可能的数值精度问题）
    threshold <- 1e-10
    actual_edges <- sum(abs(temp_matrix) > threshold)
    
    # 对于无向图，每条边被计算两次，所以除以2
    actual_edges <- actual_edges / 2
    
    # 最大可能边数
    max_edges <- n * (n - 1) / 2
    
    if(max_edges == 0) {
      cat("⚠️ 最大边数为0\n")
      return(0)
    }
    
    density <- actual_edges / max_edges
    density <- min(max(density, 0), 1)  # 确保在0-1范围内
    
    cat("📊 密度计算: 实际边数=", actual_edges, ", 最大边数=", max_edges, ", 密度=", round(density, 3), "\n")
    
    return(density)
    
  }, error = function(e) {
    cat("❌ 密度计算错误:", e$message, "\n")
    return(0.4)  # 出错时返回默认值
  })
}

#' 获取powerly预设配置
#' @param preset 预设名称
#' @return 配置参数列表
get_powerly_preset <- function(preset = "balanced") {
  if(!exists("POWERLY_CONFIG")) {
    # 如果配置未加载，使用默认值
    return(list(
      range_lower = 300,
      range_upper = 2000,
      samples = 30,
      replications = 30,
      boots = 1000,
      cores = 2,
      tolerance = 50,
      iterations = 10
    ))
  }
  
  # 获取预设配置
  if(preset %in% names(POWERLY_CONFIG$presets)) {
    config <- POWERLY_CONFIG$presets[[preset]]
  } else {
    config <- POWERLY_CONFIG$presets$balanced
  }
  
  # 合并默认参数
  defaults <- POWERLY_CONFIG$defaults
  for(name in names(defaults)) {
    if(!name %in% names(config)) {
      config[[name]] <- defaults[[name]]
    }
  }
  
  return(config)
}

#' 根据网络规模调整参数
#' @param params 参数列表
#' @param nodes 节点数
#' @return 调整后的参数列表
adjust_params_for_network_size <- function(params, nodes) {
  if(!exists("POWERLY_CONFIG")) {
    return(params)
  }
  
  # 确定网络规模类别
  size_category <- "medium"
  if(nodes <= POWERLY_CONFIG$performance$network_size_adjustments$small$nodes_max) {
    size_category <- "small"
  } else if(nodes <= POWERLY_CONFIG$performance$network_size_adjustments$medium$nodes_max) {
    size_category <- "medium"
  } else {
    size_category <- "large"
  }
  
  # 应用规模调整
  adjustments <- POWERLY_CONFIG$performance$network_size_adjustments[[size_category]]
  
  # 调整boots和cores
  if(!is.null(adjustments$boots)) {
    params$boots <- min(params$boots, adjustments$boots)
  }
  if(!is.null(adjustments$cores)) {
    params$cores <- min(params$cores, adjustments$cores)
  }
  
  # 大型网络启用内存节省模式
  if(size_category == "large") {
    params$save_memory <- TRUE
  }
  
  return(params)
}

#' 生成样本量分析报告
#' @param powerly_result powerly分析结果
#' @return 文本报告
generate_sample_size_report <- function(powerly_result) {
  if(is.null(powerly_result)) {
    return("样本量分析未完成")
  }
  
  # 提取关键信息
  recommendation <- powerly_result$recommendation
  if(is.null(recommendation)) {
    recommendation <- "无法确定"
  }
  
  network_info <- powerly_result$network_info
  if(is.null(network_info)) {
    network_info <- list(nodes = "未知", density = "未知")
  }
  
  # 生成解释
  interpretation <- interpret_sample_size(recommendation)
  
  # 构建报告
  report <- paste0(
    "# 样本量分析报告\n\n",
    "## 网络特征\n",
    "- 节点数量: ", network_info$nodes, "\n",
    "- 网络密度: ", round(network_info$density, 3), "\n\n",
    "## 分析结果\n", 
    "- 推荐样本量: **", recommendation, "**\n",
    "- 分析日期: ", format(network_info$analysis_date, "%Y-%m-%d %H:%M:%S"), "\n\n",
    "## 结果解释\n",
    interpretation, "\n\n",
    "## 建议\n",
    "- 建议的样本量是基于目标敏感性和统计功效计算得出\n",
    "- 实际研究中应考虑预期的数据缺失率\n",
    "- 如需更保守的结果，可考虑增加20-30%的样本量\n"
  )
  
  return(report)
}

#' 解释样本量结果
#' @param sample_size 推荐样本量
#' @return 解释文本
interpret_sample_size <- function(sample_size) {
  # 处理向量输入，取第一个值或中位数
  if(is.numeric(sample_size) && length(sample_size) > 1) {
    sample_size <- sample_size[length(sample_size) %/% 2 + 1]  # 取中间值
  }
  
  # 确保是单个数值
  if(!is.numeric(sample_size) || length(sample_size) != 1 || is.na(sample_size)) {
    return("样本量解释不可用")
  }
  
  if(!exists("POWERLY_CONFIG")) {
    return("样本量解释信息不可用")
  }
  
  ranges <- POWERLY_CONFIG$interpretation$sample_size_ranges
  
  for(range_name in names(ranges)) {
    if(sample_size <= ranges[[range_name]]$max) {
      return(ranges[[range_name]]$interpretation)
    }
  }
  
  return("优秀样本，支持复杂网络分析和比较")
}

#' 测试powerly包是否正常工作
#' @return 测试结果
test_powerly_package <- function() {
  if(!requireNamespace("powerly", quietly = TRUE)) {
    return("powerly包未安装")
  }
  
  cat("🧪 测试powerly包基础功能...\n")
  
  tryCatch({
    # 简化的测试，只使用关键参数
    test_result <- powerly::powerly(
      range_lower = 50,
      range_upper = 100,
      nodes = 3,
      density = 0.5,
      cores = 1
    )
    
    cat("- Powerly测试成功\n")
    cat("- 测试结果类型:", class(test_result), "\n")
    
    if(!is.null(test_result$recommendation)) {
      cat("- 测试推荐值:", test_result$recommendation, "\n")
    }
    
    return("测试成功")
    
  }, error = function(e) {
    cat("- Powerly测试失败:", e$message, "\n")
    return(paste("测试失败:", e$message))
  })
}

#' 调试网络对象结构
#' @param network_result 网络分析结果
#' @return 调试信息字符串
debug_network_structure <- function(network_result) {
  if(is.null(network_result)) {
    return("网络结果为NULL")
  }
  
  info <- c()
  info <- c(info, paste("对象类型:", paste(class(network_result), collapse = ", ")))
  
  if(is.list(network_result)) {
    info <- c(info, paste("列表元素数量:", length(network_result)))
    info <- c(info, paste("元素名称:", paste(names(network_result), collapse = ", ")))
    
    for(name in names(network_result)) {
      element <- network_result[[name]]
      if(is.matrix(element)) {
        info <- c(info, paste("  ", name, ": 矩阵", dim(element)[1], "x", dim(element)[2]))
      } else if(is.data.frame(element)) {
        info <- c(info, paste("  ", name, ": 数据框", nrow(element), "x", ncol(element)))
      } else if(is.list(element)) {
        info <- c(info, paste("  ", name, ": 列表，长度", length(element)))
        if(name == "Edgelist" && length(element) > 0) {
          # 详细检查Edgelist结构
          info <- c(info, paste("    Edgelist子元素:", paste(names(element), collapse = ", ")))
        } else if(name == "graphData" && inherits(element, "bootnetResult")) {
          # 详细检查bootnetResult结构
          info <- c(info, paste("    bootnetResult子元素:", paste(names(element), collapse = ", ")))
          if(!is.null(element$graph) && is.matrix(element$graph)) {
            info <- c(info, paste("    graph矩阵:", dim(element$graph)[1], "x", dim(element$graph)[2]))
          }
          if(!is.null(element$sampleTable) && is.data.frame(element$sampleTable)) {
            info <- c(info, paste("    sampleTable:", nrow(element$sampleTable), "行"))
          }
        }
      } else {
        info <- c(info, paste("  ", name, ":", class(element)[1], "长度", length(element)))
      }
    }
    
    # 如果是qgraph对象，尝试提取特征信息
    if(inherits(network_result, "qgraph")) {
      info <- c(info, "\n=== qgraph特征提取测试 ===")
      
      # 测试layout
      if(!is.null(network_result$layout) && is.matrix(network_result$layout)) {
        info <- c(info, paste("Layout节点数:", nrow(network_result$layout)))
      }
      
      # 测试graphData
      if(!is.null(network_result$graphData) && inherits(network_result$graphData, "bootnetResult")) {
        bootnet_obj <- network_result$graphData
        if(!is.null(bootnet_obj$graph)) {
          info <- c(info, paste("GraphData类型:", class(bootnet_obj$graph)))
          if(is.matrix(bootnet_obj$graph)) {
            info <- c(info, paste("GraphData矩阵维度:", paste(dim(bootnet_obj$graph), collapse = "x")))
          }
        }
      }
      
      # 测试Edgelist
      if(!is.null(network_result$Edgelist)) {
        edgelist <- network_result$Edgelist
        info <- c(info, paste("Edgelist类型:", class(edgelist)))
        if(is.list(edgelist) && !is.null(edgelist$from)) {
          info <- c(info, paste("Edgelist边数:", length(edgelist$from)))
        }
      }
    }
  } else if(is.matrix(network_result)) {
    info <- c(info, paste("矩阵维度:", paste(dim(network_result), collapse = "x")))
  }
  
  return(paste(info, collapse = "\n"))
}

#' 测试powerly包是否正常工作
#' @return 测试结果
test_powerly_package <- function() {
  if(!requireNamespace("powerly", quietly = TRUE)) {
    return("powerly包未安装")
  }
  
  cat("🧪 测试powerly包基础功能...\n")
  
  tryCatch({
    # 简化的测试，使用更大的范围
    test_result <- powerly::powerly(
      range_lower = 50,
      range_upper = 200,
      nodes = 3,
      density = 0.5,
      cores = 1
    )
    
    cat("- Powerly测试成功\n")
    cat("- 测试结果类型:", class(test_result), "\n")
    
    if(!is.null(test_result$recommendation)) {
      cat("- 测试推荐值:", test_result$recommendation, "\n")
    }
    
    return("测试成功")
    
  }, error = function(e) {
    cat("- Powerly测试失败:", e$message, "\n")
    return(paste("测试失败:", e$message))
  })
}

#' 计算变量频数分布
#' @param data 数据框
#' @param variables 变量名向量
#' @param show_missing 是否显示缺失值统计
#' @return 包含频数分布信息的列表
calculate_frequency_distribution <- function(data, variables, show_missing = TRUE) {
  if(is.null(data) || is.null(variables) || length(variables) == 0) {
    return(NULL)
  }
  
  # 确保变量存在于数据中
  existing_vars <- variables[variables %in% names(data)]
  if(length(existing_vars) == 0) {
    return(NULL)
  }
  
  freq_results <- list()
  
  for(var in existing_vars) {
    var_data <- data[[var]]
    
    # 跳过完全缺失的变量
    if(all(is.na(var_data))) {
      freq_results[[var]] <- list(
        type = "全缺失",
        frequencies = data.frame(value = "NA", count = length(var_data), percent = 100.0, warning = TRUE),
        total_count = length(var_data),
        missing_count = length(var_data),
        valid_count = 0
      )
      next
    }
    
    # 移除缺失值计算频数
    valid_data <- var_data[!is.na(var_data)]
    missing_count <- sum(is.na(var_data))
    total_count <- length(var_data)
    valid_count <- length(valid_data)
    
    # 计算频数表
    freq_table <- table(valid_data, useNA = "no")
    freq_df <- data.frame(
      value = names(freq_table),
      count = as.numeric(freq_table),
      stringsAsFactors = FALSE
    )
    
    # 计算百分比
    freq_df$percent <- round((freq_df$count / valid_count) * 100, 1)
    
    # 标记警告（占比<5%）
    freq_df$warning <- freq_df$percent < 5.0
    
    # 如果需要显示缺失值且存在缺失值
    if(show_missing && missing_count > 0) {
      missing_row <- data.frame(
        value = "缺失值",
        count = missing_count,
        percent = round((missing_count / total_count) * 100, 1),
        warning = missing_count > 0,
        stringsAsFactors = FALSE
      )
      freq_df <- rbind(freq_df, missing_row)
    }
    
    # 按频数降序排序
    freq_df <- freq_df[order(-freq_df$count), ]
    
    # 判断变量类型
    var_type <- if(all(valid_data %in% c(0, 1))) {
      "二元变量"
    } else if(is.numeric(valid_data) && length(unique(valid_data)) <= 10) {
      "分类变量"
    } else if(is.numeric(valid_data)) {
      "连续变量"
    } else {
      "其他类型"
    }
    
    freq_results[[var]] <- list(
      type = var_type,
      frequencies = freq_df,
      total_count = total_count,
      missing_count = missing_count,
      valid_count = valid_count
    )
  }
  
  return(freq_results)
}

#' 生成频数分布的HTML显示
#' @param freq_results 频数分布计算结果
#' @return HTML字符串
generate_frequency_display_html <- function(freq_results) {
  if(is.null(freq_results) || length(freq_results) == 0) {
    return("<p style='color: #666;'>暂无频数分布信息</p>")
  }
  
  html_parts <- c()
  
  for(var_name in names(freq_results)) {
    freq_info <- freq_results[[var_name]]
    
    # 变量标题
    html_parts <- c(html_parts, paste0(
      "<div style='margin-bottom: 15px; border: 1px solid #ddd; padding: 10px; border-radius: 5px;'>",
      "<h5 style='margin: 0 0 10px 0; color: #2c3e50;'>",
      "<strong>", var_name, "</strong> (", freq_info$type, ")",
      "</h5>"
    ))
    
    # 样本量信息
    html_parts <- c(html_parts, paste0(
      "<p style='margin: 5px 0; font-size: 12px; color: #7f8c8d;'>",
      "总样本: ", freq_info$total_count, 
      " | 有效: ", freq_info$valid_count,
      if(freq_info$missing_count > 0) paste0(" | 缺失: ", freq_info$missing_count) else "",
      "</p>"
    ))
    
    # 频数表
    if(nrow(freq_info$frequencies) > 0) {
      html_parts <- c(html_parts, "<table style='width: 100%; border-collapse: collapse; font-size: 12px;'>")
      html_parts <- c(html_parts, "<tr style='background-color: #f8f9fa;'>")
      html_parts <- c(html_parts, "<th style='border: 1px solid #dee2e6; padding: 5px; text-align: left;'>取值</th>")
      html_parts <- c(html_parts, "<th style='border: 1px solid #dee2e6; padding: 5px; text-align: right;'>频数</th>")
      html_parts <- c(html_parts, "<th style='border: 1px solid #dee2e6; padding: 5px; text-align: right;'>百分比</th>")
      html_parts <- c(html_parts, "</tr>")
      
      for(i in 1:nrow(freq_info$frequencies)) {
        row <- freq_info$frequencies[i, ]
        
        # 设置行样式（警告行用红色背景）
        row_style <- if(row$warning) {
          "background-color: #ffebee; color: #c62828;"
        } else {
          "background-color: white;"
        }
        
        html_parts <- c(html_parts, paste0(
          "<tr style='", row_style, "'>",
          "<td style='border: 1px solid #dee2e6; padding: 5px;'>", row$value, "</td>",
          "<td style='border: 1px solid #dee2e6; padding: 5px; text-align: right;'>", row$count, "</td>",
          "<td style='border: 1px solid #dee2e6; padding: 5px; text-align: right;'>", row$percent, "%</td>",
          "</tr>"
        ))
      }
      
      html_parts <- c(html_parts, "</table>")
      
      # 警告提示
      warning_rows <- freq_info$frequencies[freq_info$frequencies$warning, ]
      if(nrow(warning_rows) > 0 && any(warning_rows$value != "缺失值")) {
        warning_values <- warning_rows$value[warning_rows$value != "缺失值"]
        if(length(warning_values) > 0) {
          html_parts <- c(html_parts, paste0(
            "<p style='margin: 8px 0 0 0; font-size: 11px; color: #d32f2f;'>",
            "<i class='fa fa-exclamation-triangle'></i> ",
            "⚠️ 以下类别占比<5%: ", paste(warning_values, collapse = ", "),
            "</p>"
          ))
        }
      }
    }
    
    html_parts <- c(html_parts, "</div>")
  }
  
  return(paste(html_parts, collapse = "\n"))
}
