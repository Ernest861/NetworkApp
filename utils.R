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
  args <- c(args, list(...))
  
  # 调用quickNet
  network_result <- do.call(quickNet::quickNet, args)
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
      cat("🔍 调试模型", model_name, "的温度计算...\n")
      cat("  parameters表列名:", paste(names(params), collapse = ", "), "\n")
      cat("  parameters表前5行:\n")
      print(head(params, 5))
      
      # 检查各种可能的列名和方法
      beta_params <- c()
      
      if("matrix" %in% names(params)) {
        # 方法1: 使用matrix列 (参考calculate_temperature.R第178行)
        beta_params <- params[params$matrix == "beta", "est"]
        cat("  使用params$matrix找到beta参数数量:", length(beta_params), "\n")
      }
      
      if(length(beta_params) == 0 && "param" %in% names(params)) {
        # 方法2: 使用param列
        beta_params <- params[params$param == "beta", "est"]
        cat("  使用params$param找到beta参数数量:", length(beta_params), "\n")
      }
      
      if(length(beta_params) == 0 && "par" %in% names(params)) {
        # 方法3: 使用par列
        beta_params <- params[params$par == "beta", "est"]
        cat("  使用params$par找到beta参数数量:", length(beta_params), "\n")
      }
      
      if(length(beta_params) == 0) {
        cat("  所有方法都未找到beta参数\n")
      }
      
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
    
    # 手动计算AIC/BIC比较
    cat("  使用手动方法计算模型比较...\n")
    aic_values <- tryCatch({
      sapply(models, function(m) psychonetrics::fit(m)$AIC)
    }, error = function(e2) {
      cat("    AIC计算失败:", e2$message, "\n")
      rep(NA, length(models))
    })
    
    bic_values <- tryCatch({
      sapply(models, function(m) psychonetrics::fit(m)$BIC)
    }, error = function(e2) {
      cat("    BIC计算失败:", e2$message, "\n")
      rep(NA, length(models))
    })
    
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
`%||%` <- function(x, y) if(is.null(x)) y else x