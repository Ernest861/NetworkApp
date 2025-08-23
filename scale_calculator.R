# =============================================================================
# 量表计算器 - Scale Calculator
# 基于CSV配置文件的动态量表计算模块，适配Shiny应用
# =============================================================================

#' 读取量表配置文件
#' @param config_path 配置文件路径
#' @return 配置数据框
load_scale_config <- function(config_path = "scale_config_complete.csv") {
  if (!file.exists(config_path)) {
    return(data.frame())  # 返回空配置
  }
  
  tryCatch({
    read.csv(config_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  }, error = function(e) {
    warning("无法读取量表配置文件: ", e$message)
    return(data.frame())
  })
}

#' 解析题目编号字符串（支持范围格式如"1-9"和列表格式如"1,2,3"）
#' @param item_numbers_str 题目编号字符串
#' @return 数值向量
parse_item_numbers <- function(item_numbers_str) {
  if (is.na(item_numbers_str) || item_numbers_str == "") {
    return(numeric(0))
  }
  
  # 分割逗号分隔的项目
  items <- trimws(strsplit(item_numbers_str, ",")[[1]])
  numbers <- c()
  
  for (item in items) {
    if (grepl("-", item)) {
      # 处理范围格式如"1-9"
      range_parts <- as.numeric(strsplit(item, "-")[[1]])
      if (length(range_parts) == 2 && !any(is.na(range_parts))) {
        numbers <- c(numbers, seq(range_parts[1], range_parts[2]))
      }
    } else {
      # 处理单个数字
      num <- as.numeric(item)
      if (!is.na(num)) {
        numbers <- c(numbers, num)
      }
    }
  }
  
  return(unique(numbers))
}

#' 检测数据中存在的量表
#' @param data 输入数据框
#' @param config 量表配置
#' @return 检测结果列表
detect_available_scales <- function(data, config) {
  if (nrow(config) == 0) {
    return(list())
  }
  
  available_scales <- list()
  
  for (i in 1:nrow(config)) {
    scale_name <- config$scale_name[i]
    item_prefix <- config$item_prefix[i]
    item_numbers_str <- config$item_numbers[i]
    
    # 解析题目编号
    item_numbers <- parse_item_numbers(item_numbers_str)
    scale_items <- paste0(item_prefix, item_numbers)
    
    # 检查数据中存在的题目
    existing_items <- intersect(scale_items, names(data))
    
    if (length(existing_items) >= 3) {  # 至少3个题目才认为量表可用
      available_scales[[scale_name]] <- list(
        config_row = i,
        total_items = length(scale_items),
        existing_items = existing_items,
        coverage_rate = length(existing_items) / length(scale_items),
        item_prefix = item_prefix,
        calculation_method = config$calculation_method[i],
        subscales = config$subscales_definition[i],
        notes = config$notes[i]
      )
    }
  }
  
  return(available_scales)
}

#' 应用反向计分
#' @param data 数据框
#' @param reverse_items 反向计分题目
#' @param reverse_method 反向计分方法
#' @return 处理后的数据
apply_reverse_coding <- function(data, reverse_items, reverse_method = "auto") {
  if (length(reverse_items) == 0) {
    return(data)
  }
  
  for (rev_item in reverse_items) {
    if (rev_item %in% names(data)) {
      item_values <- data[[rev_item]]
      non_na_values <- item_values[!is.na(item_values)]
      
      if (length(non_na_values) > 0) {
        if (reverse_method == "auto" || reverse_method == "max_min") {
          # 自动反向计分：(max + min) - 原值
          max_val <- max(non_na_values)
          min_val <- min(non_na_values)
          data[[rev_item]] <- (max_val + min_val) - data[[rev_item]]
        } else if (reverse_method == "6minus") {
          # 6减法：6 - 原值
          data[[rev_item]] <- 6 - data[[rev_item]]
        } else if (reverse_method == "5minus") {
          # 5减法：5 - 原值
          data[[rev_item]] <- 5 - data[[rev_item]]
        }
      }
    }
  }
  
  return(data)
}

#' 解析子量表定义
#' @param subscales_definition 子量表定义字符串
#' @param item_prefix 题目前缀
#' @return 子量表列表
parse_subscales <- function(subscales_definition, item_prefix) {
  if (is.na(subscales_definition) || subscales_definition == "") {
    return(list())
  }
  
  subscales <- list()
  
  # 分割不同的子量表
  subscale_parts <- strsplit(subscales_definition, "\\|")[[1]]
  
  for (part in subscale_parts) {
    if (grepl(":", part)) {
      parts <- strsplit(part, ":")[[1]]
      if (length(parts) == 2) {
        subscale_name <- trimws(parts[1])
        item_numbers_str <- trimws(parts[2])
        
        item_numbers <- parse_item_numbers(item_numbers_str)
        subscale_items <- paste0(item_prefix, item_numbers)
        
        subscales[[subscale_name]] <- subscale_items
      }
    }
  }
  
  return(subscales)
}

#' 计算量表分数
#' @param data 输入数据
#' @param scale_config 单个量表的配置行
#' @return 包含计算结果的列表
calculate_single_scale <- function(data, scale_config) {
  
  scale_name <- scale_config$scale_name
  item_prefix <- scale_config$item_prefix
  item_numbers_str <- scale_config$item_numbers
  reverse_items_str <- scale_config$reverse_items
  min_valid_items <- scale_config$min_valid_items
  calculation_method <- scale_config$calculation_method
  subscales_definition <- scale_config$subscales_definition
  special_logic <- scale_config$special_logic  # 添加特殊逻辑字段
  
  # 解析题目编号
  item_numbers <- parse_item_numbers(item_numbers_str)
  scale_items <- paste0(item_prefix, item_numbers)
  existing_items <- intersect(scale_items, names(data))
  
  if (length(existing_items) == 0) {
    return(list(success = FALSE, message = paste("量表", scale_name, "未找到任何题目")))
  }
  
  # 提取量表数据
  scale_data <- data[, existing_items, drop = FALSE]
  result_data <- data
  new_variables <- character(0)
  
  # 处理反向计分
  if (!is.na(reverse_items_str) && reverse_items_str != "") {
    reverse_numbers <- parse_item_numbers(reverse_items_str)
    reverse_items <- paste0(item_prefix, reverse_numbers)
    reverse_items <- intersect(reverse_items, existing_items)
    
    if (length(reverse_items) > 0) {
      scale_data <- apply_reverse_coding(scale_data, reverse_items)
    }
  }
  
  # 🔥 处理特殊逻辑（重要：在反向计分后、总分计算前）
  if (!is.na(special_logic) && special_logic != "") {
    result_data <- apply_special_logic_calculator(result_data, scale_name, item_prefix, item_numbers, special_logic)
    # 重新提取处理后的数据（特殊逻辑可能修改了原始题目数据）
    scale_data <- result_data[, existing_items, drop = FALSE]
  }
  
  # 计算有效题目数
  valid_count <- rowSums(!is.na(scale_data))
  
  # 设置最少有效题目数
  if (is.na(min_valid_items)) {
    min_valid_items <- ceiling(length(existing_items) * 0.8)
  }
  
  # 计算总分 - 根据计算方法命名
  if (calculation_method == "mean") {
    total_score_name <- paste0(scale_name, "_mean")
    # 均值计算
    result_data[[total_score_name]] <- ifelse(valid_count >= min_valid_items,
                                             rowMeans(scale_data, na.rm = TRUE),
                                             NA)
    new_variables <- c(new_variables, total_score_name)
  } else if (calculation_method == "sum") {
    total_score_name <- paste0(scale_name, "_sum")
    # 求和计算  
    result_data[[total_score_name]] <- ifelse(valid_count >= min_valid_items,
                                             rowSums(scale_data, na.rm = TRUE),
                                             NA)
    new_variables <- c(new_variables, total_score_name)
  } else if (calculation_method == "weighted_mean") {
    total_score_name <- paste0(scale_name, "_weighted")
    # 加权平均
    result_data[[total_score_name]] <- ifelse(valid_count >= min_valid_items,
                                             rowMeans(scale_data, na.rm = TRUE),
                                             NA)
    new_variables <- c(new_variables, total_score_name)
  } else if (calculation_method == "threshold") {
    total_score_name <- paste0(scale_name, "_threshold")
    # 阈值计算（用于诊断量表）
    result_data[[total_score_name]] <- calculate_threshold_score(scale_data, subscales_definition)
    new_variables <- c(new_variables, total_score_name)
  } else if (calculation_method == "special") {
    # 🔥 特殊计算方法：总分已由特殊逻辑函数处理，检查是否存在
    total_score_name <- paste0(scale_name, "_Total")
    if (total_score_name %in% names(result_data)) {
      new_variables <- c(new_variables, total_score_name)
    }
    # 检查其他可能的特殊变量（如诊断变量）
    possible_vars <- c(paste0(scale_name, "_Diagnosis"), "dsm1", "dsm2", "dsm4", "dsm5", "dsm6")
    existing_special_vars <- possible_vars[possible_vars %in% names(result_data)]
    if (length(existing_special_vars) > 0) {
      new_variables <- c(new_variables, existing_special_vars)
    }
  } else {
    # 其他方法或Total - 使用Total后缀
    total_score_name <- paste0(scale_name, "_Total")
    result_data[[total_score_name]] <- ifelse(valid_count >= min_valid_items,
                                             rowMeans(scale_data, na.rm = TRUE),
                                             NA)
    new_variables <- c(new_variables, total_score_name)
  }
  
  # 计算子量表分数（特殊方法跳过常规子量表计算）
  if (calculation_method != "special") {
    subscales <- parse_subscales(subscales_definition, item_prefix)
    
    for (subscale_name in names(subscales)) {
      subscale_items <- subscales[[subscale_name]]
      existing_subscale_items <- intersect(subscale_items, existing_items)
      
      if (length(existing_subscale_items) >= 2) {
        subscale_data <- scale_data[, existing_subscale_items, drop = FALSE]
        subscale_valid_count <- rowSums(!is.na(subscale_data))
        min_subscale_items <- ceiling(length(existing_subscale_items) * 0.7)
        
        if (calculation_method == "sum") {
          result_data[[subscale_name]] <- ifelse(subscale_valid_count >= min_subscale_items,
                                                rowSums(subscale_data, na.rm = TRUE),
                                                NA)
        } else {
          result_data[[subscale_name]] <- ifelse(subscale_valid_count >= min_subscale_items,
                                                rowMeans(subscale_data, na.rm = TRUE),
                                                NA)
        }
        
        new_variables <- c(new_variables, subscale_name)
      }
    }
  }
  
  return(list(
    success = TRUE,
    data = result_data,
    new_variables = new_variables,
    scale_name = scale_name,
    total_items = length(scale_items),
    existing_items = length(existing_items),
    coverage_rate = length(existing_items) / length(scale_items)
  ))
}

#' 阈值计分（用于诊断量表）
#' @param scale_data 量表数据
#' @param subscales_definition 子量表定义（包含阈值信息）
#' @return 阈值分数
calculate_threshold_score <- function(scale_data, subscales_definition) {
  # 这里是简化版本，具体阈值逻辑需要根据量表特点定制
  # 暂时返回均值
  return(rowMeans(scale_data, na.rm = TRUE))
}

#' 批量计算所有可用量表
#' @param data 输入数据
#' @param config 量表配置
#' @param selected_scales 用户选择的量表（NULL表示全部）
#' @return 计算结果列表
calculate_all_scales <- function(data, config, selected_scales = NULL) {
  
  if (nrow(config) == 0) {
    return(list(success = FALSE, message = "没有可用的量表配置"))
  }
  
  # 检测可用量表
  available_scales <- detect_available_scales(data, config)
  
  if (length(available_scales) == 0) {
    return(list(success = FALSE, message = "数据中未检测到任何完整的量表"))
  }
  
  # 如果指定了选择的量表，则过滤
  if (!is.null(selected_scales)) {
    available_scales <- available_scales[names(available_scales) %in% selected_scales]
  }
  
  if (length(available_scales) == 0) {
    return(list(success = FALSE, message = "所选量表在数据中不完整"))
  }
  
  # 批量计算
  result_data <- data
  all_new_variables <- character(0)
  calculation_summary <- list()
  
  for (scale_name in names(available_scales)) {
    scale_info <- available_scales[[scale_name]]
    config_row <- config[scale_info$config_row, ]
    
    calc_result <- calculate_single_scale(result_data, config_row)
    
    if (calc_result$success) {
      result_data <- calc_result$data
      all_new_variables <- c(all_new_variables, calc_result$new_variables)
      
      calculation_summary[[scale_name]] <- list(
        total_items = calc_result$total_items,
        existing_items = calc_result$existing_items,
        coverage_rate = calc_result$coverage_rate,
        new_variables = calc_result$new_variables
      )
    }
  }
  
  return(list(
    success = TRUE,
    data = result_data,
    new_variables = all_new_variables,
    summary = calculation_summary,
    available_scales = available_scales
  ))
}

#' 生成量表计算报告
#' @param calculation_result 计算结果
#' @return HTML格式的报告
generate_calculation_report <- function(calculation_result) {
  
  if (!calculation_result$success) {
    return(paste0("<div class='alert alert-danger'>", calculation_result$message, "</div>"))
  }
  
  summary <- calculation_result$summary
  total_new_vars <- length(calculation_result$new_variables)
  
  report <- paste0(
    "<div class='panel panel-success'>",
    "<div class='panel-heading'><h5>📊 量表计算完成</h5></div>",
    "<div class='panel-body'>",
    "<p><strong>成功计算 ", length(summary), " 个量表，新增 ", total_new_vars, " 个变量</strong></p>",
    "<hr>"
  )
  
  for (scale_name in names(summary)) {
    info <- summary[[scale_name]]
    coverage_color <- if (info$coverage_rate >= 0.9) "success" else if (info$coverage_rate >= 0.7) "warning" else "danger"
    
    # 检查是否为手动规则
    is_manual <- !is.null(info$is_manual) && info$is_manual
    scale_type_icon <- if (is_manual) "📝" else "📋"
    scale_type_text <- if (is_manual) " (手动规则)" else " (配置文件)"
    
    report <- paste0(report,
      "<h6>", scale_type_icon, " ", scale_name, scale_type_text, "</h6>",
      "<ul class='small'>",
      "<li>选用变量: ", info$existing_items, "/", info$total_items, 
      " (<span class='label label-", coverage_color, "'>", round(info$coverage_rate * 100, 1), "%</span>)</li>",
      "<li>新增变量: ", paste(info$new_variables, collapse = ", "), "</li>",
      if (is_manual) paste0("<li>计算方法: ", info$calculation_method, "</li>") else "",
      "</ul>"
    )
  }
  
  report <- paste0(report, "</div></div>")
  
  return(report)
}

#' 自定义算法接口
#' @param data 数据框
#' @param items 题目列表
#' @param method 算法名称
#' @param params 参数列表
#' @return 计算结果
custom_calculation <- function(data, items, method, params = list()) {
  
  existing_items <- intersect(items, names(data))
  if (length(existing_items) == 0) {
    return(rep(NA, nrow(data)))
  }
  
  scale_data <- data[, existing_items, drop = FALSE]
  
  switch(method,
    "weighted_mean" = {
      # 加权平均
      weights <- params$weights %||% rep(1, ncol(scale_data))
      if (length(weights) == ncol(scale_data)) {
        return(apply(scale_data, 1, function(x) {
          if (sum(!is.na(x)) == 0) return(NA)
          weighted.mean(x, weights, na.rm = TRUE)
        }))
      } else {
        return(rowMeans(scale_data, na.rm = TRUE))
      }
    },
    "max_score" = {
      # 最高分
      return(apply(scale_data, 1, max, na.rm = TRUE))
    },
    "min_score" = {
      # 最低分  
      return(apply(scale_data, 1, min, na.rm = TRUE))
    },
    "median_score" = {
      # 中位数
      return(apply(scale_data, 1, median, na.rm = TRUE))
    },
    # 默认使用均值
    return(rowMeans(scale_data, na.rm = TRUE))
  )
}

#' 检测下划线分隔的变量模式
#' @param data 数据框
#' @return 按模式分组的变量列表
detect_underscore_patterns <- function(data) {
  
  var_names <- names(data)
  underscore_vars <- var_names[grepl("_", var_names)]
  
  if(length(underscore_vars) == 0) {
    return(list())
  }
  
  # 提取前缀模式
  patterns <- list()
  
  for(var in underscore_vars) {
    # 提取下划线前的部分作为模式
    parts <- strsplit(var, "_")[[1]]
    if(length(parts) >= 2) {
      # 使用前缀作为模式
      prefix <- parts[1]
      
      # 如果后面是数字，则认为是量表题目
      if(length(parts) >= 2 && grepl("^\\d+$", parts[2])) {
        pattern_key <- prefix
      } else if(length(parts) >= 3 && grepl("^\\d+$", parts[3])) {
        # 如果是 PREFIX_SUBTYPE_NUMBER 格式
        pattern_key <- paste(parts[1], parts[2], sep = "_")
      } else {
        # 其他情况用前两部分
        pattern_key <- paste(parts[1], parts[2], sep = "_")
      }
      
      if(is.null(patterns[[pattern_key]])) {
        patterns[[pattern_key]] <- character(0)
      }
      patterns[[pattern_key]] <- c(patterns[[pattern_key]], var)
    }
  }
  
  # 只保留有3个或以上变量的模式
  patterns <- patterns[sapply(patterns, length) >= 3]
  
  return(patterns)
}

# 🔥 特殊逻辑处理函数（scale_calculator版本）
apply_special_logic_calculator <- function(data, scale_name, item_prefix, item_numbers, special_logic) {
  if (grepl("recode_1_5to1;dsm_binary_logic", special_logic)) {
    # NSSI_DSM5特殊处理：先重编码1-5到0-1，然后应用DSM诊断逻辑
    return(calculate_nssi_dsm5_logic_calculator(data, scale_name, item_prefix, item_numbers))
  } else if (grepl("minus_1_first", special_logic)) {
    # YFAS系列：先减1处理
    return(apply_minus_1_first_calculator(data, scale_name, item_prefix, item_numbers, special_logic))
  }
  
  return(data)
}

# NSSI DSM-5 特殊逻辑处理（按照用户实际处理方法）
calculate_nssi_dsm5_logic_calculator <- function(data, scale_name, item_prefix, item_numbers) {
  
  cat("🔧 执行NSSI_DSM5特殊逻辑处理...\n")
  
  # 构建题目变量名
  item_vars <- paste0(item_prefix, item_numbers)
  existing_items <- item_vars[item_vars %in% names(data)]
  
  if(length(existing_items) == 0) {
    cat("❌ 未找到NSSI_DSM5相关变量\n")
    return(data)
  }
  
  # 🧼 第一步：清理数据，去掉标签并转换为数值
  cat("第1步：数据清理和转换为数值\n")
  for(item in existing_items) {
    # 处理带标签的数据（类似haven::zap_labels）
    if(is.factor(data[[item]])) {
      data[[item]] <- as.numeric(as.character(data[[item]]))
    } else if(!is.numeric(data[[item]])) {
      data[[item]] <- as.numeric(data[[item]])
    }
  }
  
  # 🔄 第二步：NSSI_DSM5_1的特殊重编码逻辑（只有1或5算满足）
  cat("第2步：NSSI_DSM5_1特殊重编码（1或5→1，其他→0）\n")
  if(paste0(item_prefix, "1") %in% names(data)) {
    original_var <- paste0(item_prefix, "1")
    original_values <- data[[original_var]]
    # 只有回答1或5的算满足（1），其他都是0
    data[[original_var]] <- ifelse(is.na(original_values), NA,
                                 ifelse(original_values %in% c(1, 5), 1, 0))
    cat("  - NSSI_DSM5_1重编码完成\n")
  }
  
  # 🎯 第三步：创建六个DSM维度变量（dsm1到dsm6）
  cat("第3步：创建DSM维度变量（dsm1-dsm6）\n")
  
  # dsm1 = NSSI_DSM5_1 (直接使用重编码后的值)
  if(paste0(item_prefix, "1") %in% names(data)) {
    data[["dsm1"]] <- data[[paste0(item_prefix, "1")]]
    cat("  - dsm1创建完成（等于NSSI_DSM5_1）\n")
  }
  
  # dsm2: 检查是否有多选题目（NSSI_DSM5_2_1_multi到NSSI_DSM5_2_5_multi）
  dsm2_multi_items <- paste0(item_prefix, "2_", 1:5, "_multi")
  dsm2_multi_existing <- dsm2_multi_items[dsm2_multi_items %in% names(data)]
  
  if(length(dsm2_multi_existing) > 0) {
    # 多选题逻辑：任意一个子题满足就算1
    data[["dsm2"]] <- as.numeric(rowSums(data[dsm2_multi_existing], na.rm = TRUE) > 0)
    cat("  - dsm2创建完成（基于多选子题）\n")
  } else if(paste0(item_prefix, "2") %in% names(data)) {
    # 如果没有多选子题，使用主题目
    data[["dsm2"]] <- ifelse(data[[paste0(item_prefix, "2")]] == 1, 1, 0)
    cat("  - dsm2创建完成（基于主题目）\n")
  }
  
  # dsm3: 检查是否有多选题目（NSSI_DSM5_3_1_multi到NSSI_DSM5_3_4_multi）
  dsm3_multi_items <- paste0(item_prefix, "3_", 1:4, "_multi")
  dsm3_multi_existing <- dsm3_multi_items[dsm3_multi_items %in% names(data)]
  
  if(length(dsm3_multi_existing) > 0) {
    # 多选题逻辑：任意一个子题满足就算1
    data[["dsm3"]] <- as.numeric(rowSums(data[dsm3_multi_existing], na.rm = TRUE) > 0)
    cat("  - dsm3创建完成（基于多选子题）\n")
  } else if(paste0(item_prefix, "3") %in% names(data)) {
    # 如果没有多选子题，使用主题目
    data[["dsm3"]] <- ifelse(data[[paste0(item_prefix, "3")]] == 1, 1, 0)
    cat("  - dsm3创建完成（基于主题目）\n")
  }
  
  # dsm4: NSSI_DSM5_4（是否满足）
  if(paste0(item_prefix, "4") %in% names(data)) {
    data[["dsm4"]] <- as.numeric(data[[paste0(item_prefix, "4")]] == 1)
    cat("  - dsm4创建完成\n")
  }
  
  # dsm5: NSSI_DSM5_5（是否满足）
  if(paste0(item_prefix, "5") %in% names(data)) {
    data[["dsm5"]] <- as.numeric(data[[paste0(item_prefix, "5")]] == 1)
    cat("  - dsm5创建完成\n")
  }
  
  # dsm6: NSSI_DSM5_6（是否满足）
  if(paste0(item_prefix, "6") %in% names(data)) {
    data[["dsm6"]] <- as.numeric(data[[paste0(item_prefix, "6")]] == 1)
    cat("  - dsm6创建完成\n")
  }
  
  # 🧮 第四步：计算NSSI总分（只加前五个维度：dsm1+dsm2+dsm3+dsm4+dsm5）
  cat("第4步：计算NSSI总分（dsm1+dsm2+dsm3+dsm4+dsm5）\n")
  dsm_components <- c("dsm1", "dsm2", "dsm3", "dsm4", "dsm5")
  available_components <- dsm_components[dsm_components %in% names(data)]
  
  if(length(available_components) > 0) {
    data[["NSSI"]] <- rowSums(data[available_components], na.rm = TRUE)
    cat("  - NSSI总分计算完成，使用组件：", paste(available_components, collapse = ", "), "\n")
  }
  
  # 🏷️ 第五步：重命名维度变量为最终格式
  cat("第5步：重命名变量为最终格式\n")
  rename_mapping <- list(
    "dsm1" = paste0(scale_name, "_1"),
    "dsm2" = paste0(scale_name, "_2"), 
    "dsm3" = paste0(scale_name, "_3"),
    "dsm4" = paste0(scale_name, "_4"),
    "dsm5" = paste0(scale_name, "_5"),
    "dsm6" = paste0(scale_name, "_6"),
    "NSSI" = paste0(scale_name, "_Total")
  )
  
  for(old_name in names(rename_mapping)) {
    new_name <- rename_mapping[[old_name]]
    if(old_name %in% names(data)) {
      data[[new_name]] <- data[[old_name]]
      # 保留原名，因为可能在其他地方使用
      cat("  - ", old_name, " → ", new_name, "\n")
    }
  }
  
  cat("✅ NSSI_DSM5特殊逻辑处理完成！\n\n")
  
  # 📊 显示处理结果摘要
  final_vars <- c(paste0(scale_name, "_", 1:6), paste0(scale_name, "_Total"))
  existing_final_vars <- final_vars[final_vars %in% names(data)]
  
  if(length(existing_final_vars) > 0) {
    cat("📋 最终生成变量：", paste(existing_final_vars, collapse = ", "), "\n")
    
    # 显示变量的取值分布
    for(var in existing_final_vars) {
      if(var %in% names(data) && !all(is.na(data[[var]]))) {
        unique_vals <- sort(unique(data[[var]][!is.na(data[[var]])]))
        cat("  - ", var, ": ", paste(unique_vals, collapse = ", "), "\n")
      }
    }
  }
  
  return(data)
}

# YFAS系列特殊逻辑：minus_1_first（scale_calculator版本）
apply_minus_1_first_calculator <- function(data, scale_name, item_prefix, item_numbers, special_logic) {
  
  # 构建题目变量名
  item_vars <- paste0(item_prefix, item_numbers)
  existing_items <- item_vars[item_vars %in% names(data)]
  
  # 对所有题目先减1
  for(item in existing_items) {
    data[[item]] <- data[[item]] - 1
  }
  
  return(data)
}

# 空值合并运算符
`%||%` <- function(a, b) if (is.null(a)) b else a