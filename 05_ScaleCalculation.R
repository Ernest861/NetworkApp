# =============================================================================
# Step 5: 增强版量表配置化计算 (带XLSX输出)
# 输入: step4_quality_controlled_data_2025-XX-XX.rds + scale_config_complete.csv
# 输出: step5_scales_calculated_2025-XX-XX.rds + 详细的xlsx报告
# =============================================================================

calculate_scales <- function(
    input_path = "../2.processed_data/step4_quality_controlled_2025-07-30.rds",
    config_path = "/Users/macbear/SynologyDrive/2AppUseage/QuesDataset/1.code/parameter/scale_config_complete.csv",
    output_dir = "../2.processed_data",
    verbose = TRUE
) {
  
  if (verbose) {
    cat("=== Step 5: 增强版量表配置化计算 ===\n")
  }
  
  # 加载必要的包
  library(dplyr)
  library(stringr)
  library(openxlsx)
  
  # 初始化处理日志
  processing_log <- list()
  log_entry <- function(message, level = "INFO") {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    processing_log <<- append(processing_log, list(list(
      timestamp = timestamp,
      level = level,
      message = message
    )))
    if (verbose) cat("[", timestamp, "]", level, ":", message, "\n")
  }
  
  # =========================================================================
  # 5.1 读取数据和配置
  # =========================================================================
  
  log_entry("读取质量控制后数据...")
  data <- readRDS(input_path)
  
  log_entry("读取量表配置文件...")
  config <- read.csv(config_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  
  log_entry(paste("数据维度:", nrow(data), "行", ncol(data), "列"))
  log_entry(paste("配置文件包含", nrow(config), "个量表配置"))
  
  # 检查必要的ID列
  required_id_cols <- c("research_uuid", "subject_id", "city_code", "session_code")
  missing_id_cols <- required_id_cols[!required_id_cols %in% names(data)]
  if (length(missing_id_cols) > 0) {
    log_entry(paste("缺少必要ID列:", paste(missing_id_cols, collapse = ", ")), "ERROR")
    stop("缺少必要的ID列")
  }
  
  # =========================================================================
  # 5.2 量表计算主循环
  # =========================================================================
  
  scale_results <- list()
  reliability_results <- list()
  group_statistics <- list()
  calculated_scales <- character(0)
  
  for (i in 1:nrow(config)) {
    scale_name <- config$scale_name[i]
    scale_type <- config$scale_type[i]
    
    log_entry(paste("开始处理量表:", scale_name))
    
    # 跳过非问卷类型
    if (scale_type != "questionnaire") {
      log_entry(paste("跳过非问卷类型:", scale_type))
      next
    }
    
    # 解析配置
    item_prefix <- config$item_prefix[i]
    item_numbers_str <- config$item_numbers[i]
    reverse_items_str <- config$reverse_items[i]
    min_valid_items <- config$min_valid_items[i]
    calculation_method <- config$calculation_method[i]
    subscales_definition <- config$subscales_definition[i]
    special_logic <- config$special_logic[i]
    
    # 解析题目编号（处理范围格式如"1-9"）
    item_numbers <- parse_item_numbers(item_numbers_str)
    
    # 构建题目变量名
    scale_items <- paste0(item_prefix, item_numbers)
    existing_items <- scale_items[scale_items %in% names(data)]
    
    if (length(existing_items) == 0) {
      log_entry(paste("量表", scale_name, "未找到任何题目，跳过"), "WARN")
      next
    }
    
    log_entry(paste("量表", scale_name, "找到", length(existing_items), "/", length(scale_items), "个题目"))
    
    # 解析反向计分题目
    reverse_items <- character(0)
    if (!is.na(reverse_items_str) && reverse_items_str != "") {
      reverse_numbers <- parse_item_numbers(reverse_items_str)
      reverse_items <- paste0(item_prefix, reverse_numbers)
      reverse_items <- reverse_items[reverse_items %in% existing_items]
      
      if (length(reverse_items) > 0) {
        log_entry(paste("量表", scale_name, "反向计分题目:", length(reverse_items), "个"))
      }
    }
    
    # =====================================================================
    # 5.3 计算量表总分
    # =====================================================================
    
    scale_calculation_result <- calculate_scale_scores(
      data, scale_name, existing_items, reverse_items, 
      min_valid_items, calculation_method, subscales_definition
    )
    
    # 更新数据
    data <- scale_calculation_result$data
    calculated_scales <- c(calculated_scales, scale_calculation_result$new_variables)
    
    log_entry(paste("量表", scale_name, "计算完成，新增", length(scale_calculation_result$new_variables), "个变量"))
    
    # =====================================================================
    # 5.4 特殊逻辑处理
    # =====================================================================
    
    if (!is.na(special_logic) && special_logic != "") {
      log_entry(paste("量表", scale_name, "应用特殊逻辑:", special_logic))
      
      data <- tryCatch({
        apply_special_logic(data, scale_name, item_prefix, item_numbers, special_logic)
      }, error = function(e) {
        log_entry(paste("特殊逻辑处理失败:", e$message), "ERROR")
        data
      })
    }
    
    # =====================================================================
    # 5.5 分组统计和信效度分析
    # =====================================================================
    
    if (config$reliability_check[i] && length(existing_items) >= 3) {
      log_entry(paste("量表", scale_name, "开始信效度分析..."))
      
      # 整体信效度
      overall_reliability <- calculate_reliability_comprehensive(data, existing_items, scale_name)
      reliability_results[[scale_name]] <- overall_reliability
      
      # 分组统计 (按city_code和session_code)
      group_stats <- calculate_group_statistics(data, existing_items, scale_name)
      group_statistics[[scale_name]] <- group_stats
      
      log_entry(paste("量表", scale_name, "信效度分析完成"))
    }
    
    # 保存量表基本信息
    total_var <- paste0(scale_name, "_Total")
    if (total_var %in% names(data)) {
      scale_results[[scale_name]] <- list(
        n_items = length(existing_items),
        n_total_cases = nrow(data),
        n_valid_cases = sum(!is.na(data[[total_var]])),
        missing_rate = mean(is.na(data[[total_var]])),
        mean_score = mean(data[[total_var]], na.rm = TRUE),
        sd_score = sd(data[[total_var]], na.rm = TRUE),
        min_score = min(data[[total_var]], na.rm = TRUE),
        max_score = max(data[[total_var]], na.rm = TRUE),
        median_score = median(data[[total_var]], na.rm = TRUE),
        items_used = paste(existing_items, collapse = ", "),
        reverse_items = paste(reverse_items, collapse = ", "),
        calculation_method = calculation_method
      )
    }
  }
  
  # =========================================================================
  # 5.6 生成综合报告
  # =========================================================================
  
  processing_report <- create_processing_report(data, calculated_scales, scale_results, 
                                                reliability_results, processing_log)
  
  # =========================================================================
  # 5.7 保存结果
  # =========================================================================
  
  output_paths <- save_results(data, processing_report, scale_results, 
                                        reliability_results, group_statistics, 
                                        processing_log, output_dir, verbose)
  
  log_entry("量表计算流程全部完成")
  
  return(list(
    data_with_scales = data,
    scale_results = scale_results,
    reliability_results = reliability_results,
    group_statistics = group_statistics,
    processing_report = processing_report,
    processing_log = processing_log,
    output_paths = output_paths
  ))
}

# =============================================================================
# 辅助函数
# =============================================================================

#' 解析题目编号（支持范围格式）
parse_item_numbers <- function(item_numbers_str) {
  if (is.na(item_numbers_str) || item_numbers_str == "") {
    return(numeric(0))
  }
  
  # 分割逗号
  parts <- trimws(strsplit(item_numbers_str, ",")[[1]])
  numbers <- numeric(0)
  
  for (part in parts) {
    if (grepl("-", part)) {
      # 处理范围格式 "1-9"
      range_parts <- as.numeric(strsplit(part, "-")[[1]])
      if (length(range_parts) == 2) {
        numbers <- c(numbers, range_parts[1]:range_parts[2])
      }
    } else {
      # 处理单个数字
      numbers <- c(numbers, as.numeric(part))
    }
  }
  
  return(unique(numbers[!is.na(numbers)]))
}

#' 计算量表分数
calculate_scale_scores <- function(data, scale_name, existing_items, reverse_items, 
                                   min_valid_items, calculation_method, subscales_definition) {
  
  scale_data <- data[, existing_items, drop = FALSE]
  new_variables <- character(0)
  
  # 反向计分
  if (length(reverse_items) > 0) {
    for (rev_item in reverse_items) {
      if (rev_item %in% names(scale_data)) {
        item_values <- scale_data[[rev_item]]
        non_na_values <- item_values[!is.na(item_values)]
        if (length(non_na_values) > 0) {
          max_val <- max(non_na_values)
          min_val <- min(non_na_values)
          scale_data[[rev_item]] <- (max_val + min_val) - scale_data[[rev_item]]
        }
      }
    }
  }
  
  # 计算有效题目数
  valid_count <- rowSums(!is.na(scale_data))
  
  # 设置最少有效题目数阈值
  if (is.na(min_valid_items)) {
    min_valid_items <- ceiling(length(existing_items) * 0.8)
  }
  
  # 计算总分
  if (calculation_method == "mean") {
    scale_score <- rowMeans(scale_data, na.rm = TRUE)
  } else {
    scale_score <- rowSums(scale_data, na.rm = TRUE)
  }
  
  # 不满足最少题目数的设为NA
  scale_score[valid_count < min_valid_items] <- NA
  
  # 添加总分到数据
  total_var_name <- paste0(scale_name, "_Total")
  data[[total_var_name]] <- scale_score
  new_variables <- c(new_variables, total_var_name)
  
  # 计算子量表
  if (!is.na(subscales_definition) && subscales_definition != "") {
    subscale_result <- calculate_subscales(data, scale_data, scale_name, subscales_definition, calculation_method)
    data <- subscale_result$data
    new_variables <- c(new_variables, subscale_result$new_variables)
  }
  
  return(list(data = data, new_variables = new_variables))
}

#' 计算子量表
calculate_subscales <- function(data, scale_data, scale_name, subscales_definition, calculation_method) {
  new_variables <- character(0)
  
  if (grepl(":", subscales_definition)) {
    subscale_parts <- strsplit(subscales_definition, "\\|")[[1]]
    
    for (subscale_part in subscale_parts) {
      parts <- strsplit(subscale_part, ":")[[1]]
      if (length(parts) == 2) {
        subscale_name <- trimws(parts[1])
        subscale_items_str <- trimws(parts[2])
        subscale_items_num <- parse_item_numbers(subscale_items_str)
        
        # 找到实际存在的子量表题目
        subscale_cols <- paste0(gsub("_.*", "_", names(scale_data)[1]), subscale_items_num)
        subscale_existing <- subscale_cols[subscale_cols %in% names(scale_data)]
        
        if (length(subscale_existing) > 0) {
          subscale_data <- scale_data[, subscale_existing, drop = FALSE]
          
          if (calculation_method == "mean") {
            subscale_score <- rowMeans(subscale_data, na.rm = TRUE)
          } else {
            subscale_score <- rowSums(subscale_data, na.rm = TRUE)
          }
          
          # 设置最少题目数阈值
          subscale_valid <- rowSums(!is.na(subscale_data))
          min_subscale_items <- ceiling(length(subscale_existing) * 0.7)
          subscale_score[subscale_valid < min_subscale_items] <- NA
          
          subscale_var_name <- paste0(scale_name, "_", subscale_name)
          data[[subscale_var_name]] <- subscale_score
          new_variables <- c(new_variables, subscale_var_name)
        }
      }
    }
  }
  
  return(list(data = data, new_variables = new_variables))
}

#' 综合信效度分析
calculate_reliability_comprehensive <- function(data, items, scale_name) {
  scale_data <- data[, items, drop = FALSE]
  complete_data <- scale_data[complete.cases(scale_data), ]
  
  if (nrow(complete_data) < 10) {
    return(list(
      scale_name = scale_name,
      n_items = length(items),
      n_complete_cases = nrow(complete_data),
      error = "样本量不足"
    ))
  }
  
  # 计算Cronbach's α
  alpha_result <- calculate_cronbach_alpha(complete_data)
  
  # 计算描述性统计
  total_score <- rowSums(complete_data)
  descriptive_stats <- list(
    mean = mean(total_score),
    sd = sd(total_score),
    min = min(total_score),
    max = max(total_score),
    median = median(total_score),
    q25 = quantile(total_score, 0.25),
    q75 = quantile(total_score, 0.75)
  )
  
  return(list(
    scale_name = scale_name,
    n_items = length(items),
    n_complete_cases = nrow(complete_data),
    cronbach_alpha = alpha_result$alpha,
    alpha_if_deleted = alpha_result$alpha_if_deleted,
    item_total_correlations = alpha_result$item_total_cor,
    descriptive_stats = descriptive_stats,
    items_used = paste(items, collapse = ", ")
  ))
}

#' 增强版Cronbach's α计算
calculate_cronbach_alpha <- function(data) {
  if (nrow(data) < 10 || ncol(data) < 2) {
    return(list(alpha = NA, item_total_cor = rep(NA, ncol(data)), alpha_if_deleted = rep(NA, ncol(data))))
  }
  
  # 计算总分
  total_score <- rowSums(data)
  
  # 计算项目-总分相关（排除该项目后的总分）
  n_items <- ncol(data)
  item_total_cor <- numeric(n_items)
  alpha_if_deleted <- numeric(n_items)
  
  for (i in 1:n_items) {
    corrected_total <- total_score - data[, i]
    item_total_cor[i] <- cor(data[, i], corrected_total, use = "complete.obs")
    
    # 计算删除该项目后的α
    if (n_items > 2) {
      remaining_data <- data[, -i, drop = FALSE]
      alpha_if_deleted[i] <- calculate_simple_alpha(remaining_data)
    }
  }
  
  # 计算整体α
  alpha <- calculate_simple_alpha(data)
  
  return(list(
    alpha = alpha,
    item_total_cor = item_total_cor,
    alpha_if_deleted = alpha_if_deleted
  ))
}

#' 简单α计算
calculate_simple_alpha <- function(data) {
  if (ncol(data) < 2) return(NA)
  
  cor_matrix <- cor(data, use = "complete.obs")
  n_items <- ncol(data)
  
  # 计算平均相关系数
  mean_cor <- mean(cor_matrix[upper.tri(cor_matrix)], na.rm = TRUE)
  
  # Cronbach's α公式
  alpha <- (n_items * mean_cor) / (1 + (n_items - 1) * mean_cor)
  
  return(alpha)
}

#' 按组计算统计
calculate_group_statistics <- function(data, items, scale_name) {
  scale_data <- data[, items, drop = FALSE]
  total_var <- paste0(scale_name, "_Total")
  
  if (!total_var %in% names(data)) {
    return(NULL)
  }
  
  group_stats <- data %>%
    select(city_code, session_code, all_of(items), all_of(total_var)) %>%
    group_by(city_code, session_code) %>%
    summarise(
      n_total = n(),
      n_valid = sum(!is.na(.data[[total_var]])),
      missing_rate = mean(is.na(.data[[total_var]])),
      mean_score = mean(.data[[total_var]], na.rm = TRUE),
      sd_score = sd(.data[[total_var]], na.rm = TRUE),
      min_score = min(.data[[total_var]], na.rm = TRUE),
      max_score = max(.data[[total_var]], na.rm = TRUE),
      median_score = median(.data[[total_var]], na.rm = TRUE),
      
      # 计算该组的Cronbach's α
      cronbach_alpha = {
        group_scale_data <- cur_data()[, items, drop = FALSE]
        complete_group_data <- group_scale_data[complete.cases(group_scale_data), ]
        if (nrow(complete_group_data) >= 10) {
          calculate_simple_alpha(complete_group_data)
        } else {
          NA
        }
      },
      
      .groups = 'drop'
    ) %>%
    mutate(
      scale_name = scale_name,
      reliability_level = case_when(
        is.na(cronbach_alpha) ~ "无法计算",
        cronbach_alpha >= 0.9 ~ "优秀",
        cronbach_alpha >= 0.8 ~ "良好", 
        cronbach_alpha >= 0.7 ~ "可接受",
        TRUE ~ "较差"
      )
    )
  
  return(group_stats)
}

#' 应用特殊逻辑
apply_special_logic <- function(data, scale_name, item_prefix, item_numbers, special_logic) {
  if (special_logic == "HRF_dimensions") {
    return(calculate_hrf_dimensions(data, scale_name, item_prefix, item_numbers))
  }
  # 可以添加更多特殊逻辑
  return(data)
}

#' 增强版HRF三维度计算
calculate_hrf_dimensions <- function(data, scale_name, item_prefix, item_numbers) {
  if (length(item_numbers) >= 18) {
    # HRF标准三维度分配
    habit_nums <- c(3,6,7,10,14,16)
    reward_nums <- c(2,4,9,12,15,17)
    fear_nums <- c(1,5,8,11,13,18)
    
    habit_items <- paste0(item_prefix, habit_nums)
    reward_items <- paste0(item_prefix, reward_nums)
    fear_items <- paste0(item_prefix, fear_nums)
    
    habit_existing <- habit_items[habit_items %in% names(data)]
    reward_existing <- reward_items[reward_items %in% names(data)]
    fear_existing <- fear_items[fear_items %in% names(data)]
    
    if (length(habit_existing) >= 3) {
      data[[paste0(scale_name, "_Habit")]] <- rowSums(data[habit_existing], na.rm = TRUE)
    }
    if (length(reward_existing) >= 3) {
      data[[paste0(scale_name, "_Reward")]] <- rowSums(data[reward_existing], na.rm = TRUE)
    }
    if (length(fear_existing) >= 3) {
      data[[paste0(scale_name, "_Fear")]] <- rowSums(data[fear_existing], na.rm = TRUE)
    }
  }
  
  return(data)
}

#' 创建处理报告
create_processing_report <- function(data, calculated_scales, scale_results, reliability_results, processing_log) {
  list(
    processing_time = Sys.time(),
    input_dimensions = c(nrow(data), ncol(data) - length(calculated_scales)),
    output_dimensions = c(nrow(data), ncol(data)),
    scales_processed = length(scale_results),
    scales_with_reliability = length(reliability_results),
    new_variables_created = length(calculated_scales),
    total_log_entries = length(processing_log),
    city_codes = unique(data$city_code),
    session_codes = unique(data$session_code),
    sample_sizes_by_group = data %>%
      group_by(city_code, session_code) %>%
      summarise(n = n(), .groups = 'drop')
  )
}

#' 保存增强版结果（包含XLSX输出）
save_results <- function(data, processing_report, scale_results, reliability_results, 
                                  group_statistics, processing_log, output_dir, verbose) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  
  # 保存RDS数据
  output_data_path <- file.path(output_dir, paste0("step5_scales_calculated_", date_str, ".rds"))
  saveRDS(data, output_data_path)
  
  # 创建详细的XLSX报告
  xlsx_path <- file.path(output_dir, paste0("step5_scales_comprehensive_report_", date_str, ".xlsx"))
  
  # 准备各个工作表的数据
  wb <- createWorkbook()
  
  # 1. 处理日志表
  log_df <- data.frame(
    timestamp = sapply(processing_log, function(x) x$timestamp),
    level = sapply(processing_log, function(x) x$level),
    message = sapply(processing_log, function(x) x$message),
    stringsAsFactors = FALSE
  )
  addWorksheet(wb, "Processing_Log")
  writeData(wb, "Processing_Log", log_df)
  
  # 2. 整体量表统计表
  if (length(scale_results) > 0) {
    overall_stats_df <- map_dfr(names(scale_results), function(scale_name) {
      stats <- scale_results[[scale_name]]
      data.frame(
        scale_name = scale_name,
        n_items = stats$n_items,
        n_total_cases = stats$n_total_cases,
        n_valid_cases = stats$n_valid_cases,
        missing_rate = round(stats$missing_rate, 3),
        mean_score = round(stats$mean_score, 2),
        sd_score = round(stats$sd_score, 2),
        min_score = stats$min_score,
        max_score = stats$max_score,
        median_score = round(stats$median_score, 2),
        calculation_method = stats$calculation_method,
        stringsAsFactors = FALSE
      )
    })
    addWorksheet(wb, "Overall_Scale_Stats")
    writeData(wb, "Overall_Scale_Stats", overall_stats_df)
  }
  
  # 3. 信效度分析表
  if (length(reliability_results) > 0) {
    reliability_df <- map_dfr(names(reliability_results), function(scale_name) {
      rel <- reliability_results[[scale_name]]
      if (!is.null(rel$cronbach_alpha)) {
        data.frame(
          scale_name = scale_name,
          n_items = rel$n_items,
          n_complete_cases = rel$n_complete_cases,
          cronbach_alpha = round(rel$cronbach_alpha, 3),
          mean_score = round(rel$descriptive_stats$mean, 2),
          sd_score = round(rel$descriptive_stats$sd, 2),
          median_score = round(rel$descriptive_stats$median, 2),
          reliability_level = case_when(
            rel$cronbach_alpha >= 0.9 ~ "优秀",
            rel$cronbach_alpha >= 0.8 ~ "良好",
            rel$cronbach_alpha >= 0.7 ~ "可接受",
            TRUE ~ "较差"
          ),
          stringsAsFactors = FALSE
        )
      }
    })
    
    if (nrow(reliability_df) > 0) {
      addWorksheet(wb, "Reliability_Analysis")
      writeData(wb, "Reliability_Analysis", reliability_df)
    }
  }
  
  # 4. 分组统计表（按城市-时间组合）
  if (length(group_statistics) > 0) {
    group_stats_df <- map_dfr(group_statistics, function(x) x)
    if (nrow(group_stats_df) > 0) {
      addWorksheet(wb, "Group_Statistics")
      writeData(wb, "Group_Statistics", group_stats_df)
    }
  }
  
  # 5. 样本分布表
  sample_distribution <- processing_report$sample_sizes_by_group
  addWorksheet(wb, "Sample_Distribution")
  writeData(wb, "Sample_Distribution", sample_distribution)
  
  # 保存XLSX文件
  saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  
  if (verbose) {
    cat("\n=== 保存结果完成 ===\n")
    cat("数据文件:", output_data_path, "\n")
    cat("XLSX报告:", xlsx_path, "\n")
  }
  
  return(list(
    data_file = output_data_path,
    xlsx_report = xlsx_path
  ))
}

# =============================================================================
# 使用示例
# =============================================================================

# 运行增强版量表计算
result5 <- calculate_scales()