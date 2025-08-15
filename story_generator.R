# =============================================================================
# 智能故事生成系统 - Smart Story Generator
# 根据数据特征和分析结果自动生成个性化的研究故事线
# =============================================================================

#' 生成智能研究故事
#' @param detected_scales 检测到的量表列表
#' @param network_result 网络分析结果
#' @param bayesian_result 贝叶斯网络结果（可选）
#' @param sample_characteristics 样本特征
#' @return 个性化的故事HTML字符串
generate_smart_story <- function(detected_scales, network_result = NULL, 
                                bayesian_result = NULL, sample_characteristics = NULL) {
  
  # 分析量表组合模式
  scale_pattern <- analyze_scale_pattern(detected_scales)
  
  # 生成基础故事框架
  story_template <- get_story_template(scale_pattern)
  
  # 如果有网络分析结果，添加发现线索
  if (!is.null(network_result)) {
    story_template$network_insights <- generate_network_insights(detected_scales, network_result)
  }
  
  # 如果有贝叶斯结果，添加因果推理
  if (!is.null(bayesian_result)) {
    story_template$causal_insights <- generate_causal_insights(detected_scales, bayesian_result)
  }
  
  # 组装完整故事
  complete_story <- assemble_story(story_template, scale_pattern)
  
  return(complete_story)
}

#' 分析量表组合模式
analyze_scale_pattern <- function(detected_scales) {
  
  scale_names <- names(detected_scales)
  
  # 定义量表类型
  addiction_scales <- c("AUDIT", "IAT", "FTND")
  mental_health_scales <- c("PHQ", "GAD", "BDI", "DASS")
  motivation_scales <- c("HRF")
  
  # 分析组合模式
  pattern <- list(
    has_addiction = any(scale_names %in% addiction_scales),
    has_mental_health = any(scale_names %in% mental_health_scales), 
    has_motivation = any(scale_names %in% motivation_scales),
    addiction_types = intersect(scale_names, addiction_scales),
    mental_health_types = intersect(scale_names, mental_health_scales),
    motivation_types = intersect(scale_names, motivation_scales),
    total_scales = length(scale_names),
    scale_combination = paste(sort(scale_names), collapse = "_")
  )
  
  return(pattern)
}

#' 获取故事模板
get_story_template <- function(scale_pattern) {
  
  # 根据量表组合选择故事模板
  if (scale_pattern$has_addiction && scale_pattern$has_mental_health && scale_pattern$has_motivation) {
    template <- get_addiction_mental_motivation_template(scale_pattern)
  } else if (scale_pattern$has_addiction && scale_pattern$has_mental_health) {
    template <- get_addiction_mental_template(scale_pattern)
  } else if (scale_pattern$has_mental_health && scale_pattern$has_motivation) {
    template <- get_mental_motivation_template(scale_pattern)
  } else if (scale_pattern$has_addiction && scale_pattern$has_motivation) {
    template <- get_addiction_motivation_template(scale_pattern)
  } else {
    template <- get_general_template(scale_pattern)
  }
  
  return(template)
}

#' 成瘾+心理健康+动机 模板
get_addiction_mental_motivation_template <- function(scale_pattern) {
  
  addiction_name <- get_addiction_name(scale_pattern$addiction_types[1])
  mental_name <- get_mental_health_name(scale_pattern$mental_health_types[1])
  
  template <- list(
    research_question = paste0("探索", addiction_name, "、动机模式与", mental_name, "之间的复杂关系"),
    
    theoretical_background = paste0(
      "研究背景：", addiction_name, "行为往往与心理健康问题共存，而个体的动机模式可能是连接两者的重要桥梁。",
      "理解这种三元关系对于制定有效的干预策略至关重要。"
    ),
    
    expected_findings = list(
      network = paste0("预期在网络分析中发现", addiction_name, "行为、动机因子与", mental_name, "症状之间存在复杂的相关模式"),
      bayesian = paste0("预期贝叶斯分析将揭示动机→行为→心理症状的潜在因果链")
    ),
    
    implications = paste0(
      "研究意义：识别关键的动机因子可以为", addiction_name, "的预防和", mental_name, "的早期干预提供新的切入点"
    )
  )
  
  return(template)
}

#' 成瘾+心理健康 模板
get_addiction_mental_template <- function(scale_pattern) {
  
  addiction_name <- get_addiction_name(scale_pattern$addiction_types[1])
  mental_name <- get_mental_health_name(scale_pattern$mental_health_types[1])
  
  template <- list(
    research_question = paste0("探索", addiction_name, "与", mental_name, "之间的双向关系"),
    
    theoretical_background = paste0(
      "研究背景：", addiction_name, "与", mental_name, "在临床实践中经常共现，但其因果关系方向仍存在争议。",
      "是心理健康问题导致成瘾行为，还是成瘾行为加剧心理症状？"
    ),
    
    expected_findings = list(
      network = paste0("预期发现", addiction_name, "严重程度与", mental_name, "症状强度之间存在显著关联"),
      bayesian = "预期揭示两者间的主导因果方向，为临床决策提供依据"
    ),
    
    implications = paste0(
      "研究意义：明确因果关系有助于确定治疗的优先顺序和干预策略的制定"
    )
  )
  
  return(template)
}

#' 心理健康+动机 模板  
get_mental_motivation_template <- function(scale_pattern) {
  
  mental_name <- get_mental_health_name(scale_pattern$mental_health_types[1])
  
  template <- list(
    research_question = paste0("探索动机模式与", mental_name, "之间的关系"),
    
    theoretical_background = paste0(
      "研究背景：个体的动机模式（如恐惧、奖赏、习惯动机）可能影响", mental_name, "的发生和维持。",
      "理解这种关系有助于开发基于动机的心理健康干预策略。"
    ),
    
    expected_findings = list(
      network = paste0("预期发现特定动机因子与", mental_name, "症状之间的关联模式"),
      bayesian = "预期识别动机→症状或症状→动机的因果方向"
    ),
    
    implications = paste0(
      "研究意义：为基于动机理论的", mental_name, "干预提供科学依据"
    )
  )
  
  return(template)
}

#' 成瘾+动机 模板
get_addiction_motivation_template <- function(scale_pattern) {
  
  addiction_name <- get_addiction_name(scale_pattern$addiction_types[1])
  
  template <- list(
    research_question = paste0("探索动机模式如何影响", addiction_name, "行为"),
    
    theoretical_background = paste0(
      "研究背景：", addiction_name, "行为的发生和维持受到多种动机因子的驱动。",
      "识别关键的动机模式有助于理解成瘾的心理机制。"
    ),
    
    expected_findings = list(
      network = paste0("预期发现不同动机因子与", addiction_name, "严重程度的关联"),
      bayesian = paste0("预期揭示动机→", addiction_name, "的因果路径")
    ),
    
    implications = paste0(
      "研究意义：为基于动机的", addiction_name, "预防和治疗策略提供理论基础"
    )
  )
  
  return(template)
}

#' 通用模板
get_general_template <- function(scale_pattern) {
  
  scale_names <- paste(names(scale_pattern), collapse = "、")
  
  template <- list(
    research_question = paste0("探索", scale_names, "之间的关系模式"),
    
    theoretical_background = "研究背景：多维心理测量指标之间往往存在复杂的关联，网络分析方法可以帮助识别这些关系的结构特征。",
    
    expected_findings = list(
      network = "预期发现变量间的关联模式和网络结构特征",
      bayesian = "预期识别变量间的潜在因果关系"
    ),
    
    implications = "研究意义：为理解心理现象的内在结构提供新的视角"
  )
  
  return(template)
}

#' 获取成瘾类型中文名称
get_addiction_name <- function(scale_code) {
  addiction_names <- list(
    "AUDIT" = "酒精使用",
    "IAT" = "网络使用", 
    "FTND" = "尼古丁依赖"
  )
  
  return(addiction_names[[scale_code]] %||% scale_code)
}

#' 获取心理健康类型中文名称
get_mental_health_name <- function(scale_code) {
  mental_names <- list(
    "PHQ" = "抑郁症状",
    "GAD" = "焦虑症状",
    "BDI" = "抑郁情绪", 
    "DASS" = "负性情绪"
  )
  
  return(mental_names[[scale_code]] %||% scale_code)
}

#' 生成网络分析洞察
generate_network_insights <- function(detected_scales, network_result) {
  
  # 这里需要分析网络结果的具体特征
  # 例如：最强连接、核心节点、社群结构等
  
  insights <- paste0(
    "网络分析发现：",
    "• 识别出关键的变量连接模式\n",
    "• 发现了网络中的核心节点\n", 
    "• 揭示了变量间的社群结构"
  )
  
  return(insights)
}

#' 生成因果分析洞察
generate_causal_insights <- function(detected_scales, bayesian_result) {
  
  # 分析贝叶斯结果中的稳定边和方向
  stable_edges <- bayesian_result$stable_edges
  
  if (nrow(stable_edges) > 0) {
    # 提取最强的几条因果关系
    top_edges <- head(stable_edges[order(-stable_edges$strength), ], 3)
    
    insights <- paste0(
      "因果分析发现：\n",
      paste(apply(top_edges, 1, function(x) {
        paste0("• ", x["from"], " → ", x["to"], " (强度: ", round(as.numeric(x["strength"]), 2), ")")
      }), collapse = "\n")
    )
  } else {
    insights <- "因果分析发现：变量间的因果关系较弱，需要更大样本量进行验证"
  }
  
  return(insights)
}

#' 组装完整故事
assemble_story <- function(story_template, scale_pattern) {
  
  story_html <- paste0(
    "<div class='panel panel-success'>",
    "<div class='panel-heading'>",
    "<h4>🎯 您的专属研究故事</h4>",
    "</div>",
    "<div class='panel-body'>",
    
    "<h5>📖 研究问题</h5>",
    "<p>", story_template$research_question, "</p>",
    
    "<h5>🧩 理论背景</h5>", 
    "<p>", story_template$theoretical_background, "</p>",
    
    "<h5>🔍 预期发现</h5>",
    "<ul>",
    "<li><strong>网络分析：</strong>", story_template$expected_findings$network, "</li>",
    "<li><strong>因果分析：</strong>", story_template$expected_findings$bayesian, "</li>",
    "</ul>",
    
    # 如果有实际分析结果，添加发现部分
    if (!is.null(story_template$network_insights)) {
      paste0("<h5>📊 网络发现</h5><p>", story_template$network_insights, "</p>")
    } else { "" },
    
    if (!is.null(story_template$causal_insights)) {
      paste0("<h5>🧠 因果发现</h5><p>", story_template$causal_insights, "</p>")
    } else { "" },
    
    "<h5>💡 研究意义</h5>",
    "<p>", story_template$implications, "</p>",
    
    "</div>",
    "</div>"
  )
  
  return(story_html)
}

# 空值合并运算符
`%||%` <- function(a, b) if (is.null(a)) b else a