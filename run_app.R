# =============================================================================
# 网络分析应用启动脚本 - Application Launcher
# 检查依赖并启动Shiny应用
# =============================================================================

# 检查并安装必要的包
required_packages <- c(
  "shiny",
  "shinydashboard", 
  "shinyWidgets",
  "DT",
  "plotly",
  "readxl",
  "dplyr",
  "ggplot2",
  "bootnet"
)

# quickNet 需要从GitHub安装
github_packages <- list(
  "quickNet" = "LeiGuo0812/quickNet"
)

# Bioconductor包
bioc_packages <- c("Rgraphviz")

# 安装函数
install_if_missing <- function(packages, type = "cran") {
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  if(length(missing_packages) > 0) {
    cat("正在安装缺失的包:", paste(missing_packages, collapse = ", "), "\n")
    
    if(type == "cran") {
      install.packages(missing_packages, dependencies = TRUE)
    } else if(type == "github") {
      if(!requireNamespace("devtools", quietly = TRUE)) {
        install.packages("devtools")
      }
      for(pkg in missing_packages) {
        devtools::install_github(github_packages[[pkg]])
      }
    } else if(type == "bioc") {
      if(!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager")
      }
      BiocManager::install(missing_packages)
    }
  }
}

# 安装依赖
cat("=== 检查和安装依赖包 ===\n")

# CRAN包
install_if_missing(required_packages, "cran")

# GitHub包
install_if_missing(names(github_packages), "github")

# Bioconductor包
install_if_missing(bioc_packages, "bioc")

# 加载必要的包
cat("=== 加载依赖包 ===\n")
suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(plotly)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  
  # 尝试加载quickNet
  if(requireNamespace("quickNet", quietly = TRUE)) {
    library(quickNet)
    cat("✓ quickNet 加载成功\n")
  } else {
    cat("✗ quickNet 加载失败，请手动安装：devtools::install_github('LeiGuo0812/quickNet')\n")
  }
  
  # 完全跳过bruceR以避免冲突
  cat("⚠ bruceR 已跳过以避免函数冲突\n")
})


# 设置应用参数
APP_CONFIG <- list(
  title = "心理量表网络分析应用",
  version = "1.0.0",
  author = "Network Analysis Team",
  description = "基于quickNet包的心理量表多层级网络可视化工具",
  
  # 服务器配置
  host = "127.0.0.1",  # 本地运行
  port = 3838,         # 默认端口
  
  # 应用设置  
  max_file_size = 100,  # MB
  session_timeout = 3600  # 秒
)

# 设置Shiny选项
options(shiny.maxRequestSize = APP_CONFIG$max_file_size * 1024^2)

# 检查应用文件
required_files <- c("app.R", "config.R", "utils.R")
missing_files <- required_files[!file.exists(required_files)]

if(length(missing_files) > 0) {
  stop("缺少必要的应用文件: ", paste(missing_files, collapse = ", "))
}

# 启动信息
cat("\n=== 启动网络分析应用 ===\n")
cat("应用名称:", APP_CONFIG$title, "\n")
cat("版本:", APP_CONFIG$version, "\n")
cat("访问地址: http://", APP_CONFIG$host, ":", APP_CONFIG$port, "\n")
cat("最大文件大小:", APP_CONFIG$max_file_size, "MB\n")

# 创建示例数据（如果不存在）
if (!file.exists("example_data.csv")) {
  cat("创建示例数据文件...\n")
  
  set.seed(123)
  n <- 200
  
  # 基本信息
  ID <- 1:n
  Age <- sample(18:65, n, replace = TRUE)
  Gender <- sample(c("Male", "Female"), n, replace = TRUE)
  
  # AUDIT 10题 (0-4)
  AUDIT_mat <- replicate(10, sample(0:4, n, replace = TRUE,
                                    prob = c(0.4, 0.3, 0.2, 0.08, 0.02)))
  
  # HRF 18题 (1-7)
  HRF_mat <- replicate(18, sample(1:7, n, replace = TRUE))
  
  # PHQ9 9题 (0-3)
  PHQ9_mat <- replicate(9, sample(0:3, n, replace = TRUE,
                                  prob = c(0.5, 0.3, 0.15, 0.05)))
  
  # ---- 添加相关性 ----
  ## AUDIT: 如果第1题 >= 2，则第2题倾向高分
  idx_high <- which(AUDIT_mat[, 1] >= 2)
  AUDIT_mat[idx_high, 2] <- sample(2:4, length(idx_high), replace = TRUE,
                                   prob = c(0.5, 0.3, 0.2))
  
  ## HRF Habit 项目（6题）
  habit_idx <- c(3, 6, 7, 10, 14, 16)  # HRF 题号
  habit_mean <- rowMeans(HRF_mat[, habit_idx], na.rm = TRUE)
  
  for (j in habit_idx[-1]) {
    diff_val <- abs(HRF_mat[, j] - habit_mean)
    adjust_idx <- which(!is.na(diff_val) & diff_val > 2)
    HRF_mat[adjust_idx, j] <- sapply(habit_mean[adjust_idx], function(m) {
      sample(max(1, round(m) - 1):min(7, round(m) + 1), 1)
    })
  }
  
  # ---- 合并所有数据 ----
  example_data <- data.frame(
    ID = ID,
    Age = Age,
    Gender = Gender,
    setNames(as.data.frame(AUDIT_mat), paste0("AUDIT10_", 1:10)),
    setNames(as.data.frame(HRF_mat), paste0("HRF18_", 1:18)),
    setNames(as.data.frame(PHQ9_mat), paste0("PHQ9_", 1:9))
  )
  
  write.csv(example_data, "example_data.csv", row.names = FALSE)
  cat("✓ 示例数据已创建: example_data.csv\n")
}

# 打印系统信息
cat("\n=== 系统信息 ===\n")
cat("R版本:", R.version.string, "\n")
cat("操作系统:", Sys.info()["sysname"], Sys.info()["release"], "\n")
cat("工作目录:", getwd(), "\n")

# 启动应用
cat("\n正在启动应用...\n")
cat("提示：按 Ctrl+C 或 Cmd+C 停止应用\n")
cat("======================================\n\n")

# 启动Shiny应用
runApp(".", 
       host = APP_CONFIG$host,
       port = APP_CONFIG$port,
       launch.browser = TRUE)  # 自动打开浏览器