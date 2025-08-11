# =============================================================================
# 网络分析应用启动脚本 - 简易版
# =============================================================================

cat("🚀 正在启动心理量表网络分析应用...\n")
cat("======================================\n\n")

# 设置工作目录到应用文件夹
if(!grepl("NetworkApp", getwd())) {
  # 如果当前不在NetworkApp目录，需要设置正确路径
  cat("⚠️  请确保当前工作目录为NetworkApp文件夹\n")
  cat("   当前目录:", getwd(), "\n")
  cat("   请运行: setwd('path/to/NetworkApp')\n\n")
}

# 检查必要文件
required_files <- c("app.R", "config.R", "utils.R", "run_app.R")
missing_files <- required_files[!file.exists(required_files)]

if(length(missing_files) > 0) {
  cat("❌ 缺少必要文件:", paste(missing_files, collapse = ", "), "\n")
  stop("请确保所有应用文件都在当前目录中")
}

cat("✅ 应用文件检查完成\n")

# 检查和加载必要的包
cat("📦 检查R包依赖...\n")

required_packages <- c("shiny", "shinydashboard", "shinyWidgets", "DT", "readxl", "dplyr")

for(pkg in required_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    cat("正在安装", pkg, "...\n")
    install.packages(pkg)
  }
}

# 检查quickNet
if(!requireNamespace("quickNet", quietly = TRUE)) {
  cat("正在安装quickNet...\n")
  if(!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("LeiGuo0812/quickNet")
}

cat("✅ R包依赖检查完成\n\n")

# 启动应用
cat("🎯 启动应用中...\n")
cat("💡 应用将在浏览器中自动打开\n")
cat("🌐 访问地址: http://127.0.0.1:3838\n")
cat("⚠️  按 Ctrl+C (Windows) 或 Cmd+C (Mac) 停止应用\n")
cat("======================================\n\n")

# 运行应用
library(shiny)
runApp("app.R", port = 3838, host = "127.0.0.1", launch.browser = TRUE)