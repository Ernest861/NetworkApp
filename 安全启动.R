# =============================================================================
# 安全启动脚本 - 避免所有包冲突
# =============================================================================

# 首先清理环境中可能的bruceR包
if("bruceR" %in% loadedNamespaces()) {
  try(unloadNamespace("bruceR"), silent = TRUE)
}

if("package:bruceR" %in% search()) {
  try(detach("package:bruceR", unload = TRUE, force = TRUE), silent = TRUE)
}

cat("🚀 正在安全启动心理量表网络分析应用...\n")
cat("======================================\n\n")

# 设置工作目录检查
if(!grepl("NetworkApp", getwd())) {
  cat("⚠️  请确保当前工作目录为NetworkApp文件夹\n")
  cat("   当前目录:", getwd(), "\n\n")
}

# 检查必要文件
required_files <- c("app.R", "config.R", "utils.R")
missing_files <- required_files[!file.exists(required_files)]

if(length(missing_files) > 0) {
  cat("❌ 缺少必要文件:", paste(missing_files, collapse = ", "), "\n")
  stop("请确保所有应用文件都在当前目录中")
}

cat("✅ 应用文件检查完成\n")

# 检查和安装必要的包（不包括bruceR）
cat("📦 检查R包依赖...\n")

required_packages <- c("shiny", "shinydashboard", "shinyWidgets", "DT", "readxl", "dplyr", "ggplot2", "bootnet", "bnlearn", "igraph")

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

# 明确警告bruceR冲突问题
cat("⚠️  安全提示:\n")
cat("   为避免函数冲突，此版本不加载bruceR包\n")
cat("   所有统计分析功能仍然完整可用\n\n")

# 启动应用
cat("🎯 启动应用中...\n")
cat("💡 应用将在浏览器中自动打开\n")
cat("🌐 访问地址: http://127.0.0.1:3838\n")
cat("⚠️  按 Ctrl+C (Windows) 或 Cmd+C (Mac) 停止应用\n")
cat("======================================\n\n")

# 安全启动
library(shiny)
runApp("app.R", port = 3838, host = "127.0.0.1", launch.browser = TRUE)