# minimal_test.R

# --- 核心测试区域 ---
# 目标：找出是哪个 library() 调用导致了应用启动失败

print("正在加载所有依赖包...")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(readxl)
library(dplyr)
library(ggplot2)

# quickNet 会自动加载 Rgraphviz, qgraph, BiocGenerics 等
# 这是最可能的冲突来源，我们把它放在最后加载
library(quickNet) 
print("所有包装载完毕。")


# --- 一个空白的Shiny应用 ---
print("正在尝试启动一个最小化Shiny应用...")
ui <- fluidPage(
  titlePanel("最小化测试应用"),
  h3("如果能看到这个页面，说明所有包可以在Shiny环境中和平共处。")
)

server <- function(input, output, session) {
  # 服务器端无任何逻辑
}

# 启动应用
shinyApp(ui, server)