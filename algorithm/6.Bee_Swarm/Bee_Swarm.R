# 加载必要的包
library(ggplot2)
library(ggbeeswarm)
library(reshape2)

# 读取数据
file_path <- "./file/Bee_Swarm_Demo.csv"

# 第六类
bee_swarm_plot_f <- function(file_path){

  data <- read.csv(file_path)

  # 获取所有列名（假设最后一列不是目标变量）
  variables_of_interest <- names(data)

  # 如果需要排除某些特定的非数值型变量或目标变量，可以在下面添加过滤逻辑
  # 例如，只选择数值型变量：
  # variables_of_interest <- names(data)[sapply(data, is.numeric)]

  # 从数据集中选择指定的变量
  selected_data <- data[, variables_of_interest, drop = FALSE]

  # 将数据转换为长格式
  long_data <- melt(selected_data, id.vars = NULL, variable.name = "Variable", value.name = "Value")

  # 绘制Bee Swarm Plot
  p <- ggplot(long_data, aes(x=Variable, y=Value, color=Variable)) +
    geom_quasirandom(size=2, alpha=0.7) +  # 使用quasirandom方法绘制蜂群图
    labs(title="Bee Swarm Plots for Selected Variables",
         x="Variables",
         y="Values") +
    theme_minimal() +  # 使用简洁主题
    theme(legend.position="bottom",  # 将图例放在底部
          axis.text.x = element_text(angle = 45, hjust = 1),  # 旋转X轴标签以避免重叠
          legend.title = element_blank())  # 移除图例标题

  # 显示图表
  # print(p)
  return (list(plot = p, info = ""))
}
