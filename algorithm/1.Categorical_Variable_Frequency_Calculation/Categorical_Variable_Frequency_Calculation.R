# 加载必要的库
library(readr)
library(ggplot2)
library(dplyr)  # 用于数据操作

# 读取CSV文件
data_path <- "./file/Categorical_Variable_Frequency_Calculation_Demo.csv"

category_plot <- function(file_path){
  data <- read_csv(data_path)

  # 查看数据前几行以了解数据结构
  head(data)

  # 打印数据结构
  str(data)

  # 过滤掉任何NA值的行，并确保分类变量是因子类型
  data_cleaned <- data %>%
    filter(!is.na(.[[ncol(.)]])) %>%  # 使用 .[[ncol(.)]] 来引用最后一列
    mutate(category_labels = as.factor(.[[ncol(.)]]))  # 确保分类变量是因子类型

  # 创建带颜色的柱状图
  p <- ggplot(data_cleaned, aes(x = category_labels, fill = category_labels)) +
    geom_bar() +  # 默认统计每个类别的计数
    labs(title = "Frequency Calculation", x = "Category", y = "Frequency") +
    theme_minimal() +  # 使用简约主题
    geom_text(stat='count', aes(label=..count..), vjust=-0.5)  # 在柱子顶部添加数据标签显示频数

  # 显示图形
  # print(p)
  return (list(plot = p, info = ""))
}
