# 加载所需的库
library(ggplot2)  # 用于绘图
library(tidyr)    # 用于数据重塑

# 定义读取CSV文件的路径（请替换为实际路径）
file_path <- "./file/Related_Samples_Test_Demo.csv"

# 两样本T检验
related_samples_test_plot <- function(file_path){

  # 动态读取CSV文件
  data_cleaned <- read.csv(file_path, stringsAsFactors = FALSE)

  # 确认数据集结构
  if (ncol(data_cleaned) != 3) {
    stop("数据集应包含三列：一个ID列和两个连续数值型变量。")
  }

  # 获取列名
  col_names <- colnames(data_cleaned)
  id_col <- col_names[1]
  pre_measurement <- col_names[2]
  post_measurement <- col_names[3]

  # 检查并转换第一列（ID列）为字符型
  if (!is.character(data_cleaned[[id_col]])) {
    data_cleaned[[id_col]] <- as.character(data_cleaned[[id_col]])
    cat("已将ID列转换为字符型。\n")
  }

  # 检查并转换第二列和第三列为数值型
  convert_to_numeric <- function(x) {
    as.numeric(gsub("[^0-9.-]", "", as.character(x)))
  }

  for (col in c(pre_measurement, post_measurement)) {
    if (!is.numeric(data_cleaned[[col]])) {
      data_cleaned[[col]] <- convert_to_numeric(data_cleaned[[col]])
      cat("已将列", col, "转换为数值型。\n")
    }
  }

  # 检查是否存在缺失值，并删除含有缺失值的行（只考虑数值列）
  missing_summary <- sapply(data_cleaned, function(x) mean(is.na(x)))
  print("缺失值比例：")
  print(missing_summary)

  # 删除在数值列中有任何NA值的行，但保留ID列
  data_cleaned <- subset(data_cleaned, !is.na(data_cleaned[[pre_measurement]]) & !is.na(data_cleaned[[post_measurement]]))

  # 检查是否有足够的数据进行分析
  if (nrow(data_cleaned) < 2) {
    stop("在删除缺失值后，剩余的数据不足以进行配对样本T检验。")
  }

  # 将数据转换为长格式
  data_long <- pivot_longer(
    data = data_cleaned,
    cols = c(pre_measurement, post_measurement),  # 指定需要转换的列
    names_to = "measurement_time",  # 新的列名，用于表示测量时间
    values_to = "value"  # 新的列名，用于表示值
  )

  # 调整 measurement_time 列为因子，并指定水平顺序和标签
  data_long$measurement_time <- factor(data_long$measurement_time,
                                       levels = c(pre_measurement, post_measurement),
                                       labels = c("Pre", "Post"))

  # 计算每一对观测的差值并创建新数据框
  data_diff <- transform(data_cleaned, difference = data_cleaned[[post_measurement]] - data_cleaned[[pre_measurement]])

  # 可视化差值分布：箱线图
  plot_show <- ggplot(data_diff, aes(x = "", y = difference)) +
    geom_boxplot() +
    labs(title = "Distribution of Differences",
         x = "",
         y = "Difference (Post - Pre)") +
    theme_minimal()

  # 执行相关样本t检验
  paired_t_test <- t.test(data_cleaned[[post_measurement]], data_cleaned[[pre_measurement]], paired = TRUE)

  # 输出t检验结果
  # print(paired_t_test)
  return (list(plot = plot_show, info = toString(paired_t_test)))
}
