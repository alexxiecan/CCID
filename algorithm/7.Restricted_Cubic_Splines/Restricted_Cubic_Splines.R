# 加载必要的库
library(rms)
library(survival)
library(ggplot2)
library(dplyr)

# 读取CSV文件
file_path <- "./file/Restricted_Cubic_Splines_Demo.csv"

# 第四类
restricted_cubic_splines_plot_f <- function(file_path) {

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # 获取数据框的列名
  colnames_data <- colnames(data)

  # 假设第一列是时间（time），第二列是要应用RCS的主连续变量，最后一列是事件状态（event）
  time_column <- data[, 1]
  main_continuous_var <- data[, 2]
  event_column <- data[, ncol(data)]

  # 创建Surv对象
  surv_data <- Surv(time = time_column, event = event_column)

  # 提取所有协变量
  covariates <- setdiff(colnames_data, c(colnames_data[1], colnames_data[2], colnames_data[ncol(data)]))

  # 使用rms包创建设计矩阵
  dd <- datadist(data)  # 应该传入整个数据框
  options(datadist = dd)

  # 构建模型公式
  formula <- as.formula(paste("surv_data ~ rcs(", colnames_data[2], ", 4) +", paste(covariates, collapse = " + ")))

  # 拟合包含RCS的Cox比例风险模型
  fit <- cph(formula, data = data, x = TRUE, y = TRUE)

  # 显示模型摘要
  summary(fit)

  # 获取主连续变量的列名
  predictor_name <- colnames_data[2]

  # 计算预测值和置信区间
  p <- as.data.frame(Predict(fit, name = predictor_name, fun = exp, conf.int = TRUE))

  # 用 ggplot2 绘制 RCS 曲线
  p_plot <- ggplot(p, aes(x = !!sym(predictor_name), y = yhat)) +
    geom_line(color = "blue", size = 1) +  # 绘制RCS曲线
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +  # 置信区间
    labs(title = paste("Hazard Ratio for", predictor_name, "(RCS)"),
         x = paste0(predictor_name, " Level"),
         y = "Hazard Ratio") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # 标题居中

  # print(p_plot)  # 确保图像显示

  return (list(plot = p_plot, info = ""))
}

# 运行函数
# restricted_cubic_splines_plot_f(file_path)
