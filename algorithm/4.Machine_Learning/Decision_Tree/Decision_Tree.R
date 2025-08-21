# 加载必要的库
library(rpart)
library(rpart.plot)
library(caret)

# 读取CSV文件
file_path <- "./file/Decision_Tree_Demo.csv"

# 第四类Machine Learning
decision_tree_plot_f <- function(file_path){

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # 查看数据前几行
  head(data)

  # 检查数据的结构
  str(data)

  # 假设最后一列是目标变量
  labels <- data[, ncol(data)]

  # 提取特征
  features <- data[, -ncol(data)]

  # 检查是否有缺失值
  if (any(is.na(features))) {
    # 简单处理：用均值填充数值型特征的缺失值
    features[is.na(features)] <- lapply(features, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))
  }

  # 设置随机种子以确保结果可重复
  set.seed(123)

  # 使用sample函数划分数据
  indices <- sample(1:nrow(data), nrow(data) * 0.7)
  trainIndex <- indices
  testIndex <- setdiff(1:nrow(data), trainIndex)

  # 划分数据集
  trainFeatures <- features[trainIndex,]
  testFeatures  <- features[testIndex,]
  trainLabels   <- labels[trainIndex]
  testLabels    <- labels[testIndex]

  # 确认划分
  nrow(trainFeatures)
  nrow(testFeatures)

  # 训练决策树模型
  dt_model <- rpart(trainLabels ~ .,
                    data = as.data.frame(trainFeatures),
                    method = "anova",  # 回归分析
                    control = rpart.control(cp = 0.01))  # 控制复杂度参数

  # 绘制决策树
  rpart.plot(dt_model, main = "Decision Tree for Regression")

  # 预测
  preds <- predict(dt_model, newdata = as.data.frame(testFeatures))

  # 计算并打印性能指标
  # MSE (Mean Squared Error)
  mse <- mean((preds - testLabels)^2)
  print(paste("MSE:", mse))

  # RMSE (Root Mean Squared Error)
  rmse <- sqrt(mse)
  print(paste("RMSE:", rmse))

  # R-squared
  rsq <- 1 - sum((testLabels - preds)^2) / sum((testLabels - mean(testLabels))^2)
  print(paste("R-squared:", rsq))

  # 创建一个数据框来存储这些指标
  metrics_data <- data.frame(
    Metric = c("MSE", "RMSE", "R-squared"),
    Value = c(mse, rmse, rsq)
  )

  # 绘制性能指标
  metrics_data_plot <- ggplot(metrics_data, aes(x = Metric, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.8, alpha = 0.7) +
    labs(title = "Model Performance Metrics",
         x = "Metric",
         y = "Value") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")

  # 绘制预测值与实际值的散点图
  comparison_data <- data.frame(
    Actual = testLabels,
    Predicted = preds
  )

  decision_tree_plot <- ggplot(comparison_data, aes(x = Actual, y = Predicted)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = "Actual vs. Predicted Values",
         x = "Actual Values",
         y = "Predicted Values") +
    theme_minimal()

  return (list(plot = decision_tree_plot, info = ""))

}
