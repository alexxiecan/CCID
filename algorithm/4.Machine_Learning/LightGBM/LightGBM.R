# 加载必要的库
library(lightgbm)
library(pROC)
library(caret)
library(ggplot2)

# 读取CSV文件
file_path <- "./file/LightGBM_Demo.csv"

# 第四类Machine Learning
lightgbm_plot_f <- function(file_path){

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # 查看数据前几行
  head(data)

  # 检查数据的结构
  str(data)

  # 假设最后一列是目标变量
  labels <- data[, ncol(data)]

  # 检查labels的长度
  length(labels)

  # 查看labels的内容
  print(table(labels))

  # 提取特征
  features <- data[, -ncol(data)]

  # 检查是否有缺失值
  if (any(is.na(features))) {
    # 简单处理：用均值填充数值型特征的缺失值
    features[is.na(features)] <- lapply(features, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))
  }

  # 查看预处理后的数据
  head(features)
  summary(labels)

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

  # 定义LightGBM参数
  params <- list(
    objective = "binary",  # 二分类问题
    metric = "auc",
    boosting = "gbdt",
    num_leaves = 31,
    learning_rate = 0.05,
    feature_fraction = 0.9,
    bagging_fraction = 0.8,
    bagging_freq = 5,
    verbose = 0
  )

  # 创建数据集对象
  train_data <- lgb.Dataset(data = as.matrix(trainFeatures), label = trainLabels)
  valid_data <- lgb.Dataset(data = as.matrix(testFeatures), label = testLabels, reference = train_data)

  # 训练模型
  model <- lgb.train(params, train_data, nrounds = 2000, valids = list(valid = valid_data))

  # 预测
  preds <- predict(model, as.matrix(testFeatures))

  # 计算并绘制ROC曲线
  roc_obj <- roc(testLabels, preds)
  plot(roc_obj, main="ROC Curve", col="blue", lwd=2)
  abline(a=0, b=1, col="gray", lty=2)

  # 打印AUC值
  print(paste("AUC:", auc(roc_obj)))

  # 其他评估指标
  # 将预测概率转换为类别标签（例如，阈值为0.5）
  predicted_labels <- ifelse(preds > 0.5, 1, 0)

  # 计算混淆矩阵
  conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(testLabels))

  # 打印混淆矩阵
  print(conf_matrix$table)

  # 打印准确率
  print(paste("Accuracy:", conf_matrix$overall['Accuracy']))

  # 打印精确率
  print(paste("Precision:", conf_matrix$byClass['Precision']))

  # 打印召回率
  print(paste("Recall:", conf_matrix$byClass['Recall']))

  # 打印F1分数
  print(paste("F1 Score:", conf_matrix$byClass['F1']))

  # 手动计算对数损失
  log_loss_manual <- function(actual, predicted) {
    epsilon <- 1e-15
    predicted <- pmin(1 - epsilon, pmax(epsilon, predicted))  # 避免取对数时出现无穷大
    -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  }

  # 计算对数损失
  log_loss_value <- log_loss_manual(as.numeric(as.factor(testLabels)) - 1, preds)
  print(paste("Log Loss (Manual):", log_loss_value))

  # 创建一个数据框来存储这些指标
  metrics_data <- data.frame(
    Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
    Value = c(conf_matrix$overall['Accuracy'], conf_matrix$byClass['Precision'], conf_matrix$byClass['Recall'], conf_matrix$byClass['F1'])
  )

  # 对数损失单独处理
  log_loss_data <- data.frame(
    Metric = "Log Loss",
    Value = log_loss_value
  )

  # 将所有指标合并到一个数据框中
  all_metrics_data <- rbind(metrics_data, log_loss_data)

  # 绘制综合图表
  light_gbm_plot <- ggplot(all_metrics_data, aes(x = Metric, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.8, alpha = 0.7) +
    geom_point(data = subset(all_metrics_data, Metric == "Log Loss"), size = 4, color = "red") +
    geom_text(data = subset(all_metrics_data, Metric == "Log Loss"), aes(label = round(Value, 3)), vjust = -0.5, color = "red") +
    labs(title = "Model Performance Metrics",
         x = "Metric",
         y = "Value") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")

  return (list(plot = light_gbm_plot, info = ""))

}
