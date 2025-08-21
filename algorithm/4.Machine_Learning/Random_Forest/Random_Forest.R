library(randomForest)
library(ggplot2)
library(pROC)
library(caret)

# 读取CSV文件
file_path <- "./file/Random_Forest_Demo.csv"

# 第三类Proportion Test
random_forest_plot_f <- function(file_path){

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # 查看数据前几行
  head(data)

  # 检查数据结构
  str(data)

  # 检查是否存在缺失值
  anyNA(data)

  # 如果有缺失值，可以选择删除含有缺失值的行或者填充缺失值
  # 例如，删除含有缺失值的行
  data <- na.omit(data)

  # 假设目标变量名为 "hospital_expire_flag"
  summary(data$hospital_expire_flag)

  # 将目标变量转换为因子类型
  if (is.numeric(data$hospital_expire_flag)) {
    data$hospital_expire_flag <- as.factor(data$hospital_expire_flag)
  }

  # 对于数值型变量绘制直方图
  if(is.numeric(data$hospital_expire_flag)) {
    ggplot(data, aes(x=hospital_expire_flag)) + geom_histogram(binwidth=1, fill="blue", color="black") + theme_minimal()
  } else if(is.factor(data$hospital_expire_flag)) {
    # 对于因子类型的变量绘制条形图
    ggplot(data, aes(x=hospital_expire_flag)) + geom_bar(fill="blue", color="black") + theme_minimal()
  }

  set.seed(123)  # 设置随机种子以保证结果可重复
  trainIndex <- sample(1:nrow(data), 0.7 * nrow(data))
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]

  # 确保测试数据中的目标变量也是因子类型
  if (is.numeric(testData$hospital_expire_flag)) {
    testData$hospital_expire_flag <- as.factor(testData$hospital_expire_flag)
  }

  # 假设目标变量为 "hospital_expire_flag"，其他列为特征
  model <- randomForest(hospital_expire_flag ~ ., data = trainData, ntree = 10, importance = TRUE, do.trace = 10)
  print(model)

  # 获取概率预测
  predictions <- predict(model, newdata = testData, type = "prob")

  # 获取类别预测
  predicted_classes <- predict(model, newdata = testData, type = "response")

  # 计算AUROC
  roc_obj <- roc(testData$hospital_expire_flag, predictions[, 2])  # 假设目标变量是二分类，取第二列的概率
  auc <- auc(roc_obj)

  # 打印AUROC
  cat("AUROC:", auc, "\n")

  # 计算混淆矩阵
  conf_matrix <- confusionMatrix(data = predicted_classes, reference = testData$hospital_expire_flag)

  # 打印混淆矩阵
  print(conf_matrix$table)

  # 从混淆矩阵中提取精确率、召回率和F1分数
  precision <- conf_matrix$byClass["Precision"]
  recall <- conf_matrix$byClass["Recall"]
  f1_score <- conf_matrix$byClass["F1"]

  # 打印这些指标
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1 Score:", f1_score, "\n")

  # 从混淆矩阵中提取其他指标
  accuracy <- conf_matrix$overall["Accuracy"]
  specificity <- conf_matrix$byClass["Specificity"]
  kappa <- conf_matrix$overall["Kappa"]

  # 打印这些指标
  cat("Accuracy:", accuracy, "\n")
  cat("Specificity:", specificity, "\n")
  cat("Kappa:", kappa, "\n")

  # 使用ggplot绘制ROC曲线
  roc_data <- data.frame(
    specificity = rev(roc_obj$specificities),  # specificity
    sensitivity = rev(roc_obj$sensitivities)   # sensitivity
  )

  ggroc_plot <- ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
    geom_line(color = "blue", size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(title = paste("ROC Curve (AUC =", round(auc, 2), ")"),
         x = "1 - Specificity", y = "Sensitivity") +
    theme_minimal()

  # 打印ROC图
  # print(ggroc_plot)

  return (list(plot = ggroc_plot, info = ""))
}

# random_forest_plot_f(file_path)
