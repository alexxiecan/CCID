library(ggplot2)
library(caret)
library(pROC)
library(dplyr)
library(glmnet)

# 读取CSV文件
file_path <- "./file/Logistic_Regression_Demo.csv"

# 第四类Machine Learning
logistic_regression_plot_f <- function(file_path){

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # 查看数据结构
  str(data)

  # 假设最后一列是结局变量（response variable）
  outcome_variable <- names(data)[ncol(data)]

  # 准备数据
  set.seed(123)  # 设置随机种子以保证结果可重复
  trainIndex <- createDataPartition(data[[outcome_variable]], p = .8, list = FALSE, times = 1)
  trainSet <- data[trainIndex, ]
  testSet  <- data[-trainIndex, ]

  # 分离特征与目标变量
  X_train <- trainSet %>% select(-all_of(outcome_variable))
  y_train <- as.factor(trainSet[[outcome_variable]])
  X_test  <- testSet %>% select(-all_of(outcome_variable))
  y_test  <- as.factor(testSet[[outcome_variable]])

  # 将因子转换为0/1数值
  y_train_numeric <- as.numeric(y_train) - 1
  y_test_numeric <- as.numeric(y_test) - 1

  # 训练逻辑回归模型（带L2正则化）
  x_train_matrix <- as.matrix(X_train)

  # 选择一个合适的lambda值
  cv_fit <- cv.glmnet(x = x_train_matrix, y = y_train_numeric, family = "binomial", alpha = 0)
  best_lambda <- cv_fit$lambda.min

  # 使用最佳lambda训练模型
  logistic_model <- glmnet(x = x_train_matrix, y = y_train_numeric, family = "binomial", lambda = best_lambda, alpha = 0)

  # 预测概率
  predictions <- predict(logistic_model, newx = as.matrix(X_test), type = "response", s = best_lambda)

  # 将预测概率转换为类别
  predicted_classes <- ifelse(predictions > 0.5, "1", "0")  # 假设二分类问题

  # 模型评估
  conf_matrix <- table(Predicted = predicted_classes, Actual = y_test)

  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  f1_score <- 2 * (precision * recall) / (precision + recall)

  cat("Accuracy: ", accuracy, "\n")
  cat("Precision: ", precision, "\n")
  cat("Recall: ", recall, "\n")
  cat("F1 Score: ", f1_score, "\n")

  # 绘制混淆矩阵
  conf_matrix_df <- as.data.frame(as.table(conf_matrix))
  colnames(conf_matrix_df) <- c("Actual", "Predicted", "Count")
  conf_matrix_plot <- ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
    geom_tile() +
    geom_text(aes(label = Count), vjust = 0.5, hjust = 0.5) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
    theme_minimal()

  # 计算ROC曲线
  roc_obj <- roc(y_test_numeric, predictions)

  # 使用ggplot绘制ROC曲线
  roc_data <- data.frame(
    specificity = rev(roc_obj$specificities),  # specificity
    sensitivity = rev(roc_obj$sensitivities)   # sensitivity
  )

  ggroc_plot <- ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
    geom_line(color = "blue", size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(title = paste("ROC Curve (AUC =", round(auc(roc_obj), 2), ")"),
         x = "1 - Specificity", y = "Sensitivity") +
    theme_minimal()

  # 打印ROC图
  # print(ggroc_plot)

  # plot_record <- recordPlot()  # 捕获当前绘图

  return (list(plot = ggroc_plot, info = ""))
}

# logistic_regression_plot_f(file_path)
