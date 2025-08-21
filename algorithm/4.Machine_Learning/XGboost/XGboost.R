# 导入必要的库
library(xgboost)
library(pROC)  # 用于计算和绘制ROC曲线
library(caret)  # 用于绘制混淆矩阵
library(yardstick)  # 用于生成分类报告
library(dplyr)  # 用于数据处理
library(purrr)  # 用于 map_dfr 函数
library(ggplot2)  # 用于 ggplot 绘图

# 读取CSV文件
file_path <- "./file/XGboost_Demo.csv"

# 第四类Machine Learning
xgboost_plot_f <- function(file_path){

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # 分割输入变量和目标变量
  inputs <- data[, -ncol(data)]  # 假设最后一列是目标变量
  target <- data$hospital_expire_flag

  # 检查并转换非数值型列
  for (col in names(inputs)) {
    if (!is.numeric(inputs[[col]])) {
      if (is.factor(inputs[[col]])) {
        # 将因子变量转换为虚拟变量
        inputs <- model.matrix(~ . - 1, data = inputs)
        break  # 一旦使用了 model.matrix，就退出循环
      } else {
        # 其他非数值型列，尝试转换为数值型
        inputs[[col]] <- as.numeric(as.character(inputs[[col]]))
      }
    }
  }

  # 打印检查
  print(head(inputs))  # 查看输入特征的前几行
  print(head(target))  # 查看目标变量的前几行

  # 创建训练集和测试集
  set.seed(100)  # 设置随机种子以确保结果可重复
  sample <- sample.int(nrow(inputs), floor(0.75 * nrow(inputs)), replace = FALSE)
  train_inputs <- inputs[sample, ]
  train_target <- target[sample]
  test_inputs <- inputs[-sample, ]
  test_target <- target[-sample]

  # 打印检查
  print(dim(train_inputs))  # 查看训练集输入特征的维度
  print(length(train_target))  # 查看训练集目标变量的长度
  print(dim(test_inputs))  # 查看测试集输入特征的维度
  print(length(test_target))  # 查看测试集目标变量的长度

  # 将数据转换为DMatrix格式
  dtrain <- xgb.DMatrix(data = as.matrix(train_inputs), label = train_target)
  dtest <- xgb.DMatrix(data = as.matrix(test_inputs), label = test_target)

  # 打印检查
  print(dtrain)  # 查看训练集DMatrix对象
  print(dtest)  # 查看测试集DMatrix对象

  # 创建xgboost模型
  params <- list(
    booster = "gbtree",
    eta = 0.1,
    max_depth = 5,
    subsample = 1,
    colsample_bytree = 1,
    objective = "binary:logistic"  # 使用二分类目标函数
  )

  watchlist <- list(train = dtrain, test = dtest)

  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 50,
    watchlist = watchlist,
    early_stopping_rounds = 10,
    verbose = 0  # 可选：设置为0可以关闭输出信息
  )

  # 进行预测
  prediction <- predict(xgb_model, dtest)

  # 计算AUC
  roc_obj <- roc(test_target, prediction)
  auc_value <- auc(roc_obj)
  #print(auc_value)

  # 使用ggplot绘制ROC曲线
  roc_data <- data.frame(
    specificity = rev(roc_obj$specificities),  # specificity
    sensitivity = rev(roc_obj$sensitivities)   # sensitivity
  )

  plot_record <- ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
    geom_line(color = "blue", size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(title = paste("ROC Curve (AUC =", round(auc_value, 2), ")"),
         x = "1 - Specificity", y = "Sensitivity") +
    theme_minimal()

  # 混淆矩阵
  predicted_labels <- ifelse(prediction > 0.5, 1, 0)
  confusionMatrix <- confusionMatrix(factor(predicted_labels, levels = c(0, 1)), factor(test_target, levels = c(0, 1)))
  print(confusionMatrix)
  fourfoldplot(confusionMatrix$table, main = "Confusion Matrix")

  # 精确率-召回率曲线
  pr_obj <- roc(test_target, prediction, levels = c(0, 1), direction = "<")
  xgboost_plot <- plot(pr_obj, main = "Precision-Recall Curve", col = "blue", lwd = 2)
  abline(a = 0, b = 1, col = "gray", lty = 2)  # 添加对角线
  legend("bottomright", legend = paste("AUC =", round(auc(pr_obj), 2)), col = "blue", lwd = 2)

  # F1 分数
  # 将预测标签和实际标签转换为 tibble 格式
  results <- tibble(
    truth = factor(test_target, levels = c(0, 1)),
    pred = factor(predicted_labels, levels = c(0, 1))
  )

  # 计算 F1 分数
  f1_score <- f_meas(results, truth, pred, beta = 1)
  #print(paste("F1 Score:", f1_score$.estimate))

  # plot_record <- recordPlot()  # 捕获当前绘图

  return(list(plot = plot_record, info = ""))
}

# xgboost_plot_f(file_path)
