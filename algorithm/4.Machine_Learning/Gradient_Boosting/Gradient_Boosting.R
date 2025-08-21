library(xgboost)
library(dplyr)
library(caret)

# 读取CSV文件
file_path <- "./file/Gradient_Boosting_Demo.csv"

# 第四类Machine Learning
gradient_boosting_plot_f <- function(file_path){

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

  # 将数据转换为xgboost所需的格式
  dtrain <- xgb.DMatrix(data = as.matrix(trainSet %>% select(-all_of(outcome_variable))),
                        label = trainSet[[outcome_variable]])
  dtest  <- xgb.DMatrix(data = as.matrix(testSet %>% select(-all_of(outcome_variable))),
                        label = testSet[[outcome_variable]])

  # 定义参数
  params <- list(
    objective = "reg:squarederror",  # 回归任务
    eta = 0.3,                       # 学习率
    max_depth = 6,                   # 树的最大深度
    subsample = 0.8,                 # 每棵树的样本比例
    colsample_bytree = 0.8           # 每棵树使用的特征比例
  )

  # 训练模型
  bst <- xgb.train(params = params,
                   data = dtrain,
                   nrounds = 100,  # 迭代次数
                   watchlist = list(val = dtest, train = dtrain),
                   early_stopping_rounds = 10,  # 早停机制
                   maximize = FALSE, verbose = 1)

  # 预测
  predictions <- predict(bst, dtest)

  # 评估模型
  postResample(predictions, testSet[[outcome_variable]])

  # 生成残差
  residuals <- testSet[[outcome_variable]] - predictions

  # 残差图
  residual_plot <- ggplot(data = data.frame(Residuals = residuals, Predicted = predictions), aes(x = Predicted, y = Residuals)) +
    geom_point() +  # 绘制散点
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # 添加零线
    labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals") +
    theme_minimal()
  print(residual_plot)

  # 实际 vs 预测散点图
  actual_vs_predicted_plot <- ggplot(data = data.frame(Actual = testSet[[outcome_variable]], Predicted = predictions), aes(x = Actual, y = Predicted)) +
    geom_point() +  # 绘制散点
    geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +  # 添加y=x线
    labs(title = "Actual vs Predicted Values", x = "Actual Values", y = "Predicted Values") +
    theme_minimal()
  # print(actual_vs_predicted_plot)

  # 特征重要性图
  importance_matrix <- xgb.importance(feature_names = names(trainSet %>% select(-all_of(outcome_variable))), model = bst)
  xgb.plot.importance(importance_matrix, top_n = 10)  # 显示前10个最重要的特征

  # 保存残差图
  # ggsave("D:\\r_proj\\residual_plot.png", residual_plot, width = 10, height = 8, units = "in")

  # 保存实际 vs 预测散点图
  # ggsave("D:\\r_proj\\actual_vs_predicted_plot.png", actual_vs_predicted_plot, width = 10, height = 8, units = "in")

  return (list(plot = residual_plot, info = ""))

}
