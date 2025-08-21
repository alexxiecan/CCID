# 加载必要的库
library(survival)
library(survminer)

# 读取CSV文件
file_path <- "./file/Kaplan-Meier_Demo.csv"

# 第四类
kaplan_meier_plot_f <- function(file_path){

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # 直接拟合模型
  km_fit <- survfit(Surv(time = data[[1]], event = data[[ncol(data)]]) ~ 1)

  # 绘图
  result_plot <- ggsurvplot(km_fit,
                            data = data,
                            risk.table = TRUE,
                            pval = FALSE,
                            conf.int = TRUE,
                            ggtheme = theme_minimal(),
                            palette = "jco",
                            title = "Kaplan-Meier Survival Curve",
                            xlab = "Time (units)",
                            ylab = "Survival Probability")

  return(list(plot = result_plot, info = "Plot created."))
}

# kaplan_meier_plot_f(file_path)
