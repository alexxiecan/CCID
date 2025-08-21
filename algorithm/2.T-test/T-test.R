# 加载必要的库
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

# 定义读取CSV文件的路径
file_path <- "./file/T-test_Demo.csv"

# 两样本T检验
t_test_plot_f <- function(file_path){

  # 动态读取CSV文件，并自动检测列类型
  data <- read_csv(file_path, show_col_types = FALSE)

  # 动态获取分类变量名（最后一列）
  last_col_name <- colnames(data)[ncol(data)]
  cat("分类变量名为:", last_col_name, "\n")

  # 获取所有数值型变量名（除了最后一列外的所有列）
  numeric_var_names <- colnames(data)[-ncol(data)]

  if (length(numeric_var_names) == 0) {
    stop("没有找到数值型变量")
  } else {
    cat("数值变量为:", paste(numeric_var_names, collapse = ", "), "\n")
  }

  # 检查每一列的缺失值比例，并决定如何处理
  missing_summary <- summarise_all(data, ~mean(is.na(.))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_rate")

  print(missing_summary)

  # 删除缺失值比例低于20%的列
  data_cleaned <- select(data, where(function(x) mean(!is.na(x)) > 0.8))

  # 定义一个函数来清理非数值数据并返回是否为纯数值型
  clean_and_check_numeric <- function(x) {
    # 尝试将非数值数据转换为NA
    x_cleaned <- as.numeric(as.character(x))
    # 返回清理后的数据和是否为纯数值型的标志
    list(cleaned_data = x_cleaned, is_pure_numeric = !any(is.na(x_cleaned), na.rm = TRUE))
  }

  # 对数值型变量进行处理：根据条件使用插值法填充缺失值
  data_cleaned <- mutate(data_cleaned, across(all_of(numeric_var_names),
                                              function(x) if (sum(!is.na(x)) >= 2) na.approx(x, na.rm = FALSE, rule = 2) else x))

  # 使用符号引用动态列名并处理分类变量中的缺失值
  group_var_sym <- sym(last_col_name)
  data_cleaned[[as.character(group_var_sym)]] <- ifelse(is.na(data_cleaned[[as.character(group_var_sym)]]), "Unknown", data_cleaned[[as.character(group_var_sym)]])

  # 删除所有NA行
  data_cleaned <- drop_na(data_cleaned)

  # 打印清理后的数据集前几行以检查
  head(data_cleaned)

  # 分离两组数据以准备T检验
  group_a <- filter(data_cleaned, !!group_var_sym == "male") %>%
    select(-!!group_var_sym)

  group_b <- filter(data_cleaned, !!group_var_sym == "female") %>%
    select(-!!group_var_sym)

  # 确保两个分组的数据具有相同的列名
  common_columns <- intersect(names(group_a), names(group_b))

  # 确保用于T检验的列是数值型，并且只对数值型列进行T检验
  results <- list()
  for (col in common_columns) {
    # 清理并检查是否为纯数值型
    cleaned_group_a <- clean_and_check_numeric(group_a[[col]])
    cleaned_group_b <- clean_and_check_numeric(group_b[[col]])

    group_a[[col]] <- cleaned_group_a$cleaned_data
    group_b[[col]] <- cleaned_group_b$cleaned_data

    # 检查是否所有值都是数值类型并且没有NA值
    if (cleaned_group_a$is_pure_numeric && cleaned_group_b$is_pure_numeric) {
      # 确认列确实是数值型
      if (is.numeric(group_a[[col]]) && is.numeric(group_b[[col]])) {
        t_test_result <- tryCatch({
          t.test(group_a[[col]], group_b[[col]])
        }, error = function(e) e)

        results[[col]] <- t_test_result
      } else {
        cat("警告: 列", col, "不是数值型，跳过T检验。\n")
      }
    } else {
      cat("警告: 列", col, "包含非数值数据或NA值，跳过T检验。\n")
    }
  }

  # 打印结果
  print(results)

  # 创建箱线图（选择第一个数值变量作为示例）
  first_numeric_var <- numeric_var_names[1]
  ggplot(data_cleaned, aes(x = !!group_var_sym, y = !!sym(first_numeric_var))) +
    geom_boxplot(aes(fill = !!group_var_sym), outlier.shape = NA) +  # 移除异常点以使图形更清晰
    geom_jitter(width = 0.2, alpha = 0.6) +  # 添加抖动点以显示数据点分布
    labs(title = paste(first_numeric_var, "Distribution by", last_col_name),
         x = last_col_name,
         y = first_numeric_var) +
    theme_minimal()

  # 计算平均值和标准误（不使用管道）
  summary_stats <- summarise(group_by(data_cleaned, !!group_var_sym),
                             across(all_of(numeric_var_names),
                                    list(mean = ~mean(.x, na.rm = TRUE),
                                         se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                                    .names = "{col}_{fn}"))

  # 创建密度图（选择第一个数值变量作为示例）
  density_plot <- ggplot(data_cleaned, aes_string(x = first_numeric_var, fill = last_col_name)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density of", first_numeric_var, "by", last_col_name),
         x = first_numeric_var,
         y = "Density") +
    theme_minimal()

  # print(density_plot)
  return (list(plot = density_plot, info = ""))
}
