library(dplyr)
library(ggplot2)

# 读取CSV文件
file_path <- "./file/Proportion_Test_Demo.csv"

# 第三类Proportion Test
proportion_test_plot_f <- function(file_path){

  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # 查看数据结构以了解列名
  str(data)

  # 检查是否存在重复行
  duplicate_rows <- data[duplicated(data), ]
  if (nrow(duplicate_rows) > 0) {
    cat("检测到重复行，正在删除...\n")

    # 删除重复行（基于所有列）
    data_unique <- distinct(data)

    # 确认删除重复后的行数
    cat("原始数据行数: ", nrow(data), "\n")
    cat("删除重复后数据行数: ", nrow(data_unique), "\n")
  } else {
    cat("没有检测到重复行，继续进行后续步骤。\n")
    data_unique <- data
  }

  # 获取最后一列的列名并确保它是因子类型
  last_column <- names(data)[ncol(data)]  # 自动获取最后一列的列名

  # 检查最后一列中的缺失值
  missing_values <- sum(is.na(data_unique[[last_column]]))
  total_rows <- nrow(data_unique)
  missing_percentage <- missing_values / total_rows

  cat("最后一列中缺失值的数量: ", missing_values, "\n")
  cat("最后一列中缺失值的比例: ", round(missing_percentage * 100, 2), "%\n")

  if (missing_percentage < 0.2) {
    cat("缺失值比例小于20%，正在删除含有缺失值的行...\n")
    data_unique <- na.omit(data_unique)
  } else if (missing_percentage >= 0.2) {
    cat("缺失值比例大于或等于20%，正在使用众数填充缺失值...\n")
    mode_value <- as.character(Hmisc::stat.mode(data_unique[[last_column]], na.rm = TRUE))  # 使用Hmisc包中的stat.mode函数计算众数
    data_unique[[last_column]] <- replace_na(as.character(data_unique[[last_column]]), mode_value)
    data_unique[[last_column]] <- as.factor(trimws(data_unique[[last_column]]))  # 去除多余空格并转换为因子
  }

  # 移除"Undecided"类别的观测值
  undecided_count <- sum(data_unique[[last_column]] == "Undecided", na.rm = TRUE)
  if (undecided_count > 0) {
    cat("检测到", undecided_count, "个'Undecided'类别，正在移除...\n")
    data_unique <- data_unique[data_unique[[last_column]] != "Undecided", ]
  }

  # 计算每个类别的频数
  category_counts <- data_unique %>%
    group_by(.data[[last_column]]) %>%
    summarise(count = n())

  # 查看类别计数
  print(category_counts)

  # 定义预期比例（例如0.5表示期望的比例是50%）
  expected_proportion <- 0.5

  # 初始化一个列表来保存所有比例检验的结果
  prop_test_results <- list()

  # 获取唯一类别，并确保它们是字符类型以避免匹配问题
  categories <- unique(as.character(data_unique[[last_column]]))

  if (length(categories) == 0) {
    stop("最后一列中没有有效的类别数据。请检查数据。")
  }

  # 循环遍历每个类别并进行比例检验
  for (category in categories) {
    cat("\n--- Proportion Test for Category:", category, "---\n")

    # 进行比例检验
    count_for_category <- category_counts$count[as.character(category_counts[[last_column]]) == category]

    if (length(count_for_category) == 0) {
      cat("警告: 类别'", category, "'没有观测值，跳过此类别。\n")
      next
    }

    total_count <- sum(category_counts$count)

    prop_test_result <- tryCatch({
      prop.test(
        x = count_for_category,
        n = total_count,
        p = expected_proportion,
        alternative = "two.sided",
        conf.level = 0.95
      )
    }, error = function(e) {
      cat("错误: 在类别'", category, "'上进行比例检验时出错。\n", e$message, "\n")
      NULL
    })

    if (!is.null(prop_test_result)) {
      # 输出结果
      print(prop_test_result)

      # 保存结果
      prop_test_results[[category]] <- prop_test_result
    } else {
      cat("警告: 类别'", category, "'的比例检验结果为空，跳过绘图。\n")
    }
  }

  # 绘制所有主要类别的观测值柱状图
  bar_plot_all <- ggplot(category_counts, aes(x = .data[[last_column]], y = count, fill = .data[[last_column]])) +
    geom_bar(stat = "identity") +
    labs(title = "Observation Counts for All Categories",
         x = "Category",
         y = "Count",
         caption = paste0("Total Observations: ", total_count)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # print(bar_plot_all)
  return (list(plot = bar_plot_all, info = ""))
}
# ggsave(filename = "D:/r_proj/data/proportion_test_all_categories.png", plot = bar_plot_all)

# # 如果需要将所有比例检验结果保存到一个文件中
# output_file <- "./proportion_test_results.txt"
# sink(output_file)
# cat("Proportion Test Results:\n\n")
# for (category in names(prop_test_results)) {
#   cat("--- Proportion Test for Category:", category, "---\n")
#   print(prop_test_results[[category]])
#   cat("\n")
# }
# sink()
