# 加载必要的库
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

source('./algorithm/1.Categorical_Variable_Frequency_Calculation/Categorical_Variable_Frequency_Calculation.R')

source('./algorithm/2.T-test/T-test.R')
source('./algorithm/2.T-test/Related_Samples_Test.R')

source('./algorithm/3.Proportion_Test/Proportion_Test.R')

source('./algorithm/4.Machine_Learning/Decision_Tree/Decision_Tree.R')
source('./algorithm/4.Machine_Learning/Gradient_Boosting/Gradient_Boosting.R')
source('./algorithm/4.Machine_Learning/LightGBM/LightGBM.R')
source('./algorithm/4.Machine_Learning/Logistic_Regression/Logistic_Regression.R')
source('./algorithm/4.Machine_Learning/Random_Forest/Random_Forest.R')
source('./algorithm/4.Machine_Learning/XGboost/XGboost.R')

source('./algorithm/5.Kaplan-Meier/Kaplan-Meier.R')
source('./algorithm/6.Bee_Swarm/Bee_Swarm.R')
source('./algorithm/7.Restricted_Cubic_Splines/Restricted_Cubic_Splines.R')



# 第一类Categorical Variable Frequency Calculation
category <- function(input, output, session){

  # 监控选择的下拉菜单，然后刷新图和下载按钮
  observeEvent(input$category_select, {

    file_path <- "./file/Categorical_Variable_Frequency_Calculation_Demo.csv"
    plot <- category_plot(file_path)$plot
    info <- category_plot(file_path)$info

    # 定义下载按钮的逻辑
    output$download_category_demo <- downloadHandler(
      filename = function() {
        paste(input$category_select, Sys.Date(), ".csv", sep = "_")
      },
      content = function(file) {
        # 假设源文件已加载到 `data` 中
        data <- read.csv(file_path)
        write.csv(data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    output$category_variable_plot <- renderPlot({
      plot
    })

    output$category_variable_info  <- renderText({
      info
    })

    output$download_category_plot <- downloadHandler(
      filename = function() {
        paste0("category_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)
        plot <- category_plot(file_path)$plot
        print(plot)
        dev.off()
      }
    )

  })
}




# 第二类T test
t_test <- function(input, output, session){

  # 监控选择的下拉菜单，然后刷新图和下载按钮
  observeEvent(input$ttest_select, {
    if (input$ttest_select == "T-test"){
      file_path <- "./file/T-test_Demo.csv"
      plot <- t_test_plot_f(file_path)$plot
      info <- t_test_plot_f(file_path)$info
    }else{
      file_path <- "./file/Related_Samples_Test_Demo.csv"
      plot <- related_samples_test_plot(file_path)$plot
      info <- related_samples_test_plot(file_path)$info
    }

    # 定义下载按钮的逻辑
    output$download_ttest_demo <- downloadHandler(
      filename = function() {
        paste(input$ttest_select, Sys.Date(), ".csv", sep = "_")
      },
      content = function(file) {
        # 假设源文件已加载到 `data` 中
        data <- read.csv(file_path)
        write.csv(data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    output$t_test_plot <- renderPlot({
      plot
    })

    output$t_test_info  <- renderText({
      info
    })

    output$download_t_test_plot <- downloadHandler(
      filename = function() {
        paste0("t_test_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)
        if (input$ttest_select == "T-test"){
          plot <- t_test_plot_f(file_path)$plot
        }else{
          plot <- related_samples_test_plot(file_path)$plot
        }
        print(plot)
        dev.off()
      }
    )

  })
}

# 第三类Proportion Test
proportion_test <- function(input, output, session){

  # 监控选择的下拉菜单，然后刷新图和下载按钮
  observeEvent(input$proportion_test_select, {

    file_path <- "./file/Proportion_Test_Demo.csv"
    plot <- proportion_test_plot_f(file_path)$plot
    info <- proportion_test_plot_f(file_path)$info

    # 定义下载按钮的逻辑
    output$download_proportion_demo <- downloadHandler(
      filename = function() {
        paste(input$proportion_test_select, Sys.Date(), ".csv", sep = "_")
      },
      content = function(file) {
        # 假设源文件已加载到 `data` 中
        data <- read.csv(file_path)
        write.csv(data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    output$proportion_test_plot <- renderPlot({
      plot
    })

    output$proportion_test_info  <- renderText({
      info
    })

    output$download_proportion_test_plot <- downloadHandler(
      filename = function() {
        paste0("proportion_test_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)
        plot <- proportion_test_plot_f(file_path)$plot
        print(plot)
        dev.off()
      }
    )

  })

}
# 第四类Machine Learning
machine_learning <- function(input, output, session){

  # 监控选择的下拉菜单，然后刷新图和下载按钮
  observeEvent(input$machine_learning_select, {

    if (input$machine_learning_select == "Decision Tree"){
      file_path <- "./file/Decision_Tree_Demo.csv"
      plot <- decision_tree_plot_f(file_path)$plot
      info <- decision_tree_plot_f(file_path)$info
    }else if (input$machine_learning_select == "Gradient Boosting"){
      file_path <- "./file/Gradient_Boosting_Demo.csv"
      plot <- gradient_boosting_plot_f(file_path)$plot
      info <- gradient_boosting_plot_f(file_path)$info
    }else if (input$machine_learning_select == "LightGBM"){
      file_path <- "./file/LightGBM_Demo.csv"
      plot <- lightgbm_plot_f(file_path)$plot
      info <- lightgbm_plot_f(file_path)$info
    }else if (input$machine_learning_select == "Logistic Regression"){
      file_path <- "./file/Logistic_Regression_Demo.csv"
      plot <- logistic_regression_plot_f(file_path)$plot
      info <- logistic_regression_plot_f(file_path)$info
    }else if (input$machine_learning_select == "Random Forest"){
      file_path <- "./file/Random_Forest_Demo.csv"
      plot <- random_forest_plot_f(file_path)$plot
      info <- random_forest_plot_f(file_path)$info
    }else if (input$machine_learning_select == "XGboost"){
      file_path <- "./file/XGboost_Demo.csv"
      plot <- xgboost_plot_f(file_path)$plot
      info <- xgboost_plot_f(file_path)$info
    }

    # 定义下载按钮的逻辑
    output$download_machine_learning_demo <- downloadHandler(
      filename = function() {
        paste(input$machine_learning_select, Sys.Date(), ".csv", sep = "_")
      },
      content = function(file) {
        # 假设源文件已加载到 `data` 中
        data <- read.csv(file_path)
        write.csv(data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    output$machine_learning_plot <- renderPlot({
      plot
    })

    output$machine_learning_info  <- renderText({
      info
    })

    output$download_machine_learning_plot <- downloadHandler(
      filename = function() {
        paste0("machine_learning_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)
        if (input$machine_learning_select == "Decision Tree"){
          plot <- decision_tree_plot_f(file_path)$plot
        }else if (input$machine_learning_select == "Gradient Boosting"){
          plot <- gradient_boosting_plot_f(file_path)$plot
        }else if (input$machine_learning_select == "LightGBM"){
          plot <- lightgbm_plot_f(file_path)$plot
        }else if (input$machine_learning_select == "Logistic Regression"){
          plot <- logistic_regression_plot_f(file_path)$plot
        }else if (input$machine_learning_select == "Random Forest"){
          plot <- random_forest_plot_f(file_path)$plot
        }else if (input$machine_learning_select == "XGboost"){
          plot <- xgboost_plot_f(file_path)$plot
        }
        print(plot)
        dev.off()
      }
    )

  })

}
# 第五类Kaplan Meier
kaplan_meier <- function(input, output, session){

  # 监控选择的下拉菜单，然后刷新图和下载按钮
  observeEvent(input$kaplan_meier_select, {

    file_path <- "./file/Kaplan-Meier_Demo.csv"
    plot <- kaplan_meier_plot_f(file_path)$plot
    info <- kaplan_meier_plot_f(file_path)$info

    # 定义下载按钮的逻辑
    output$download_kaplan_meier_demo <- downloadHandler(
      filename = function() {
        paste(input$kaplan_meier_select, Sys.Date(), ".csv", sep = "_")
      },
      content = function(file) {
        # 假设源文件已加载到 `data` 中
        data <- read.csv(file_path)
        write.csv(data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    output$kaplan_meier_plot <- renderPlot({
      plot
    })

    output$kaplan_meier_info  <- renderText({
      info
    })

    output$download_kaplan_meier_plot <- downloadHandler(
      filename = function() {
        paste0("kaplan_meier_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)
        plot <- kaplan_meier_plot_f(file_path)$plot
        print(plot)
        dev.off()
      }
    )

  })

}
# 第六类Bee Swarm
bee_swarn <- function(input, output, session){

  # 监控选择的下拉菜单，然后刷新图和下载按钮
  observeEvent(input$bee_swarm_select, {

    file_path <- "./file/Bee_Swarm_Demo.csv"
    plot <- bee_swarm_plot_f(file_path)$plot
    info <- bee_swarm_plot_f(file_path)$info

    # 定义下载按钮的逻辑
    output$download_bee_swarm_demo <- downloadHandler(
      filename = function() {
        paste(input$bee_swarm_select, Sys.Date(), ".csv", sep = "_")
      },
      content = function(file) {
        # 假设源文件已加载到 `data` 中
        data <- read.csv(file_path)
        write.csv(data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    output$bee_swarm_plot <- renderPlot({
      plot
    })

    output$bee_swarm_info  <- renderText({
      info
    })

    output$download_bee_swarm_plot <- downloadHandler(
      filename = function() {
        paste0("bee_swarm_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)
        plot <- bee_swarm_plot_f(file_path)$plot
        print(plot)
        dev.off()
      }
    )

  })

}
# 第七类Restricted Cubic Splines
restricted_cubic_splines <- function(input, output, session){

  # 监控选择的下拉菜单，然后刷新图和下载按钮
  observeEvent(input$restricted_cubic_splines_select, {

    file_path <- "./file/Restricted_Cubic_Splines_Demo.csv"
    plot <- restricted_cubic_splines_plot_f(file_path)$plot
    info <- restricted_cubic_splines_plot_f(file_path)$info

    # 定义下载按钮的逻辑
    output$download_restricted_cubic_splines_demo <- downloadHandler(
      filename = function() {
        paste(input$restricted_cubic_splines_select, Sys.Date(), ".csv", sep = "_")
      },
      content = function(file) {
        # 假设源文件已加载到 `data` 中
        data <- read.csv(file_path)
        write.csv(data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

    output$restricted_cubic_splines_plot <- renderPlot({
      plot
    })

    output$restricted_cubic_splines_info  <- renderText({
      info
    })

    output$download_restricted_cubic_splines_plot <- downloadHandler(
      filename = function() {
        paste0("restricted_cubic_splines_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 6)
        plot <- restricted_cubic_splines_plot_f(file_path)$plot
        print(plot)
        dev.off()
      }
    )

    cat("已重新绘制")

  })

}



observe_upload <- function(input, output, session){

  # 第一类Categorical Variable Frequency Calculation
  observeEvent(input$category_file_upload, {
    if (!is.null(input$category_file_upload)) {

      file.copy(input$category_file_upload$datapath, "load_data.csv", overwrite = TRUE)
      output$category_upload_status <- renderText("File uploaded successfully as 'load_data.csv'.")

      file_path <- "load_data.csv"

      plot <- category_plot(file_path)$plot
      info <- category_plot(file_path)$info

      output$category_variable_plot <- renderPlot({
        plot
      })
      output$category_variable_info  <- renderText({
        info
      })

      output$download_category_plot <- downloadHandler(
        filename = function() {
          paste0("category_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          pdf(file, width = 8, height = 6)
          plot <- category_plot(file_path)$plot
          print(plot)
          dev.off()
        }
      )

      cat("已重新绘制")
    }
  })

  # 第二类T test
  observeEvent(input$ttest_file_upload, {
    if (!is.null(input$ttest_file_upload)) {

      file.copy(input$ttest_file_upload$datapath, "load_data.csv", overwrite = TRUE)
      output$ttest_upload_status <- renderText("File uploaded successfully as 'load_data.csv'.")

      file_path <- "load_data.csv"
      if (input$ttest_select == "T-test"){
        plot <- t_test_plot_f(file_path)$plot
        info <- t_test_plot_f(file_path)$info
      }else{
        plot <- related_samples_test_plot(file_path)$plot
        info <- related_samples_test_plot(file_path)$info
      }

      output$t_test_plot <- renderPlot({
        plot
      })
      output$t_test_info  <- renderText({
        info
      })

      output$download_t_test_plot <- downloadHandler(
        filename = function() {
          paste0("t_test_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          pdf(file, width = 8, height = 6)
          if (input$ttest_select == "T-test"){
            plot <- t_test_plot_f(file_path)$plot
          }else{
            plot <- related_samples_test_plot(file_path)$plot
          }
          print(plot)
          dev.off()
        }
      )

      cat("已重新绘制")
    }
  })

  # 第三类Proportion Test
  observeEvent(input$proportion_file_upload, {
    if (!is.null(input$proportion_file_upload)) {

      file.copy(input$proportion_file_upload$datapath, "load_data.csv", overwrite = TRUE)
      output$proportion_upload_status <- renderText("File uploaded successfully as 'load_data.csv'.")

      file_path <- "load_data.csv"

      plot <- proportion_test_plot_f(file_path)$plot
      info <- proportion_test_plot_f(file_path)$info

      output$proportion_plot <- renderPlot({
        plot
      })
      output$proportion_info  <- renderText({
        info
      })

      output$download_proportion_test_plot <- downloadHandler(
        filename = function() {
          paste0("proportion_test_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          pdf(file, width = 8, height = 6)
          plot <- proportion_test_plot_f(file_path)$plot
          print(plot)
          dev.off()
        }
      )

      cat("已重新绘制")
    }
  })

  # 第四类Machine Learning
  observeEvent(input$machine_learning_file_upload, {
    if (!is.null(input$machine_learning_file_upload)) {

      file.copy(input$machine_learning_file_upload$datapath, "load_data.csv", overwrite = TRUE)
      output$machine_learning_upload_status <- renderText("File uploaded successfully as 'load_data.csv'.")

      file_path <- "load_data.csv"
      if (input$machine_learning_select == "Decision Tree"){
        plot <- decision_tree_plot_f(file_path)$plot
        info <- decision_tree_plot_f(file_path)$info
      }else if (input$machine_learning_select == "Gradient Boosting"){
        plot <- gradient_boosting_plot_f(file_path)$plot
        info <- gradient_boosting_plot_f(file_path)$info
      }else if (input$machine_learning_select == "LightGBM"){
        plot <- lightgbm_plot_f(file_path)$plot
        info <- lightgbm_plot_f(file_path)$info
      }else if (input$machine_learning_select == "Logistic Regression"){
        plot <- logistic_regression_plot_f(file_path)$plot
        info <- logistic_regression_plot_f(file_path)$info
      }else if (input$machine_learning_select == "Random Forest"){
        plot <- random_forest_plot_f(file_path)$plot
        info <- random_forest_plot_f(file_path)$info
      }else if (input$machine_learning_select == "XGboost"){
        plot <- xgboost_plot_f(file_path)$plot
        info <- xgboost_plot_f(file_path)$info
      }

      output$machine_learning_plot <- renderPlot({
        plot
      })
      output$machine_learning_info  <- renderText({
        info
      })

      output$download_machine_learning_plot <- downloadHandler(
        filename = function() {
          paste0("machine_learning_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          pdf(file, width = 8, height = 6)
          if (input$machine_learning_select == "Decision Tree"){
            plot <- decision_tree_plot_f(file_path)$plot
          }else if (input$machine_learning_select == "Gradient Boosting"){
            plot <- gradient_boosting_plot_f(file_path)$plot
          }else if (input$machine_learning_select == "LightGBM"){
            plot <- lightgbm_plot_f(file_path)$plot
          }else if (input$machine_learning_select == "Logistic Regression"){
            plot <- logistic_regression_plot_f(file_path)$plot
          }else if (input$machine_learning_select == "Random Forest"){
            plot <- random_forest_plot_f(file_path)$plot
          }else if (input$machine_learning_select == "XGboost"){
            plot <- xgboost_plot_f(file_path)$plot
          }
          print(plot)
          dev.off()
        }
      )

      cat("已重新绘制")
    }
  })

  # 第五类Kaplan Meier
  observeEvent(input$kaplan_meier_file_upload, {
    if (!is.null(input$kaplan_meier_file_upload)) {

      file.copy(input$kaplan_meier_file_upload$datapath, "load_data.csv", overwrite = TRUE)
      output$kaplan_meier_upload_status <- renderText("File uploaded successfully as 'load_data.csv'.")

      file_path <- "load_data.csv"

      plot <- kaplan_meier_plot_f(file_path)$plot
      info <- kaplan_meier_plot_f(file_path)$info

      output$kaplan_meier_plot <- renderPlot({
        plot
      })
      output$kaplan_meier_info  <- renderText({
        info
      })

      output$download_kaplan_meier_plot <- downloadHandler(
        filename = function() {
          paste0("kaplan_meier_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          pdf(file, width = 8, height = 6)
          plot <- kaplan_meier_plot_f(file_path)$plot
          print(plot)
          dev.off()
        }
      )

      cat("mkaplan_meier_file_upload已重新绘制")
    }
  })

  # 第六类Bee Swarm
  observeEvent(input$bee_swarm_file_upload, {
    if (!is.null(input$bee_swarm_file_upload)) {

      file.copy(input$bee_swarm_file_upload$datapath, "load_data.csv", overwrite = TRUE)
      output$bee_swarm_upload_status <- renderText("File uploaded successfully as 'load_data.csv'.")

      file_path <- "load_data.csv"

      plot <- bee_swarm_plot_f(file_path)$plot
      info <- bee_swarm_plot_f(file_path)$info

      output$bee_swarm_plot <- renderPlot({
        plot
      })
      output$bee_swarm_info  <- renderText({
        info
      })

      output$download_bee_swarm_plot <- downloadHandler(
        filename = function() {
          paste0("bee_swarm_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          pdf(file, width = 8, height = 6)
          plot <- bee_swarm_plot_f(file_path)$plot
          print(plot)
          dev.off()
        }
      )

      cat("已重新绘制")
    }
  })

  # 第七类Restricted Cubic Splines
  observeEvent(input$restricted_cubic_file_upload, {
    if (!is.null(input$restricted_cubic_file_upload)) {

      file.copy(input$restricted_cubic_file_upload$datapath, "load_data.csv", overwrite = TRUE)
      output$restricted_cubic_upload_status <- renderText("File uploaded successfully as 'load_data.csv'.")

      file_path <- "load_data.csv"

      plot <- restricted_cubic_splines_plot_f(file_path)$plot
      info <- restricted_cubic_splines_plot_f(file_path)$info

      output$restricted_cubic_splines_plot <- renderPlot({
        plot
      })
      output$restricted_cubic_splines_info  <- renderText({
        info
      })

      output$download_restricted_cubic_splines_plot <- downloadHandler(
        filename = function() {
          paste0("restricted_cubic_splines_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          pdf(file, width = 8, height = 6)
          plot <- restricted_cubic_splines_plot_f(file_path)$plot
          print(plot)
          dev.off()
        }
      )

      cat("Restricted Cubic Splines已重新绘制")
    }
  })

}

# 测试
# file_path <- "./file/two_sample_T_test_demo.csv"
# print(two_sample_T_test_plot(file_path))
# file_path <- "./file/category_demo.csv"
# print(category_plot(file_path))

