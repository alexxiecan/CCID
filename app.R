library(shiny)
library(shinyjs)
library(bs4Dash)
library(DT)
library(DBI)
library(RPostgres)
library(rms)
library(ggplot2)
#library(future)
#library(promises)
library(shinycssloaders)
library(DT)

source('./plotFunction.R')
source('./databaseFunction.R')

options(shiny.maxRequestSize = 50 * 1024^2)
pdf(file = NULL)

# 重定向标准输出和标准错误流到日志文件
# log_file <- "/var/log/shiny-server/app_output.log"
# sink(log_file, append = TRUE, type = "output")  # 捕获正常输出
# sink(log_file, append = TRUE, type = "message")  # 捕获错误输出
# sink("/srv/shiny-server/demo/app_output.txt")  # 捕获错误输出

# 数据库连接函数
connect_to_db <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname = "ICUDB_NEW",          # 数据库名称
    host = "*******",          # 数据库主机名或IP
    port = 5432,                 # PostgreSQL 端口号
    user = "*******",      # 替换为你的 PostgreSQL 用户名
    password = "******"   # 替换为你的 PostgreSQL 密码
  )
}

# 每页显示的行数
rows_per_page <- 10

# 查询表 public.bicarbonate_inhospital 的行数
total_pages <- function() {
  conn <- connect_to_db()
  on.exit(dbDisconnect(conn))  # 确保在查询结束后断开连接

  query <- "SELECT COUNT(*) FROM public.Demographics_TRANS"
  result <- dbGetQuery(conn, query)

  return(ceiling(result[1, 1] / 10))  # 返回计数值
}

# cat("================", total_pages())

# UI 部分
ui <- bs4DashPage(
  dashboardHeader(

    # tags$head(
    #   tags$style(HTML("
    #     /* 隐藏右上角的问号 */
    #     .fa-question {
    #       display: none;
    #     }
    #
    #     /* 隐藏开关控件（例如主题切换按钮、用户菜单等） */
    #     .navbar-toggler, .dropdown-toggle {
    #       display: none;
    #     }
    #   "))
    # ),

    title = tagList(
      tags$a(
        href = "http://123.130.112.90:60011/",  # 替换为你需要跳转的链接
        target = "_blank",                 # 新标签页打开
        tags$img(src = "logo.png", height = "100px")
      )
      # tags$hr(style = "border-top: 1px solid #D3D3D3; margin: 5px 0;")
    ),
    titleWidth = 250,
    rightUi = tags$li(  # 必须包裹在<li>中
      class = "dropdown",  # 必须包含dropdown类
      tags$a(
        href = "http://123.130.112.90:60011/",
        target = "_blank",  # 新标签页打开链接
        class = "nav-link",  # 使用bs4Dash的导航链接样式
        icon("home"),  # 主页图标
        "HOME"  # 按钮文字
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Introduction", tabName = "Introduction", icon = icon("home")),
      menuItem("Table Show", tabName = "table_show", icon = icon("table")),

      menuItem("Frequency Calculation", tabName = "category_tab", icon = icon("chart-line")),
      menuItem("T Test", tabName = "t_test_tab", icon = icon("chart-line")),
      menuItem("Proportion Test", tabName = "proportion_test_tab", icon = icon("chart-line")),
      menuItem("Machine Learning", tabName = "machine_learning_tab", icon = icon("chart-line")),
      menuItem("Kaplan Meier", tabName = "kaplan_meier_tab", icon = icon("chart-line")),
      menuItem("Bee Swarm", tabName = "bee_swarm_tab", icon = icon("chart-line")),
      menuItem("Restricted Cubic Splines", tabName = "restricted_cubic_splines_tab", icon = icon("chart-line"))

    )
  ),
  dashboardBody(

    useShinyjs(),  # 启用 shinyjs
    tags$head(
      tags$style(HTML(
        ".loader_huang {
          position: fixed;
          top: 50%;
          left: 50%;
          z-index: 9999;
          display: none;
          animation: spin 2s linear infinite;
        }
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }"
      ))
    ),

    # 加载图标
    div(id = "loading", class = "loader_huang",
        tags$img(src = "loading.png", height = "150px")
    ),

    tabItems(

      # 主页
      tabItem(
        tabName = "Introduction",
        h2("Welcome to the Dashboard")
      ),

      # 展示表格
      tabItem(
        tabName = "table_show",
        h2("Table Show"),

        fluidRow(
          column(3, textInput("fields_input", "Fields to Display", "")),  # 新增输入框，输入要展示的字段
          column(3, textInput("search_id", "Search by Patient ID", "")),
          column(3, selectInput("database_select", "Choose Table", choices = c("Demographics","Diagnosis","Surgical_History","Examinations","APACHEII","SOFA","Medical_Orders","Laboratory","AST"))),
          column(2, selectInput("transfer_select", "Tranfer Department", choices = c("Yes", "No"))),
          column(1, actionButton("query_btn", "Search", icon = icon("search")))
        ),

        # DTOutput("dataTable"),
        # 使用 withSpinner 包裹 DTOutput
        shinycssloaders::withSpinner(DTOutput("dataTable"), type = 6, color = "#0275D8", size = 1, caption = "Loading data..."),
        
        fluidRow(
          #column(6,
                 #fluidRow(
                  # column(2, numericInput("min_value", NULL, value = 1, min = 1)),
                  # column(2, numericInput("max_value", NULL, value = 10, min = 1)),
                  # column(4, downloadButton("download_data", "Download")),
                  # column(4, downloadButton("download_data_search", "Download Search"))
                 #)
                #),
          column(6, offset = 3,  # 居中 column(6)
                 fluidRow(
                   column(3, actionButton("prevPage", "prevPage", icon = icon("arrow-left"))),
                   column(6,
                          fluidRow(
                            column(6, numericInput("pageInput", NULL, value = 1, min = 1, max = total_pages(), width = "80px")),
                            column(6, textOutput("pageInfo"))
                          )
                   ),
                   column(3, actionButton("nextPage", "nextPage", icon = icon("arrow-right")))
                 )
                )
                )
        ),

      # 第一类Categorical Variable Frequency Calculation
      tabItem(
        tabName = "category_tab",
        h2("Categorical Variable Frequency Calculation"),
        p("Please download the demo.csv file first, then upload the demo.csv file, and the program will automatically draw and display it below. You can also organize your data in demo.csv file format and upload file drawings."),

        fluidRow(
          column(3, selectInput("category_select", NULL, choices = c("Frequency Calculation"))),
          column(3, downloadButton("download_category_demo", "Download Demo CSV")),  # 新增输入框，输入要展示的字段
          column(3, fileInput(
            inputId = "category_file_upload",
            label = NULL,
            accept = c(".csv")  # 限制上传文件的格式为 CSV
          )),
          column(3, textOutput("category_upload_status"))
        ),

        # plotOutput("category_variable_plot", height = "500px"),  # 显示绘图
        shinycssloaders::withSpinner(plotOutput("category_variable_plot", height = "500px"), type = 6, color = '#0275D8', size = 1, caption = 'please wait...'),
        downloadButton("download_category_plot", "Download PDF"),
        textOutput("category_variable_info")
      ),

      # 第二类T test
      tabItem(
        tabName = "t_test_tab",
        h2("T Test"),
        p("Please download the demo.csv file first, then upload the demo.csv file, and the program will automatically draw and display it below. You can also organize your data in demo.csv file format and upload file drawings."),

        fluidRow(
          column(3, selectInput("ttest_select", NULL, choices = c("T-test", "Related samples test"))),
          column(3, downloadButton("download_ttest_demo", "Download Demo CSV")),  # 新增输入框，输入要展示的字段
          column(3, fileInput(
            inputId = "ttest_file_upload",
            label = NULL,
            accept = c(".csv")  # 限制上传文件的格式为 CSV
          )),
          column(3, textOutput("ttest_upload_status"))
        ),

        # plotOutput("t_test_plot", height = "500px"),  # 显示绘图
        shinycssloaders::withSpinner(plotOutput("t_test_plot", height = "500px"), type = 6, color = '#0275D8', size = 1, caption = 'please wait...'),
        downloadButton("download_t_test_plot", "Download PDF"),
        textOutput("t_test_info")
      ),

      # 第三类Proportion Test
      tabItem(
        tabName = "proportion_test_tab",
        h2("Proportion Test"),
        p("Please download the demo.csv file first, then upload the demo.csv file, and the program will automatically draw and display it below. You can also organize your data in demo.csv file format and upload file drawings."),

        fluidRow(
          column(3, selectInput("proportion_test_select", NULL, choices = c("Proportion Test"))),
          column(3, downloadButton("download_proportion_demo", "Download Demo CSV")),  # 新增输入框，输入要展示的字段
          column(3, fileInput(
            inputId = "proportion_file_upload",
            label = NULL,
            accept = c(".csv")  # 限制上传文件的格式为 CSV
          )),
          column(3, textOutput("proportion_upload_status"))
        ),

        # plotOutput("proportion_test_plot", height = "500px"),  # 显示绘图
        shinycssloaders::withSpinner(plotOutput("proportion_test_plot", height = "500px"), type = 6, color = '#0275D8', size = 1, caption = 'please wait...'),
        downloadButton("download_proportion_test_plot", "Download PDF"),
        textOutput("proportion_test_info")
      ),

      # 第四类Machine Learning
      tabItem(
        tabName = "machine_learning_tab",
        h2("Machine Learning"),
        p("Please download the demo.csv file first, then upload the demo.csv file, and the program will automatically draw and display it below. You can also organize your data in demo.csv file format and upload file drawings."),

        fluidRow(
          column(3, selectInput("machine_learning_select", NULL, choices = c("Decision Tree", "Gradient Boosting", "LightGBM", "Logistic Regression", "Random Forest", "XGboost"))),
          column(3, downloadButton("download_machine_learning_demo", "Download Demo CSV")),  # 新增输入框，输入要展示的字段
          column(3, fileInput(
            inputId = "machine_learning_file_upload",
            label = NULL,
            accept = c(".csv")  # 限制上传文件的格式为 CSV
          )),
          column(3, textOutput("machine_learning_upload_status"))
        ),

        # plotOutput("machine_learning_plot", height = "500px"),  # 显示绘图
        shinycssloaders::withSpinner(plotOutput("machine_learning_plot", height = "500px"), type = 6, color = '#0275D8', size = 1, caption = 'please wait...'),
        downloadButton("download_machine_learning_plot", "Download PDF"),
        textOutput("machine_learning_info")
      ),

      # 第五类Kaplan Meier
      tabItem(
        tabName = "kaplan_meier_tab",
        h2("Kaplan Meier"),
        p("Please download the demo.csv file first, then upload the demo.csv file, and the program will automatically draw and display it below. You can also organize your data in demo.csv file format and upload file drawings."),

        fluidRow(
          column(3, selectInput("kaplan_meier_select", NULL, choices = c("Kaplan Meier"))),
          column(3, downloadButton("download_kaplan_meier_demo", "Download Demo CSV")),  # 新增输入框，输入要展示的字段
          column(3, fileInput(
            inputId = "kaplan_meier_file_upload",
            label = NULL,
            accept = c(".csv")  # 限制上传文件的格式为 CSV
          )),
          column(3, textOutput("kaplan_meier_upload_status"))
        ),

        #plotOutput("kaplan_meier_plot", height = "500px"),  # 显示绘图
        shinycssloaders::withSpinner(plotOutput("kaplan_meier_plot", height = "500px"), type = 6, color = '#0275D8', size = 1, caption = 'please wait...'),
        downloadButton("download_kaplan_meier_plot", "Download PDF"),
        textOutput("kaplan_meier_info")
      ),

      # 第六类Bee Swarm
      tabItem(
        tabName = "bee_swarm_tab",
        h2("Bee Swarm"),
        p("Please download the demo.csv file first, then upload the demo.csv file, and the program will automatically draw and display it below. You can also organize your data in demo.csv file format and upload file drawings."),

        fluidRow(
          column(3, selectInput("bee_swarm_select", NULL, choices = c("Bee Swarm"))),
          column(3, downloadButton("download_bee_swarm_demo", "Download Demo CSV")),  # 新增输入框，输入要展示的字段
          column(3, fileInput(
            inputId = "bee_swarm_file_upload",
            label = NULL,
            accept = c(".csv")  # 限制上传文件的格式为 CSV
          )),
          column(3, textOutput("bee_swarm_upload_status"))
        ),


        # plotOutput("bee_swarm_plot", height = "500px"),  # 显示绘图
        shinycssloaders::withSpinner(plotOutput("bee_swarm_plot", height = "500px"), type = 6, color = '#0275D8', size = 1, caption = 'please wait...'),
        downloadButton("download_bee_swarm_plot", "Download PDF"),
        textOutput("bee_swarm_info")
      ),

      # 第七类Restricted Cubic Splines
      tabItem(
        tabName = "restricted_cubic_splines_tab",
        h2("Restricted Cubic Splines"),
        p("Please download the demo.csv file first, then upload the demo.csv file, and the program will automatically draw and display it below. You can also organize your data in demo.csv file format and upload file drawings."),

        fluidRow(
          column(3, selectInput("restricted_cubic_splines_select", NULL, choices = c("Restricted Cubic Splines"))),
          column(3, downloadButton("download_restricted_cubic_splines_demo", "Download Demo CSV")),  # 新增输入框，输入要展示的字段
          column(3, fileInput(
            inputId = "restricted_cubic_file_upload",
            label = NULL,
            accept = c(".csv")  # 限制上传文件的格式为 CSV
          )),
          column(3, textOutput("restricted_cubic_upload_status"))
        ),

        # plotOutput("restricted_cubic_splines_plot", height = "500px"),  # 显示绘图
        shinycssloaders::withSpinner(plotOutput("restricted_cubic_splines_plot", height = "500px"), type = 6, color = '#0275D8', size = 1, caption = 'please wait...'),
        downloadButton("download_restricted_cubic_splines_plot", "Download PDF"),
        textOutput("restricted_cubic_splines_info")
      )

    )
  )
)


# Server 部分
server <- function(input, output, session) {

  # 表格展示功能
  table_show(input, output, session, connect_to_db)
  # 表格下载功能
  # download_data_botton(input, output, session, connect_to_db)
  # 表格搜索功能
  search_change(input, output, session, connect_to_db)

  # # 第一类Categorical Variable Frequency Calculation
   category(input, output, session)
  # # 第二类T test
   t_test(input, output, session)
  # # 第三类Proportion Test
   proportion_test(input, output, session)
  # # 第四类Machine Learning
   machine_learning(input, output, session)
  # # 第五类Kaplan Meier
   kaplan_meier(input, output, session)
  # # 第六类Bee Swarm
   bee_swarn(input, output, session)
  # # 第七类Restricted Cubic Splines
   restricted_cubic_splines(input, output, session)
  #
  # # 算法Demo文件下载功能
   observe_upload(input, output, session)

}
# sink()  # 关闭标准输出
shinyApp(ui, server)
