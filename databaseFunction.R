library(shiny)
library(shinyjs)
library(bs4Dash)
library(DT)
library(DBI)
library(RPostgres)
library(rms)
library(ggplot2)

# 数据库连接函数
connect_to_db <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname = "ICUDB_NEW",          # 数据库名称
    host = "*****",          # 数据库主机名或IP
    port = 5432,                 # PostgreSQL 端口号
    user = "*****",      # 替换为你的 PostgreSQL 用户名
    password = "*****"   # 替换为你的 PostgreSQL 密码
  )
}

rows_per_page <- 10

database_name_f <- function(input){

  if (input$database_select != "Examinations" && input$database_select != "APACHEII" && input$database_select != "SOFA"){
    if (input$transfer_select == "Yes"){
      database_name <- paste(input$database_select, "TRANS", sep = "_")
    }else {
      database_name <- paste(input$database_select, "NON_TRANS", sep = "_")
    }
    return (database_name)
  }else {
    return (input$database_select)
  }
}



show_data <- function(input, output, session, connect_to_db, pageIndex){

  database_name <- database_name_f(input)

  conn <- connect_to_db()
  on.exit(dbDisconnect(conn))

  # 根据输入的搜索条件拼接查询
  if (input$search_id != "") {
    search_id_input <- input$search_id
    search_ids <- unlist(strsplit(search_id_input, ","))
    search_ids <- paste("'", search_ids, "'", sep = "")
    condition <- paste(" AND patient_id IN ", "(", paste(search_ids, collapse = ","), ")", sep = "")
  }else{
    condition <- ""
  }

  # 如果用户输入了要展示的字段，则修改查询语句，只选择这些字段
  if (input$fields_input != "") {
    fields <- input$fields_input
  }else{
    fields <- "*"
  }

  start_row <- (pageIndex - 1) * rows_per_page
  query <- paste("SELECT", fields, "FROM public.", database_name, " WHERE 1=1", condition, "ORDER BY patient_id", "LIMIT", rows_per_page, "OFFSET", start_row, sep = " ")
  cat("Generated SQL Query: ", query, "\n")

  query_result <- NULL
  err_result <- tryCatch({
    query_result <- dbGetQuery(conn, query)
    "查询成功"
  }, error = function(e) {
    message("Error: ", e$message)
    query_result <- NULL
    "查询失败，请重新查询"
  })


  if (is.null(query_result) || nrow(query_result) == 0){
    output$dataTable <- renderDT({
      datatable(query_result, options = list(

        dom = 'Bfrtip',  # 启用按钮区域
        buttons = c('csv'),  # 启用CSV和Excel下载按钮

        language = list(
          info = ""
        ),
        scrollX = TRUE,  # 启用水平滚动
        #scrollY = "400px",  # 设置垂直滚动的最大高度
        paging = FALSE,   # 取消默认的分页，使用自己的分页按钮
        searching = FALSE  # 关闭默认的搜索框
      ), rownames = FALSE)
    })
    output$pageInfo <- renderText({
      "0 / 0"
    })
  } else {
    end_row <- min(pageIndex * rows_per_page, start_row + nrow(query_result))

    query_result <- cbind(RowNumber = seq(start_row + 1, end_row), query_result)


    output$dataTable <- renderDT({
      datatable(query_result, options = list(

        dom = 'Bfrtip',  # 启用按钮区域
        buttons = c('csv'),  # 启用CSV和Excel下载按钮

        language = list(
          info = ""
        ),
        scrollX = TRUE,  # 启用水平滚动
        #scrollY = "400px",  # 设置垂直滚动的最大高度
        paging = FALSE,   # 取消默认的分页，使用自己的分页按钮
        searching = FALSE  # 关闭默认的搜索框
      ), rownames = FALSE)
    })

    conn <- connect_to_db()
    on.exit(dbDisconnect(conn))
    query <- paste("SELECT COUNT(*) FROM public.", database_name, " WHERE 1=1", condition, sep = "")

    err_result <- tryCatch({
      result <- dbGetQuery(conn, query)
      "查询成功"
    }, error = function(e) {
      message("Error: ", e$message)
      "查询失败，请重新查询"
    })

    row_count <- result[1, 1]
    total_pages <- (ceiling(row_count / 10))

    # 在 UI 中动态显示页码信息
    output$pageInfo <- renderText({
      paste(pageIndex, " / ", total_pages, sep = "")
    })
  }
}

search_change <- function(input, output, session, connect_to_db){


  observeEvent(input$query_btn, {

    show_data(input, output, session, connect_to_db, 1)

  })

}

total_pages_search <- function(input, output, session, connect_to_db){

  database_name <- database_name_f(input)

  if (input$search_id != "") {
    search_id_input <- input$search_id
    search_ids <- unlist(strsplit(search_id_input, ","))  # 按逗号分割
    search_ids <- paste("'", search_ids, "'", sep = "")   # 添加单引号
    condition <- paste(" AND patient_id IN ", "(", paste(search_ids, collapse = ","), ")", sep = "")
  }else{
    condition <- ""
  }
  conn <- connect_to_db()
  on.exit(dbDisconnect(conn))
  query <- paste("SELECT COUNT(*) FROM public.", database_name, " WHERE 1=1", condition, sep = "")


  err_result <- tryCatch({
    result <- dbGetQuery(conn, query)
    row_count <- result[1, 1]
    return(ceiling(row_count / 10))
    "页码查询成功"
  }, error = function(e) {
    message("Error: ", e$message)
    return (0)
    "页码查询失败，请重新查询"
  })

}



download_data_botton <- function(input, output, session, connect_to_db){

  download_datas <- function(){

    database_name <- database_name_f(input)

    # 输入值检验
    min <- input$min_value
    max <- input$max_value

    if (min < 0) {
      min <- abs(min)
      updateNumericInput(session, "min_value", value = min)
    }

    if (min > max) {
      temp <- min
      min <- max
      max <- temp
      updateNumericInput(session, "min_value", value = min)
      updateNumericInput(session, "max_value", value = max)
    }

    conn <- connect_to_db()
    on.exit(dbDisconnect(conn))
    query <- paste("SELECT * FROM public.", database_name, " WHERE 1=1", "ORDER BY patient_id", "LIMIT", (max-min+1), "OFFSET", (min-1), sep = " ")

    query_result <- NULL
    err_result <- tryCatch({
      query_result <- dbGetQuery(conn, query)
    }, error = function(e) {
      message("Error: ", e$message)
      query_result <- NULL
      "查询失败，请重新查询"
    })

    cat("Generated SQL Query: ", query, "\n")

    return(query_result)
  }

  download_datas_search <- function(){

    database_name <- database_name_f(input)

    conn <- connect_to_db()
    on.exit(dbDisconnect(conn))

    # 根据输入的搜索条件拼接查询
    if (input$search_id != "") {
      search_id_input <- input$search_id
      search_ids <- unlist(strsplit(search_id_input, ","))
      search_ids <- paste("'", search_ids, "'", sep = "")
      condition <- paste(" AND patient_id IN ", "(", paste(search_ids, collapse = ","), ")", sep = "")
    }else{
      condition <- ""
    }

    # 如果用户输入了要展示的字段，则修改查询语句，只选择这些字段
    if (input$fields_input != "") {
      fields <- input$fields_input
    }else{
      fields <- "*"
    }

    query <- paste("SELECT", fields, "FROM public.", database_name, " WHERE 1=1", condition, "ORDER BY patient_id", "LIMIT 10000", sep = " ")
    cat("Generated SQL Query: ", query, "\n")

    query_result <- NULL
    err_result <- tryCatch({
      query_result <- dbGetQuery(conn, query)
      "查询成功"
    }, error = function(e) {
      message("Error: ", e$message)
      query_result <- NULL
      "查询失败，请重新查询"
    })

    return (query_result)

  }

  # 使用 downloadHandler 函数处理文件下载
  output$download_data <- downloadHandler(
    filename = function() {
      # 获取数据库名称和当前时间生成文件名
      database_name <- database_name_f(input)
      paste(database_name, format(Sys.time(), "%Y-%m-%d-%H-%M"), ".csv", sep = "_")
    },
    content = function(file) {
      # 获取查询结果，并写入 CSV 文件
      write.csv(download_datas(), file, row.names = FALSE)
    }
  )

  # 使用 downloadHandler 函数处理文件下载
  output$download_data_search <- downloadHandler(
    filename = function() {
      # 获取数据库名称和当前时间生成文件名
      database_name <- database_name_f(input)
      paste("Search", database_name, format(Sys.time(), "%Y-%m-%d-%H-%M"), ".csv", sep = "_")
    },
    content = function(file) {
      # 获取查询结果，并写入 CSV 文件
      write.csv(download_datas_search(), file, row.names = FALSE)
    }
  )

}

table_show <- function(input, output, session, connect_to_db){

  # 监听上一页按钮
  observeEvent(input$prevPage, {
    total_pages <- total_pages_search(input, output, session, connect_to_db)
    if (input$pageInput > 1) {
      pages <- input$pageInput
      if (input$pageInput > total_pages) {
        pages <- total_pages
      }
      new_page <- pages - 1
      updateNumericInput(session, "pageInput", value = new_page)
    }
  })

  # 监听下一页按钮
  observeEvent(input$nextPage, {
    total_pages <- total_pages_search(input, output, session, connect_to_db)
    # 最大值范围未限定
    if (input$pageInput < total_pages) {
      new_page <- input$pageInput + 1
      updateNumericInput(session, "pageInput", value = new_page)
    }
  })

  # 动态更新页码输入框的最大值
  observeEvent(input$pageInput, {
    if (input$pageInput == "" || !grepl("^[0-9]+$", input$pageInput)) {
      pageIndex <- 1  # 如果不是有效自然数，则将页码设置为 1
    }else{
      total_pages <- total_pages_search(input, output, session, connect_to_db)

      if (input$pageInput < 1) {
        pageIndex <- 1
      } else if (input$pageInput > total_pages) {
        pageIndex <- total_pages
      } else{
        pageIndex <- input$pageInput
      }

    }
    # updateNumericInput(session, "pageInput", value = pageIndex)
    show_data(input, output, session, connect_to_db, pageIndex)
  })

}

