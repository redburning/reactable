library(shiny)
library(shinyjs)
library(shinytoastr)
library(RSQLite)
library(reactable)
library(magrittr)

user_base <- function() {
  con <-  RSQLite::dbConnect(RSQLite::SQLite(), "./data/users.db")
  base <- con %>% dplyr::tbl("users") %>% as.data.frame()
  RSQLite::dbDisconnect(con)
  return(base)
}


ui <- fluidPage(
  useShinyjs(),
  hr(),
  br(), br(),
  fluidRow(
    actionButton(inputId = "adduser", label = "新增", icon = icon("plus"), 
                 style = "background-color: #007BFF; 
                          color: #ffffff; border: none; 
                          margin-left: 15px; 
                          margin-bottom: 15px"),
    uiOutput(outputId = "edituser", inline = TRUE),
    uiOutput(outputId = "deleteuser", inline = TRUE)
  ),
  fluidRow(
    column(width = 12, reactableOutput("user_table"))
  )
)



server <- function(input, output, session) {
  user_data <- user_base()
  selected <- reactive(getReactableState("user_table", "selected"))
  output$user_table <- renderReactable({
    options(reactable.language = reactableLang(
      pageSizeOptions = "\u663e\u793a {rows}",
      pageInfo = "{rowStart} \u81f3 {rowEnd} \u9879\u7ed3\u679c,\u5171 {rows} \u9879",
      pagePrevious = "\u4e0a\u9875",
      pageNext = "\u4e0b\u9875"
    ))
    reactable(data = user_data[c("username", "age", "birthday", "address")],
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              filterable = TRUE,
              defaultPageSize = 5,
              showPageSizeOptions = TRUE,
              selection = "multiple",
              onClick = "select",
              defaultColDef = colDef(
                align = "left",
                minWidth = 50
              ),
              columns = list(
                .selection = colDef(
                  width = 80,
                  style = list(cursor = "pointer"),
                  headerStyle = list(cursor = "pointer")
                ),
                address = colDef(minWidth = 140)  # overrides the default
              )
    )
  })
  
  observeEvent(input$adduser, {
    showModal(modalDialog(
      title = "新建用户",
      size = "m",
      fluidPage(
        fluidRow(
          column(width = 12, textInput(inputId = "username", label = "姓名"))
        ),
        fluidRow(
          column(width = 12, textInput(inputId = "age", label = "年龄"))
        ),
        fluidRow(
          column(width = 12, dateInput(inputId = "birthday", label = "出生日期"))
        ),
        fluidRow(
          column(width = 12, textInput(inputId = "address", label = "地址"))
        )
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = "useradd_confirm", label = "确认"),
        modalButton(label = "取消")
      )
    ))
  })
  
  observeEvent(input$useradd_confirm, {
    con <-  RSQLite::dbConnect(RSQLite::SQLite(), "./data/users.db")
    tryCatch({
      addusersql <- paste0("INSERT INTO users (username, age, birthday, address) VALUES ", "('", 
                           input$username, "', '", 
                           input$age, "', '", 
                           input$birthday, "', '", 
                           input$address, "')")
      RSQLite::dbExecute(con, addusersql)
      # refresh table instance
      user_data <<- user_base()
      
      output$user_table <- renderReactable({
        # reactable中文
        options(reactable.language = reactableLang(
          pageSizeOptions = "\u663e\u793a {rows}",
          pageInfo = "{rowStart} \u81f3 {rowEnd} \u9879\u7ed3\u679c,\u5171 {rows} \u9879",
          pagePrevious = "\u4e0a\u9875",
          pageNext = "\u4e0b\u9875"
        ))
        reactable(data = user_data[c("username", "age", "birthday", "address")],
                  bordered = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  filterable = TRUE,
                  defaultPageSize = 5,
                  showPageSizeOptions = TRUE,
                  selection = "multiple",
                  onClick = "select",
                  defaultColDef = colDef(
                    align = "left",
                    minWidth = 50
                  ),
                  columns = list(
                    .selection = colDef(
                      width = 80,
                      style = list(cursor = "pointer"),
                      headerStyle = list(cursor = "pointer")
                    ),
                    address = colDef(minWidth = 140)  # overrides the default
                  )
        )
      })
      toastr_success(title = "保存成功", message = "")
      removeModal()
    },
    warning = function(w) {
      toastr_warning("保存失败", message = w)
    },
    error = function(e) {
      toastr_error("保存失败", message = e)
    },
    finally = {
      RSQLite::dbDisconnect(con)
    })
  })
  
  # 编辑
  output$edituser <- renderUI({
    # 只有选择一项时可以编辑
    if (length(selected()) == 1) {
      actionButton(inputId = "edituser", label = "编辑", icon = icon("edit"),
                   style = "background-color: #007BFF; 
                            color: #ffffff; 
                            border: none; 
                            margin-left: 15px; 
                            margin-bottom: 15px")

    } else {
      shinyjs::disabled(actionButton(inputId = "edituser", label = "编辑", icon = icon("edit"),
                                     style = "background-color: #007BFF; 
                                              color: #ffffff; 
                                              border: none; 
                                              margin-left: 15px; 
                                              margin-bottom: 15px"))
    }
  })
  
  observeEvent(input$edituser, {
    showModal(modalDialog(
      title = "修改信息",
      size = "m",
      fluidPage(
        fluidRow(
          column(width = 12, 
                 textInput(inputId = "username_new", label = "姓名", 
                           value = user_data[selected(), "username"]))
        ),
        fluidRow(
          column(width = 12, 
                 textInput(inputId = "age_new", label = "年龄", 
                           value = user_data[selected(), "age"]))
        ),
        fluidRow(
          column(width = 12, 
                 dateInput(inputId = "birthday_new", label = "出生日期", 
                           value = user_data[selected(), "birthday"]))
        ),
        fluidRow(
          column(width = 12, 
                 textInput(inputId = "address_new", label = "地址", 
                           value = user_data[selected(), "address"]))
        )
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = "useredit_confirm", label = "确认"),
        modalButton(label = "取消")
      )
    ))
  })
  
  observeEvent(input$useredit_confirm, {
    # 更新用户信息
    con <-  RSQLite::dbConnect(RSQLite::SQLite(), "./data/users.db")
    tryCatch({
      edit_standard_user_sql <- paste0("UPDATE users set username=", "'", input$username_new, "'", 
                                       ", age=", "'", input$age_new, "'", 
                                       ", birthday=", "'", input$birthday_new, "'", 
                                       ", address=", "'", input$address_new, "'",
                                       " WHERE id=", "'", user_data[selected(), "id"], "'")
      RSQLite::dbExecute(con, edit_standard_user_sql)
      # refresh table instance
      user_data <<- user_base()
      
      output$user_table <- renderReactable({
        options(reactable.language = reactableLang(
          pageSizeOptions = "\u663e\u793a {rows}",
          pageInfo = "{rowStart} \u81f3 {rowEnd} \u9879\u7ed3\u679c,\u5171 {rows} \u9879",
          pagePrevious = "\u4e0a\u9875",
          pageNext = "\u4e0b\u9875"
        ))
        reactable(data = user_data[c("username", "age", "birthday", "address")],
                  bordered = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  filterable = TRUE,
                  defaultPageSize = 5,
                  showPageSizeOptions = TRUE,
                  selection = "multiple",
                  onClick = "select",
                  defaultColDef = colDef(
                    align = "left",
                    minWidth = 50
                  ),
                  columns = list(
                    .selection = colDef(
                      width = 80,
                      style = list(cursor = "pointer"),
                      headerStyle = list(cursor = "pointer")
                    ),
                    address = colDef(minWidth = 140)  # overrides the default
                  )
        )
      })
      toastr_success(title = "修改成功", message = "")
      removeModal()
    },
    warning = function(w) {
      toastr_warning(title = "修改失败", message = w)
    },
    error = function(e) {
      toastr_error(title = "修改失败", message = e)
    },
    finally = {
      RSQLite::dbDisconnect(con)
    })
  })
  
  # 删除
  output$deleteuser <- renderUI({
    if (length(selected()) > 0) {
      actionButton(inputId = "deleteuser", label = "删除", icon = icon("trash-alt"), 
                   style = "background-color: #DC3545; 
                            color: #ffffff; 
                            border: none; 
                            margin-left: 15px; 
                            margin-bottom: 15px")
    } else {
      shinyjs::disabled(actionButton(inputId = "deleteuser", label = "删除", icon = icon("trash-alt"), 
                                     style = "background-color: #DC3545; 
                                              color: #ffffff; 
                                              border: none; 
                                              margin-left: 15px; 
                                              margin-bottom: 15px"))
    }
  })
  
  observeEvent(input$deleteuser, {
    showModal(modalDialog(
      title = "删除用户",
      size = "m",
      fluidPage(
        div("确定删除用户 ", 
            span(paste(user_data[selected(), "username"], collapse = ", "), 
                 style="padding: .2em .2em; 
                        margin:0; 
                        font-size:85%; 
                        background-color:rgb(175,184,193,20%); 
                        color:#DC3545; 
                        border-radius:2px;"), 
            " 吗 ?")
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = "userdelete_confirm", label = "确认"),
        modalButton(label = "取消")
      )
    ))
  })
  
  observeEvent(input$userdelete_confirm, {
    con <-  RSQLite::dbConnect(RSQLite::SQLite(), "./data/users.db")
    tryCatch({
      deleteSql <- paste0("DELETE FROM users WHERE id=", "'", user_data[selected(), "id"], "'")
      for (sql in deleteSql) {
        RSQLite::dbExecute(con, sql)
      }
      # refresh table instance
      user_data <<- user_base()
      
      output$user_table <- renderReactable({
        options(reactable.language = reactableLang(
          pageSizeOptions = "\u663e\u793a {rows}",
          pageInfo = "{rowStart} \u81f3 {rowEnd} \u9879\u7ed3\u679c,\u5171 {rows} \u9879",
          pagePrevious = "\u4e0a\u9875",
          pageNext = "\u4e0b\u9875"
        ))
        reactable(data = user_data[c("username", "age", "birthday", "address")],
                  bordered = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  filterable = TRUE,
                  defaultPageSize = 5,
                  showPageSizeOptions = TRUE,
                  selection = "multiple",
                  onClick = "select",
                  defaultColDef = colDef(
                    align = "left",
                    minWidth = 50
                  ),
                  columns = list(
                    .selection = colDef(
                      width = 80,
                      style = list(cursor = "pointer"),
                      headerStyle = list(cursor = "pointer")
                    ),
                    address = colDef(minWidth = 140)  # overrides the default
                  )
        )
      })
      toastr_success(title = "删除成功", message = "")
      removeModal()
    },
    warning = function(w) {
      toastr_warning(title = "删除失败", message = w)
    },
    error = function(e) {
      toastr_error(title = "删除失败", message = e)
    },
    finally = {
      RSQLite::dbDisconnect(con)
    })
  })
}

shinyApp(ui = ui, server = server)