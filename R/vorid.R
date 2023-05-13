voridUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      actionButton(ns('virBtn'), 'Вироиш', class = "btn-info"),
      disabled(actionButton(ns('omodBtn'), 'Омодасозӣ', class = "btn-info")),
      disabled(actionButton(ns('saveBtn'), 'Сабт', class = "btn-info")),
      disabled(actionButton(ns('movBtn'), 'Ҳифзи файлҳо', class = "btn-info"))
    ),
    rHandsontableOutput(ns("hottable"))
    
  )
}

voridServer <- function(id, df, autch, catch, typch) {
  dif = setdiff(df$author, autch$name)
  dif = row.names(df)[df$author %in% dif] %>% as.integer(dif)-1
  df2 = reactiveValues(dd = NULL)
  moduleServer(
    id,
    function(input, output, session) {
      output$hottable <- renderRHandsontable({
        rhandsontable(df, row_highlight = dif, colHeaders = c("Ном","Муаллиф","Сана", "Бахш", "Мавод", "Файл"), 
                      width = 2000, overflow = "visible") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, width = 12) %>% 
          hot_cols(colWidths = c(500, 200, 100, 200, 200, 250), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             if (instance.params) {
                       hrows = instance.params.row_highlight
                       hrows = hrows instanceof Array ? hrows : [hrows]
                       hcols = 1
                       hcols = hcols instanceof Array ? hcols : [hcols]

                       if (hcols.includes(col) && hrows.includes(row)) {
                          td.style.background = 'lightgrey';
                        }
                     }
           }") %>%
          hot_col(col = "Муаллиф", type = "dropdown", source = autch$name, strict = FALSE, allowInvalid = TRUE) %>%
          hot_col(col = "Бахш", type = "dropdown", source = catch$cat, strict = FALSE, allowInvalid = TRUE) %>%
          hot_col(col = "Мавод", type = "dropdown", source = typch$type, strict = FALSE, allowInvalid = TRUE)
      })
      observe({
        df2$dd = hot_to_r(input$hottable)
      })
      observeEvent(input$virBtn,{
        dif = setdiff(df2$dd$author, autch$name)
        dif = row.names(df2$dd)[df2$dd$author %in% dif] %>% as.integer(dif)-1
        if(length(dif)>0){
          showModal(modalDialog(
            title = 'ДАР СУТУНИ МУАЛЛИФ ХАТО ВУҶУД ДОРАД!!',
            h1('Хаторо истоҳ кунед!!'),
            footer = modalButton('Dismiss')
            ))
        }else{
        output$hottable <- renderRHandsontable({
          rhandsontable(df2$dd, row_highlight = dif, colHeaders = c("Ном","Муаллиф","Сана", "Бахш", "Мавод", "Файл"), 
                        width = 2000, overflow = "visible", readOnly = TRUE) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, width = 12) %>% 
            hot_cols(colWidths = c(500, 200, 100, 200, 200, 250))
        })
        disable("virBtn")
        enable("omodBtn")
        }
        
      })
      
      observeEvent(input$omodBtn, {
        for (i in c(1:length(df2$dd$author))) {
          
          for (j in c(1:length(autch$name))) {
            if(df2$dd$author[i] == autch$name[j]){
              df2$dd$author[i] = autch$id[j]
            }
          }
          for (j in c(1:length(typch$type))) {
            if(df2$dd$typ[i] == typch$type[j]){
              df2$dd$typ[i] = typch$id[j]
            }
          }
          for (j in c(1:length(catch$cat))) {
            if(df2$dd$cat[i] == catch$cat[j]){
              df2$dd$cat[i] = catch$id[j]
            }
          }
          
          df2$dd$pub_date[i] = as.character(as.Date(df2$dd$pub_date[i], tryFormats = c("%d.%m.%Y","%d/%m/%Y", "%d-%m-%Y", "%d,%m,%Y")))
        }
         
        print(df2$dd)
        disable("omodBtn")
        enable("saveBtn")
      })
      
      observeEvent(input$saveBtn, {
        con <- dbConnect(odbc::odbc(), "PostgreSQL35W", timeout = 10)
          dbWriteTable(con, "works22", df2$dd, append = TRUE)
        dbDisconnect(con)
        x = df2$dd[1,]
        print(x)
        showModal(modalDialog(
          title = 'tesr', 
          textInput('t', 'title', x$title),
          textInput('a', 'author', x$author),
          
          footer = modalButton('Dismiss')))
        disable("saveBtn")
        enable("movBtn")
      })
      observeEvent(input$saveBtn, {
        for (i in df2$dd$link) {
          file.move(
            paste0("C:/Users/mehr1/Downloads/1/1/", i),
            "C:/Users/mehr1/Downloads/1/"
          )
        }
        x = df2$dd[1,]
        print(x)
        showModal(modalDialog(
          title = 'tesr', 
          textInput('t', 'title', x$title),
          textInput('a', 'author', x$author),
          
          footer = modalButton('Dismiss')))
        disable("movBtn")
      })
    }
  )
}