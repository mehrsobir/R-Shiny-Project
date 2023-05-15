library(shiny)
library(shinythemes)
library(shinyjs)
library(DBI)
library(officer)
library(tidyverse)
library(rhandsontable)

ui <- fluidPage(theme = shinytheme("cerulean"), 
                useShinyjs(),  
  navbarPage("МОРМОИ",
    tabPanel("Сабти корҳо",
       wellPanel(
         actionButton('voridBtn', 'Воридкунӣ', class = "btn-info"),
         disabled(actionButton('omodBtn', 'Омодасозӣ', class = "btn-info")),
         disabled(actionButton('saveBtn', 'Сабт', class = "btn-info")),
         disabled(actionButton('movBtn', 'Ҳифзи файлҳо', class = "btn-info"))
       ),       
      mainPanel(
          rHandsontableOutput("voridtable")
        ),
        width = 12
        )
      ),
   )


server <- function(input, output) {
    con <- dbConnect(odbc::odbc(), "PostgreSQL35W", timeout = 10)
      cat = dbReadTable(con, 'cats')
      typ = dbReadTable(con, 'types')
      staf = dbReadTable(con, 'stuff') %>% select(id, name, sname)
    dbDisconnect(con)
  
    observeEvent(input$voridBtn,{
      df = vorid(staf)
      output$voridtable <- renderRHandsontable({
        vorid_tbl(df, staf, cat, typ)
      })
      if(length(df$topic)){
        disable("voridBtn")
        enable("omodBtn")
      }
    })
    
    observeEvent(input$voridtable, {
      df = hot_to_r(input$voridtable)
      output$voridtable <- renderRHandsontable({
        vorid_tbl(df, staf, cat, typ)
      })
    })
    
    observeEvent(input$omodBtn, {
      df = hot_to_r(input$voridtable)
      dif = setdiff(df$author, staf$name)
      difdt = df$pub_date[is.na(parse_date_time(df$pub_date,orders="dmy"))]
      if(length(dif) == 0 && length(difdt) == 0){
        df = clean_data(df, staf, typ, cat)
        output$voridtable <- renderRHandsontable({
          vorid_tbl(df, staf, cat, typ)
        })
        disable("omodBtn")
        enable("saveBtn")
      } else{
        output$voridtable <- renderRHandsontable({
          vorid_tbl(df, staf, cat, typ)
        })
      }
      
    })
    
    observeEvent(input$saveBtn, {
      df = hot_to_r(input$voridtable)
      con <- dbConnect(odbc::odbc(), "PostgreSQL35W", timeout = 10)
        dbWriteTable(con, "works", df, append = TRUE)
      dbDisconnect(con)
      disable("saveBtn")
      enable("movBtn")
    })
    
    observeEvent(input$movBtn, {
      df = hot_to_r(input$voridtable)
      for (i in df$link) {
        file.move(
          paste0("C:/Users/mehr1/Downloads/1/1/", i),
          paste0("E:/top/mavod/", i)
        )
      }
      showModal(modalDialog(
        title = 'Success',
        h1("Done"),
        footer = modalButton('Dismiss')
      ))
      disable("movBtn")
    })
    
}

# Run the application 
# shinyApp(ui = ui, server = server)
runGadget(ui, server, viewer = dialogViewer("МОРМОИ", width = 2200, height = 1600))
