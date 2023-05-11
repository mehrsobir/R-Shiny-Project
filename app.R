library(shiny)
library(shinythemes)
library(shinyjs)
library(DBI)
library(officer)
library(tidyverse)
library(RecordLinkage)
library(rhandsontable)


modUI <- function(id, df) {
  ns <- NS(id)
  # autch = autch %>% add_row(id=0, name='xxx')
  # catch = setNames(as.character(catch$id), catch$cat)
  # typch = setNames(as.character(typch$id), typch$type)
  # autch = setNames(as.character(autch$id), autch$name)
  
  tagList(
    rhandsontable(df, width = 600, height = 300) %>%
      hot_col("author", allowInvalid = TRUE)
    
  )
}

vorid <- function(id, aut){
  aa = list.files('C:/Users/mehr1/Downloads/1/1')
  aa = aa[substr(aa, 1, 2) != '~$']
  bb = aa[substr(aa, nchar(aa)-3, nchar(aa)) == '.doc']
  aa = aa[substr(aa, nchar(aa)-4, nchar(aa)) == '.docx']
  df = data.frame(title=character(0), author=factor(aut$id, levels = aut$name), pub_date=character(0),
                  cat=integer(0), typ=integer(0), link=character(0))
  
  for (i in aa) {
    doc = read_docx(paste('C:/Users/mehr1/Downloads/1/1/', i, sep = ''))
    doc = docx_summary(doc)$text
    doc = doc[doc != ""]
    link = paste0(str_replace_all(now(), ':', ''), '.docx')
    file.move(
      paste0("C:/Users/mehr1/Downloads/1/1/", i),
      paste0("C:/Users/mehr1/Downloads/1/1/", link)
    )
    author = 0
    for(i in  c(1:length(staf$id))){
      if((levenshteinSim(staf$name[i], trimws(doc[length(doc)-1])) > 0.6)
         | (levenshteinSim(staf$sname[i], trimws(doc[length(doc)-1])) > 0.6 )){
        author = staf$id[i]
      }
      
    }
    # df = rbind(df, data.frame(title=trimws(doc[1]), author=author, pub_date=trimws(doc[length(doc)]), cat=NA, typ=NA, link=link))
    df = df %>% add_row(title=trimws(doc[1]), author=author, pub_date=trimws(doc[length(doc)]),
                        cat=NA, typ=NA, link=link)
  }
  print(typeof(df))
  return(df)
}

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"), useShinyjs(),  
  navbarPage("МОРМОИ",
    # Application title
    tabPanel("Сабти корҳо",
      #sidebarPanel(tags$h1("")),
    
      mainPanel(
        wellPanel(
          actionButton('voridBtn', 'Воридкунӣ', class = "btn-info"),
          disabled(actionButton('virBtn', 'Вироиш', class = "btn-info")),
          disabled(actionButton('omodBtn', 'Омодасозӣ', class = "btn-info")),
          disabled(actionButton('saveBtn', 'Сабт', class = "btn-info")),
          disabled(actionButton('movBtn', 'Ҳифзи файлҳо', class = "btn-info")),
          disabled(actionButton('reBtn', 'Азнав', class = "btn-info"))
        ),
        testUI('d', df, staf, cat, typ),
        uiOutput(
          outputId = 'main'
        ),
        
        width = 12
        
        )
      ),
    tabPanel("Ҳисобот",
             sidebarPanel(
               tags$h1("ttt")
             ),
             mainPanel(
               textInput("txt", "Text input:", "text here"),
               DT::dataTableOutput('tab1'), width = 12
               
             )
    )
    
  )
)

# Define  logic required to draw a histogram
server <- function(input, output) {

    output$tab1 <- DT::renderDataTable({
        DT::datatable(df, selection = 'none', editable = TRUE,
                      extensions = 'Buttons',
                      options = list(
                        dom = 'Blrtip',
                        buttons = c('copy', 'pdf', 'print')
                      )
                      )
    })
    observeEvent(input$voridBtn,{
      con <- dbConnect(odbc::odbc(), "PostgreSQL35W", timeout = 10)
        cat = dbReadTable(con, 'cats')
        typ = dbReadTable(con, 'types')
        staf = dbReadTable(con, 'stuff') %>% select(id, name, sname)
      dbDisconnect(con)
      
      df = vorid('d', staf)
      

      
      disable("voridBtn")
      enable("virBtn")
      output$main <- renderUI(modUI('sss', df))
    })
    
    observeEvent(input$virBtn,{
      for (j in c(1:(length(df$title)))) {
        tit = paste('ttl', j, sep = '_')
        aut_id = paste('aut', j, sep = '_')
        dt = paste('dt', j, sep = '_')
        cat_id = paste('cat', j, sep = '_')
        t_id = paste('ty', j, sep = '_')
        
      }
      qq = input$id_1
      print(qq)
      disable("virBtn")
      enable("saveBtn")
      output$main <- renderUI(
        DT::datatable(cat %>% arrange(id), selection = 'none', editable = TRUE,
                           extensions = 'Buttons',
                           options = list(
                             dom = 'Blrtip',
                             buttons = c('copy', 'pdf', 'print')
                           )
        ) 
      )
    })
    observeEvent(input$saveBtn, {
      x = df[1,]
      print(x)
      showModal(modalDialog(
        title = 'tesr', 
        textInput('t', 'title', x$title),
        textInput('a', 'author', x$author),
        
        footer = modalButton('Dismiss')))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
