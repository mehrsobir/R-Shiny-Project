library(shiny)
library(shinythemes)
library(shinyjs)
library(DBI)
library(officer)
library(tidyverse)
library(RecordLinkage)


modUI <- function(id, df, autch, catch, typch) {
  ns <- NS(id)
  autch = autch %>% add_row(id=0, name='xxx')
  catch = setNames(as.character(catch$id), catch$cat)
  typch = setNames(as.character(typch$id), typch$type)
  autch = setNames(as.character(autch$id), autch$name)
  
  
  tagList(
    lapply(1:(length(df)-1), function(i) {
      fluidRow(
        column(textInput(ns(paste('id', i, sep = '_')), NULL, i, width = 50),  width = 1),
        column(textInput(ns(paste('ttl', i, sep = '_')), NULL, df[i, 1], width = 1000),  width = 4),
        column(selectInput(ns(paste('aut', i, sep = '_')), NULL, choices = autch, selected = df[i, 2]), width = 1),
        column(dateInput(ns(paste('dt', i, sep = '_')), NULL, as.Date(df[i, 3], format = "%d.%m.%Y")),  width = 2),
        column(selectInput(ns(paste('cat', i, sep = '_')), NULL, choices = catch, selected = 1), width = 1),
        column(selectInput(ns(paste('ty', i, sep = '_')), NULL, choices = typch, selected = 1), width = 2),
        # column(actionButton(paste('rm', i, sep = '_'), 'Del', width = 50, class = "btn-danger"),  width = 1),
        
      )
    })
    
  )
}



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"), useShinyjs(),  
  navbarPage("МОРМОИ",
    # Application title
    tabPanel("Сабти корҳо",
      #sidebarPanel(tags$h1("")),
    
      mainPanel(
        actionButton('voridBtn', 'Воридкунӣ', class = "btn-info"),
        disabled(actionButton('virBtn', 'Вироиш', class = "btn-info")),
        disabled(actionButton('omodBtn', 'Омодасозӣ', class = "btn-info")),
        disabled(actionButton('saveBtn', 'Сабт', class = "btn-info")),
        disabled(actionButton('movBtn', 'Ҳифзи файлҳо', class = "btn-info")),
        disabled(actionButton('reBtn', 'Азнав', class = "btn-info")),hr(),
        div(
          id = 'main'
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
      

      aa = list.files('C:/Users/mehr1/Downloads/1/1')
      aa = aa[substr(aa, 1, 2) != '~$']
      bb = aa[substr(aa, nchar(aa)-3, nchar(aa)) == '.doc']
      aa = aa[substr(aa, nchar(aa)-4, nchar(aa)) == '.docx']
      df = data.frame(title=character(0), author=integer(0), pub_date=character(0),
                      cat=integer(0), typ=integer(0))
      
      for (i in aa) {
        doc = read_docx(paste('C:/Users/mehr1/Downloads/1/1/', i, sep = ''))
        doc = docx_summary(doc)$text
        doc = doc[doc != ""]
        author = 0
        for(i in  c(1:length(staf$id))){
          if((levenshteinSim(staf$name[i], trimws(doc[length(doc)-1])) > 0.6)
             | (levenshteinSim(staf$sname[i], trimws(doc[length(doc)-1])) > 0.6 )){
            author = staf$id[i]
          }
          
        }
        df = df %>% add_row(title=trimws(doc[1]), author=author, pub_date=trimws(doc[length(doc)]),
                            cat=NA, typ=NA)
        
        
      }
      # 
      # ss = DT::datatable(cat %>% arrange(id), selection = 'none', editable = TRUE,
      #               extensions = 'Buttons',
      #               options = list(
      #                 dom = 'Blrtip',
      #                 buttons = c('copy', 'pdf', 'print')
      #               )
      # )
      
      disable("voridBtn")
      enable("virBtn")
      insertUI('#main', ui=modUI('sss', df, staf, cat, typ))
    })
    
    observeEvent(input$virBtn,{
      for (j in c(1:(length(df)-1))) {
        tit = paste('ttl', j, sep = '_')
        aut_id = paste('aut', j, sep = '_')
        dt = paste('dt', j, sep = '_')
        cat_id = paste('cat', j, sep = '_')
        t_id = paste('ty', j, sep = '_')
      }
      disable("virBtn")
      enable("virBtn")
      insertUI('#main', ui=modUI('jjj'))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
