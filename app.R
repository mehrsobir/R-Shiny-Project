library(shiny)
library(shinythemes)
library(shinyjs)
library(DBI)
library(officer)
library(tidyverse)
library(RecordLinkage)
library(rhandsontable)


vorid <- function(id){
  aa = list.files('C:/Users/mehr1/Downloads/1/1')
  aa = aa[substr(aa, 1, 2) != '~$']
  bb = aa[substr(aa, nchar(aa)-3, nchar(aa)) == '.doc']
  aa = aa[substr(aa, nchar(aa)-4, nchar(aa)) == '.docx']
  
  df = data.frame(title=character(0), author=character(0), pub_date=character(0),
                  cat=character(0), typ=character(0), link=character(0))
  
  for (i in aa) {
    doc = read_docx(paste('C:/Users/mehr1/Downloads/1/1/', i, sep = ''))
    doc = docx_summary(doc)$text
    doc = doc[doc != ""]
    link = paste0(str_replace_all(now(), ':', ''), '.docx')
    file.move(
      paste0("C:/Users/mehr1/Downloads/1/1/", i),
      paste0("C:/Users/mehr1/Downloads/1/1/", link)
    )
    author = trimws(doc[length(doc)-1])
    for(i in  c(1:length(staf$id))){
      if((levenshteinSim(staf$name[i], trimws(doc[length(doc)-1])) > 0.8)
         | (levenshteinSim(staf$sname[i], trimws(doc[length(doc)-1])) > 0.8 )){
        author = staf$name[i]
      }
      
    }
    # df = rbind(df, data.frame(title=trimws(doc[1]), author=author, pub_date=trimws(doc[length(doc)]), cat=NA, typ=NA, link=link))
    df = df %>% add_row(title=trimws(doc[1]), author=author, pub_date=trimws(doc[length(doc)]),
                        cat="Дастгирии сиёсат", typ="Мақола", link=link)
  }
  return(df)
}

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"), useShinyjs(),  
  navbarPage("МОРМОИ",
    # Application title
    tabPanel("Сабти корҳо",
      mainPanel(
        uiOutput(
          outputId = 'main',
        ),
        width = 12
        )
      ),
   )
)

# Define  logic required to draw a histogram
server <- function(input, output) {
  output$main <- renderUI(actionButton('voridBtn', 'Воридкунӣ', class = "btn-info"))
    observeEvent(input$voridBtn,{
      con <- dbConnect(odbc::odbc(), "PostgreSQL35W", timeout = 10)
        cat = dbReadTable(con, 'cats')
        typ = dbReadTable(con, 'types')
        staf = dbReadTable(con, 'stuff') %>% select(id, name, sname)
      dbDisconnect(con)
      
      df = vorid('d')
      nn = reactiveValues(data = df)
      voridServer('sss', nn$data, staf, cat, typ)
      output$main <- renderUI(voridUI('sss'))
    })
    
}

# Run the application 
# shinyApp(ui = ui, server = server)
runGadget(ui, server, viewer = dialogViewer("МОРМОИ", width = 2200, height = 1600))
