library(shiny)
library(shinythemes)
library(shinyjs)

modUI <- function(id, label = "Input text: ") {
  ns <- NS(id)
  tagList(
    lapply(1:(length(df)-1), function(i) {
      fluidRow(
        column(textInput(ns(paste('id', i, sep = '_')), NULL, i, width = 50),  width = 1),
        column(textInput(ns(paste('ttl', i, sep = '_')), NULL, df[i, 1], width = 1000),  width = 4),
        column(textInput(ns(paste('aut', i, sep = '_')), NULL, df[i, 2]), width = 2),
        column(dateInput(ns(paste('dt', i, sep = '_')), NULL, as.Date(df[i, 3], format = "%d.%m.%Y")),  width = 2),
        column(selectInput(ns(paste('cat', i, sep = '_')), NULL, choices = ch, selected = 2), width = 1),
        column(selectInput(ns(paste('ty', i, sep = '_')), NULL, choices = ch, selected = 1), width = 2),
        # column(actionButton(paste('rm', i, sep = '_'), 'Del', width = 50, class = "btn-danger"),  width = 1),
        
      )
    })
    
  )
}
ch = c(d=1, w=2)


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
      for (j in c(1:(length(df)-1))) {
        tit = paste('ttl', j, sep = '_')
        aut_id = paste('aut', j, sep = '_')
        dt = paste('dt', j, sep = '_')
        cat_id = paste('cat', j, sep = '_')
        t_id = paste('ty', j, sep = '_')
      }
      disable("voridBtn")
      enable("virBtn")
      insertUI('#main', ui=modUI('jjj'))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
