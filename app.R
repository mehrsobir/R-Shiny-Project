library(shiny)
library(shinythemes)
data("iris")
ch = c(d=1, w=2)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  navbarPage("МОРМОИ",
    # Application title
    tabPanel("Сабти корҳо",
      #sidebarPanel(tags$h1("")),
    
      mainPanel(
        lapply(1:(length(df)-1), function(i) {
          fluidRow(
            column(textInput(paste('id', i, sep = '_'), NULL, i, width = 50),  width = 1),
            column(textInput(paste('ttl', i, sep = '_'), NULL, df[i, 1], width = 1000),  width = 4),
            column(textInput(paste('aut', i, sep = '_'), NULL, df[i, 2]), width = 2),
            column(dateInput(paste('dt', i, sep = '_'), NULL, as.Date(df[i, 3], format = "%d.%m.%Y")),  width = 2),
            column(selectInput(paste('cat', i, sep = '_'), NULL, choices = ch, selected = 2), width = 1),
            column(selectInput(paste('ty', i, sep = '_'), NULL, choices = ch, selected = 1), width = 2),
            # column(actionButton(paste('rm', i, sep = '_'), 'Del', width = 50, class = "btn-danger"),  width = 1),
            
          )
        }),
        actionButton('tbBtn', 'Save', class = "btn-info"),
        
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
    observeEvent(input$tbBtn,{
      for (j in c(1:(length(df)-1))) {
        tit = paste('ttl', j, sep = '_')
        aut_id = paste('aut', j, sep = '_')
        dt = paste('dt', j, sep = '_')
        cat_id = paste('cat', j, sep = '_')
        t_id = paste('ty', j, sep = '_')
        print(input[[aut_id]])
        renderUI()
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
