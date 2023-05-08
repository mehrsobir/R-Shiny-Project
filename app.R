library(shiny)
library(shinythemes)
data("iris")
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  navbarPage("МОРМОИ",
    # Application title
    tabPanel("Сабти корҳо",
      #sidebarPanel(tags$h1("")),
    
      mainPanel(
        DT::dataTableOutput('tab1'), width = 12
        )
      ),
    tabPanel("Ҳисобот",
             sidebarPanel(
               tags$h1("ttt")
             ),
             mainPanel(
               textInput("txt", "Text input:", "text here"),
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
    observeEvent(
      input$df_cell_edit, {print}
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
