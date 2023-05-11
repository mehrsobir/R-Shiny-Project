testUI <- function(id, df, autch, catch, typch) {
  ns <- NS(id)
  autch = autch %>% add_row(id=0, name='xxx')
  catch = setNames(as.character(catch$id), catch$cat)
  typch = setNames(as.character(typch$id), typch$type)
  autch = setNames(as.character(autch$id), autch$name)
  tagList(
    lapply(1:(length(df$title)), function(i) {
      fluidRow(
        column(textInput(ns(paste('id', i, sep = '_')), NULL, ns(paste('id', i, sep = '_')), width = 500),  width = 5),
        column(textInput(ns(paste('ttl', i, sep = '_')), NULL, df[i, 1], width = 1000),  width = 3),
        column(selectInput(ns(paste('aut', i, sep = '_')), NULL, choices = autch, selected = df[i, 2]), width = 1),
        column(dateInput(ns(paste('dt', i, sep = '_')), NULL, as.Date(df[i, 3], format = "%d.%m.%Y"), min = '2023-01-01', max = '2025-01-01'),  width = 2),
        column(selectInput(ns(paste('cat', i, sep = '_')), NULL, choices = catch, selected = 1), width = 1),
        column(selectInput(ns(paste('ty', i, sep = '_')), NULL, choices = typch, selected = 1), width = 1),
        column(textInput(ns(paste('ttl', i, sep = '_')), NULL, df[i, 6], width = 1000),  width = 2),
      )
    })
  )
}

testServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}