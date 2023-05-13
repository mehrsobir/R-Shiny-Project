# # v <- gl(3, 0, labels = c("Tampa", "Seattle","Boston"))
# # print(v)
# 
# df <- data.frame(aut = factor(0, levels=staf$name),
#                  LETTERS= factor(0, levels=LETTERS[1:10]),
#                 
#                  stringsAsFactors = FALSE)
# print(df)
# 
# rhandsontable(df, useTypes = TRUE, width = 800,overflow = "visible")

# DF = data.frame( big = staf$name[1:10])
# DF <- setNames(DF, c("Ном","Муаллиф","cc", "Бахш", "dd", "dd"))
# 
# rhandsontable(DF) %>%
#   hot_col(col = "Ном", type = "dropdown", source = staf$name)
# 
# print(DF)



# output$tab1 <- DT::renderDataTable({
#   DT::datatable(df, selection = 'none', editable = TRUE,
#                 extensions = 'Buttons',
#                 options = list(          ,"Муаллиф","cc", "Бахш", "dd", "dd"
#                   dom = 'Blrtip',
#                   buttons = c('copy', 'pdf', 'print')
#                 )
#   )
# })

# x <- c(1,2,3,4)
# y <- c(2,3,4)
# setdiff(x,y)
modUI <- function(id, df) {
  ns <- NS(id)
  # autch = autch %>% add_row(id=0, name='xxx')
  # catch = setNames(as.character(catch$id), catch$cat)
  # typch = setNames(as.character(typch$id), typch$type)
  # autch = setNames(as.character(autch$id), autch$name)
  
  tagList(
    rhandsontable(df, width = 600, height = 300)
    
  )
}

DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

col_highlight = 2
row_highlight = c(5, 7)

rhandsontable(DF, col_highlight = col_highlight, 
              row_highlight = row_highlight,
              width = 550, height = 300)%>%
  hot_cols(renderer = "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      
      tbl = this.HTMLWidgets.widgets[0]

      hcols = tbl.params.col_highlight
      hcols = hcols instanceof Array ? hcols : [hcols] 
      hrows = tbl.params.row_highlight
      hrows = hrows instanceof Array ? hrows : [hrows] 

      if (hcols.includes(col) && hrows.includes(row)) {
        td.style.background = 'red';
      }
      
      
      return td;
  }")