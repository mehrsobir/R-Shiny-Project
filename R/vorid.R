vorid <- function(staf){
  aa = list.files('C:/Users/mehr1/Downloads/1/1')
  aa = aa[substr(aa, 1, 2) != '~$']
  bb = aa[substr(aa, nchar(aa)-3, nchar(aa)) == '.doc']
  aa = aa[substr(aa, nchar(aa)-4, nchar(aa)) == '.docx']
  
  df = data.frame(topic=character(0), stuff_id=character(0), pub_date=character(0),
                  cats_id=character(0), types_id=character(0), link=character(0))
  
  for (i in aa) {
    doc = read_docx(paste('C:/Users/mehr1/Downloads/1/1/', i, sep = ''))
    doc = docx_summary(doc)$text
    doc = doc[doc != ""]
    link = paste0(str_replace_all(now(), ':', ''), '.docx')
    file.move(
      paste0("C:/Users/mehr1/Downloads/1/1/", i),
      paste0("C:/Users/mehr1/Downloads/1/1/", link)
    )
    stuff_id = trimws(doc[length(doc)-1])
    for(i in  c(1:length(staf$id))){
      if((levenshteinSim(staf$name[i], trimws(doc[length(doc)-1])) > 0.8)
         | (levenshteinSim(staf$sname[i], trimws(doc[length(doc)-1])) > 0.8 )){
        stuff_id = staf$name[i]
      }
      
    }
    
    df = df %>% add_row(topic=trimws(doc[1]), stuff_id=stuff_id, pub_date=trimws(doc[length(doc)]),
                        cats_id="Дастгирии сиёсат", types_id="Мақола", link=link)
  }
  return(df)
}

vorid_tbl <- function(df, staf, cats, types) {
  dif = setdiff(df$stuff_id, staf$name)
  dif = row.names(df)[df$stuff_id %in% dif] %>% as.integer(dif)-1
  date_dif = row.names(df)[is.na(parse_date_time(df$pub_date, orders="dmy"))] %>% as.integer(dif)-1
  tbl = rhandsontable(df, row_highlight = dif, row_date_highlight = date_dif, colHeaders = c("Ном","Муаллиф","Сана", "Бахш", "Мавод", "Файл"), 
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
                 
                 hrowsdate = instance.params.row_date_highlight
                 hrowsdate = hrowsdate instanceof Array ? hrowsdate : [hrowsdate]
                 hcolsdate = 2
                 hcolsdate = hcolsdate instanceof Array ? hcolsdate : [hcolsdate]

                 if (hcols.includes(col) && hrows.includes(row)) {
                    td.style.background = 'lightgrey';
                 }
                  if (hcolsdate.includes(col) && hrowsdate.includes(row)) {
                    td.style.background = 'lightgrey';
                  }
               }
     }") %>%
    hot_col(col = "Муаллиф", type = "dropdown", source = staf$name, strict = FALSE, allowInvalid = TRUE) %>%
    hot_col(col = "Бахш", type = "dropdown", source = cats$cat, strict = FALSE, allowInvalid = TRUE) %>%
    hot_col(col = "Мавод", type = "dropdown", source = types$type, strict = FALSE, allowInvalid = TRUE)
  
    return(tbl)
  }

clean_data <- function(df, staf, types, cats){
  for (i in c(1:length(df$stuff_id))) {
    
    for (j in c(1:length(staf$name))) {
      if(df$stuff_id[i] == staf$name[j]){
        df$stuff_id[i] = staf$id[j]
      }
    }
    for (j in c(1:length(types$type))) {
      if(df$types_id[i] == types$type[j]){
        df$types_id[i] = types$id[j]
      }
    }
    for (j in c(1:length(cats$cat))) {
      if(df$cats_id[i] == cats$cat[j]){
        df$cats_id[i] = cats$id[j]
      }
    }
    
    df$pub_date[i] = as.character(as.Date(df$pub_date[i], tryFormats = c("%d.%m.%Y","%d/%m/%Y", "%d-%m-%Y", "%d,%m,%Y")))
  }
  return(df)
}



      