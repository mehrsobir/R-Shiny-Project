library(officer)
library(tidyverse)
# library(filesstrings)

aa = list.files('C:/Users/mehr1/Downloads/1/1')
aa = aa[substr(aa, 1, 2) != '~$']
bb = aa[substr(aa, nchar(aa)-3, nchar(aa)) == '.doc']
aa = aa[substr(aa, nchar(aa)-4, nchar(aa)) == '.docx']
# columns = c("title","author","date") 
# df = data.frame(matrix(nrow = 0, ncol = length(columns))) 

# colnames(df) = columns
df = data.frame(title=character(0), author=character(0), pub_date=character(0),
                cat=character(0), typ=character(0))

for (i in aa) {
  doc = read_docx(paste('C:/Users/mehr1/Downloads/1/1/', i, sep = ''))
  doc = docx_summary(doc)$text
  doc = doc[doc != ""]
  df = df %>% add_row(title=trimws(doc[1]), author=trimws(doc[length(doc)-1]), pub_date=trimws(doc[length(doc)]),
                      cat=NA, typ=NA)
 
  file.move(
    paste0("C:/Users/mehr1/Downloads/1/1/", i),
    paste0("C:/Users/mehr1/Downloads/1/1/", str_replace_all(now(), ':', ''), '.docx')
  )
  
  # file.copy(from = paste("C:/Users/mehr1/Downloads/1/1/", i, sep = ''),
  #           to   = paste0("C:/Users/mehr1/Downloads/1/", now(), ".docx"))
  # file.remove(paste("C:/Users/mehr1/Downloads/1/", i, sep = ''))
  
}


df
