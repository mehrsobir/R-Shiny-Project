# library(officer)
# library(tidyverse)

aa = list.files('C:/Users/User/Downloads/1/1')

# columns = c("title","author","date") 
# df = data.frame(matrix(nrow = 0, ncol = length(columns))) 

# colnames(df) = columns
df = data.frame(title=character(0), author=character(0), pub_date=character(0),
                cat=character(0), typ=character(0))

df
for (i in aa) {
  doc = read_docx(paste('C:/Users/User/Downloads/1/1/', i, sep = ''))
  doc = docx_summary(doc)$text
  doc = doc[doc != ""]
  doc[1]
  df = df %>% add_row(title=doc[1], author=doc[length(doc)-1], pub_date=doc[length(doc)],
                      cat=NA, typ=NA)
}


df
