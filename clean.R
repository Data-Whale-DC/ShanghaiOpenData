library(tidyverse)
library(tidylog)
# read 上海统计局 xls. file ---
all_stat <- list.files(path = "Data", pattern = ".xls", full.names = T)
df <- all_stat %>% 
  map(readxl::read_xls) 

# 处理
clean_stat <- function(df){
  # skip 1, merge row 2 and 3 to column names
  names(df) <- paste(str_remove_all(names(df)," "), df[1, ], sep = "")
  df[-1,]
}



