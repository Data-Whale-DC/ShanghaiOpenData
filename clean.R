library(tidyverse)
library(tidylog)
# read 上海统计局 xls. file ---
all_stat <- list.files(path = "Data", pattern = ".xls", full.names = T)
data <- all_stat %>% 
  map(readxl::read_xls) 

# 处理
paste_name <- paste(df[n, ], name)

clean_stat <- function(df){
  # find which row starts with 全市 or 总计
  n <- grep("全|总",df[,1] %>% pull())-1
  # n <- ifelse(is.na(n), 3)
  
  names(df) <- df[1:n, ] %>%
    split(1:n) %>% 
    reduce(paste, sep = " ") %>% 
    str_remove_all("NA") %>% 
    str_remove_all(" ")
  
  df[-(1:n), ]
}

cleaned <- data %>% 
  map(clean_stat)

# dataset names
names(cleaned) <- map_chr(1:length(data),~ names(data[[.x]])[[1]])

# Manual clean
cleaned %>% 
  openxlsx::write.xlsx(file = "Data/census.xlsx")

library(readxl)

list <- excel_sheets("Data/census.xlsx")
cleaned <- list %>% 
  map(read_xlsx, path = "Data/census.xlsx")
names(cleaned) <- list

cleaned <- cleaned %>% 
  map(~mutate(.x, across(c(-地区), 
                         ~as.numeric(gsub("…", "", .x)))))

# final
save(cleaned, file = "MapShanghai/data.rda")

