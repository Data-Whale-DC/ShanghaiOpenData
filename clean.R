library(tidyverse)
library(tidylog)
# read 上海统计局 xls. file ---
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

clean_raw <- function(year){
  all_stat <- list.files(path = paste0("Data/",year), pattern = ".xls", full.names = T)
  data <- all_stat %>% 
    map(readxl::read_xls) 
  
  # 处理
  # paste_name <- paste(df[n, ], name)
  
  
  
  cleaned <- data %>% 
    map(clean_stat)
  
  # dataset names
  names(cleaned) <- map_chr(1:length(data),~ names(data[[.x]])[[1]])
  return(cleaned)
}

cleaned_2017 <- clean_raw(2017)
cleaned_2012 <- clean_raw(2012)


# Manual clean
cleaned_2017 %>% 
  openxlsx::write.xlsx(file = "Data/census_2017.xlsx")

cleaned_2012 %>% 
  openxlsx::write.xlsx(file = "Data/census_2012.xlsx")


library(readxl)

clean_final <- function(year){
  list <- excel_sheets(paste0("Data/census_",year,".xlsx"))
  cleaned <- list %>% 
    map(read_xlsx, path = paste0("Data/census_",year,".xlsx"))
  names(cleaned) <- list
  
  cleaned <- cleaned %>% 
    map(~mutate(.x, across(c(-地区), 
                           ~as.numeric(gsub("…", "", .x)))))
}

merge_zhabei <- function(df){
  df %>% 
    mutate(地区 = ifelse(地区 == "闸北区", "静安区", 地区),
             地区 = ifelse(地区 == "崇明县", "崇明区", 地区)) %>% 
    group_by(地区) %>% 
    summarise(across(where(is.numeric), sum, na.rm = T))
}

cleaned <- clean_final(2017)
cleaned_2012 <- clean_final(2012) %>% 
  map(merge_zhabei)

# final
save(cleaned, file = "MapShanghai/data.rda")
save(cleaned_2012, file = "MapShanghai/data_2012.rda")

