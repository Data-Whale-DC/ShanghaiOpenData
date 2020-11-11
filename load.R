
# API ==========
# datawhale1
# 1755Mass

# terminal: 成功，but return null data

# curl -X POST https://data.sh.gov.cn/interface/2071 \
# -H 'content-type: application/json' \
# -H 'cache-control: no-cache' \
# -H 'token: 78ec6c3c64163770f827ea9e96aa962d' \
# -d '{ 	"id":1, 	"limit":20, 	"offset":0 }'

# httr: fail, status code not 200
library(httr)
library(jsonlite)

url <- paste0( "https://data.sh.gov.cn/interface/2071")

response <- httr::POST(
  url = url,
  content_type_json(),
  body = list(
    id = "1",
    limit = 20,
    offset = 0
  ),
  add_headers(.headers = c(token = Sys.getenv("SODA"),
                           `cache-control` = "no-cache")))

response$request
toJSON(fromJSON(content(response, as = "text")), pretty = TRUE)

# read data ====
library(tidyverse)
emerg <- read_csv("Data/2016全市医疗机构区域分科门急诊情况.csv", 
                      locale = locale(encoding = "GBK"),
                      col_names = F, 
                      skip = 1
) %>% 
  mutate(X1 = ifelse(is.na(X1),"name",X1)) %>% 
  janitor::row_to_names(row_number = 1) %>%
  mutate(across(-name, ~as.numeric(.)))

save(emerg, file = "MapShanghai/emerg.rda")
# Map data ===
# from http://datav.aliyun.com/

library(geojsonsf)
# shanghai <- geojsonsf::geojson_sf(url("https://geo.datav.aliyun.com/areas_v2/bound/310000_full.json"))
# save(shanghai, file = "shanghai.rda")

load("MapShanghai/data.rda")
load("MapShanghai/shanghai.rda")

data <- cleaned$`表2.2 各区土地面积、常住人口及人口密度（2017）` %>% 
  rename(name = 地区) %>% 
  mutate(across(-name, as.numeric))

map_data <- shanghai %>% 
  left_join(data, by= "name")

# ggplot2 method -----
library(ggplot2)
library(plotly)

create_map <- function(col){
  col <- enquo(col)
  map <- ggplot(map_data, aes(fill = !!col, text = name))+
    geom_sf()+
    ggthemes::theme_map()+
    theme(text = element_text(family = "Hei"))
  
  ggplotly(map)
}
 
create_map(`人口密度（人/平方公里）`)

# tmap method -----
library(tmap)

# Sys.setlocale(category="LC_ALL",locale="en_US.UTF-8")
tm_shape(shanghai)+
  tm_polygons(col = "name")+
  tm_layout(frame = F, fontfamily = "Hei")

# SANDBOX ===
# https://gadm.org/download_country_v3.html

# raster::getData('GADM',country="CHN",level=1)
library(sf)

level <- 1 # choose between 0 - 3
map_url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_CHN_",
                  level,"_sf.rds")

map_china <- readRDS(url(map_url))
save(map_china_l3, file = "map_china.rda")




