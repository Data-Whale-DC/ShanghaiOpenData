
# API ==========
# datawhale1
# 1755Mass

# terminal: 成功，but return null data

# curl -X POST https://data.sh.gov.cn/interface/2071 \
# -H 'content-type: application/json' \
# -H 'cache-control: no-cache' \
# -H 'token: 78ec6c3c64163770f827ea9e96aa962d' \
# -d '{ 	"id":"avg_income", 	"limit":20, 	"offset":0 }'

# httr: fail, status code not 200
library(httr)
library(jsonlite)

url <- paste0( "https://data.sh.gov.cn/interface/2273")

response <- httr::POST(
  url = url,
  content_type_json(),
  body = list(
    id = "avg_expenses",
    limit = 20,
    offset = 0
  ),
  add_headers(.headers = c(token = Sys.getenv("SODA"),
                           `cache-control` = "no-cache")))

response$request
toJSON(fromJSON(content(response, as = "text")), pretty = TRUE)


# Map data ===

# from http://datav.aliyun.com/

library(geojsonsf)
shanghai <- geojsonsf::geojson_sf(url("https://geo.datav.aliyun.com/areas_v2/bound/310000_full.json"))
library(ggplot2)

ggplot(shanghai, aes(fill = name))+
  geom_sf()+
  theme_classic()


# SANDBOX ===
# https://gadm.org/download_country_v3.html

# raster::getData('GADM',country="CHN",level=1)
library(sf)
library(tmap)
level <- 1 # choose between 0 - 3
map_url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_CHN_",
                  level,"_sf.rds")

map_china <- readRDS(url(map_url))
save(map_china_l3, file = "map_china.rda")

