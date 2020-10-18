
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


