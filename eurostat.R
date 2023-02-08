#Eurostat query test

library(tidyverse)
library(httr)
url <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/namq_10_gdp?format=JSON"
getresponse <- GET(url)
getresponse

json <- content(getresponse, as = "text")
substr(json, 1, 100)

df <- fromJSONstat(json)
