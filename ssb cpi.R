#consumer price index

options(encoding="UTF-8")
library(httr)
library(rjstat) #For JSON
library(tidyverse)
library(janitor)
library(dplyr)
library(zoo)
url <- "https://data.ssb.no/api/v0/en/table/03013/"
# 
data.tmp <- '
{
  "query": [
    {
      "code": "Konsumgrp",
      "selection": {
        "filter": "vs:CoiCop2016niva1",
        "values": [
          "TOTAL"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
d.tmp <- POST(url , body = data.tmp, encode = "json", verbose())
ssb.cpi <- fromJSONstat(content(d.tmp, "text"))
ssb.cpi <- clean_names(ssb.cpi)


ssb.cpi <- tibble(ssb.cpi) 

ssb.cpi<- separate(ssb.cpi, month, into = c("year", "month"), sep = "M")
ssb.cpi$date  <- paste0(ssb.cpi$year,"-", ssb.cpi$month) 

ssb.cpi$date <- as.Date(as.yearmon(ssb.cpi$date), frac = 1)

ssb.cpi <- ssb.cpi %>%
  select(date, everything())
ssb.cpi.expand <- pivot_wider(ssb.cpi, 
                                id_expand = FALSE,
                                names_from = contents,
                                values_from = c("value"))
ssb.cpi.expand <- ssb.cpi.expand %>%
  clean_names()

remove(d.tmp)
