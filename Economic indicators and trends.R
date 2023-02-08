
library(httr) # API import
library(rjstat) # For JSON text conversion/API import
library(tidyverse) #dplyr, ggplot, you name it
library(klassR) #Import of standardized industries, occupations
library(janitor) # scrub, and clean the objects
library(zoo) # for the conversion of a year and quarter to date/class
library(rstudioapi) # for getting file path
library(data.table) #for fast if else
library(rvest) # for HTML-webscraping




# Main economic indicators. Accounts and forecasts 1991 - 2025

url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/12880/



                      ", "\n"))
# 
data.tmp <- '
{
  "query": [],
  "response": {
    "format": "json-stat2"
  }
}
'
d.tmp <- POST(url , body = data.tmp, encode = "json", verbose())
macro_ds <- fromJSONstat(content(d.tmp, "text"))
macro_ds <- clean_names(macro_ds)
macro_ds <- tibble(macro_ds) 

#This series of if statements converts years, quarters and months into dates using zoo
if("month" %in% colnames(macro_ds)) {
  macro_ds<- separate(macro_ds, month, into = c("year", "month"), sep = "M")
  macro_ds$date  <- paste0(macro_ds$year,"-", macro_ds$month) 
  macro_ds$date <- as.Date(as.yearmon(macro_ds$date), frac = 1)
}else{ if("quarter" %in% colnames(macro_ds)) {
  macro_ds<- separate(macro_ds, quarter, into = c("year", "quarter"), sep = "K")
  macro_ds$date  <- paste0(macro_ds$year," Q", macro_ds$quarter) 
  macro_ds$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",macro_ds$date)),frac = 1)
}else{ macro_ds$date  <- paste0(macro_ds$year," Q4") 
macro_ds$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",macro_ds$date)),frac = 1)
}
}


macro_ds <- macro_ds %>%
  select(date, everything())
macro_ds_expand <- pivot_wider(macro_ds, 
                                   id_expand = FALSE,
                                   names_from = contents,
                                   values_from = c("value"))
macro_ds_expand <- macro_ds_expand %>%
  clean_names()


