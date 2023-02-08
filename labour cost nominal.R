
library(httr) # API import
library(rjstat) # For JSON text conversion/API import
library(tidyverse) #dplyr, ggplot, you name it
library(klassR) #Import of standardized industries, occupations
library(janitor) # scrub, and clean the objects
library(zoo) # for the conversion of a year and quarter to date/class
library(rstudioapi) # for getting file path
library(data.table) #for fast if else
library(rvest) # for HTML-webscraping




# Average labour costs per full-time equivalent employee, by industry (SIC2007) 

url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/07685/
                      
                      ", "\n"))
# 
data.tmp <- '
{
  "query": [
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACE225hovedomr",
        "values": [
          "B_",
          "C",
          "D",
          "E",
          "F",
          "G_",
          "H_",
          "I_",
          "J",
          "K_",
          "L",
          "M",
          "N",
          "O",
          "P",
          "Q",
          "R",
          "S_"
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
labour_costs <- fromJSONstat(content(d.tmp, "text"))
labour_costs <- clean_names(labour_costs)
labour_costs <- tibble(labour_costs) 

#This series of if statements converts years, quarters and months into dates using zoo
if("month" %in% colnames(labour_costs)) {
  labour_costs<- separate(labour_costs, month, into = c("year", "month"), sep = "M")
  labour_costs$date  <- paste0(labour_costs$year,"-", labour_costs$month) 
  labour_costs$date <- as.Date(as.yearmon(labour_costs$date), frac = 1)
}else{ if("quarter" %in% colnames(labour_costs)) {
  labour_costs<- separate(labour_costs, quarter, into = c("year", "quarter"), sep = "K")
  labour_costs$date  <- paste0(labour_costs$year," Q", labour_costs$quarter) 
  labour_costs$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",labour_costs$date)),frac = 1)
}else{ labour_costs$date  <- paste0(labour_costs$year," Q4") 
labour_costs$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",labour_costs$date)),frac = 1)
  }
}


labour_costs <- labour_costs %>%
  select(date, everything())
labour_costs_expand <- pivot_wider(labour_costs, 
                                   id_expand = FALSE,
                                   names_from = contents,
                                   values_from = c("value"))
labour_costs_expand <- labour_costs_expand %>%
  clean_names()


