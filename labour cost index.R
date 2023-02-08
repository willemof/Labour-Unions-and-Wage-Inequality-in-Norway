
library(httr) # API import
library(rjstat) # For JSON text conversion/API import
library(tidyverse) #dplyr, ggplot, you name it
library(klassR) #Import of standardized industries, occupations
library(janitor) # scrub, and clean the objects
library(zoo) # for the conversion of a year and quarter to date/class
library(rstudioapi) # for getting file path
library(data.table) #for fast if else
library(rvest) # for HTML-webscraping




#labour cost index

url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/07251/

                      
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
labour_costs <- fromJSONstat(content(d.tmp, "text"))
labour_costs <- clean_names(labour_costs)
labour_costs <- tibble(labour_costs) 

if("month" %in% colnames(labour_costs)) {
  labour_costs<- separate(labour_costs, month, into = c("year", "month"), sep = "M")
  labour_costs$date  <- paste0(labour_costs$year,"-", labour_costs$month) 
  labour_costs$date <- as.Date(as.yearmon(labour_costs$date), frac = 1)
}else{ if("quarter" %in% colnames(labour_costs)) {
  labour_costs<- separate(labour_costs, quarter, into = c("year", "quarter"), sep = "K")
  labour_costs$date  <- paste0(labour_costs$year," Q", labour_costs$quarter) 
  labour_costs$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",labour_costs$date)),frac = 1)
      }
    }



labour_costs <- labour_costs %>%
  select(date, everything())
labour_costs_expand <- pivot_wider(labour_costs, 
                              id_expand = FALSE,
                              names_from = contents,
                              values_from = c("value"))
labour_costs_expand <- labour_costs.expand %>%
  clean_names()

filter_labour_costs <- labour_costs_expand %>% 
  filter(year==2010) %>%
  filter(industry_sic2007 == unique(industry_sic2007)[2])

t_costs <- labour_costs_expand %>% 
  filter("Aquaculture" %in% unique(industry_sic2007))

remove(d.tmp)
