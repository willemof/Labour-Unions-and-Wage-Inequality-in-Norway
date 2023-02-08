
library(httr) # API import
library(rjstat) # For JSON text conversion/API import
library(tidyverse) #dplyr, ggplot, you name it
library(klassR) #Import of standardized industries, occupations
library(janitor) # scrub, and clean the objects
library(zoo) # for the conversion of a year and quarter to date/class
library(rstudioapi) # for getting file path
library(data.table) #for fast if else
library(rvest) # for HTML-webscraping




# Employers' associations. Establishments and persons employed per 31 December by contents, employers' associations, work related issues and year

url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/03532/


                      ", "\n"))
# 
data.tmp <- '
{
  "query": [
    {
      "code": "ArbForhold",
      "selection": {
        "filter": "item",
        "values": [
          "00",
          "01"
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
employer_associations <- fromJSONstat(content(d.tmp, "text"))
employer_associations <- clean_names(employer_associations)
employer_associations <- tibble(employer_associations) 

#This series of if statements converts years, quarters and months into dates using zoo
if("month" %in% colnames(employer_associations)) {
  employer_associations<- separate(employer_associations, month, into = c("year", "month"), sep = "M")
  employer_associations$date  <- paste0(employer_associations$year,"-", employer_associations$month) 
  employer_associations$date <- as.Date(as.yearmon(employer_associations$date), frac = 1)
}else{ if("quarter" %in% colnames(employer_associations)) {
  employer_associations<- separate(employer_associations, quarter, into = c("year", "quarter"), sep = "K")
  employer_associations$date  <- paste0(employer_associations$year," Q", employer_associations$quarter) 
  employer_associations$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",employer_associations$date)),frac = 1)
}else{ employer_associations$date  <- paste0(employer_associations$year," Q4") 
employer_associations$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",employer_associations$date)),frac = 1)
}
}


employer_associations <- employer_associations %>%
  select(date, everything())
employer_associations_expand <- pivot_wider(employer_associations, 
                               id_expand = FALSE,
                               names_from = contents,
                               values_from = c("value"))

employer_associations_expand <- pivot_wider(employer_associations_expand, 
                                            id_expand = FALSE,
                                            names_from = work_related_issues,
                                            values_from = c("establishments", "persons_employed"))


employer_associations_expand <- employer_associations_expand %>%
  clean_names()


