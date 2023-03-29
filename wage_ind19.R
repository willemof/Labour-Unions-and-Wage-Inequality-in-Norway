
library(httr) # API import
library(rjstat) # For JSON text conversion/API import
library(tidyverse) #dplyr, ggplot, you name it
library(klassR) #Import of standardized industries, occupations
library(janitor) # scrub, and clean the objects
library(zoo) # for the conversion of a year and quarter to date/class
library(rstudioapi) # for getting file path
library(data.table) #for fast if else
library(rvest) # for HTML-webscraping




# wage before 2015

url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/08053/


                      ", "\n"))
# 
data.tmp <- '
{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "1",
          "2"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "A-U",
          "A",
          "B",
          "C",
          "D",
          "E",
          "F",
          "G",
          "H",
          "I",
          "J",
          "K",
          "L",
          "M",
          "N",
          "O",
          "P",
          "Q",
          "R",
          "S"
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
ind19wage_ds <- fromJSONstat(content(d.tmp, "text"))
ind19wage_ds <- clean_names(ind19wage_ds)
ind19wage_ds <- tibble(ind19wage_ds) 

#This series of if statements converts years, quarters and months into dates using zoo
if("month" %in% colnames(ind19wage_ds)) {
  ind19wage_ds<- separate(ind19wage_ds, month, into = c("year", "month"), sep = "M")
  ind19wage_ds$date  <- paste0(ind19wage_ds$year,"-", ind19wage_ds$month) 
  ind19wage_ds$date <- as.Date(as.yearmon(ind19wage_ds$date), frac = 1)
}else{ if("quarter" %in% colnames(ind19wage_ds)) {
  ind19wage_ds<- separate(ind19wage_ds, quarter, into = c("year", "quarter"), sep = "K")
  ind19wage_ds$date  <- paste0(ind19wage_ds$year," Q", ind19wage_ds$quarter) 
  ind19wage_ds$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",ind19wage_ds$date)),frac = 1)
}else{ ind19wage_ds$date  <- paste0(ind19wage_ds$year," Q4") 
ind19wage_ds$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",ind19wage_ds$date)),frac = 1)
}
}


ind19wage_ds <- ind19wage_ds %>%
  select(date, everything())
ind19wage_ds_expand <- pivot_wider(ind19wage_ds, 
                               id_expand = FALSE,
                               names_from = contents,
                               values_from = c("value"))
ind19wage_ds_expand <- ind19wage_ds_expand %>%
  clean_names()
ind19wage_ds_expand <- ind19wage_ds_expand %>%
  filter(industry_sic2007 != "Total")



cypherind19 <- tibble(unique(ind19wage_ds_expand$industry_sic2007))
cypherlevel1 <- tibble(unique(level2tolevel1indus$parentname))
cypherind19$no <- 1:19
cypherlevel1$no <- 0:21
cypher <- full_join(cypherind19, cypherlevel1)
cypher <- cypher %>%
  mutate(name19 = cypher$`unique(ind19wage_ds_expand$industry_sic2007)`, .keep = "unused") %>%
  mutate(name = cypher$`unique(level2tolevel1indus$parentname)`, .keep = "unused")%>%
  select(name19, name)
cypher$parentcode <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","00.0","T","U")


x_loop <- level2tolevel1indus #This is the big ds to 'translate'
x_g <- c()
x_t <- x_loop
x_f <- x_loop
x_f$name19 <- c("")
for (i in 1:NROW(cypher)) { #this is the cypher
  x_f <- x_t %>%                                 #filtering the common denominator
    filter(x_t$parentcode==cypher$parentcode[i]) #filtering the common denominator
  x_t <- x_t %>%                                 #filtering the remainder
    filter(x_t$parentcode!=cypher$parentcode[i]) #filtering the remainder
  x_f$name19=cypher$name19[i] #in the common denominator, put the translated equivalent key
  x_g <- rbind(x_g, x_f)     #connect the translated
}

level2tolevel1indus <- x_g


x_loop <- ind19wage_ds_expand #This is the big ds to 'translate'
x_g <- c()
x_t <- x_loop
x_f <- x_loop
x_f$parentname <- c("")
for (i in 1:NROW(level2tolevel1indus)) { #this is the cypher
  x_f <- x_t %>%                                 #filtering the common denominator
    filter(x_t$industry_sic2007==level2tolevel1indus$name19[i]) #filtering the common denominator
  x_t <- x_t %>%                                 #filtering the remainder
    filter(x_t$industry_sic2007!=level2tolevel1indus$name19[i]) #filtering the remainder
  x_f$parentname=level2tolevel1indus$parentname[i] #in the common denominator, put the translated equivalent key
  x_g <- rbind(x_g, x_f)     #connect the translated
}


ind19wage_ds_expand <- x_g

write_csv(ind19wage_ds_expand, file = "csv/ssb/earningsindustry1314.csv")
