#production

options(encoding="UTF-8")
library(httr)
library(rjstat) #For JSON
library(tidyverse)
library(janitor)
library(dplyr)
library(zoo)



url <- "https://data.ssb.no/api/v0/en/table/09171/"

data.tmp <- '
{
  "query": [
    {
      "code": "NACE",
      "selection": {
        "filter": "item",
        "values": [
          "nr23_6",
          "pub2X01_02",
          "pub2X03",
          "pub2X05",
          "nr2X06_09",
          "pub2X06",
          "pub2X09",
          "nr23ind",
          "pub2X10_12",
          "nr2310",
          "nr2312",
          "pub2X13_15",
          "nr2315",
          "nr2316",
          "pub2X18",
          "pub2X19_21",
          "nr2319",
          "pub2X22_23",
          "pub2X24",
          "pub2X25_28",
          "pub2X29_30",
          "pub2X31_32",
          "pub2X33",
          "pub2X35",
          "pub2X36_39",
          "pub2X41_43",
          "pub2X45_47",
          "pub2X49B",
          "pub2X50A",
          "pub2X49A_52",
          "pub2X53",
          "pub2X55_56",
          "pub2X58_63",
          "pub2X64_66",
          "pub2X68A",
          "pub2X68B",
          "pub2X69_75",
          "pub2X77_82",
          "pub2X84",
          "pub2X85",
          "pub2X86_88",
          "pub2X90_97",
          "nr24_5",
          "nr24_",
          "nr24sivil",
          "nr2482",
          "nr25_",
          "nr23_6fn",
          "nr23fn",
          "nr23mark",
          "nrimark"
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
ssb.prod <- fromJSONstat(content(d.tmp, "text"))
ssb.prod <- clean_names(ssb.prod)
## adding date class
ssb.prod <- tibble(ssb.prod) 

ssb.prod<- separate(ssb.prod, quarter, into = c("year", "quarter"), sep = "K")
ssb.prod$date  <- paste0(ssb.prod$year," Q", ssb.prod$quarter) 
ssb.prod$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",ssb.prod$date)),frac = 1)


ssb.prod <- ssb.prod %>%
  select(date, everything())
ssb.prod.wide <- pivot_wider(ssb.prod, 
                              id_expand = FALSE,
                              names_from = contents,
                              values_from = c("value"))
ssb.prod.wide <- ssb.prod.wide %>%
  clean_names()

### 

colnames(ssb.prod.wide)

ssb.prod.selected <- ssb.prod.wide %>%
  select(date,
         industry,
         year,
         output_at_basic_values_current_prices_nok_million,
         intermediate_consumption_current_prices_nok_million,
         value_added_at_basic_prices_constant_2020_prices_nok_million,
         value_added_at_basic_prices_constant_2020_prices_seasonally_adjusted_nok_million)

ssb.prod.selected <- ssb.prod.selected %>%
  clean_names()