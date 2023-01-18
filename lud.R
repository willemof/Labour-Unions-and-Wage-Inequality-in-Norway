options(encoding="UTF-8")
library(httr)
library(tidyverse)
library(httr)
library(tinytex)
library(tidyverse)
library(janitor)
library(kableExtra)
library(rjstat)
library(dplyr)
library(stringr)


url <- "https://data.ssb.no/api/v0/no/table/03546/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "NHO",
      "selection": {
        "filter": "item",
        "values": [
          "00",
          "01",
          "53",
          "07",
          "02",
          "04",
          "51",
          "56",
          "05",
          "50",
          "58",
          "09",
          "78",
          "79",
          "80",
          "62",
          "10",
          "03",
          "81",
          "82",
          "08",
          "11",
          "64",
          "40",
          "06",
          "66",
          "69",
          "12",
          "16",
          "18",
          "13",
          "14",
          "83",
          "41",
          "43",
          "15",
          "84",
          "42",
          "17",
          "46",
          "85",
          "68",
          "19",
          "44",
          "20",
          "52",
          "86",
          "87",
          "55",
          "45",
          "59",
          "24",
          "77",
          "22",
          "23",
          "88",
          "89",
          "21",
          "25",
          "90",
          "27",
          "31",
          "54",
          "29",
          "91",
          "34",
          "28",
          "30",
          "63",
          "33",
          "32",
          "92",
          "26",
          "35",
          "71",
          "72",
          "74",
          "38",
          "70",
          "57",
          "37",
          "60",
          "76",
          "61",
          "75",
          "39",
          "73",
          "65",
          "67"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
d.tmp <- POST(url , body = data, encode = "json", verbose())

lud <- fromJSONstat(content(d.tmp, "text"))

lud <- janitor::clean_names(lud)

lud <- lud %>%
  pivot_wider(
    names_from = statistikkvariabel) %>%
  clean_names()


lud_nominal <- lud %>%
  select(-yrkesaktive_medlemer)

lud_nominal <- lud_nominal %>%
  pivot_wider(
    names_from = landsforening,
    values_from = medlemer)%>%
  clean_names()



lud <- lud %>%
  pivot_wider(
    names_from =landsforening,
    names_sep = "_",
    values_from = c(medlemer, yrkesaktive_medlemer))  %>%
  clean_names()

