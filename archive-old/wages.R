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


url <- "https://data.ssb.no/api/v0/no/table/09174/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "NACE",
      "selection": {
        "filter": "vs:NRNaeringPubAgg",
        "values": [
          "nr23ind",
          "pub2X55_56",
          "pub2X64_66",
          "pub2X84",
          "pub2X85"
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

wages <- fromJSONstat(content(d.tmp, "text"))

url_en <- "https://data.ssb.no/api/v0/no/table/09174/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "NACE",
      "selection": {
        "filter": "vs:NRNaeringPubAgg",
        "values": [
          "nr23ind",
          "pub2X55_56",
          "pub2X64_66",
          "pub2X84",
          "pub2X85"
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

wages_en <- fromJSONstat(content(d.tmp, "text"))

wages_en <- janitor::clean_names(wages_en)

wages_en <- wages_en %>%
  pivot_wider(
  names_from = naering) %>%
  clean_names()

wages_en <- wages_en %>%
  pivot_wider(
    names_from = statistikkvariabel,
    names_sep = ".",
   values_from = c(industri,overnattings_og_serveringsvirksomhet,
                   finansierings_og_forsikringsvirksomhet,
                   offentlig_administrasjon_og_forsvar, 
                   undervisning)) %>%
    clean_names()

wages_en <- janitor::clean_names(wages_en)

wages_plot <- ggplot(wages_en, aes(x=ar, y = industri_lonn_mill_kr, group = )) +
          geom_line(colour = "black") +
          geom_point(colour = "black") +
  ggtitle("Wages from 1970-2021")
ggplot(wages_en, aes(x=ar, y = overnattings_og_serveringsvirksomhet_lonn_mill_kr)) +
  geom_point(colour = "black") #+
  #theme_dark()

#Basically I want to plot over time industri_lonn_mill_kr, finansierings_og_forsikringsvirksomhet_lonn_mill_kr, offentlig_administrasjon_og_forsvar_lonn_mill_kr and undervisning_lonn_mill_kr
wages_plot

