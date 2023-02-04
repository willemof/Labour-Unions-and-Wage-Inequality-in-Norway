#production

url <- "https://data.ssb.no/api/v0/en/table/09170/"

data.tmp <- '
{
  "query": [
    {
      "code": "NACE",
      "selection": {
        "filter": "vs:NRNaeringA88NP",
        "values": [
          "A88NP2X01",
          "A88NP2X02",
          "A88NP2X03",
          "A88NP2X05",
          "A88NP2X06",
          "A88NP2X07",
          "A88NP2X08",
          "A88NP2X09",
          "A88NP2X10",
          "A88NP2X11",
          "A88NP2X12",
          "A88NP2X13",
          "A88NP2X14",
          "A88NP2X15",
          "A88NP2X16",
          "A88NP2X17",
          "A88NP2X18",
          "A88NP2X19_21",
          "A88NP2X22",
          "A88NP2X23",
          "A88NP2X24",
          "A88NP2X25",
          "A88NP2X26",
          "A88NP2X27",
          "A88NP2X28",
          "A88NP2X29",
          "A88NP2X30",
          "A88NP2X31",
          "A88NP2X32",
          "A88NP2X33",
          "A88NP2X35",
          "A88NP2X36",
          "A88NP2X37",
          "A88NP2X38",
          "A88NP2X39",
          "A88NP2X41",
          "A88NP2X42",
          "A88NP2X43",
          "A88NP2X45",
          "A88NP2X46",
          "A88NP2X47",
          "A88NP2X49",
          "A88NP2X50",
          "A88NP2X51",
          "A88NP2X52",
          "A88NP2X53",
          "A88NP2X55",
          "A88NP2X56",
          "A88NP2X58",
          "A88NP2X59",
          "A88NP2X60",
          "A88NP2X61",
          "A88NP2X62",
          "A88NP2X63",
          "A88NP2X64",
          "A88NP2X65",
          "A88NP2X66",
          "A88NP2X68",
          "A88NP2X69",
          "A88NP2X70",
          "A88NP2X71",
          "A88NP2X72",
          "A88NP2X73",
          "A88NP2X74",
          "A88NP2X75",
          "A88NP2X77",
          "A88NP2X78",
          "A88NP2X79",
          "A88NP2X80",
          "A88NP2X81",
          "A88NP2X82",
          "A88NP2X84",
          "A88NP2X85",
          "A88NP2X86",
          "A88NP2X87",
          "A88NP2X88",
          "A88NP2X90",
          "A88NP2X91",
          "A88NP2X92",
          "A88NP2X93",
          "A88NP2X94",
          "A88NP2X95",
          "A88NP2X96",
          "A88NP2X97"
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

ssb.prod$date  <- paste0(ssb.prod$year," Q4") 
ssb.prod$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",ssb.prod$date)),frac = 1)


ssb.prod <- ssb.prod %>%
  select(date, everything())
ssb.prod.wide <- pivot_wider(ssb.prod, 
                              id_expand = FALSE,
                              names_from = contents,
                              values_from = c("value"))
ssb.prod.wide <- ssb.prod.wide %>%
  clean_names()%>%
  mutate(name=industry, .keep = "unused")

### 
