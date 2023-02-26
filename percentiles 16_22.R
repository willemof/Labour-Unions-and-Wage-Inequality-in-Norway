

url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/13860/



                      ", "\n"))


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
          "S",
          "T",
          "U",
          "A-Z",
          "Z"
        ]
      }
    },
    {
      "code": "Desiler",
      "selection": {
        "filter": "vs:DesilPersentil09",
        "values": [
          "P091",
          "P092",
          "P093",
          "P094",
          "P095",
          "P096",
          "P097",
          "P098",
          "P099",
          "P100"
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

percentilewage16_22 <- fromJSONstat(content(d.tmp, "text"))
percentilewage16_22 <- clean_names(percentilewage16_22)
percentilewage16_22 <- tibble(percentilewage16_22) 

x<- percentilewage16_22
#This series of if statements converts years, quarters and months into dates using zoo
if("month" %in% colnames(x)) {
  x<- separate(x, month, into = c("year", "month"), sep = "M")
  x$date  <- paste0(x$year,"-", x$month) 
  x$date <- as.Date(as.yearmon(x$date), frac = 1)
}else{ if("quarter" %in% colnames(x)) {
  x<- separate(x, quarter, into = c("year", "quarter"), sep = "K")
  x$date  <- paste0(x$year," Q", x$quarter) 
  x$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",x$date)),frac = 1)
}else{ x$date  <- paste0(x$year," Q4") 
x$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",x$date)),frac = 1)
}
}
write_csv(percentilewage16_22, file = ("csv/percentilewage16_22.csv"))

percentilewage16_22 <- read_csv(file = ("csv/percentilewage16_22.csv"))
percentilewage16_22 <- separate(percentilewage16_22, decil_group, c("trash", "percentile"), sep = " ")
percentilewage16_22 <- percentilewage16_22 %>%
  select(-trash)
percentilewage16_22_expand <- pivot_wider(percentilewage16_22, 
                                      id_expand = FALSE,
                                      names_from = contents,
                                      values_from = c("value"))
