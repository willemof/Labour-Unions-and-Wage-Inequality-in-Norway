#11419: Monthly earnings, by measuring method, occupation, sector, industry (SIC2007), sex, 
#contractual/usual working hours per week, contents and year



url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/11419/



                      ", "\n"))


data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "item",
        "values": [
          "0-9",
          "1",
          "1120",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9"
        ]
      }
    },
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
          "ALLE",
          "A+B+D+E",
          "6500",
          "6100"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACELonnalle02",
        "values": [
          "A-S",
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
          "00"
        ]
      }
    },
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
      "code": "AvtaltVanlig",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "5",
          "6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2022"
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

monthlyewage15_22 <- fromJSONstat(content(d.tmp, "text"))
monthlyewage15_22 <- clean_names(monthlyewage15_22)
monthlyewage15_22 <- tibble(monthlyewage15_22) 

data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "item",
        "values": [
          "0-9",
          "1",
          "1120",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9"
        ]
      }
    },
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
          "ALLE",
          "A+B+D+E",
          "6500",
          "6100"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACELonnalle02",
        "values": [
          "A-S",
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
          "00"
        ]
      }
    },
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
      "code": "AvtaltVanlig",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "5",
          "6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2021"
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

monthlyewage15_22_M <- fromJSONstat(content(d.tmp, "text"))
monthlyewage15_22_M <- clean_names(monthlyewage15_22)
monthlyewage15_22_M <- tibble(monthlyewage15_22) 
monthlyewage15_22<- full_join(monthlyewage15_22, monthlyewage15_22_M)
write_csv(monthlyewage15_22, file = ("csv/monthlyewage15_22_1.csv"))
remove(monthlyewage15_22_M)
data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "item",
        "values": [
          "0-9",
          "1",
          "1120",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9"
        ]
      }
    },
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
          "ALLE",
          "A+B+D+E",
          "6500",
          "6100"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACELonnalle02",
        "values": [
          "A-S",
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
          "00"
        ]
      }
    },
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
      "code": "AvtaltVanlig",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "5",
          "6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2020"
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

monthlyewage15_22 <- fromJSONstat(content(d.tmp, "text"))
monthlyewage15_22 <- clean_names(monthlyewage15_22)
monthlyewage15_22 <- tibble(monthlyewage15_22) 

data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "item",
        "values": [
          "0-9",
          "1",
          "1120",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9"
        ]
      }
    },
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
          "ALLE",
          "A+B+D+E",
          "6500",
          "6100"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACELonnalle02",
        "values": [
          "A-S",
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
          "00"
        ]
      }
    },
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
      "code": "AvtaltVanlig",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "5",
          "6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2019"
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

monthlyewage15_22_M <- fromJSONstat(content(d.tmp, "text"))
monthlyewage15_22_M <- clean_names(monthlyewage15_22)
monthlyewage15_22_M <- tibble(monthlyewage15_22) 
monthlyewage15_22<- full_join(monthlyewage15_22, monthlyewage15_22_M)
write_csv(monthlyewage15_22, file = ("csv/monthlyewage15_22_2.csv"))
remove(monthlyewage15_22_M)

data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "item",
        "values": [
          "0-9",
          "1",
          "1120",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9"
        ]
      }
    },
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
          "ALLE",
          "A+B+D+E",
          "6500",
          "6100"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACELonnalle02",
        "values": [
          "A-S",
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
          "00"
        ]
      }
    },
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
      "code": "AvtaltVanlig",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "5",
          "6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2018"
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

monthlyewage15_22 <- fromJSONstat(content(d.tmp, "text"))
monthlyewage15_22 <- clean_names(monthlyewage15_22)
monthlyewage15_22 <- tibble(monthlyewage15_22) 

data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "item",
        "values": [
          "0-9",
          "1",
          "1120",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9"
        ]
      }
    },
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
          "ALLE",
          "A+B+D+E",
          "6500",
          "6100"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACELonnalle02",
        "values": [
          "A-S",
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
          "00"
        ]
      }
    },
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
      "code": "AvtaltVanlig",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "5",
          "6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2017"
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

monthlyewage15_22_M <- fromJSONstat(content(d.tmp, "text"))
monthlyewage15_22_M <- clean_names(monthlyewage15_22)
monthlyewage15_22_M <- tibble(monthlyewage15_22) 
monthlyewage15_22<- full_join(monthlyewage15_22, monthlyewage15_22_M)
write_csv(monthlyewage15_22, file = ("csv/monthlyewage15_22_3.csv"))
remove(monthlyewage15_22_M)


data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "item",
        "values": [
          "0-9",
          "1",
          "1120",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9"
        ]
      }
    },
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
          "ALLE",
          "A+B+D+E",
          "6500",
          "6100"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACELonnalle02",
        "values": [
          "A-S",
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
          "00"
        ]
      }
    },
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
      "code": "AvtaltVanlig",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "5",
          "6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2016"
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

monthlyewage15_22 <- fromJSONstat(content(d.tmp, "text"))
monthlyewage15_22 <- clean_names(monthlyewage15_22)
monthlyewage15_22 <- tibble(monthlyewage15_22) 

data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "item",
        "values": [
          "0-9",
          "1",
          "1120",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9"
        ]
      }
    },
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
          "ALLE",
          "A+B+D+E",
          "6500",
          "6100"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "vs:NACELonnalle02",
        "values": [
          "A-S",
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
          "00"
        ]
      }
    },
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
      "code": "AvtaltVanlig",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "5",
          "6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2015"
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

monthlyewage15_22_M <- fromJSONstat(content(d.tmp, "text"))
monthlyewage15_22_M <- clean_names(monthlyewage15_22)
monthlyewage15_22_M <- tibble(monthlyewage15_22) 
monthlyewage15_22<- full_join(monthlyewage15_22, monthlyewage15_22_M)
write_csv(monthlyewage15_22, file = ("csv/monthlyewage15_22_4.csv"))
remove(monthlyewage15_22_M)




monthlyewage15_22_4 <- read_csv(file = ("csv/monthlyewage15_22_4.csv"))
monthlyewage15_22_3 <- read_csv(file = ("csv/monthlyewage15_22_3.csv"))

monthlyewage15_22_4 <- full_join(monthlyewage15_22_4, monthlyewage15_22_3)

monthlyewage15_22_2 <- read_csv(file = ("csv/monthlyewage15_22_2.csv"))
monthlyewage15_22_1 <- read_csv(file = ("csv/monthlyewage15_22_1.csv"))

monthlyewage15_22_2 <- full_join(monthlyewage15_22_2, monthlyewage15_22_1)

monthlywage15_22 <- full_join(monthlyewage15_22_2, monthlyewage15_22_4)

write_csv(monthlyewage15_22, file = ("csv/monthlywage15_22.csv"))

filter_monthlywage <- monthlyewage15_22 %>%
  filter(year %in% c("2016", "2017"))

monthlywage15_22_expand <- pivot_wider(filter_monthlywage, 
                                      id_expand = FALSE,
                                      names_from = contents,
                                      values_from = c("value"))


monthlywage15_22_expand <- pivot_wider(monthlywage15_22_expand, 
                                       id_expand = FALSE,
                                       names_from = measuring_method,
                                       values_from = colnames(monthlywage15_22_expand[8:14]))


write_csv(monthlywage15_22_expand, file = ("csv/monthlywage15_22_expand.csv"))
