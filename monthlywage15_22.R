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
monthlyewage2022 <- monthlyewage15_22
write_csv(monthlyewage2022, file = ("csv/monthlyewage2022.csv"))
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
monthlyewage15_22_M <- clean_names(monthlyewage15_22_M)
monthlyewage15_22_M <- tibble(monthlyewage15_22_M) 

monthlyewage2021 <- monthlyewage15_22_M
write_csv(monthlyewage2021, file = ("csv/monthlyewage2021.csv"))


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
monthlyewage2020 <- monthlyewage15_22
write_csv(monthlyewage2020, file = ("csv/monthlyewage2020.csv"))

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
monthlyewage15_22_M <- clean_names(monthlyewage15_22_M)
monthlyewage15_22_M <- tibble(monthlyewage15_22_M) 

monthlyewage2019 <- monthlyewage15_22_M
write_csv(monthlyewage2019, file = ("csv/monthlyewage2019.csv"))


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

monthlyewage2018 <- monthlyewage15_22
write_csv(monthlyewage2018, file = ("csv/monthlyewage2018.csv"))

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
monthlyewage15_22_M <- clean_names(monthlyewage15_22_M)
monthlyewage15_22_M <- tibble(monthlyewage15_22_M) 

monthlyewage2017 <- monthlyewage15_22_M
write_csv(monthlyewage2017, file = ("csv/monthlyewage2017.csv"))



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

monthlyewage2016 <- monthlyewage15_22
write_csv(monthlyewage2016, file = ("csv/monthlyewage2016.csv"))


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
monthlyewage15_22_M <- clean_names(monthlyewage15_22_M)
monthlyewage15_22_M <- tibble(monthlyewage15_22) 

monthlyewage2015 <- monthlyewage15_22_M
write_csv(monthlyewage2015, file = ("csv/monthlyewage2015.csv"))



monthlyewage2015 <- read_csv(file = ("csv/monthlyewage2015.csv"))
monthlyewage2016 <- read_csv(file = ("csv/monthlyewage2016.csv"))
monthlyewage2017 <- read_csv(file = ("csv/monthlyewage2017.csv"))
monthlyewage2018 <- read_csv(file = ("csv/monthlyewage2018.csv"))
monthlyewage2019 <- read_csv(file = ("csv/monthlyewage2019.csv"))
monthlyewage2020 <- read_csv(file = ("csv/monthlyewage2020.csv"))
monthlyewage2021 <- read_csv(file = ("csv/monthlyewage2021.csv"))
monthlyewage2022 <- read_csv(file = ("csv/monthlyewage2022.csv"))

monthlyewage2015_22 <- full_join(monthlyewage2015,
                              monthlyewage2016)
monthlyewage2015_22M <- monthlyewage2017
monthlyewage2015_22 <- full_join(monthlyewage2015_22,
                                 monthlyewage2015_22M)
monthlyewage2015_22M <- monthlyewage2018
monthlyewage2015_22 <- full_join(monthlyewage2015_22,
                                 monthlyewage2015_22M)
monthlyewage2015_22M <- monthlyewage2019
monthlyewage2015_22 <- full_join(monthlyewage2015_22,
                                 monthlyewage2015_22M)
monthlyewage2015_22M <- monthlyewage2020
monthlyewage2015_22 <- full_join(monthlyewage2015_22,
                                 monthlyewage2015_22M)
monthlyewage2015_22M <- monthlyewage2021
monthlyewage2015_22 <- full_join(monthlyewage2015_22,
                                 monthlyewage2015_22M)
monthlyewage2015_22M <- monthlyewage2022
monthlyewage2015_22 <- full_join(monthlyewage2015_22,
                                 monthlyewage2015_22M)

write_csv(monthlyewage2015_22, file = ("csv/monthlywage15_22_long.csv"))


monthlywage15_22_expand <- pivot_wider(monthlyewage2015_22, 
                                      id_expand = FALSE,
                                      names_from = contents,
                                      values_from = c("value"))


monthlywage15_22_expand <- pivot_wider(monthlywage15_22_expand, 
                                       id_expand = FALSE,
                                       names_from = measuring_method,
                                       values_from = colnames(monthlywage15_22_expand[8:14]))


write_csv(monthlywage15_22_expand, file = ("csv/monthlywage15_22_wide.csv"))
