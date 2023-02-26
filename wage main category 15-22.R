  
  # 2015-2022 lønnsfordeling
  

#problem: api doesnt allow too many data points, needs from 2015-2022
  url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/11421/




                      ", "\n"))
  # 
  data.tmp <- '{
  "query": [
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
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
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "00-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "060+"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "ArbeidsTid",
      "selection": {
        "filter": "item",
        "values": [
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
          "2015",
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
  monthly_earnings15_22 <- fromJSONstat(content(d.tmp, "text"))
  monthly_earnings15_22 <- clean_names(monthly_earnings15_22)
  monthly_earnings15_22 <- tibble(monthly_earnings15_22) 
  
  
  data.tmp <- '
{
  "query": [
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
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
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "00-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "060+"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "ArbeidsTid",
      "selection": {
        "filter": "item",
        "values": [
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
          "2017",
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
  monthly_earnings15_22 <- fromJSONstat(content(d.tmp, "text"))
  monthly_earnings15_22 <- clean_names(monthly_earnings15_22)
  monthly_earnings15_22 <- tibble(monthly_earnings15_22) 
  
  
  data.tmp <- '
{
  "query": [
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
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
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "00-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "060+"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "ArbeidsTid",
      "selection": {
        "filter": "item",
        "values": [
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
          "2019",
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
  monthly_earnings15_22M <- fromJSONstat(content(d.tmp, "text"))
  monthly_earnings15_22M <- clean_names(monthly_earnings15_22M)
  monthly_earnings15_22M <- tibble(monthly_earnings15_22M) 
  monthly_earnings15_22<- full_join(monthly_earnings15_22, monthly_earnings15_22M)
  
  d.tmp <- POST(url , body = data.tmp, encode = "json", verbose())
  monthly_earnings15_22 <- fromJSONstat(content(d.tmp, "text"))
  monthly_earnings15_22 <- clean_names(monthly_earnings15_22)
  monthly_earnings15_22 <- tibble(monthly_earnings15_22) 
  
  
  data.tmp <- '
{
  "query": [
    {
      "code": "Sektor",
      "selection": {
        "filter": "item",
        "values": [
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
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "00-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "060+"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "ArbeidsTid",
      "selection": {
        "filter": "item",
        "values": [
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
          "2021",
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
  monthly_earnings15_22M <- fromJSONstat(content(d.tmp, "text"))
  monthly_earnings15_22M <- clean_names(monthly_earnings15_22M)
  monthly_earnings15_22M <- tibble(monthly_earnings15_22M) 
  monthly_earnings15_22<- full_join(monthly_earnings15_22, monthly_earnings15_22M)
  
  x<- monthly_earnings15_22
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
  
  write_csv(monthly_earnings15_22, file = ("csv/monthly_earnings15_22.csv"))
  earningsdistribution <- x %>%
    select(date, everything())
  earningsdistribution_expand <- pivot_wider(earningsdistribution, 
                                             id_expand = FALSE,
                                             names_from = contents,
                                             values_from = c("value"))
  
  earningsdistribution <- earningsdistribution %>%
    clean_names()
  