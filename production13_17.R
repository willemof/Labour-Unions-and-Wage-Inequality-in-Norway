#09174: Wages and salaries, employment and productivity by contents, industry and year

#loading industry codec
level2tolevel1indus <- read_csv("csv/ssb/level2tolevel1indus.csv")

url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/09174/




                      ", "\n"))


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
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Lonn",
          "LKostnader",
          "SysselsatteLonn",
          "TimeverkL"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2013",
          "2014",
          "2015",
          "2016",
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

production13_17 <- fromJSONstat(content(d.tmp, "text"))
production13_17 <- clean_names(production13_17)
production13_17 <- tibble(production13_17) 

x<- production13_17
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

production13_17 <- x %>%
  select(date, everything())
production13_17_expand <- pivot_wider(production13_17, 
                                      id_expand = FALSE,
                                      names_from = contents,
                                      values_from = c("value"))
x <- production13_17_expand

# Define unique_industries
unique_industries <- tibble(altname= (unique(x$industry)))

# Define missing_industries
missing_industries <- tibble(
  code = c("06", "16", "19", "23", "25", "30", "45", "46", "47", "62", "65", "70", "71", "79", "84", "97"),
  altname = c(
    "Oil and gas extraction",
    "Manufacture of wood and wood products, except furniture",
    "Refined petroleum, chemical and pharmaceutical products",
    "Manufacture of other non-metallic mineral products",
    "Fabricated metal products, except machinery and equipment",
    "Building of ships, oil platforms and moduls",
    "Wholesale and retail trade and repair of motor vehicles",
    "Wholesale trade, except of motor vehicles",
    "Retail trade, except of motor vehicles",
    "Computer programming",
    "Insurance, except compulsory social security",
    "Activities of head offices",
    "Architectural and engineering consultancy activities",
    "Travel agency and tour operator reservation service",
    "Public administration and defence",
    "Activities of households as employers"
  )
)
test <- left_join(unique_industries, missing_industries)

test2 <- left_join(level2tolevel1indus, test) %>%
  select(altname, everything())

for (i in 1:NROW(test2)) {
  if (is.na(test2$altname[i]) == TRUE) {
    test2$altname[i] <- test2$name[i]
  }
}

level2tolevel1indus <- test2 %>%
  select(code, everything())


# adding parentcode/industry main categories
x <- production13_17_expand
x_loop <- x
x_g <- c()
x_t <- x_loop
x_f <- x_loop
x_f$industryparentname <- c()
for (i in 1:NROW(level2tolevel1indus)) {
  if(level2tolevel1indus$altname[i] %in% unique(x_t$industry)==FALSE) {print(paste0("Something's wrong, I can feel it i = ", i, " codec <- ", level2tolevel1indus$altname[i]))
    next}
  x_f <- x_t %>%
    filter(x_t$industry==level2tolevel1indus$altname[i])
  x_t <- x_t %>%
    filter(x_t$industry!=level2tolevel1indus$altname[i])
  x_f$industryparentname=level2tolevel1indus$parentname[i] #add in english industry-parent names
  x_f$parentcode_indus=level2tolevel1indus$parentcode[i] #add in english industry-parent names
  
  x_g <- rbind(x_g, x_f)
}
x <- x_g %>%
  select(industryparentname, everything())
remove(x_f,x_t, x_g, x_loop)




aggregated_data <- x %>%
  group_by(parentcode_indus, industryparentname, date, year) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

write_csv(production13_17_expand, file = ("csv/production13_17.csv"))
