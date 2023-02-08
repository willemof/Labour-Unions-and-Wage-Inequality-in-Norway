
library(httr) # API import
library(rjstat) # For JSON text conversion/API import
library(tidyverse) #dplyr, ggplot, you name it
library(klassR) #Import of standardized industries, occupations
library(janitor) # scrub, and clean the objects
library(zoo) # for the conversion of a year and quarter to date/class
library(rstudioapi) # for getting file path
library(data.table) #for fast if else
library(rvest) # for HTML-webscraping

occupation_wage <- c()


# Average labour costs per full-time equivalent employee, by industry (SIC2007) 

url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/11418/



                      ", "\n"))
# 
data.tmp <- '
{
  "query": [
    {
      "code": "Yrke",
      "selection": {
        "filter": "vs:NYK08Lonnansatt",
        "values": [
          "0000",
          "0110",
          "0210",
          "0310",
          "1111",
          "1112",
          "1114",
          "1120",
          "1211",
          "1212",
          "1213",
          "1219",
          "1221",
          "1222",
          "1223",
          "1311",
          "1312",
          "1321",
          "1322",
          "1323",
          "1324",
          "1330",
          "1341",
          "1342",
          "1343",
          "1344",
          "1345",
          "1346",
          "1349",
          "1411",
          "1412",
          "1420",
          "1431",
          "1439",
          "2111",
          "2112",
          "2113",
          "2114",
          "2120",
          "2131",
          "2132",
          "2133",
          "2141",
          "2142",
          "2143",
          "2144",
          "2145",
          "2146",
          "2149",
          "2151",
          "2152",
          "2153",
          "2161",
          "2162",
          "2163",
          "2164",
          "2165",
          "2166",
          "2211",
          "2212",
          "2221",
          "2222",
          "2223",
          "2224",
          "2250",
          "2261",
          "2262",
          "2263",
          "2264",
          "2265",
          "2266",
          "2267",
          "2269",
          "2310",
          "2320",
          "2330",
          "2341",
          "2342",
          "2351",
          "2352",
          "2353",
          "2354",
          "2355",
          "2356",
          "2359",
          "2411",
          "2412",
          "2413",
          "2421",
          "2422",
          "2423",
          "2424",
          "2431",
          "2432",
          "2433",
          "2434",
          "2511",
          "2512",
          "2513",
          "2514",
          "2519",
          "2521",
          "2522",
          "2523",
          "2529",
          "2611",
          "2612",
          "2619",
          "2621",
          "2622",
          "2631",
          "2632",
          "2633",
          "2634",
          "2635",
          "2636",
          "2641",
          "2642",
          "2643",
          "2651",
          "2652",
          "2653",
          "2654",
          "2655",
          "2656",
          "2659",
          "3112",
          "3113",
          "3114",
          "3115",
          "3116",
          "3117",
          "3118",
          "3119",
          "3121",
          "3122",
          "3123",
          "3131",
          "3132",
          "3133",
          "3134",
          "3135",
          "3139",
          "3141",
          "3142",
          "3143",
          "3151",
          "3152",
          "3153",
          "3154",
          "3155",
          "3211",
          "3212",
          "3213",
          "3214",
          "3230",
          "3240",
          "3251",
          "3254",
          "3256",
          "3257",
          "3258",
          "3259",
          "3311",
          "3312",
          "3313",
          "3315",
          "3321",
          "3322",
          "3323",
          "3324",
          "3331",
          "3332",
          "3333",
          "3334",
          "3339",
          "3341",
          "3342",
          "3343",
          "3351",
          "3352",
          "3353",
          "3354",
          "3355",
          "3359",
          "3411",
          "3412",
          "3413",
          "3421",
          "3422",
          "3423",
          "3431",
          "3432",
          "3433",
          "3434",
          "3439",
          "3511",
          "3512",
          "3513",
          "3514",
          "3521",
          "3522",
          "4110",
          "4131",
          "4132",
          "4211",
          "4212",
          "4213",
          "4214",
          "4221",
          "4222",
          "4223",
          "4224",
          "4225",
          "4226",
          "4227",
          "4229",
          "4311",
          "4312",
          "4313",
          "4321",
          "4322",
          "4323",
          "4411",
          "4412",
          "4413",
          "4415",
          "4416",
          "5111",
          "5112",
          "5113",
          "5120",
          "5131",
          "5132",
          "5141",
          "5142",
          "5151",
          "5152",
          "5153",
          "5161",
          "5163",
          "5164",
          "5165",
          "5169",
          "5211",
          "5212",
          "5221",
          "5222",
          "5223",
          "5230",
          "5241",
          "5242",
          "5243",
          "5244",
          "5245",
          "5246",
          "5249",
          "5311",
          "5312",
          "5321",
          "5322",
          "5329",
          "5411",
          "5413",
          "5414",
          "5419",
          "6111",
          "6112",
          "6113",
          "6114",
          "6121",
          "6122",
          "6123",
          "6129",
          "6130",
          "6210",
          "6221",
          "6222",
          "6224",
          "7112",
          "7113",
          "7114",
          "7115",
          "7119",
          "7121",
          "7122",
          "7123",
          "7124",
          "7125",
          "7126",
          "7127",
          "7131",
          "7132",
          "7133",
          "7211",
          "7212",
          "7213",
          "7214",
          "7215",
          "7221",
          "7222",
          "7223",
          "7224",
          "7231",
          "7232",
          "7233",
          "7234",
          "7311",
          "7312",
          "7313",
          "7314",
          "7315",
          "7316",
          "7317",
          "7318",
          "7319",
          "7321",
          "7322",
          "7323",
          "7411",
          "7412",
          "7413",
          "7421",
          "7422",
          "7511",
          "7512",
          "7513",
          "7514",
          "7515",
          "7522",
          "7531",
          "7532",
          "7534",
          "7535",
          "7536",
          "7541",
          "7542",
          "7543",
          "7544",
          "7549",
          "8111",
          "8112",
          "8113",
          "8114",
          "8121",
          "8122",
          "8131",
          "8132",
          "8141",
          "8142",
          "8143",
          "8151",
          "8152",
          "8153",
          "8154",
          "8155",
          "8156",
          "8157",
          "8159",
          "8160",
          "8171",
          "8172",
          "8181",
          "8182",
          "8183",
          "8189",
          "8211",
          "8212",
          "8219",
          "8311",
          "8312",
          "8322",
          "8331",
          "8332",
          "8341",
          "8342",
          "8343",
          "8344",
          "8350",
          "9111",
          "9112",
          "9122",
          "9123",
          "9129",
          "9211",
          "9212",
          "9213",
          "9214",
          "9215",
          "9216",
          "9311",
          "9312",
          "9313",
          "9321",
          "9329",
          "9331",
          "9333",
          "9334",
          "9412",
          "9510",
          "9611",
          "9612",
          "9613",
          "9621",
          "9622",
          "9623",
          "9629"
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
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Manedslonn"
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
occupation_wage <- fromJSONstat(content(d.tmp, "text"))
occupation_wage <- clean_names(occupation_wage)
occupation_wage <- tibble(occupation_wage) 

#This series of if statements converts years, quarters and months into dates using zoo
if("month" %in% colnames(occupation_wage)) {
  occupation_wage<- separate(occupation_wage, month, into = c("year", "month"), sep = "M")
  occupation_wage$date  <- paste0(occupation_wage$year,"-", occupation_wage$month) 
  occupation_wage$date <- as.Date(as.yearmon(occupation_wage$date), frac = 1)
}else{ if("quarter" %in% colnames(occupation_wage)) {
  occupation_wage<- separate(occupation_wage, quarter, into = c("year", "quarter"), sep = "K")
  occupation_wage$date  <- paste0(occupation_wage$year," Q", occupation_wage$quarter) 
  occupation_wage$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",occupation_wage$date)),frac = 1)
}else{ occupation_wage$date  <- paste0(occupation_wage$year," Q4") 
occupation_wage$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",occupation_wage$date)),frac = 1)
}
}


##occupation

nb_occu <- GetKlass(
  klass ="7", 
  date = NULL,
  correspond = NULL,
  variant = NULL,
  output_level = NULL,
  language = "nb",
  output_style = "normal",
  notes = FALSE)

nb_occu <- nb_occu %>% rename(nb_name = name)

en_occu <- GetKlass(
  klass ="7", 
  date = NULL,
  correspond = NULL,
  variant = NULL,
  output_level = NULL,
  language = "en",
  output_style = "normal",
  notes = FALSE)

occu<- full_join(nb_occu, en_occu) %>%
  filter(level==4)

remove(nb_occu, en_occu, code)

##adding occupation code to the name
x_loop <- occupation_wage 
x_g <- c()
x_t <- x_loop
x_f <- x_loop
x_f$code <- x_f$occupation_nb <- c("")
for (i in 1:NROW(occu)) {
  if(occu$name[i] %in% unique(x_t$occupation==FALSE)) {next}
  x_f <- x_t %>%
    filter(x_t$occupation==occu$name[i])
  x_t <- x_t %>%
    filter(x_t$occupation!=occu$name[i])
  x_f$code=occu$code[i] #add in occupation code
  x_f$occupation_nb=occu$nb_name[i] #add in nb occupation name
  x_g <- rbind(x_g, x_f)
}
x <- x_g %>%
  select(date, code, occupation, occupation_nb, everything())

remove(x_f,x_t, x_g, x_loop)

occupation_wage <- x
occupation_wage_expand <- pivot_wider(occupation_wage, 
                                   id_expand = FALSE,
                                   names_from = measuring_method,
                                   values_from = c("value"))
occupation_wage_expand <- occupation_wage_expand %>%
  clean_names()

occupation_wage_expand1 <- occupation_wage_expand %>%
  filter(grepl("teacher", occupation))

occupation_wage_expand2 <- occupation_wage_expand %>%
  filter(grepl("economist", occupation)) %>%
  filter(grepl("Both sexes" , sex))

occupation_wage_expand2 <- occupation_wage_expand %>%
  filter(grepl("^(263)+[0-9]", code)) %>%
  filter(grepl("Both sexes" , sex))%>%
  filter(grepl("Full" , contractual_usual_working_hours_per_week))
