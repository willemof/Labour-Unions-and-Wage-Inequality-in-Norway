#run first
options(encoding="UTF-8")
library(httr) # API import
library(rjstat) # For JSON text conversion/API import
library(tidyverse) #dplyr, ggplot, you name it
library(klassR) #Import of standardized industries, occupations
library(janitor) # scrub, and clean the objects
library(zoo) # for the conversion of a year and quarter to date/class
library(rstudioapi) # for getting file path

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#getting norwegian industry codes and names
nb_indus <- GetKlass(
  klass ="6", 
  date = NULL,
  correspond = NULL,
  variant = NULL,
  output_level = NULL,
  language = "nb",
  output_style = "normal",
  notes = FALSE)

nb_indus <- nb_indus %>% rename(nb_name = name)

#getting english industry codes and names
en_indus <- GetKlass(
  klass ="6", 
  date = NULL,
  correspond = NULL,
  variant = NULL,
  output_level = NULL,
  language = "en",
  output_style = "normal",
  notes = FALSE)

industable<- full_join(nb_indus, en_indus)


labeltable <- read_csv("csv/labeltable.csv", 
                       col_names = c("lname","code","navn","trunc",
                                     col_types ="c"))
labeltable <- labeltable %>%
  select(-trunc)

labeltable_hnar <- labeltable %>%
  filter(lname=="Hnar") %>%
  select(-lname)


#adding a leading zero to be consistent with future tables
for (i in 1:8) {
  labeltable_hnar$code[i]<- str_c("0",labeltable_hnar$code[i])
}
#adding a zero for unspecified
new_row <- c("00", "Uspesifisert eller uidentifiserbar næring")
labeltable_hnar <- rbind(new_row, labeltable_hnar)

#same but for industable, will be important when grouping together
industable <- industable %>% select(code, parentCode, level, name, nb_name)
new_row <- c("00","00.0","2","Unspecified or unidentifiable industry","Uspesifisert eller uidentifiserbar næring")
industable <- rbind(new_row, industable)


new_row <- c("00.0","","1","Unspecified or unidentifiable industry","Uspesifisert eller uidentifiserbar næring")
industable <- rbind(new_row, industable)

industable <-full_join(industable,labeltable_hnar) 

industable$navn <-  str_squish(industable$navn)
industable$navn <- str_sub(industable$navn,1,45)

industable <- industable %>%
  mutate(parentcode=parentCode, .keep="unused")

indus_level1<- industable %>%
  filter(level==1) %>%
  select(-parentcode)


indus_level2 <- industable %>%
  filter(level==2)

remove(nb_indus, en_indus, new_row, indus)



##below is basically the extraction of wages, but from it it generates a short name
##list of NACE2007 which I wasn't easily able to find, the code below merges the
##shortlist together with the industry key table
url <- "https://data.ssb.no/api/v0/en/table/12314/"
# 
data.tmp <- '
{
  "query": [
    {
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "00-99",
          "01",
          "02",
          "03",
          "05",
          "06",
          "07",
          "08",
          "09",
          "10",
          "11",
          "12",
          "13",
          "14",
          "15",
          "16",
          "17",
          "18",
          "19",
          "20",
          "21",
          "22",
          "23",
          "24",
          "25",
          "26",
          "27",
          "28",
          "29",
          "30",
          "31",
          "32",
          "33",
          "35",
          "36",
          "37",
          "38",
          "39",
          "41",
          "42",
          "43",
          "45",
          "46",
          "47",
          "49",
          "50",
          "51",
          "52",
          "53",
          "55",
          "56",
          "58",
          "59",
          "60",
          "61",
          "62",
          "63",
          "64",
          "65",
          "66",
          "68",
          "69",
          "70",
          "71",
          "72",
          "73",
          "74",
          "75",
          "77",
          "78",
          "79",
          "80",
          "81",
          "82",
          "84",
          "85",
          "86",
          "87",
          "88",
          "90",
          "91",
          "92",
          "93",
          "94",
          "95",
          "96",
          "97",
          "99",
          "00"
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
ssb_wages <- fromJSONstat(content(d.tmp, "text"))
ssb_wages <- ssb_wages %>%
  clean_names() %>%
  mutate(shortname = industry_sic2007, .keep = "unused")
shortname <- unique(ssb_wages$shortname)
shortname <- tibble(shortname)
shortname <- shortname %>% slice(2:88)
new_row <- c("Unspecified")
shortname <- rbind(new_row, shortname)



code <- indus_level2$code
code <- tibble(code)
shortname <- tibble(code, shortname)
industable <- right_join(shortname, industable)


indus_level2 <- industable %>%
  filter(level==2)

indus_level1 <- industable %>%
  filter(level==1)

indus_level1 <- indus_level1 %>%
  mutate(parentname=name, .keep ="unused")

level2tolevel1indus <- right_join(indus_level1,indus_level2)


x_loop <- level2tolevel1indus
x_g <- c()
x_t <- x_loop
x_f <- x_loop
x_f$parentname <- c()
for (i in 1:NROW(indus_level1)) {
  x_f <- x_t %>%
    filter(x_t$parentcode==indus_level1$code[i])
  x_t <- x_t %>%
    filter(x_t$parentcode!=indus_level1$code[i])
  x_f$parentname=indus_level1$parentname[i] #add in english industry-parent names
  x_g <- rbind(x_g, x_f)
}
level2tolevel1indus <- x_g


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
  filter(level==4 | level ==2)

remove(nb_occu, en_occu, code)

write_csv(level2tolevel1indus, file = ("csv/ssb/level2tolevel1indus.csv"))
write_csv(ssb_wages, file = ("csv/ssb/ssb_wages.csv"))
write_csv(indus_level1, file = ("csv/ssb/indus_level1.csv"))
write_csv(indus_level2, file = ("csv/ssb/indus_level2.csv"))
write_csv(occu, file = ("csv/ssb/occu.csv"))

