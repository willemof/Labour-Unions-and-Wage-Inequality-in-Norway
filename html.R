#13-15 webscrape

library(rvest)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

url <- ("https://www.ssb.no/en/statbank/list/lonnansatt")

wage_timeseries_html <- "./webscrape/wage_timeseries.html"

if(file.exists(dirname(wage_timeseries_html))) {
  read <- read_html(wage_timeseries_html) 
} else {
download.file(url, # Download one html-file after another int
              destfile = "./webscrape/wage_timeseries.html")
  print(paste0("The webpage has been downloaded and saved"))
}

string <- read %>%
  html_text2()

str_locate_all(read, "http")
read %>%
  html_elements("a") %>%
  html_attr("href") 

#get a single text string of the html data of ssb

substring <- str_subset(string, "SIC2007") 


str_locate_all(substring, "SIC2007")

substring <- str_conv(substring, "UTF-8")
#makes strings with delimiter "noTitleTime"
split <- str_split %>%
  map(substring, "noTitleTime")

#to check the strings extracted
sub_extract <- split %>%
  map(., ~ str_subset(., "2008"))

sub_extract
extract <- map(split, str_subset(split, "SIC2007"))
t_extract <- tibble(extract)
ext <- map(ext, str_subset(extract, "2008"))

ext <- str_subset(extract, "2008")
t_ext <- tibble(ext)
