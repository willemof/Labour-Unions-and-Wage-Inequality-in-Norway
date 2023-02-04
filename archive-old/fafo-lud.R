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
library(RVerbalExpressions)
library(readtext)

#Attempt to extract data from pdf




library(rvest)

url.list <- c("https://www.fafo.no/media/com_netsukii/643.pdf",
              "https://www.fafo.no/images/pub/2020/20750.pdf",
              )


#This for-loop downloads the pdfs in url.list
for(i in 1:NROW(url.list)) {
  url <- url.list[i]
  file.tmp <- paste0("./pdf/", str_extract(url,"[0-9]+.pdf"))
if(file.exists(file.tmp)) {
  next } else {
download.file(url, file.tmp, mode="wb")
}
}


pdf <- read_html("./pdf/20750.pdf")




