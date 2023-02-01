options(encoding="UTF-8")
library(tidyverse)
library(klassR)
library(janitor)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


nb_indus <- GetKlass(
  klass ="6", 
  date = NULL,
  correspond = NULL,
  variant = NULL,
  output_level = NULL,
  language = "nb",
  output_style = "normal",
  notes = FALSE)

nb_indus <- nb_indus %>% rename(navn = name)

en_indus <- GetKlass(
  klass ="6", 
  date = NULL,
  correspond = NULL,
  variant = NULL,
  output_level = NULL,
  language = "en",
  output_style = "normal",
  notes = FALSE)

#indus<- full_join(nb_indus, en_indus)
indus_filter <- en_indus

list_hnar <- read_delim("csv/list hnar.txt", 
                        delim = "\t", escape_double = FALSE, 
                        col_names = FALSE, trim_ws = TRUE)
list_hnar <- rename(list_hnar, navn=X1)
list_hnar$code <- ""
for (i in 1:NROW(list_hnar)) {
  list_hnar$code[i]=str_extract(list_hnar[i,1],"^[0-9]+")
}
for (i in 1:NROW(list_hnar)) {
  list_hnar[i,1] <- str_remove_all(list_hnar[i,1],"^[0-9]+")
}
list_hnar$code<- as.character(list_hnar$code)
for (i in 1:8) {
  list_hnar$code[i]<- str_c("0",list_hnar$code[i])
}

list_hnar$navnn <- list_hnar %>%
mutate(navnn = navn)
list_hnar$navnn <- substr(list_hnar$navn, 1, 55)

indus_filter <- full_join(en_indus, list_hnar)

