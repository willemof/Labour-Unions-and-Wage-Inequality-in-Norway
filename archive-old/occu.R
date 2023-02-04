#run first
options(encoding="UTF-8")
library(tidyverse)
library(klassR)
library(janitor)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


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
