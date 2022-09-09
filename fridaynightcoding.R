#Friday attempt
#wrangle from scratch -- some intitial work from stata is done though. 

#lib
library(tidyverse)
library(haven)
library(janitor)

setwd("C:/Users/willemo/OneDrive - Universitetet i Oslo/Master Thesis")


ds_raw <- read_dta("./Stata generated files/13_17.dta")
ds <- janitor::clean_names(ds_raw)

ds<- ds %>%
  select(year, industrystring, unionizedstring, genderstring, everything())


