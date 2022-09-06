library(tidyverse)
library(haven)
library(janitor)
setwd("C:/Users/willemo/OneDrive - Universitetet i Oslo/Master Thesis")

#ds <- load("./stata_r_experiment.dta")

ds <- read_dta("stata_r_experiment.dta")
ds <- janitor::clean_names(ds)



for(i in 1:nrow(ds)){
  if(is.na(ds$yescount[i])) {
    ds$yescount[i] = 0
  }
  if(is.na(ds$nocount[i])) {
    ds$nocount[i] = 0
  }
  if(is.na(ds$yescountmain[i])) {
    ds$yescountmain[i] = 0
  }
  if(is.na(ds$nocountmain[i])) {
    ds$nocountmain[i] = 0
  }
}
