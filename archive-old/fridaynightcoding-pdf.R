#Friday attempt
#wrangle from scratch -- some intitial work from stata is done though. 

#lib
library(tidyverse)
library(haven)
library(janitor)
library(ggplot2)

setwd("C:/Users/willemo/OneDrive - Universitetet i Oslo/Master Thesis")


ds_raw <- read_dta("./Stata generated files/13_17.dta")
ds <- janitor::clean_names(ds_raw)

ds<- ds %>%
  select(year, industrystring, unionizedstring, genderstring, everything())




agg_ds <- ds %>%
  aggregate(by = list(ds$year, ds$industrystring),
            FUN = mean)

ds <- ds %>%
  mutate(unionizedstatus = unionized)

ds<- ds %>%
  select(unionizedstatus, everything())

ds %>%
  mutate_at(vars(unionized, industrystring), character)

#for(i in NROW(ds)){
#  if(ds$unionized[i] == 1) {
#    ds$unionizedstatus[i] == str("Yes") 
#    next }
#  if(ds$unionized[i] == 2) {
#      ds$unionizedstatus[i] == str("No") 
#    next}
#   na_if(ds$unionizedstatus, ds$unionizedstatus[i]) 
#}


ggplot(data = ds, 
       mapping = aes(x = industrystring ,y = unionized)) +
  geom_point(size = 3)



class(ds)

class(ds$unionizedstatus)

ds$unionizedstatus <- as.character(ds$unionizedstatus)
ds$industrystring <- as.character(ds$industrystring)

ds$unionizedstring <- as.factor(ds$unionizedstring)

levels(ds$unionizedstring)
unique(ds$unionizedstring)

