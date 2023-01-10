library(tidyverse)
library(haven)
library(janitor)
library(ggplot2)
library(expss)
library(labelled)

setwd("C:/Users/willemo/OneDrive - Universitetet i Oslo/Master Thesis")


ds_raw <- read_dta("./Stata generated files/13_17.dta")
ds <- janitor::clean_names(ds_raw)
remove(ds_raw) # removing ds_raw for clarity of objects
ds <- ds %>%
     select(year, industrystring, industry, gender, genderstring, everything()) # this line just rearranges the columns
ds <- ds %>%
  mutate(is_male = gender)
ds$is.male <- ifelse(ds$gender ==2, 0, 1)

ds <- ds %>%
  mutate(is_unionized = unionized)
ds$is.unionized <- ifelse(ds$unionized==1,1,0)

ds <- ds %>%
  select(year, industrystring, industry, is.male, gender, genderstring, unionizedstring, is.unionized, everything()) # this line just rearranges the columns

ds_industries <- ds %>%
  select(year, industrystring, genderstring, is.unionized)

ds_industries <- ds_industries %>%
  filter(year==2013)

agg <- aggregate(ds_industries,
                by = list(ds_industries$industrystring),
                FUN = mean)

agg_gender <- aggregate(ds_industries,
                by = list(ds_industries$industrystring, ds_industries$genderstring),
                FUN = mean)

# this line is a work in progress, it returns a list in the cell and not a mean function -> ds_wide <- pivot_wider(ds_industries, names_from = industrystring, values_from = is.unionized)
