library(tidyverse)
library(haven)
library(janitor)
setwd("C:/Users/willemo/OneDrive - Universitetet i Oslo/Master Thesis")

#ds <- load("./stata_r_experiment.dta")

ds <- read_dta("stata_r_experiment.dta")
ds <- janitor::clean_names(ds)


#Loop for changing some NAs to 0, this is to make LUD rates.

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


# Loop for making some breaks in the aggregated count be consistent - that is that for somereason the total count of an industry sector would break where its not supposed to.
#like 1,1,1,NA, 1,1,3,3,3,NA,3,3,3

Q <- c()
for(i in 2:nrow(ds)) {
  if(ds$yescountmain[i]==0){
    Q <- ds$yescountmain[i-1]
  if(ds$yescountmain[i-1]==ds$yescountmain[i+1]) {
    (ds$yescountmain[i] <- Q)
  }
  }
}

for(i in 2:nrow(ds)) {
  if(ds$nocountmain[i]==0){
    Q <- ds$nocountmain[i-1]
    if(ds$nocountmain[i-1]==ds$nocountmain[i+1]) {
      (ds$nocountmain[i] <- Q)
    }
  }
}
