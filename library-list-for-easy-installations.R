## Installs List
install.packages("tidyverse")
install.packages("janitor")
install.packages("expss")
install.packages("PxWebApiData")
install.packages("zoo")
install.packages("plotly")
install.packages("klassR")
install.packages("rvest")
install.packages("ggplot2")

install.packages("lmtest")
install.packages("lfe")
install.packages("sandwich")

install.packages("AER")
install.packages("plm")

install.packages("caret", dependencies = c("Depends", "Suggests"))

install.packages("gower")
install.packages("hardhat")
install.packages("ModelMetrics")


#To update R, get package installR and run updateR()

#library and presettings

options(encoding="UTF-8")
library(httr) # API import
library(rjstat) # For JSON text conversion/API import
library(tidyverse) #dplyr, ggplot, you name it
library(klassR) #Import of standardized industries, occupations
library(janitor) # scrub, and clean the objects
library(zoo) # for the conversion of a year and quarter to date/class
library(rstudioapi) # for getting file path
library(data.table) #for fast if else
library(rvest) # for HTML-webscraping
library(plotly)

library(lmtest)
library(lfe)
library(sandwich)
library(AER)
library(plm)

library(caret)

library(stargazer)


current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path) )
