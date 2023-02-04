## Library List
install.packages("tidyverse")
install.packages("janitor")
install.packages("expss")
install.packages("PxWebApiData")
install.packages("zoo")
install.packages("plotly")
install.packages("klassR")
install.packages("rvest")


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

