## Library List
install.packages("tidyverse")
install.packages("janitor")
install.packages("devtools")
library("devtools")
devtools::install_github("VerbalExpressions/RVerbalExpressions")
install.packages("regex")
library("regex")
library(RVerbalExpressions)



if (!require("remotes")) {
  install.packages("remotes")
}
# on 64-bit Windows
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

extract


library(gridExtra)
library(tabulizer)
library(pdftools)
install.packages("pdftables")
