##run industrial first, this proceeds the wages part of the code

csv_name <- "ssb_wages" #name of csv/object required
if(exists(paste0(csv_name)) == FALSE) {
  
  ssb_wages <- read_csv(file = (paste0("csv/ssb/", csv_name, ".csv")))
}

csv_name <- "indus_level2" #name of csv/object required which is in file path as well

if(exists(paste0(csv_name)) == FALSE) {
  indus_level2 <- read_csv(file = (paste0("csv/ssb/", csv_name, ".csv")))
}
    
ssb_wages_year <- tibble(ssb_wages) 

ssb_wages_year<- separate(ssb_wages_year, quarter, into = c("year", "quarter"), sep = "K")




ssb_wages_year$date  <- paste0(ssb_wages_year$year," Q", ssb_wages_year$quarter) 
ssb_wages_year$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",ssb_wages_year$date)),frac = 1)

ssb_wages_year <- ssb_wages_year %>%
  select(date, year, quarter, everything())



ssb_wages_expand <- pivot_wider(ssb_wages_year, 
                                id_expand = FALSE,
                                names_from = contents,
                                values_from = c("value"))

ssb_wages_expand <- clean_names(ssb_wages_expand)


x_g <- c()
x_t <- ssb_wages_expand #dataset to "mutate"
x_f <- ssb_wages_expand#dataset to "mutate"
codec <- indus_level2
for (i in 1:NROW(codec)) {
  x_f <- x_t %>%
    filter(x_t$shortname==codec$shortname[i])
  x_t <- x_t %>%
    filter(x_t$shortname!=codec$shortname[i])
  x_f$name=codec$name[i] #add in english industry names
  x_f$code_indus=codec$code[i] #add in level 2 industrial codes
  x_f$parentcode_indus=codec$parentcode[i] # add in parentcode
  x_g <- rbind(x_g, x_f)
  if(i == NROW(codec)){
    ssb_wages_expand <- x_g
    remove(x_f, x_g, x_t, codec)
  }
}


write_csv(ssb_wages_expand, file = (paste0("csv/ssb/", objects("ssb_wages_expand") , ".csv")))
remove(ssb_wages_year, ssb_wages)