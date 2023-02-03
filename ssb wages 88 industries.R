##run industrial first, this proceeds the wages part of the code
ssb.wages.year <- tibble(ssb.wages) 

ssb.wages.year<- separate(ssb.wages.year, quarter, into = c("year", "quarter"), sep = "K")




ssb.wages.year$date  <- paste0(ssb.wages.year$year," Q", ssb.wages.year$quarter) 
ssb.wages.year$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",ssb.wages.year$date)),frac = 1)

ssb.wages.year <- ssb.wages.year %>%
  select(date, year, quarter, everything())



ssb.wages.expand <- pivot_wider(ssb.wages.year, 
                                id_expand = FALSE,
                                names_from = contents,
                                values_from = c("value"))

ssb.wages.expand <- clean_names(ssb.wages.expand)
x_g <- c()
x_t <- ssb.wages.expand
x_f <- ssb.wages.expand
for (i in 1:NROW(indus_level2)) {
  x_f <- x_t %>%
    filter(x_t$shortname==indus_level2$shortname[i])
  x_t <- x_t %>%
    filter(x_t$shortname!=indus_level2$shortname[i])
  x_f$name=indus_level2$name[i] #add in english industry names
  x_f$code_indus=indus_level2$code[i] #add in level 2 industrial codes
  x_f$parentcode_indus=indus_level2$parentcode[i] # add in parentcode
  x_g <- rbind(x_g, x_f)
}
ssb.wages.expand <- x_g
