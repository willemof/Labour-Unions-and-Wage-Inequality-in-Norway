#wage table with union rates for sector, industry, occupation, part-time, full-time

monthlywage15_22 <- read_csv(file = ("csv/monthlywage15_22.csv"))


filter_monthlywage <- monthlywage15_22 %>%
  filter(year %in% c("2016", "2017"))

monthlywage15_22_expand <- pivot_wider(filter_monthlywage, 
                                       id_expand = FALSE,
                                       names_from = contents,
                                       values_from = c("value"))


monthlywage15_22_expand <- pivot_wider(monthlywage15_22_expand, 
                                       id_expand = FALSE,
                                       names_from = measuring_method,
                                       values_from = colnames(monthlywage15_22_expand[8:14]))
