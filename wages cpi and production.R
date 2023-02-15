#run after running ssb wages.R, ssb cpi.R, and production ssb.R

csv_name <- "ssb_cpi_expand"

              #name of csv/object required

  if(exists(paste0(csv_name)) == FALSE) {
    
    ssb_cpi_expand <- read_csv(file = (paste0("csv/ssb/", csv_name, ".csv")))
  }
 
  csv_name <- "ssb_prod_wide"
  
  if(exists(paste0(csv_name)) == FALSE) {
    
    ssb_prod_wide <- read_csv(file = (paste0("csv/ssb/", csv_name, ".csv")))
  }
  
  csv_name <- "ssb_wages_expand"
  
  if(exists(paste0(csv_name)) == FALSE) {
    
    ssb_wages_expand <- read_csv(file = (paste0("csv/ssb/", csv_name, ".csv")))
  }
  

library(plotly)

ssb <- inner_join(ssb_wages_expand, ssb_cpi_expand)
ssb <- left_join(ssb, ssb_prod_wide)
#remove(d.tmp, ssb.wages, ssb.cpi, ssb.prod.wide,
#      ssb.cpi.expand, ssb.wages.expand, 
#       ssb.wages.year, ssb.prod.selected, data.tmp, url)
ssb <- ssb %>%
  mutate(industry=name, .keep="unused")
colnames(ssb)
ssb_select <- ssb %>%
  select(date,
         year,
         quarter,
         average_monthly_basic_earnings_nok, 
         average_monthly_earnings_nok, 
         number_of_employees,
         number_of_jobs_employments,
         consumer_price_index_2015_100) 

write_csv(ssb, file = (paste0("csv/ssb_merge/", "ssb" , ".csv")))
write_csv(ssb_select, file = (paste0("csv/ssb_merge/", "ssb_select" , ".csv")))