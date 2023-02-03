#run after running ssb wages.R, ssb cpi.R, and production ssb.R
library(plotly)

ssb <- inner_join(ssb.wages.expand, ssb.cpi.expand)
ssb <- left_join(ssb, ssb.prod.wide)
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
