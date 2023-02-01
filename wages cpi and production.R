#run after running ssb wages.R, ssb cpi.R, and production ssb.R
library(plotly)

ssb <- inner_join(ssb.wages.expand, ssb.cpi.expand)
ssb <- inner_join(ssb, ssb.prod.selected)
remove(d.tmp, ssb.wages, ssb.prod, ssb.cpi, ssb.wide, ssb.prod.wide,
       filter_wages_2016, ssb.cpi.expand, ssb.wages.expand, 
       ssb.wages.year, ssb.prod.selected, data.tmp, url)
colnames(ssb)
ssb_select <- ssb %>%
  select(date,
         year,
         quarter,
         wages_and_salaries_nok_million, 
         wages_and_salaries_seasonally_adjusted_nok_million, 
         compensation_of_employees_nok_million,
         compensation_of_employees_seasonally_adjusted_nok_million,
         employed_persons_employees_and_self_employed_1_000_persons,
         employed_persons_employees_and_self_employed_seasonally_adjusted_1_000_persons,
         total_hours_worked_employees_mill_of_workhours,
         total_hours_worked_for_employees_and_self_employed_seasonally_adjusted_million_workhours,
         total_hours_worked_employees_mill_of_workhours,
         consumer_price_index_2015_100,
         industry)
s_g <- c()
s_t <- ssb_select
s_f <- ssb_select
for (i in 1:NROW(indus)) {
  s_f <- s_t %>%
    filter(s_t$industry==indus$name[i])
  s_t <- s_t %>%
    filter(s_t$industry!=indus$name[i])
#  s_f$hnar=indus$name[i] #add in english industry names
  s_f$code=indus$code[i] #add in level 2 industrial codes
#  s_f$parentcode=indus$parentCode[i] # add in parentcode
  s_g <- rbind(s_g, s_f)
}
s_g <- s_g %>%
  filter(quarter==4) %>%
  filter(year %in% unique(x$year))
