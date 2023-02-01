#Wages, cpi and production
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


ssb_gg <- ggplot(
  data = ssb_select,
  mapping = aes(x = date, y = wages_and_salaries_nok_million)) +
  geom_line() + geom_point()

ssb_gg

plotly::

ggplot() +
  geom_line(data = ssb, aes(x=date, wages_and_salaries_nok_million), color = "red") +
  geom_line(data = ssb, aes(x=date, wages_and_salaries_nok_million*0.01*consumer_price_index_2015_100), color = "blue") + 
  scale_linetype_manual() +
  xlab('date') +
  ylab('wages (nok million)')

Series1$series = 1
Series2$series = 2
all_series = rbind(Series1, Series2)

ggplot(ssb,
       aes(x = date, y = wages_and_salaries_nok_million, color = wages_and_salaries_nok_million*0.01*consumer_price_index_2015_100, linetype = factor(ssb))) + 
  geom_line()


