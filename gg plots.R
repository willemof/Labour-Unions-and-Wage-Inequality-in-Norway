#GG plots
library(ggplot2)
library(plotly)
#wage

ggplot(data=ssb.wages.year,
       aes(x=date, y= industry))



ggplot(data=filter_wages_2016,
       aes(x=industry, y= filter_wages_2016$wages_and_salaries_nok_million/filter_wages_2016$employed_persons_employees_and_self_employed_1_000_persons))+
  geom_point()


##plotly plots

stringdetect <- str_subset(ssb_select$industry,"Mainland") #to detect and filter ¬
ssb_filter <- ssb_select %>%
  filter (!(industry %in%  stringdetect)) %>%  #filtering everything that doesn't include "Mainland"
filter(industry != "Total industry") %>%
  filter(quarter==4)

#stringdetect <- str_subset(ssb_filter$industry,"¬") #to detect and filter ¬
#ssb_filter_subindustry <- ssb_filter %>%
#filter(industry %in% stringdetect)




fig <- plot_ly(data=ssb_filter,
               x= ssb_filter$industry,
               y= ssb_filter$wages_and_salaries_nok_million)

fig <- fig %>%
  layout=title='Wages and LUD across time',
data = ssb_select,
x


