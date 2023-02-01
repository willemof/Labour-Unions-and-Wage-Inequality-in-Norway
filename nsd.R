#run after industry_levels.R

library(rstudioapi)
library(httr)
library(rjstat) #For JSON
library(tidyverse)
library(janitor)
library(dplyr)
library(zoo)


# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))



library(data.table) #for fast if else


x2013 <- read_csv("csv/2013.csv")
x2014 <- read_csv("csv/2014.csv")
x2016 <- read_csv("csv/2016.csv")
x2017 <- read_csv("csv/2017.csv")


x <- full_join(x2013, x2014)
x <- full_join(x, x2016)
x <- full_join(x, x2016)
x <- full_join(x, x2017)

remove(x2013,x2014,x2016,x2017)

x <- clean_names(x)
x$hnar <- str_sub(x$hnar,1,45)
x$year <- paste0(x$year)

x$number <- paste0(1:NROW(x))
x <- x %>% select(number, hnar, everything())

x_g <- c()
x_t <- x %>%select(number, hnar)
x_f <- x %>%select(number, hnar)
for (i in 1:NROW(indus)) {
x_f <- x_t %>%
  filter(x_t$hnar==indus$navn[i])
x_t <- x_t %>%
  filter(x_t$hnar!=indus$navn[i])
  x_f$hnar=indus$name[i] #add in english industry names
  x_f$code=indus$code[i] #add in level 2 industrial codes
  x_f$parentcode=indus$parentCode[i] # add in parentcode
x_g <- rbind(x_g, x_f)
}
x$hnar <- c()
x_g <- left_join(x_g, x)
x_g <- x_g %>%
  select(number, parentcode, code, everything())

x<- x_g %>%
  mutate(is.male = fifelse(kjonn == "Mann", 1,0)) %>%
  mutate(is.union = fifelse(tu29 == "Ja" , 1, 0, na = 0))

class(x$code)
x_agg <- aggregate(x, by = list(industry = x$hnar,
                                x$code,
                                x$parentcode,
                                x$year
                                ),FUN = mean)


x_agg <- x_agg %>% 
  select(-c(code, parentcode, year), everything())%>% 
  mutate(code=Group.2,.keep = c("unused"))%>% 
  mutate(parentcode=Group.3,.keep = c("unused"))%>% 
  mutate(year=Group.4,.keep = c("unused"))
x_agg <- x_agg %>%
  select(c(code, parentcode, year, industry, is.male, is.union, alder_aar,alder_uke), everything())

ssb_selected <- ssb_select %>%
  filter(quarter==4) 
nsdssb <- inner_join(x_agg, ssb_selected)
nsdssb <- nsdssb %>%
  filter(year==2016)

fig <- plot_ly(data=nsdssb,
               x= nsdssb$industry,
               y= nsdssb$wages_and_salaries_nok_million)
fig
#ggplotly
ggplot(data=nsdssb,
       aes(x= wages_and_salaries_nok_million, y= is.male))+
  geom_point()

