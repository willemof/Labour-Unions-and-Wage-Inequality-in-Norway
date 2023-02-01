#inner join

library(rstudioapi)

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
x_filter_ja <- x %>%
  filter(year==2016) %>%
  filter(tu29 == "Ja")

x_select <- x %>%
  select(year, kjonn, hnar,bnar, tu29) %>%
  filter(year == 2016)

x_lud_2016 <- x_select %>%
  mutate(is.male = fifelse(kjonn == "Mann", 1,0))


x_lud_2016 <- x_lud_2016 %>%
  mutate(is.union = fifelse(tu29 == "Ja" , 1, 0, na = 0))


sum(x_lud_2016$is.male)/NROW(x_lud_2016$is.male)

library(stats)
x_lud_2016_agg <- aggregate(x_lud_2016, by = list(x_lud_2016$hnar), FUN = mean)
x_lud_2016_agg <- x_lud_2016_agg %>% rename(navn = Group.1)

join <- full_join(x_lud_2016_agg, indus_filter)
join$year <- as.character(join$year)
join$date  <- paste0(join$year,"-12") 
join$date <- as.Date(as.yearmon(join$date), frac = 1)

unique(join$navn)


join <- left_join(join, ssb_select)
join <- join %>%
  filter(year == 2016)
