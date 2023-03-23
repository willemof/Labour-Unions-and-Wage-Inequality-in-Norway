library(survey)

install.packages("srvyr")
library(srvyr)

microdata <- read_csv(file = ("csv/microdata.csv"))

my_design <- svydesign(ids = ~1, data = microdata, weights = microdata$tuvekt)


my_design <- microdata %>% as_survey_design(weights = tuvekt)

weighted_stats <- mydesign %>% svydescribe()

x_m <- microdata


x_m<- x_m%>%
  mutate(is.male = fifelse(kjonn == "Mann", 1,0), .keep = "unused") %>%
  mutate(is.union = fifelse(tu29 == "Ja" , 1, 0, na = 0), .keep = "unused")

microdata <- x_m
plot <- ggplot(data=microdata, 
               mapping = aes(x = industryparentname, y = is.union))

plot
