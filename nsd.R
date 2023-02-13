#run after industry_levels.R


# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path) )


#Load csv by year
x2013 <- read_csv("csv/2013.csv")
x2014 <- read_csv("csv/2014.csv")
x2016 <- read_csv("csv/2016.csv")
x2017 <- read_csv("csv/2017.csv")

#merge
x <- full_join(x2013, x2014)
x <- full_join(x, x2016)
x <- full_join(x, x2016)
x <- full_join(x, x2017)

#clean/remove
remove(x2013,x2014,x2016,x2017)

x <- clean_names(x)

#shaving the first 45 characters of string, because it can't be merged with
#indus otherwise to extract code
x$hnar <- str_sub(x$hnar,1,45)
x$year <- paste0(x$year)

#listing entry number just for tidyness
x$number <- paste0(1:NROW(x))
x <- x %>% select(number, hnar, everything())

#setting up for loop to enter industry names codes, parent codes
x_loop <- x
x_g <- c()
x_t <- x
x_f <- x

#forloop
for (i in 1:NROW(indus_level2)) {
  if(indus_level2$navn[i] %in% unique(x_t$hnar)==FALSE) {next}
x_f <- x_t %>%
  filter(x_t$hnar==indus_level2$navn[i])
x_t <- x_t %>%
  filter(x_t$hnar!=indus_level2$navn[i])
  x_f$industry=indus_level2$name[i] #add in english industry names
  x_f$code_indus=indus_level2$code[i] #add in level 2 industrial codes
  x_f$parentcode_indus=indus_level2$parentcode[i] # add in parentcode
x_g <- rbind(x_g, x_f)
}

x_g <- x_g %>%
  select(-c(hnar)) %>%
  select(c(number, code_indus, everything()))

x <- x_g 

remove(x_f,x_t, x_g, x_loop)

# adding parentcode/industry main categories
x_loop <- x
x_g <- c()
x_t <- x_loop
x_f <- x_loop
x_f$industryparentname <- c()
for (i in 1:NROW(level2tolevel1indus)) {
  if(level2tolevel1indus$name[i] %in% unique(x_t$industry)==FALSE) {next}
  x_f <- x_t %>%
    filter(x_t$industry==level2tolevel1indus$name[i])
  x_t <- x_t %>%
    filter(x_t$industry!=level2tolevel1indus$name[i])
  x_f$industryparentname=level2tolevel1indus$parentname[i] #add in english industry-parent names
  x_g <- rbind(x_g, x_f)
}
x <- x_g %>%
  select(industryparentname, everything())
remove(x_f,x_t, x_g, x_loop)

##adding occupation names
x_loop <- x 
x_g <- c()
x_t <- x_loop
x_f <- x_loop
x_f$occupation <- c("")
for (i in 1:NROW(occu)) {
  if(occu$code[i] %in% unique(x_t$y_kode1_styrk08==FALSE)) {next}
  x_f <- x_t %>%
    filter(x_t$y_kode1_styrk08==occu$code[i])
  x_t <- x_t %>%
    filter(x_t$y_kode1_styrk08!=occu$code[i])
  x_f$occupation=occu$name[i] #add in english occupation names
  x_g <- rbind(x_g, x_f)
}
x <- x_g %>%
  select(occupation, everything())

remove(x_f,x_t, x_g, x_loop)

x<- x%>%
  mutate(is.male = fifelse(kjonn == "Mann", 1,0)) %>%
  mutate(is.union = fifelse(tu29 == "Ja" , 1, 0, na = 0))


x <- x %>%
  select(alder_aar, everything())

x_alder_agg <- aggregate(x, by = list(x$parentcode_indus,
                                      x$year), FUN = mean)


x_alder_agg <- x_alder_agg %>%     select(-c(parentcode_indus, 
                                 year)) 



x_alder_agg <- x_alder_agg %>% 
  rename(parentcode_indus=       colnames(x_alder_agg[1]))%>% 
  rename(year=  colnames(x_alder_agg[2]))




x_agg <- aggregate(x, by = list(x$occupation,
                                x$y_kode1_styrk08,
                                x$industry,
                                x$code_indus,
                                x$parentcode_indus,
                                x$year
                                ),FUN = mean)


#removes possibly redudant variables due to aggregation, and renames
#the aggregated values to the old variables
x_agg <- x_agg %>%     select(-c(code_indus, 
                                 industry,
                                parentcode_indus, 
                                occupation, 
                                y_kode1_styrk08,
                                year)) 
x_agg <- x_agg %>% 
  rename(occupation=       colnames(x_agg[1]))%>% 
  rename(code_occupation=  colnames(x_agg[2]))%>%
  rename(industry=         colnames(x_agg[3]))%>% 
  rename(code_indus=       colnames(x_agg[4]))%>% 
  rename(parentcode_indus= colnames(x_agg[5]))%>% 
  rename(year=             colnames(x_agg[6]))%>% 
  rename(quarter=kvartal)

#reordering the variables
x_agg <- x_agg %>%        select(c(occupation,
                                   code_indus, 
                                   parentcode_indus, 
                                   year, industry, 
                                   is.male, 
                                   is.union, 
                                   alder_aar,
                                   alder_uke), 
                                 everything())

#making sure nsd variables are characters so that it can be merged with ssb data
x_agg$quarter <- as.character(x_agg$quarter)
x_agg$parentcode_indus <- as.character(x_agg$parentcode)


#NSD data is ready to be merged! x for total set, x_agg for aggregated set
write.csv(x, file = ("csv/x.csv"))
write.csv(x_agg, file = ("csv/x_agg.csv"))
