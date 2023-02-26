#run after industry_levels.R


# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path) )

#loading industry codec
level2tolevel1indus <- read_csv("csv/ssb/level2tolevel1indus.csv")
indus_level2 <- read_csv("csv/ssb/indus_level2.csv")
occu <- read_csv("csv/ssb/indus_level2.csv")



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
x_g <- c()
x_t <- x #dataset to "mutate"
x_f <- x#dataset to "mutate"
x_f$occupation <- c()
codec <- occu %>%
  filter(level==2)
x_t <- x_t %>%
  mutate(y_kode1_styrk08 = as.character(y_kode1_styrk08),
         two_digit_code = ifelse(y_kode1_styrk08 == "0", "00", ifelse(nchar(y_kode1_styrk08) == 3, str_pad(y_kode1_styrk08, width = 4, pad = "0"), substr(y_kode1_styrk08, 1, 2)))) %>%
  mutate(two_digit_code = substr(two_digit_code, 1, 2))

for (i in 1:NROW(codec)) {
  x_f <- x_t %>%
    filter(x_t$two_digit_code==codec$code[i])
  x_t <- x_t %>%
    filter(x_t$two_digit_code!=codec$code[i])
  x_f$occupation=codec$name[i] #add in english occupation names
  x_g <- rbind(x_g, x_f)
  if(i == NROW(codec)){
    x_m <- x_g
  }
}


#x <- x_g %>%
#  select(occupation, everything())

remove(x_f,x_t, x_g, x_loop)

x_m<- x_m%>%
  mutate(is.male = fifelse(kjonn == "Mann", 1,0), .keep = "unused") %>%
  mutate(is.union = fifelse(tu29 == "Ja" , 1, 0, na = 0), .keep = "unused")

#dummy variable for education from least educated to most educated
x_m<- x_m %>%
  mutate(has.education.primary = fifelse(x_m$utd_bu == "Barneskoleutdanning", 1,0, na = 0), .keep = "unused") %>%
  mutate(has.education.juniorhigh = fifelse(x_m$utd_bu == "Ungdomsskoleutdanning", 1,0, na = 0), .keep = "unused") %>%
  mutate(has.education.some.hs = fifelse(x_m$utd_bu == "Videregående skole, nivå i - grunnkurs +vk1", 1,0, na = 0), .keep = "unused") %>%
  mutate(has.education.finished.hs = fifelse(x_m$utd_bu == "Videregående skole, nivå ii - vk2", 1,0, na = 0)) %>%
  mutate(has.education.trade.school = fifelse(x_m$utd_bu == "Påbygging til videregående opplæring (teknisk fagskole)", 1,0, na = 0), .keep = "unused") %>%
  mutate(has.education.bachelor = fifelse(x_m$utd_bu == "Univ.-høyskoleutdanning, lavere nivå", 1,0, na = 0), .keep = "unused") %>%
  mutate(has.education.master = fifelse(x_m$utd_bu == "Univ.-høyskoleutdanning, høyere nivå", 1,0, na = 0), .keep = "unused") %>%
  mutate(has.education.doctor = fifelse(x_m$utd_bu == "Forskerutdanning", 1,0, na = 0), .keep = "unused") %>%
  mutate(has.education.ungiven = fifelse(x_m$utd_bu == "Uoppgitt", 1,0, na = 0), .keep = "unused") 
  
#dummy variable for age
x_m<- x_m %>%
  mutate(lessthantwenty = fifelse(x_m$alder_aar <= 20, 1,0, na = 0), .keep = "unused") %>%
  mutate(twenty = fifelse(x_m$alder_aar >20 & x_m$alder_aar <= 30, 1,0, na = 0), .keep = "unused") %>%
  mutate(thirty = fifelse(x_m$alder_aar >30 & x_m$alder_aar <= 40, 1,0, na = 0), .keep = "unused") %>%
  mutate(fourty = fifelse(x_m$alder_aar >40 & x_m$alder_aar <= 50, 1,0, na = 0), .keep = "unused") %>%
  mutate(fifty = fifelse(x_m$alder_aar >50 & x_m$alder_aar <= 60, 1,0, na = 0), .keep = "unused") %>%
  mutate(sixty = fifelse(x_m$alder_aar >60 & x_m$alder_aar <= 70, 1,0, na = 0), .keep = "unused") %>%
  mutate(seventyplus = fifelse(x_m$alder_aar >70, 1,0, na = 0), .keep = "unused") 
  

#dummy variable for sector
x_m<- x_m %>%
  mutate(sector.private = fifelse(x_m$eier == "Privat virksomhet", 1,0, na = 0), .keep = "unused") %>%
  mutate(sector.municipal = fifelse(x_m$eier == "Kommunal forvaltning", 1,0, na = 0), .keep = "unused") %>%
  mutate(sector.county = fifelse(x_m$eier == "Fylkeskommunal forvaltning", 1,0, na = 0), .keep = "unused") %>%
  mutate(sector.centralgov = fifelse(x_m$eier == "Statlig forvaltning", 1,0, na = 0), .keep = "unused")

#dummy variable for employment status
x_m<- x_m %>%
  mutate(employment.onleave.employee = fifelse(x_m$eier == "Midl.Fravær, ansatte", 1,0, na = 0), .keep = "unused") %>%
  mutate(employment.employee = fifelse(x_m$eier == "Sysselsatte, ansatte", 1,0, na = 0), .keep = "unused") %>%
  mutate(employment.selfemployed = fifelse(x_m$eier == "Sysselsatte, selvstendige", 1,0, na = 0), .keep = "unused") %>%
  mutate(employment.onleave.selfemployed = fifelse(x_m$eier == "Midl.Fravær, selvstendige"  , 1,0, na = 0), .keep = "unused")%>%
  mutate(employment.onleave.familyemploy = fifelse(x_m$eier == "Midl.Fravær, familiearbeider", 1,0, na = 0), .keep = "unused") %>%
  mutate(employment.familyemploy = fifelse(x_m$eier == "Sysselsatte, familiearbeider" , 1,0, na = 0), .keep = "unused")%>%
  mutate(employment.employee.ungiven = fifelse(x_m$eier == "Sysselsatte, uoppgitt", 1,0, na = 0), .keep = "unused")%>%
  mutate(employment.military = fifelse(x_m$eier == "Vernepliktig" , 1,0, na = 0), .keep = "unused")


#dummy variable for employment.duration

x_m<- x_m %>%
  mutate(collective.agreement.yes = fifelse(x_m$eier == "Helt eller delvis regulert av tariffavtale/overenskomst" |
                                          x_m$eier == "Helt eller delvis regulert av tariffavtale eller overenskoms"
                                            , 1,0, na = 0), .keep = "unused") %>%
  mutate(collective.agreement.combination = fifelse(x_m$eier == "Kombinasjon av tariffavtale eller overenskomst og individuel" |
                                            x_m$eier == "Kombinasjon av tariffavtale eller overenskomst og individuel"
                                            , 1,0, na = 0), .keep = "unused") %>%
  mutate(collective.agreement.no = fifelse(x_m$eier == "Reguleres kun gjennom individuell avtale eller kontrakt" |
                                                      x_m$eier == "Reguleres kun gjennom individuell avtale / kontrakt"
                                                    , 1,0, na = 0), .keep = "unused")
  

# workplace regulated by collective agreement
x_m<- x_m %>%
  mutate(fulltime = fifelse(x_m$eier == "Fast ansatt", 1,0, na = 0), .keep = "unused") %>%
  mutate(parttime = fifelse(x_m$eier == "Midlertidig ansatt", 1,0, na = 0), .keep = "unused") 


# Create a vector of all possible 2-digit occupation codes
all_codes <- sprintf("%02d", 0:99)

# Create a vector of occupation names corresponding to each code
name_vector <- character(length(all_codes))
for (i in 1:length(all_codes)) {
  code <- all_codes[i]
  name_vector[i] <- ifelse(code %in% codec$code, codec$name[codec$code == code], "")
}

# Remove any columns with empty column names
dummy_matrix <- dummy_matrix[, name_vector != ""]

# Add the dummy variables to the x_m data frame
x_m <- cbind(x_m, dummy_matrix)


x_a <- x_m %>%
  select(tuvekt, parentcode_indus, industryparentname, year, colnames(x_m)[144:161])
x_agg_vis <- aggregate(x_a, by = list(x_a$parentcode_indus,
                                      x_a$industryparentname,
                                      x_a$year), FUN = mean)

x_agg_vis_length <- x_a %>%
  select(industryparentname, year, parentcode_indus) 
x_agg_vis_length<-   aggregate(x_agg_vis_length, by = list(x_agg_vis_length$parentcode_indus,
                                                           x_agg_vis_length$industryparentname,
                                                           x_agg_vis_length$year), FUN = length)
x_agg_vis_length <- x_agg_vis_length %>%
  mutate(nfreq = parentcode_indus, .keep = "unused")

x_agg_vis_length <- x_agg_vis_length %>%     select(-c( industryparentname,
                                         year)) 

x_agg_vis_length <- x_agg_vis_length %>% 
  rename(parentcode_indus=       colnames(x_agg_vis_length[1]))%>% 
  rename(industryparentname=  colnames(x_agg_vis_length[2])) %>%
  rename(year=  colnames(x_agg_vis_length[3]))



x_agg_vis <- x_agg_vis %>%     select(-c(parentcode_indus, 
                                             industryparentname,
                                 year)) 



x_agg_vis <- x_agg_vis %>% 
  rename(parentcode_indus=       colnames(x_agg_vis[1]))%>% 
  rename(industryparentname=  colnames(x_agg_vis[2])) %>%
  rename(year=  colnames(x_agg_vis[3]))

x_agg_vis <- full_join(x_agg_vis, x_agg_vis_length)


write_csv(x_agg_vis, file="csv/ssb/x_agg_vis.csv")

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
write_csv(x, file = ("csv/x.csv"))
write_csv(x_agg, file = ("csv/x_agg.csv"))
