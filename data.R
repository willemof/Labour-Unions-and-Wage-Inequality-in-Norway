#wage table with union rates for sector, industry, occupation, part-time, full-time

#Data Sources / Collection
monthlywage15_22 <- read_csv(file = ("csv/monthlywage15_22_long.csv"))
production13_17 <- read_csv(file = ("csv/production13_17.csv"))
wage13 <- read_csv(file=("csv/ssb/earningsindustry1314.csv"))
decilewage16 <- read_csv(file=("csv/decilewage16_22.csv"))
percentilewage16 <- read_csv(file=("csv/percentilewage16_22.csv"))
level2tolevel1indus <- read_csv("csv/ssb/level2tolevel1indus.csv")

bad_names <- c("Electricity, gas and steam",
               "Water supply, sewerage, waste",
               "Wholesale and retail trade: repair of motor vehicles and motorcycles",
               "Public administration and defence")
good_names <- c("Electricity, gas, steam and air conditioning supply",
                "Water supply; sewerage, waste management and remediation activities",
                "Wholesale and retail trade; repair of motor vehicles and motorcycles",
                "Public administration and defence; compulsory social security")
text_changer <- tibble(bad_names, good_names)

#Register Data Processing

filter_wage13 <- wage13 %>%
  filter(year %in% c("2013", "2014")) %>%
  filter((sex %in% c("Both sexes"))) 

wage13_filtered <- filter_wage13 %>%
  select(-c(sex))

remove(wage13, filter_wage13)
#######Monthly wage from 15
filter_monthlywage <- monthlywage15_22 %>%
  filter(year %in% c("2015", "2016", "2017")) %>%
  filter(occupation %in% c("All occupations")) %>%
  filter(sector %in% c("Sum all sectors")) %>%
  filter(!(industry_sic2007 %in% c("All industries"))) %>%
#                                   "Activities of household as employers",
#                                   "Activities of extraterritorial organisations and bodies",
#                                   "Unspecified"))) %>%
  filter((sex %in% c("Both sexes"))) %>%
  filter((sector %in% c("Sum all sectors"))) %>%
  filter((contractual_usual_working_hours_per_week %in% c("All employees")))
  
monthlywage15_22_expand <- pivot_wider(filter_monthlywage, 
                                       id_expand = FALSE,
                                       names_from = contents,
                                       values_from = c("value"))


monthlywage15_filtered <- pivot_wider(monthlywage15_22_expand,
                                       id_expand = FALSE,
                                       names_from = measuring_method,
                                       values_from = colnames(monthlywage15_22_expand[8:14]))

monthlywage15_filtered <- monthlywage15_filtered %>%
  select(-c(occupation, sector, sex, contractual_usual_working_hours_per_week))

x<- monthlywage15_filtered
#This series of if statements converts years, quarters and months into dates using zoo
if("month" %in% colnames(x)) {
  x<- separate(x, month, into = c("year", "month"), sep = "M")
  x$date  <- paste0(x$year,"-", x$month) 
  x$date <- as.Date(as.yearmon(x$date), frac = 1)
}else{ if("quarter" %in% colnames(x)) {
  x<- separate(x, quarter, into = c("year", "quarter"), sep = "K")
  x$date  <- paste0(x$year," Q", x$quarter) 
  x$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",x$date)),frac = 1)
}else{ x$date  <- paste0(x$year," Q4") 
x$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",x$date)),frac = 1)
}
}
monthlywage15_filtered <- x
remove(x, monthlywage15_22_expand, filter_monthlywage, monthlywage15_22)


x <- monthlywage15_filtered
x_loop <- x
x_g <- c()
x_t <- x_loop
x_f <- x_loop
codec <- text_changer 


for (i in 1:NROW(codec)) {
  x_f <- x_t %>%
    filter(x_t$industry_sic2007==codec$bad_names[i])
  x_t <- x_t %>%
    filter(x_t$industry_sic2007!=codec$bad_names[i])
  x_f$industry_sic2007=codec$good_names[i] #change bad names to good names
  
  x_g <- rbind(x_g, x_f)
}
x_g <- rbind(x_g, x_t)
x <- x_g %>%
  select(industry_sic2007, everything())

monthlywage15_filtered <- x
remove(x_f,x_t, x_g, x_loop)



#For merging 13-14, with 15,16- we need to get columns together
merging_wage15 <- monthlywage15_filtered
colnames(wage13_filtered)
merging_wage15 <- monthlywage15_filtered %>%
  select(c(date, industryparentname = industry_sic2007, year, 
           lower_quartile_nok = `Monthly earnings (NOK)_Lower quartile`,
           median_nok = `Monthly earnings (NOK)_Median`,
           mean_nok = `Monthly earnings (NOK)_Average`,
           upper_quartile_nok = `Monthly earnings (NOK)_Upper quartile`))
merging_wage13 <- wage13_filtered %>%
  select(c(industryparentname = parentname, everything()), 
         -c(industry_sic2007, employees_covered_by_the_survey))
wages13_17 <- rbind(merging_wage13, merging_wage15)


remove(merging_wage13, merging_wage15)


#Processing Microdata
#Survey data
microdata <- read_csv(file = ("csv/microdata.csv"))

microdata <- microdata %>%
  mutate(is.male = fifelse(kjonn == "Mann", 1,0), .keep = "unused") %>%
  mutate(is.union = fifelse(tu29 == "Ja" , 1, 0, na = 0), .keep = "all") %>%
  mutate(not.union = fifelse(tu29 == "Ja" , 0, 1, na = 1), .keep = "unused")


x_m <- microdata
#x_m <- x_m %>%
#  filter(!(parentcode_indus %in% c("U", "T","00.0")))


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
  mutate(sixty = fifelse(x_m$alder_aar >60 & x_m$alder_aar <= 74, 1,0, na = 0), .keep = "unused")


#dummy variable for sector
x_m<- x_m %>%
  mutate(sector.private = fifelse(x_m$eier == "Privat virksomhet", 1,0, na = 0), .keep = "unused") %>%
  mutate(sector.municipal = fifelse(x_m$eier == "Kommunal forvaltning", 1,0, na = 0), .keep = "unused") %>%
  mutate(sector.county = fifelse(x_m$eier == "Fylkeskommunal forvaltning", 1,0, na = 0), .keep = "unused") %>%
  mutate(sector.centralgov = fifelse(x_m$eier == "Statlig forvaltning", 1,0, na = 0), .keep = "unused")

#x_m <- x_m %>%
#  mutate(sector = fifelse(x_M$eier == "Privat virksomhet", "private sector", "", na ="")) %>%
#           mutate(sector = fifelse(x_M$eier == "Kommunal forvaltning", "public sector", "", na ="") |
#                 sector = fifelse(x_M$eier == "Statlig forvaltning", "public sector", "", na ="") |
#                 sector = fifelse(x_M$eier == "Fylkeskommunal forvaltning", "public sector", "", na ="")) 
                                  
                             
#dummy variable for employment status
x_m<- x_m %>%
  mutate(employment.onleave.employee = fifelse(x_m$sstat == "Midl.Fravær, ansatte", 1,0, na = 0), .keep = "unused") %>%
  mutate(employment.employee = fifelse(x_m$sstat == "Sysselsatte, ansatte", 1,0, na = 0), .keep = "unused") %>%
  mutate(employment.selfemployed = fifelse(x_m$sstat == "Sysselsatte, selvstendige", 1,0, na = 0), .keep = "unused") %>%
  mutate(employment.onleave.selfemployed = fifelse(x_m$sstat == "Midl.Fravær, selvstendige"  , 1,0, na = 0), .keep = "unused")%>%
  mutate(employment.onleave.familyemploy = fifelse(x_m$sstat == "Midl.Fravær, familiearbeider", 1,0, na = 0), .keep = "unused") %>%
  mutate(employment.familyemploy = fifelse(x_m$sstat == "Sysselsatte, familiearbeider" , 1,0, na = 0), .keep = "unused")%>%
  mutate(employment.employee.ungiven = fifelse(x_m$sstat == "Sysselsatte, uoppgitt", 1,0, na = 0), .keep = "unused")%>%
  mutate(employment.military = fifelse(x_m$sstat == "Vernepliktig" , 1,0, na = 0), .keep = "unused")


#dummy variable for employment agreement / collective agreement

x_m<- x_m %>%
  mutate(tu31 = fifelse(x_m$eier %in% c("Statlig forvaltning", "Kommunal forvaltning",
                                                             "Fylkeskommunal forvaltning")
                                            , "Helt eller delvis regulert av tariffavtale/overenskomst",x_m$tu31, na = "N/A"), .keep = "unused")


x_m<- x_m %>%
  mutate(collective.agreement.yes = fifelse(x_m$tu31 == "Helt eller delvis regulert av tariffavtale/overenskomst" |
                                              x_m$tu31 == "Helt eller delvis regulert av tariffavtale eller overenskoms"
                                            , 1,0, na = 0), .keep = "unused") %>%
  mutate(collective.agreement.combination = fifelse(x_m$tu31 == "Kombinasjon av tariffavtale eller overenskomst og individuel" |
                                                      x_m$tu31 == "Kombinasjon av tariffavtale eller overenskomst og individuel"
                                                    , 1,0, na = 0), .keep = "unused") %>%
  mutate(collective.agreement.no = fifelse(x_m$tu31 == "Reguleres kun gjennom individuell avtale eller kontrakt" |
                                             x_m$tu31 == "Reguleres kun gjennom individuell avtale / kontrakt"
                                           , 1,0, na = 0), .keep = "unused")



#Looking at unemployed population
x_e <- x_m %>%
  filter((betrkt %in% c("Arbeidsledig")))


# full time or part time
x_m<- x_m %>%
  mutate(fullemploy = fifelse(x_m$ans == "Fast ansatt", 1,0, na = 0), .keep = "unused") %>%
  mutate(tempemploy = fifelse(x_m$ans == "Midlertidig ansatt", 1,0, na = 0), .keep = "unused") ###I'm not sure if this is right, this isn't
#part time, this is a short-term contract / temporary <--- look into this

# full time or part time
x_m<- x_m %>%
  mutate(fulltime = fifelse(x_m$nj59e == "Heltidsjobb", 1,0, na = 0), .keep = "unused") %>%
  mutate(parttime = fifelse(x_m$nj59e == "Deltidsjobb", 1,0, na = 0), .keep = "unused")



#test adding in sectors in the style of industry
x_m <- x_m %>%
  mutate(sector = fifelse(x_m$eier %in% c("Statlig forvaltning", "Kommunal forvaltning",
                                                     "Fylkeskommunal forvaltning"),
                          "public sector", fifelse(x_m$eier %in% c("Privat virksomhet"),
                                                   "private sector", "N/A")))


x_m <- x_m %>%
  mutate(betrakt = fifelse((is.na(betrkt) & (x_m$sstat %in% c("Sysselsatte, ansatte", "Midl.Fravær, ansatte"))) 
                           | betrkt %in% c("Yrkesaktiv"), "Yrkesaktiv", "Ikke Yrkesaktiv"))


x_m <- x_m %>%
  select(betrakt, betrkt, sstat, everything())

x_m <- x_m %>%
  filter(!(sstat %in% c("Midl.Fravær, selvstendige", "Sysselsatte, selvstendige"))) %>%
  filter((betrakt %in% c("Yrkesaktiv")))


weighted_data <- x_m %>%
  as_survey_design(weights = tuvekt)

#weighted_data_e <- x_e %>%
#  as_survey_design(weights = tuvekt)

results <- weighted_data %>%
  group_by(year, industryparentname, parentcode_indus) %>%
  summarize(
    union_density = survey_mean(is.union, na.rm = TRUE),
    male_ratio = survey_mean(is.male, na.rm = TRUE),
    population_count = survey_total (na.rm = TRUE),
    #control variables: education
    has.education.primary = survey_mean(has.education.primary, na.rm =TRUE),
    has.education.juniorhigh = survey_mean(has.education.juniorhigh, na.rm =TRUE),
    has.education.some.hs = survey_mean(has.education.some.hs, na.rm =TRUE),
    has.education.finished.hs = survey_mean(has.education.finished.hs, na.rm =TRUE),
    has.education.bachelor = survey_mean(has.education.bachelor, na.rm =TRUE),
    has.education.master = survey_mean(has.education.master, na.rm =TRUE),
    has.education.doctor = survey_mean(has.education.doctor, na.rm =TRUE),
    has.education.ungiven = survey_mean(has.education.ungiven, na.rm =TRUE),
    #control variables: age
    teenager = survey_mean(lessthantwenty, na.rm =TRUE),
    twenties = survey_mean(twenty, na.rm =TRUE),
    thirties = survey_mean(thirty, na.rm =TRUE),
    fourties = survey_mean(fourty, na.rm =TRUE),
    fifties = survey_mean(fifty, na.rm =TRUE),
    sixtyplus = survey_mean(sixty, na.rm =TRUE),
    #seventies = survey_mean(seventyplus, na.rm =TRUE),
    #control variable: sector
    sector.private = survey_mean(sector.private, na.rm =TRUE),
    sector.municipal = survey_mean(sector.municipal, na.rm =TRUE),
    sector.county = survey_mean(sector.county, na.rm =TRUE),
    sector.centralgov = survey_mean(sector.centralgov, na.rm =TRUE),
    #control variable: collective agreement
    collective.agreement.yes = survey_mean(collective.agreement.yes, na.rm =TRUE),
    collective.agreement.combination = survey_mean(collective.agreement.combination, na.rm =TRUE),
    collective.agreement.no = survey_mean(collective.agreement.no, na.rm =TRUE),
    #control variable: employment type
    employment.onleave.employee = survey_mean(employment.onleave.employee, na.rm =TRUE),
    employment.employee = survey_mean(employment.employee, na.rm =TRUE),
    employment.selfemployed = survey_mean(employment.selfemployed, na.rm =TRUE),
    employment.onleave.selfemployed = survey_mean(employment.onleave.selfemployed, na.rm =TRUE),
    employment.onleave.familyemploy = survey_mean(employment.onleave.familyemploy, na.rm =TRUE),
    employment.familyemploy = survey_mean(employment.familyemploy, na.rm =TRUE),
    employment.employee.ungiven = survey_mean(employment.employee.ungiven, na.rm =TRUE),
    employment.military = survey_mean(employment.military, na.rm =TRUE),
    #control variable: full-time/parttime
    fulltime = survey_mean(fulltime, na.rm =TRUE),
    parttime = survey_mean(parttime, na.rm =TRUE),
    
    sample_size = n()
    ) %>%
  tibble()


sector_results <- weighted_data %>%
  group_by(year, sector) %>%
  summarize(
    union_density = survey_mean(is.union, na.rm = TRUE),
    male_ratio = survey_mean(is.male, na.rm = TRUE),
    population_count = survey_total (na.rm = TRUE),
    #control variables: education
    has.education.primary = survey_mean(has.education.primary, na.rm =TRUE),
    has.education.juniorhigh = survey_mean(has.education.juniorhigh, na.rm =TRUE),
    has.education.some.hs = survey_mean(has.education.some.hs, na.rm =TRUE),
    has.education.finished.hs = survey_mean(has.education.finished.hs, na.rm =TRUE),
    has.education.bachelor = survey_mean(has.education.bachelor, na.rm =TRUE),
    has.education.master = survey_mean(has.education.master, na.rm =TRUE),
    has.education.doctor = survey_mean(has.education.doctor, na.rm =TRUE),
    has.education.ungiven = survey_mean(has.education.ungiven, na.rm =TRUE),
    #control variables: age
    teenager = survey_mean(lessthantwenty, na.rm =TRUE),
    twenties = survey_mean(twenty, na.rm =TRUE),
    thirties = survey_mean(thirty, na.rm =TRUE),
    fourties = survey_mean(fourty, na.rm =TRUE),
    fifties = survey_mean(fifty, na.rm =TRUE),
    sixtyplus = survey_mean(sixty, na.rm =TRUE),
   # seventies = survey_mean(seventyplus, na.rm =TRUE),
    #control variable: sector
    sector.private = survey_mean(sector.private, na.rm =TRUE),
    sector.municipal = survey_mean(sector.municipal, na.rm =TRUE),
    sector.county = survey_mean(sector.county, na.rm =TRUE),
    sector.centralgov = survey_mean(sector.centralgov, na.rm =TRUE),
    #control variable: collective agreement
    collective.agreement.any = survey_mean(collective.agreement.yes|collective.agreement.combination, na.rm =TRUE),
    collective.agreement.yes = survey_mean(collective.agreement.yes, na.rm =TRUE),
    collective.agreement.combination = survey_mean(collective.agreement.combination, na.rm =TRUE),
    collective.agreement.no = survey_mean(collective.agreement.no, na.rm =TRUE),
    #control variable: employment type
    employment.onleave.employee = survey_mean(employment.onleave.employee, na.rm =TRUE),
    employment.employee = survey_mean(employment.employee, na.rm =TRUE),
    employment.selfemployed = survey_mean(employment.selfemployed, na.rm =TRUE),
    employment.onleave.selfemployed = survey_mean(employment.onleave.selfemployed, na.rm =TRUE),
    employment.onleave.familyemploy = survey_mean(employment.onleave.familyemploy, na.rm =TRUE),
    employment.familyemploy = survey_mean(employment.familyemploy, na.rm =TRUE),
    employment.employee.ungiven = survey_mean(employment.employee.ungiven, na.rm =TRUE),
    employment.military = survey_mean(employment.military, na.rm =TRUE),
    #control variable: full-time/parttime
    fulltime = survey_mean(fulltime, na.rm =TRUE),
    parttime = survey_mean(parttime, na.rm =TRUE),
    
    sample_size = n()
  ) %>%
  tibble() 


####Unemployed
#unemployed_results <- weighted_data_e %>%
#  group_by(year, parentcode_indus) %>%
#  summarize(
#    union_density = survey_mean(is.union, na.rm = TRUE),
#    male_ratio = survey_mean(is.male, na.rm = TRUE),
#    population_count = survey_total (na.rm = TRUE),
#    #control variables: education
#    has.education.primary = survey_mean(has.education.primary, na.rm =TRUE),
#    has.education.juniorhigh = survey_mean(has.education.juniorhigh, na.rm =TRUE),
#    has.education.some.hs = survey_mean(has.education.some.hs, na.rm =TRUE),
#    has.education.finished.hs = survey_mean(has.education.finished.hs, na.rm =TRUE),
#    has.education.bachelor = survey_mean(has.education.bachelor, na.rm =TRUE),
#    has.education.master = survey_mean(has.education.master, na.rm =TRUE),
#    has.education.doctor = survey_mean(has.education.doctor, na.rm =TRUE),
#    has.education.ungiven = survey_mean(has.education.ungiven, na.rm =TRUE),
#    #control variables: age
#    teenager = survey_mean(lessthantwenty, na.rm =TRUE),
#    twenties = survey_mean(twenty, na.rm =TRUE),
#    thirties = survey_mean(thirty, na.rm =TRUE),
#    fourties = survey_mean(fourty, na.rm =TRUE),
#    fifties = survey_mean(fifty, na.rm =TRUE),
##    sixtyplus = survey_mean(sixty, na.rm =TRUE),
#    seventies = survey_mean(seventyplus, na.rm =TRUE),
#    #control variable: sector
#    sector.private = survey_mean(sector.private, na.rm =TRUE),
#    sector.municipal = survey_mean(sector.municipal, na.rm =TRUE),
##    sector.county = survey_mean(sector.county, na.rm =TRUE),
#    sector.centralgov = survey_mean(sector.centralgov, na.rm =TRUE),
#    #control variable: collective agreement
#    collective.agreement.any = survey_mean(collective.agreement.yes|collective.agreement.combination, na.rm =TRUE),
#    collective.agreement.yes = survey_mean(collective.agreement.yes, na.rm =TRUE),
#    collective.agreement.combination = survey_mean(collective.agreement.combination, na.rm =TRUE),
#    collective.agreement.no = survey_mean(collective.agreement.no, na.rm =TRUE),
#    #control variable: employment type
#    employment.onleave.employee = survey_mean(employment.onleave.employee, na.rm =TRUE),
#    employment.employee = survey_mean(employment.employee, na.rm =TRUE),
#    employment.selfemployed = survey_mean(employment.selfemployed, na.rm =TRUE),
#    employment.onleave.selfemployed = survey_mean(employment.onleave.selfemployed, na.rm =TRUE),
#    employment.onleave.familyemploy = survey_mean(employment.onleave.familyemploy, na.rm =TRUE),
##    employment.familyemploy = survey_mean(employment.familyemploy, na.rm =TRUE),
#   employment.employee.ungiven = survey_mean(employment.employee.ungiven, na.rm =TRUE),
#    employment.military = survey_mean(employment.military, na.rm =TRUE),
#    sample_size = n()
#  ) %>%
#  tibble() 
#
#unemployed_results <- unemployed_results %>%
#  select(collective.agreement.any, everything())

##Unemployed end

#Sector stats
sector_results <- sector_results %>%
  select(collective.agreement.any, everything())





#Filtering away 2 industry categories

# Filter the data for the desired year
#year_data <- results %>%
#  filter(year == 2016) %>%
#  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - ")) %>%
#  arrange(desc(proportion))
#year_data_filtered <- year_data #%>%
#  filter(!parentcode_indus %in% c("00.0", "U", "T"))

# Because the main industries, T and U, represents such a small part of the labour market,
#The labour force survey has too few observations to make an accurate enough inference
#Thus we wil drop the industries so that are results can be more accurate.

#Dropping Main industries; T and U

#year_data_filtered <- year_data_filtered %>%
#  filter(!(parentcode_indus %in% c("T","U")))







full_merged_ds <- left_join(results, wages13_17)
# there add parentcodes where missing
x <- full_merged_ds
x_loop <- x
x_g <- c()
x_t <- x_loop
x_f <- x_loop
codec <- level2tolevel1indus %>%
  select(c(parentcode, parentname))
codec <- codec %>%
  group_by(parentcode, parentname) %>%
  summarize()

for (i in 1:NROW(codec)) {
  if(codec$parentname[i] %in% unique(x_t$industryparentname)==FALSE) {print(paste0("Something's wrong, I can feel it i = ", i, " codec <- ", codec$name[i]))
    next}
  x_f <- x_t %>%
    filter(x_t$industryparentname==codec$parentname[i])
  x_t <- x_t %>%
    filter(x_t$industryparentname!=codec$parentname[i])
  x_f$parentcode_indus=codec$parentcode[i] #add in english industry-parent names
  
  x_g <- rbind(x_g, x_f)
  }
x <- x_g %>%
  select(industryparentname, everything())
remove(x_f,x_t, x_g, x_loop)

full_merged_ds <- x #%>%
 # filter(!(parentcode_indus %in% c("00.0","U", "T")))

full_merged_ds <- full_merged_ds %>%
  filter(!(parentcode_indus %in% c("T","U")))

# Filter the data for the desired year
full_merged_ds_year <- full_merged_ds %>%
  filter(year == 2016) %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - ")) %>%
  arrange(parentcode_indus)




df <- full_merged_ds %>%
  na.omit #this is the full dataset

df <- df %>%
  mutate(collectivelybargained = (population_count*collective.agreement.yes + population_count*collective.agreement.combination), .keep = c("all")) %>%
  select(collectivelybargained, everything())
df <- df %>%
  mutate(collective_rate = collectivelybargained / population_count, .keep = c("all"))
# group the data by year and calculate the total collectively bargained amount for each year
summary_df <- df %>% 
  group_by(year = lubridate::year(date)) %>% 
  summarize(collectivelybargained = (population_count*collective.agreement.yes + population_count*collective.agreement.combination))

# join the summary data frame with the original data frame and calculate the national colle
summary_df <- df %>% 
  group_by(year = lubridate::year(date)) %>% 
  summarize(total_collectively_bargained = sum(population_count * collective_rate, na.rm = TRUE))
summary_df

sunny <- df %>%
  group_by(as.character(year)) %>% 
  summarize(total_total = sum(collectivelybargained, na.rm = TRUE))
sunny 


summy <- df %>%
  group_by(as.character(year)) %>% 
  summarize(total_total = sum(population_count, na.rm = TRUE))
summy 


sunny <- df %>%
  group_by(year = lubridate::year(date)) %>% 
  summarize
summary_df$year <- as.character(summary_df$year)
summary_dff <- df %>% 
  group_by(year = lubridate::year(date)) %>% 
  summarize(bargained = sum(population_count, na.rm = TRUE))
summary_dff$year <- as.character(summary_dff$year)
class(summary_dff$year)
summary_dff
national_rates <- tibble(year = summary_df$year, national_rate = summary_df$total_collectively_bargained / summary_dff$bargained)
national_rates
# group the data by year and calculate the summary statistic
summary_df <- df %>% 
  group_by(year = lubridate::year(date)) %>% 
  summarize(sum_colbargained = sum(collectivelybargained, na.rm = TRUE))

# add the summary column to the original data frame
df <- df %>% 
  left_join(summary_df, by = "year") %>%
  select(sum_colbargained, everything()) %>%
  filter(!(parentcode_indus %in% c("U", "T", "00.0")))

full_merged_ds_year <- df
#
#df$union_density_manuf <- ifelse(df$industryparentname == "Manufacturing",
#                                                 logdataset$union_density,
#                                                 0)
#
#
#summary_df <- df %>% 
#  group_by(year) %>% 
#  mutate(sum_colbargained =
#           summarize(total = sum(collectivelybargained, na.rm = TRUE)), .keep = c("all"))
# print the summary data frame
#summary_df
#
#
#



