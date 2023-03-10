#wage table with union rates for sector, industry, occupation, part-time, full-time

monthlywage15_22 <- read_csv(file = ("csv/monthlywage15_22.csv"))


filter_monthlywage <- monthlywage15_22 %>%
  filter(year %in% c("2016", "2017"))

monthlywage15_22_expand <- pivot_wider(filter_monthlywage, 
                                       id_expand = FALSE,
                                       names_from = contents,
                                       values_from = c("value"))


monthlywage15_22_expand <- pivot_wider(monthlywage15_22_expand, 
                                       id_expand = FALSE,
                                       names_from = measuring_method,
                                       values_from = colnames(monthlywage15_22_expand[8:14]))

monthlywage16_17 <- write_csv(monthlywage15_22_expand, file = ("csv/monthlywage16_17.csv"))


microdata <- read_csv(file = ("csv/microdata.csv"))

microdata <- microdata %>%
  mutate(is.male = fifelse(kjonn == "Mann", 1,0), .keep = "unused") %>%
  mutate(is.union = fifelse(tu29 == "Ja" , 1, 0, na = 0), .keep = "all") %>%
  mutate(not.union = fifelse(tu29 == "Ja" , 0, 1, na = 1), .keep = "unused")

pivot_table <- microdata %>%
  group_by(year, industryparentname, occupation, eier, heldelt, is.male, is.union) %>%
  summarize(num_employees = n()) %>%
  pivot_wider(names_from = is.union, values_from = num_employees, values_fill = 0)%>%
  mutate(total = `TRUE` + `FALSE`,
         union_rate = `TRUE` / total)





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
  mutate(collective.agreement.yes = fifelse(x_m$tu31 == "Helt eller delvis regulert av tariffavtale/overenskomst" |
                                              x_m$tu31 == "Helt eller delvis regulert av tariffavtale eller overenskoms"
                                            , 1,0, na = 0), .keep = "unused") %>%
  mutate(collective.agreement.combination = fifelse(x_m$tu31 == "Kombinasjon av tariffavtale eller overenskomst og individuel" |
                                                      x_m$tu31 == "Kombinasjon av tariffavtale eller overenskomst og individuel"
                                                    , 1,0, na = 0), .keep = "unused") %>%
  mutate(collective.agreement.no = fifelse(x_m$tu31 == "Reguleres kun gjennom individuell avtale eller kontrakt" |
                                             x_m$tu31 == "Reguleres kun gjennom individuell avtale / kontrakt"
                                           , 1,0, na = 0), .keep = "unused")


# full time or part time
x_m<- x_m %>%
  mutate(fulltime = fifelse(x_m$ans == "Fast ansatt", 1,0, na = 0), .keep = "unused") %>%
  mutate(parttime = fifelse(x_m$ans == "Midlertidig ansatt", 1,0, na = 0), .keep = "unused") 

