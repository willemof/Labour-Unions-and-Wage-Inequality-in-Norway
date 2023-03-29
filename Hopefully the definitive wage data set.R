#wage table with union rates for sector, industry, occupation, part-time, full-time

monthlywage15_22 <- read_csv(file = ("csv/monthlywage15_22_long.csv"))
production13_17 <- read_csv(file = ("csv/production13_17.csv"))
wage13 <- read_csv(file=("csv/ssb/earningsindustry1314.csv"))
decilewage16 <- read_csv(file=("csv/decilewage16_22.csv"))
percentilewage16 <- read_csv(file=("csv/percentilewage16_22.csv"))


filter_wage13 <- wage13 %>%
  filter(year %in% c("2013", "2014")) %>%
  filter((sex %in% c("Both sexes"))) 

wage13_expand <- pivot_wider(filter_wage13, 
                                       id_expand = FALSE,
                                       names_from = contents,
                                       values_from = c("value"))


wage13 <- pivot_wider(monthlywage15_22_expand,
                             id_expand = FALSE,
                             names_from = measuring_method,
                             values_from = colnames(monthlywage15_22_expand[8:14]))




#######Monthly wage from 15
filter_monthlywage <- monthlywage15_22 %>%
  filter(year %in% c("2015", "2016", "2017")) %>%
  filter(occupation %in% c("All occupations")) %>%
  filter(sector %in% c("Sum all sectors")) %>%
  filter(!(industry_sic2007 %in% c("All industries",
                                   "Activities of household as employers",
                                   "Activities of extraterritorial organisations and bodies",
                                   "Unspecified"))) %>%
  filter((sex %in% c("Both sexes"))) %>%
  filter((sector %in% c("Sum all sectors"))) %>%
  filter((contractual_usual_working_hours_per_week %in% c("All employees")))
  
monthlywage15_22_expand <- pivot_wider(filter_monthlywage, 
                                       id_expand = FALSE,
                                       names_from = contents,
                                       values_from = c("value"))


monthlywage16 <- pivot_wider(monthlywage15_22_expand,
                                       id_expand = FALSE,
                                       names_from = measuring_method,
                                       values_from = colnames(monthlywage15_22_expand[8:14]))

remove(monthlywage15_22_expand, filter_monthlywage, monthlywage15_22)



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


x_m <- microdata
x_m <- x_m %>%
  filter(!(parentcode_indus %in% c("U", "T","00.0")))

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



weighted_data <- x_m %>%
  as_survey_design(weights = tuvekt)


results <- weighted_data %>%
  group_by(year, industryparentname) %>%
  summarize(
    union_density = survey_mean(is.union, na.rm = TRUE),
    male_ratio = survey_mean(is.male, na.rm = TRUE),
    population_count = survey_total (na.rm = TRUE),
    sample_size = n()
    ) %>%
  tibble()
  
  

## visualizing distribution of industries
micro_data <- x_m


# Sampled proportions by year
sampled_proportions <- microdata %>%
  count(year, industryparentname, parentcode_indus) %>%
  group_by(year) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  mutate(data_type = "Sampled") %>%
  rename(weighted_n = n)

# Weighted proportions by year
weighted_proportions <- microdata %>%
  group_by(year, industryparentname, parentcode_indus) %>%
  summarize(weighted_n = sum(tuvekt)) %>%
  group_by(year) %>%
  mutate(proportion = weighted_n / sum(weighted_n)) %>%
  ungroup() %>%
  mutate(data_type = "Weighted")



# Combine the two data sets
combined_proportions <- rbind(sampled_proportions, weighted_proportions)

library(viridis)

# Filter the data to include only the year 2017
data_2017 <- combined_proportions %>%
  filter(year == 2017) %>%
  filter(!(parentcode_indus %in% c("U", "T", "00.0")))

# Filter the weighted_proportions data to include only the year 2017
weighted_proportions_2017 <- weighted_proportions %>%
  filter(year == 2017) %>%
  filter(!(parentcode_indus %in% c("U", "T", "00.0")))

# Custom color palette
custom_palette <- c(
  "#000000", "#FF0000", "#00FF00", "#0000FF",
  "#FFFF00", "#FF00FF", "#00FFFF", "#F99999",
  "#008000", "#000080", "#808000", "#800080",
  "#008080", "#8F6999", "#808080", "#FFA500",
  "#A52A2A", "#C0C0C0", "#2E8B57"
)

# Filter the data for the desired year
year_data <- weighted_proportions %>%
  filter(year == 2017) %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - ")) %>%
  arrange(desc(proportion))
year_data_filtered <- year_data %>%
  filter(!parentcode_indus %in% c("00.0", "U", "T"))

# Calculate breaks for the primary y-axis
primary_breaks <- scales::pretty_breaks()(year_data_filtered$proportion)

# Calculate breaks for the secondary y-axis
secondary_breaks <- primary_breaks * sum(year_data_filtered$weighted_n) / 1000


# Create the bar plot
bar_plot <- ggplot(year_data_filtered, aes(x = parentcode_indus, y = proportion, fill = industry_label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette) +
  scale_y_continuous(
    breaks = primary_breaks,
    sec.axis = sec_axis(
      trans = ~. * sum(year_data_filtered$weighted_n) / 1000,
      name = "Number of Employees (1 000)",
      labels = scales::number_format(accuracy = 1, scale = 1, big.mark = ",", label.padding = 0.5),
      breaks = secondary_breaks
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", y = "Proportion of Labor Force", title = "Proportion of Labor Force by Industry in 2017") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1))

# Print the plot
print(bar_plot)


# Create the pie chart
pie_chart <- ggplot(year_data_filtered, aes(x = "", y = proportion, fill = industry_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = custom_palette) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(fill = "Industry", title = "Proportion of Labor Force by Industry in 2017")

# Print the pie chart
print(pie_chart)


