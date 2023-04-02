#wage table with union rates for sector, industry, occupation, part-time, full-time

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


#continuing the filtering


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

#Survey data
microdata <- read_csv(file = ("csv/microdata.csv"))

microdata <- microdata %>%
  mutate(is.male = fifelse(kjonn == "Mann", 1,0), .keep = "unused") %>%
  mutate(is.union = fifelse(tu29 == "Ja" , 1, 0, na = 0), .keep = "all") %>%
  mutate(not.union = fifelse(tu29 == "Ja" , 0, 1, na = 1), .keep = "unused")


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
    sixties = survey_mean(sixty, na.rm =TRUE),
    seventies = survey_mean(seventyplus, na.rm =TRUE),
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






full_merged_ds <- full_join(results, wages13_17)
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

full_merged_ds <- x %>%
  filter(!(parentcode_indus %in% c("00.0","U", "T")))


# Filter the data for the desired year
full_merged_ds_year <- full_merged_ds %>%
  filter(year == 2017) %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - ")) %>%
  arrange(parentcode_indus)


# Create the crossbar plot
crossbar_plot <- ggplot(full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok, width = 0.7), position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok, width = 0.9), position = position_dodge(0.9), size = 1) +
  geom_point(aes(y = median_nok), color = "white", size = 2, position = position_dodge(0.9)) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", y = "Monthly Wage (NOK)", title = "Distribution of Median, Lower & Upper Quartile Wages (NOK) by Industry in 2017") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1))

# Print the crossbar plot
print(crossbar_plot)

# Create the crossbar plot with unionization rates
crossbar_plot_union <- ggplot(full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok, width = 0.7), position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok, width = 0.9), position = position_dodge(0.9), size = 1) +
  geom_point(aes(y = median_nok), color = "white", size = 2, position = position_dodge(0.9)) +
  geom_line(aes(y = union_density * 100000, group = 1, color = "Unionization Rate"), size = 1) +
  scale_fill_manual(values = custom_palette) +
  scale_y_continuous(name = "Monthly Wage (NOK)", sec.axis = sec_axis(~./100000, name = "Unionization Rate (%)")) +
  scale_color_manual(values = c("Unionization Rate" = "darkblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", title = "Distribution of Median, Lower & Upper Quartile Wages (NOK) & Unionization Rates by Industry in 2017") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1), color = guide_legend(title = NULL))

# Print the crossbar plot with unionization rates
print(crossbar_plot_union)


# Reorder the parentcode_indus factor levels by ascending unionization rate
full_merged_ds_year$parentcode_indus <- factor(
  full_merged_ds_year$parentcode_indus,
  levels = full_merged_ds_year[order(full_merged_ds_year$union_density), "parentcode_indus"]
)

# Reorder the parentcode_indus factor levels by ascending unionization rate
full_merged_ds_year$parentcode_indus <- with(full_merged_ds_year, reorder(parentcode_indus, union_density, FUN = median))

# Recreate the crossbar plot with unionization rates and sorted x-axis
crossbar_plot_union_sorted <- ggplot(full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok, width = 0.7), position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok, width = 0.9), position = position_dodge(0.9), size = 1) +
  geom_point(aes(y = median_nok), color = "white", size = 2, position = position_dodge(0.9)) +
  geom_line(aes(y = union_density * 100000, group = 1, color = "Unionization Rate"), size = 1) +
  scale_fill_manual(values = custom_palette) +
  scale_y_continuous(name = "Monthly Wage (NOK)", sec.axis = sec_axis(~./100000, name = "Unionization Rate (%)")) +
  scale_color_manual(values = c("Unionization Rate" = "darkblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", title = "Distribution of Median, Lower & Upper Quartile Wages (NOK) & Unionization Rates by Industry in 2017 (Sorted by Unionization Rate)") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1), color = guide_legend(title = NULL))

# Sort the data by unionization rate and reorder the factor levels for parentcode_indus
full_merged_ds_year_sorted <- full_merged_ds_year %>%
  arrange(union_density) %>%
  mutate(parentcode_indus = factor(parentcode_indus, levels = unique(parentcode_indus)))

# Recreate the crossbar plot with unionization rates and sorted x-axis
crossbar_plot_union_sorted <- ggplot(full_merged_ds_year_sorted, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok, width = 0.7), position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok, width = 0.9), position = position_dodge(0.9), size = 1) +
  geom_point(aes(y = median_nok), color = "white", size = 2, position = position_dodge(0.9)) +
  geom_line(data = full_merged_ds_year_sorted, aes(y = union_density * 100000, group = 1, color = "Unionization Rate"), size = 1) +
  scale_fill_manual(values = custom_palette) +
  scale_y_continuous(name = "Monthly Wage (NOK)", sec.axis = sec_axis(~./100000, name = "Unionization Rate (%)")) +
  scale_color_manual(values = c("Unionization Rate" = "darkblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", title = "Distribution of Median, Lower & Upper Quartile Wages (NOK) & Unionization Rates by Industry in 2017 (Sorted by Unionization Rate)") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1), color = guide_legend(title = NULL))

# Print the crossbar plot with unionization rates and sorted x-axis
print(crossbar_plot_union_sorted)


#Wage vs Labour Union Density, scatterplot


# Filter away data for the year 2015
full_merged_ds_filtered <- full_merged_ds %>%
  filter(!is.na(union_density))


# Create the scatterplot
scatter_plot <- ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = factor(year))) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(
    x = "Unionization Rate",
    y = "Mean Monthly Wage (NOK)",
    title = "Scatterplot of Mean Monthly Wage (NOK) vs. Labor Union Density by Year (excluding 2015)"
  )

# Print the scatterplot
print(scatter_plot)

# Create the scatterplot with black industry code labels inside colored points
scatter_plot <- ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = factor(year))) +
  geom_point(size = 3.5, alpha = 0.7) +
  geom_text(aes(label = parentcode_indus), color = "black", size = 3, fontface = "bold") +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(
    x = "Unionization Rate",
    y = "Mean Monthly Wage (NOK)",
    title = "Scatterplot of Mean Monthly Wage (NOK) vs. Labor Union Density by Year and Industry (excluding 2015)"
  )

# Print the scatterplot
print(scatter_plot)

# Regression
ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = as.factor(year), label = parentcode_indus)) +
  geom_point(size = 3) +
  geom_text(aes(label = parentcode_indus), color = "black", size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = c("2013" = "pink", "2014" = "lightblue", "2016" = "green", "2017" = "purple")) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean Wage (NOK)", color = "Year", title = "Mean Wage vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Year"))

# Create a new variable for the mean-median gap
full_merged_ds_filtered <- full_merged_ds_filtered %>%
  mutate(mean_median_gap = mean_nok - median_nok)

# Create a scatterplot with the mean-median gap on the y-axis
ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = as.factor(year), label = parentcode_indus)) +
  geom_point(size = 3) +
  geom_text(aes(label = parentcode_indus), color = "black", size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = c("2013" = "red", "2014" = "blue", "2016" = "green", "2017" = "purple")) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Year", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Year"))



# Create a scatterplot with the mean-median gap on the y-axis
full_merged_ds_filtered <- full_merged_ds_filtered %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - "))

ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = industry_label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = full_merged_ds_filtered$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Industry", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))

#no white text but good legend

# Create a scatterplot with the mean-median gap on the y-axis
full_merged_ds_filtered <- full_merged_ds_filtered %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - "))

legend_labels <- full_merged_ds_filtered %>%
  select(parentcode_indus, industry_label) %>%
  distinct() %>%
  arrange(parentcode_indus)

ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = industry_label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = legend_labels$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Industry", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))


#White text on point, mean median gap labour union density regression
full_merged_ds_filtered <- full_merged_ds_filtered %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - "),
         year_label = as.character(year - 2010))

ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = industry_label)) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_text(aes(label = year_label), color = "white", size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = legend_labels$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Industry", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))



