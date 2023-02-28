#This file merges nsd data with wage data from 2015-2022

#Load data

monthly_earnings15_22 <- read_csv("csv/monthly_earnings15_22.csv")
x_agg_vis <- read_csv(file="csv/ssb/x_agg_vis2016.csv")

monthly_earnings15_22 <- monthly_earnings15_22 %>%
  filter(year %in% c(2016, 2017)) %>%
  filter(industry_sic2007 != "Activities of extraterritorial organisations and bodies") %>%
  filter(industry_sic2007 != "Activities of household as employers")

x_agg_vis <- x_agg_vis %>%
  filter(year %in% c(2016, 2017))  %>%
  filter(parentcode_indus != "U") %>%
  filter(parentcode_indus != "T") 

codec <- read_csv("csv/ssb/indus_level1.csv")
industrynames <- tibble(wage_industry_names = unique(monthly_earnings15_22$industry_sic2007), code =c(LETTERS[1:19], "00.0"))
codec <- inner_join(codec, industrynames)

x <- monthly_earnings15_22

#I need to make sure that the parentindustryname on both datasets are the same.

##adding new industrynames
x_g <- c()
x_t <- x #dataset to "mutate"
x_f <- x#dataset to "mutate"
codec <- codec

for (i in 1:NROW(codec)) {
  x_f <- x_t %>%
    filter(x_t$industry_sic2007==codec$wage_industry_names[i])
  x_t <- x_t %>%
    filter(x_t$industry_sic2007!=codec$wage_industry_names[i])
  x_f$industryparentname=codec$parentname[i] #add in new industry name
  x_f$parentcode_indus=codec$code[i]
  x_g <- rbind(x_g, x_f)
  }

remove(x_f, x_t, x)




wage_data <- x_g %>%
  select(-industry_sic2007)
agg_data <- x_agg_vis %>%
  select(-tu31)

remove(x_agg_vis, monthly_earnings15_22)





# Pivot the wage data from long to wide format
wage_data_wide <- wage_data %>%
  pivot_wider(names_from = c(measuring_method, sector, age, sex, contractual_working_hours, contents), values_from = value)

wage_data_wide <- wage_data_wide %>%
  mutate(parentnameindustry = industry_sic2007, .keep = "unused")

wage_data_wide <- wage_data_wide %>%
  select(parentnameindustry, everything())

x_merge <- full_join(agg_data, wage_data_wide)

cols_to_keep <- complete.cases(t(wage_data_wide))
wage_data_clean_wide <- wage_data_wide[, cols_to_keep]

indus_level1 <- read_csv("csv/ssb/indus_level1.csv")
indus_level1 <- indus_level1 %>%
  filter(code != "U") %>%
  filter(code != "T") 



# Preview the resulting dataset
head(wage_data_selected)


# Group the wage data by industry and year and calculate summary statistics
wage_summary <- wage_data %>% 
  group_by(industry_sic2007, year) %>% 
  summarize(avg_earnings = mean(earnings), num_observations = n())

# Group the aggregate data by industry and year and calculate summary statistics
agg_summary <- agg_data %>% 
  group_by(industryparentname, year) %>% 
  summarize(avg_employees = mean(employees), avg_rate_male = mean(rate_male), avg_rate_unionized = mean(rate_unionized), avg_rate_sector = mean(rate_sector))

# Merge the summary data with the original datasets
merged_data <- inner_join(wage_data, wage_summary, by = c("year", "industry_sic2007")) %>% 
  inner_join(agg_summary, by = c("year", "industryparentname"))

# Preview the merged dataset
head(merged_data)