#This file merges nsd data with wage data from 2015-2022

#Load data

monthly_earnings15_22 <- read_csv("csv/monthlywage15_22_wide.csv")
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
industrynames <- tibble(wage_industry_names = unique(monthly_earnings15_22$industry_sic2007), code =c("All industries", LETTERS[1:19], "00.0"))
codec <- full_join(codec, industrynames)

codec$parentname[23] <- "All industries"

x <- monthly_earnings15_22

#I need to make sure that the parentindustryname on both datasets are the same.

##adding new industrynames
x_g <- c()
x_t <- x #dataset to "mutate"
x_f <- x#dataset to "mutate"
codec <- codec

for (i in 1:NROW(codec)) {
  if(is.na(codec$wage_industry_names[i])) {next}
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
  select(-industry_sic2007) %>%
  select(industryparentname, everything())
agg_data <- x_agg_vis %>%
  select(-tu31)

remove(x_agg_vis, monthly_earnings15_22)




wage_data_wide_filter <- wage_data %>%
  filter(!(parentcode_indus %in% c("All industries"))) %>%
  filter(occupation %in% unique(occupation)[1]) %>%
  filter(sector %in% unique(sector)[1]) %>%
  filter(sex %in% unique(sex)[1]) %>%
  filter(contractual_usual_working_hours_per_week %in% unique(contractual_usual_working_hours_per_week)[1])
x_merge <- full_join(agg_data, wage_data_wide_filter)

write_csv(x_merge, file=("csv/sds.csv"))


#visualise
sds <- read_csv(file=("csv/sds.csv"))

unionvearningsbyindustry <- ggplot(data=sds,
                                   aes(x= is.union,
                                       y= `Monthly earnings (NOK)_Median`,
                                       colour=year
                                   ))+
  geom_point()    +theme(legend.position="none")
unionvearningsbyindustry <- unionvearningsbyindustry +
  ggtitle("Labour Union Density and Monthly Wages Across Main Industries in 2016 and 2017")+
  xlab("Labour Union Density")+
  ylab("Median Monthly Wage (NOK)")

fig <- ggplotly(unionvearningsbyindustry) 
fig <- style(fig,                 
             hovertext = paste0("Union Density: ", formatC(sds$is.union, digits = 3),"\n",
                               "Median Monthly Earnings: ", formatC(sds$`Monthly earnings (NOK)_Median`, format ="f", digits = 0), "\n",
                               sds$industryparentname, "\n",
                               sds$year))
fig
log()
pdata <- pdata.frame(sds,index = c("industryparentname", "year"))

model <- plm(log(sds$`Monthly earnings (NOK)_Average`) ~ is.union, data = pdata, model = "within")
summary(model)
model1 <- lm(`Monthly earnings (NOK)_Median` ~ is.union, data = sds)
model2 <- lm(`Monthly earnings (NOK)_Median` ~ is.union + is.male, data = sds)
model3 <- lm(`Monthly earnings (NOK)_Median` ~ is.union + is.male + collective.agreement.yes, data = sds)
model4 <- lm(`Monthly earnings (NOK)_Median` ~ is.union + is.male + collective.agreement.yes +
               has.education.primary + has.education.juniorhigh + has.education.finished.hs +
               has.education.bachelor + has.education.master + has.education.doctor, data = sds)
model5 <- lm(`Monthly earnings (NOK)_Median` ~ is.union + is.male + collective.agreement.yes +
               twenty + thirty + fourty + fifty + sixty + seventy, data = sds)
model6 <- lm(`Monthly earnings (NOK)_Median` ~ is.union + is.male + collective.agreement.yes +
               sector.private + sector.municipal + sector.county + sector.centralgov, data = sds)
model7 <- lm(`Monthly earnings (NOK)_Median` ~ is.union + is.male + collective.agreement.yes +
               sds$`Contractual working hours per week (hours)_Median`, data = sds)
model8 <- lm(`Monthly earnings (NOK)_Median` ~ is.union + is.male + collective.agreement.yes, data = sds)


model_list <- list(model1, model2,model3, model4, model5, model6, model7)

# use sapply to extract the R-squared values from each model
#rsq_values <- sapply(model_list, function(x) summary(x)$r.squared)


regression_table <- stargazer(model_list,
          title = "Regression Results",
          align = TRUE,
          type = "text",
          model.names = TRUE,
          dep.var.labels.include = TRUE,
          append = TRUE)



# Fit linear regression model
model1 <- lm(`Monthly earnings (NOK)_Median` ~ is.union, data = sds)

# Create scatterplot with linear regression line
unionvearningsbyindustry <- plot_ly(data = sds,
                                    x = ~is.union,
                                    y = ~`Monthly earnings (NOK)_Median`,
                                    color = ~year,
                                    type = "scatter",
                                    mode = "markers",
                                    marker = list(size = 6)) %>%
  add_markers() %>%
  add_lines(x = ~is.union,
            y = ~predict(model1, newdata = data.frame(is.union = sds$is.union)),
            line = list(color = 'black')) %>%
  layout(xaxis = list(title = "Union membership"),
         yaxis = list(title = "Monthly earnings (NOK)"),
         title = "Scatterplot of earnings by union membership",
         showlegend = TRUE) %>%
  add_annotations(x = 0.5, y = 50000,
                  text = paste0("y = ",
                                round(coef(model1)[1], 2),
                                " + ",
                                round(coef(model1)[2], 2),
                                "x"),
                  showarrow = FALSE,
                  font = list(size = 14))

# Print plot
unionvearningsbyindustry






writeLines(regression_table, "regression_table.txt")

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