#Methodology. Run data.R first

df <- full_merged_ds_year



df <- df %>%
  select(-c(ends_with("_se")))

colnames(df[6:13]) #education
colnames(df[14:20]) #age
colnames(df[21:24]) #sector
colnames(df[25:27]) #collective coverage
colnames(df[28:35]) #employment type (leave of absence status)
colnames(df[36:37]) #full time status

string <- paste0("+ ", colnames(df[6:13]))
string
reg_mean <- lm(mean_nok ~union_density, + colnames(df[6:13]), data =df)

library(AER)
reg_model_mean <- lm(mean_nok ~union_density, data = df)
reg_model_median <- lm(median_nok ~uniondensity + is_male + df$has_education_doctor + df$has_education_master +
                         df$has_education_bachelor + df$has_education_finished_hs + df$has_education_some_hs +
                         df$has_education_primary | df$year | df$industryparentname, data = df)
reg_model <- reg_model_median
summary(reg_model)

library(stargazer)

model_table <- reg_model_mean

regression_table <- stargazer(model_table,
                              title = "Regression Results",
                              align = TRUE,
                              type = "text",
                              model.names = TRUE,
                              dep.var.labels.include = TRUE,
                              append = TRUE)





panel_data <- agemedianwage
dep_var <- panel_data$median_nok
ind_var <- panel_data$is.union
ctrl_var1 <- panel_data$has.education.primary
ctrl_var2 <- panel_data$has.education.some.hs
ctrl_var3 <- panel_data$has.education.finished.hs
ctrl_var4 <- panel_data$has.education.bachelor
ctrl_var5 <- panel_data$has.education.master
ctrl_var6 <- panel_data$has.education.doctor
ctrl_var7 <- panel_data$is.male

iv1 <- ctrl_var1
iv2 <- ctrl_var2
iv3 <- ctrl_var3
iv4 <- ctrl_var4
iv5 <- ctrl_var5
iv6 <- ctrl_var6
iv7 <- ctrl_var7

reg_model_median <- ivreg(dep_var ~ind_var  | iv1 +
                            iv2 + iv3 + iv4 + iv5 + iv6 + iv7, data = panel_data)

summary(reg_model_median)
+ ctrl_var1 + ctrl_var2 + ctrl_var3 +
  ctrl_var4 + ctrl_var5 + ctrl_var6 + ctrl_var7

library(plm) 
panel_data <- pdata.frame(df, index(c("industryparentname", "date")))

reg_model <- plm(dep_var ~ ind_var + ctrl_var1 + ctrl_var2 + ctrl_var3 +
                   ctrl_var4 + ctrl_var5 + ctrl_var6 + ctrl_var7, 
                 data = panel_data, model = "within")


library(tidyverse)
# reshape the data into a long format
df <- df %>%
  select(-c(industry_sic2007))
long_data <- df %>%
  gather(key = "variable", value = "value", -year, -industryparentname, -date, -parentcode_indus, -sex)

long_data <- long_data %>%
  select(-c(date, parentcode_indus))
# create a panel data frame
panel_data <- pdata.frame(long_data, index = c("year", "industryparentname", "variable"))

dup_obs <- table(index(panel_data), useNA = "ifany")
dup_obs[dup_obs > 1]

library(reshape2)


# check for duplicate index values
dup_obs <- table(index(panel_data), useNA = "ifany")
dup_obs <- tibble(dup_obs)
dup_obs[dup_obs > 1]


# estimate a panel data regression
reg_model <- plm(dep_var ~ ind_var, data = panel_data, model = "within")

summary(reg_model)



# Convert year to numeric
panel_data$year <- as.numeric(as.character(panel_data$year))

# Create treatment indicator variable
panel_data$treatment <- ifelse(panel_data$year > 2013, 1, 0)

panel_data$treatment <- as.numeric(panel_data$treatment)
nrow(panel_data)   # check the number of rows in the data frame
for (var in names(panel_data)) {
  cat("Variable", var, "has length", length(panel_data[[var]]), "\n")
}


# Estimate difference-in-differences model
reg_model <- plm(median_nok ~ is.union + treatment * has.education.primary + 
                   treatment * has.education.some.hs + treatment * has.education.finished.hs +
                   treatment * has.education.bachelor + treatment * has.education.master + 
                   treatment * has.education.doctor + treatment * is.male, 
                 data = panel_data, model = "within")


dep_var <- panel_data$median_nok
ind_var <- panel_data$is.union
ctrl_var1 <- panel_data$has.education.primary
ctrl_var2 <- panel_data$has.education.some.hs
ctrl_var3 <- panel_data$has.education.finished.hs
ctrl_var4 <- panel_data$has.education.bachelor
ctrl_var5 <- panel_data$has.education.master
ctrl_var6 <- panel_data$has.education.doctor
ctrl_var7 <- panel_data$is.male

