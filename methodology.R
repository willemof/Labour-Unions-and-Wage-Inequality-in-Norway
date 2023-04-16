# Methodology. Run data.R first

# Selecting variables of interest for OLS (and testing)
ds <- df %>%
  select(year, parentcode_indus, industryparentname, union_density, mean_nok)

# Dataset for panel data
dataset <- pdata.frame(df, index = c("year", "industryparentname", "parentcode_indus"))

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
OLS_reg_ind1 <- plm(ds$mean_nok ~ ds$union_density, data = ds)
OLS_reg_ind2 <- plm(df$mean_nok ~ df$union_density, data = ds)
OLS_reg_ind3 <- plm(df$mean_nok ~ df$union_density + df$collective_rate, data = ds)
OLS_reg_ind4 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = ds)
OLS_reg_ind5 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$twenties, data = ds)

# Summary for OLS models
OLS_model_list <- list(OLS_reg_ind1, OLS_reg_ind2, OLS_reg_ind3, OLS_reg_ind4, OLS_reg_ind5)
OLS_regression_table <- stargazer(OLS_model_list,
                                  title = "OLS Regression Results",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE,
                                  covariate.labels = c("Intercept", "Union Density"),
                                  add.lines = list(c(mean_nok = coef(OLS_reg_ind1)[1], union_density = "")))

# Save the table to a text file
capture.output(OLS_regression_table, file = "OLS_regression_output.txt")

# Estimate the fixed effects model
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "individual", model = "within")
FE_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density +
                     dataset$collective_rate, data = dataset, effect = "individual", model = "within")
FE_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate +
                     dataset$male_ratio, data = dataset, effect = "individual", model = "within")

FE_model_list <- list(FE_reg_ind1,FE_reg_ind2,FE_reg_ind3)
# Create a stargazer table for the fixed effects model
FE_regression_table <- stargazer(FE_model_list,
                                 title = "Fixed Effects Regression Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(FE_regression_table, file = "FE_regression_output.txt")

# Merge the two tables
merged_table <- paste(OLS_regression_table, FE_regression_table)

# Save the merged table to a text file
capture.output(merged_table, file = "combined_regression_table_output.txt")
