# Methodology. Run data.R first

# Selecting variables of interest for OLS (and testing)
ds <- df %>%
  select(year, parentcode_indus, industryparentname, union_density, mean_nok)

# Dataset for panel data
dataset <- pdata.frame(df, index = c("industryparentname", "year"))


#making log of wage for later
logdataset <- dataset %>%
  mutate(mean_nok = log(mean_nok))%>%
  mutate(median_nok = log(median_nok))%>%
  mutate(lower_quartile_nok = log(lower_quartile_nok))%>%
  mutate(upper_quartile_nok = log(upper_quartile_nok))

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#OLS_reg_ind1 <- plm(df$mean_nok ~ df$union_density, data = df)
OLS_reg_ind0 <- plm(df$mean_nok ~ df$union_density, data = df)
OLS_reg_ind1 <- plm(df$mean_nok ~ df$union_density + df$collective_rate, data = df)
OLS_reg_ind2 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = df)
OLS_reg_ind3 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = df)
#OLS_reg_ind4 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = df)
#OLS_reg_ind5 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = df)
OLS_reg_ind6 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = df)
#OLS_reg_ind7 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = df)
#OLS_reg_ind8 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = df)
#OLS_reg_ind9 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = df)
#OLS_reg_ind10 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = df)
#OLS_reg_ind11 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = df)
OLS_reg_ind12 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_reg_ind13 <- plm(df$mean_nok ~ df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_reg_ind14 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)

# Summary for OLS models
#OLS_model_list <- list(OLS_reg_ind0, OLS_reg_ind1, OLS_reg_ind2, OLS_reg_ind3, OLS_reg_ind4, OLS_reg_ind5,OLS_reg_ind6,OLS_reg_ind7,OLS_reg_ind8,OLS_reg_ind9,OLS_reg_ind10,OLS_reg_ind11,OLS_reg_ind12, OLS_reg_ind13)
OLS_model_list <- list(OLS_reg_ind0, OLS_reg_ind1, OLS_reg_ind2, OLS_reg_ind6, OLS_reg_ind14, OLS_reg_ind12, OLS_reg_ind13)

OLS_regression_table <- stargazer(OLS_model_list,
                                  title = "OLS Regression Results",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(OLS_regression_table, file = "OLS_regression_table_output.txt")
write.csv(OLS_regression_table, "OLS_regression_table.csv", row.names = FALSE)

#OLS lower quartile

OLS_lowq_ind0 <- plm(logdataset$lower_quartile_nok ~ df$union_density, data = df)
OLS_lowq_ind1 <- plm(logdataset$lower_quartile_nok ~ df$union_density + df$collective_rate, data = df)
OLS_lowq_ind2 <- plm(logdataset$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = df)
OLS_lowq_ind3 <- plm(logdataset$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = df)
OLS_lowq_ind4 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = df)
OLS_lowq_ind5 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = df)
OLS_lowq_ind6 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = df)
OLS_lowq_ind7 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = df)
OLS_lowq_ind8 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = df)
OLS_lowq_ind9 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = df)
OLS_lowq_ind10 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = df)
OLS_lowq_ind11 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = df)
OLS_lowq_ind12 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_lowq_ind13 <- plm(df$lower_quartile_nok ~ df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_lowq_ind14 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)

# Summary for lower quartile OLS models
#OLS_lowq_list <- list(OLS_lowq_ind0, OLS_lowq_ind1, OLS_lowq_ind2, OLS_lowq_ind3, OLS_lowq_ind4, OLS_lowq_ind5,OLS_lowq_ind6,OLS_lowq_ind7,OLS_lowq_ind8,OLS_lowq_ind9,OLS_lowq_ind10,OLS_lowq_ind11,OLS_lowq_ind12, OLS_lowq_ind13)
OLS_lowq_list <- list(OLS_lowq_ind0, OLS_lowq_ind1, OLS_lowq_ind2, OLS_lowq_ind6, OLS_lowq_ind14, OLS_lowq_ind12, OLS_lowq_ind13)
OLS_lowq_table <- stargazer(OLS_lowq_list,
                                  title = "OLS Regression Results",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)
capture.output(OLS_lowq_table, file = "OLS_lowq_table_output.txt")

#OLS upper quartile

OLS_upperq_ind0 <- plm(logdataset$upper_quartile_nok ~ df$union_density, data = df)
OLS_upperq_ind1 <- plm(logdataset$upper_quartile_nok ~ df$union_density + df$collective_rate, data = df)
OLS_upperq_ind2 <- plm(logdataset$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = df)
OLS_upperq_ind3 <- plm(logdataset$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = df)
OLS_upperq_ind4 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = df)
OLS_upperq_ind5 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = df)
OLS_upperq_ind6 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = df)
OLS_upperq_ind7 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = df)
OLS_upperq_ind8 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = df)
OLS_upperq_ind9 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = df)
OLS_upperq_ind10 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = df)
OLS_upperq_ind11 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = df)
OLS_upperq_ind12 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_upperq_ind13 <- plm(df$upper_quartile_nok ~ df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_upperq_ind14 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)

# Summary for lower quartile OLS models
#OLS_upperq_list <- list(OLS_upperq_ind0, OLS_upperq_ind1, OLS_upperq_ind2, OLS_upperq_ind3, OLS_upperq_ind4, OLS_upperq_ind5,OLS_upperq_ind6,OLS_upperq_ind7,OLS_upperq_ind8,OLS_upperq_ind9,OLS_upperq_ind10,OLS_upperq_ind11,OLS_upperq_ind12, OLS_upperq_ind13, OLS_upperq_ind14)
OLS_upperq_list <- list(OLS_upperq_ind0, OLS_upperq_ind1, OLS_upperq_ind2, OLS_upperq_ind6, OLS_upperq_ind14, OLS_upperq_ind12, OLS_upperq_ind13)

OLS_upperq_table <- stargazer(OLS_upperq_list,
                            title = "OLS Regression Results",
                            align = TRUE,
                            type = "text",
                            model.names = TRUE,
                            dep.var.labels.include = TRUE)
capture.output(OLS_upperq_table, file = "OLS_upperq_table_output.txt")

#OLS median

OLS_median_ind0 <- plm(logdataset$median_nok ~ df$union_density, data = df)
OLS_median_ind1 <- plm(logdataset$median_nok ~ df$union_density + df$collective_rate, data = df)
OLS_median_ind2 <- plm(logdataset$median_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = df)
OLS_median_ind3 <- plm(logdataset$median_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = df)
OLS_median_ind4 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = df)
OLS_median_ind5 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = df)
OLS_median_ind6 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = df)
OLS_median_ind7 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = df)
OLS_median_ind8 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = df)
OLS_median_ind9 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = df)
OLS_median_ind10 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = df)
OLS_median_ind11 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = df)
OLS_median_ind12 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_median_ind13 <- plm(df$median_nok ~ df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_median_ind14 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)

# Summary for lower quartile OLS models
#OLS_median_list <- list(OLS_median_ind0, OLS_median_ind1, OLS_median_ind2, OLS_median_ind3, OLS_median_ind4, OLS_median_ind5,OLS_median_ind6,OLS_median_ind7,OLS_median_ind8,OLS_median_ind9,OLS_median_ind10,OLS_median_ind11,OLS_median_ind12, OLS_median_ind13)
OLS_median_list <- list(OLS_median_ind0, OLS_median_ind1, OLS_median_ind2, OLS_median_ind6, OLS_median_ind14, OLS_median_ind12, OLS_median_ind13)
OLS_median_table <- stargazer(OLS_median_list,
                             title = "OLS Regression Results",
                             align = TRUE,
                             type = "text",
                             model.names = TRUE,
                             dep.var.labels.include = TRUE)
capture.output(OLS_median_table, file = "OLS_median_table_output.txt")

# Estimate the fixed effects model
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "time", model = "within")
FE_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density +
                     dataset$collective_rate, data = dataset, effect = "time", model = "within")
FE_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate +
                     dataset$male_ratio, data = dataset, effect = "time", model = "within")
FE_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "time", model = "within")

FE_model_list <- list(FE_reg_ind1,FE_reg_ind2,FE_reg_ind3, FE_reg_ind10)
# Create a stargazer table for the fixed effects model
FE_regression_table <- stargazer(FE_model_list,
                                 title = "Fixed Effects Regression Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(FE_regression_table, file = "FE_regression_output.txt")
write.csv(FE_regression_table, "FE_regression_table.csv", row.names = FALSE)

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_time_ind1 <- plm(df$mean_nok ~ df$union_density, data = dataset, effect = "time"", model = "within")
FE_time_ind0 <- plm(df$mean_nok ~ df$union_density, data = dataset, effect = "time", model = "within")
FE_time_ind1 <- plm(df$mean_nok ~ df$union_density + df$collective_rate, data = dataset, effect = "time", model = "within")
FE_time_ind2 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = dataset, effect = "time", model = "within")
FE_time_ind3 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = dataset, effect = "time", model = "within")
#FE_time_ind4 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = dataset, effect = "time", model = "within")
#FE_time_ind5 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = dataset, effect = "time", model = "within")
FE_time_ind6 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = dataset, effect = "time", model = "within")
#FE_time_ind7 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = dataset, effect = "time", model = "within")
#FE_time_ind8 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = dataset, effect = "time", model = "within")
#FE_time_ind9 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = dataset, effect = "time", model = "within")
#FE_time_ind10 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = dataset, effect = "time", model = "within")
#FE_time_ind11 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = dataset, effect = "time", model = "within")
FE_time_ind12 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "time", model = "within")
FE_time_ind13 <- plm(df$mean_nok ~ df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "time", model = "within")
FE_time_ind14 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "time", model = "within")

# Summary for OLS models
#OLS_model_list <- list(FE_time_ind0, FE_time_ind1, FE_time_ind2, FE_time_ind3, FE_time_ind4, FE_time_ind5,FE_time_ind6,FE_time_ind7,FE_time_ind8,FE_time_ind9,FE_time_ind10,FE_time_ind11,FE_time_ind12, FE_time_ind13)
FE_model_list <- list(FE_time_ind0, FE_time_ind1, FE_time_ind2, FE_time_ind6, FE_time_ind14, FE_time_ind12, FE_time_ind13)

FE_regression_table <- stargazer(FE_model_list,
                                 title = "Time FE Regression Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)


# Save the table to a text file
capture.output(FE_regression_table, file = "FE_time_effect_output.txt")


# Estimate the fixed effects model
FE_ind_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "time", model = "within")
FE_ind_ind2 <- plm(dataset$mean_nok ~ dataset$union_density +
                     dataset$collective_rate, data = dataset, effect = "time", model = "within")
FE_ind_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate +
                     dataset$male_ratio, data = dataset, effect = "time", model = "within")
FE_ind_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "time", model = "within")

FE_model_list <- list(FE_ind_ind1,FE_ind_ind2,FE_ind_ind3, FE_ind_ind10)
# Create a stargazer table for the fixed effects model
FE_regression_table <- stargazer(FE_model_list,
                                 title = "Fixed Effects Regression Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(FE_regression_table, file = "FE_ind_effect_output.txt")
write.csv(FE_regression_table, "FE_regression_table.csv", row.names = FALSE)


# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_ind_ind1 <- plm(df$mean_nok ~ df$union_density, data = dataset, effect = "individual", model = "within")
FE_ind_ind0 <- plm(df$mean_nok ~ df$union_density, data = dataset, effect = "individual", model = "within")
FE_ind_ind1 <- plm(df$mean_nok ~ df$union_density + df$collective_rate, data = dataset, effect = "individual", model = "within")
FE_ind_ind2 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = dataset, effect = "individual", model = "within")
FE_ind_ind3 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = dataset, effect = "individual", model = "within")
#FE_ind_ind4 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = dataset, effect = "individual", model = "within")
#FE_ind_ind5 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = dataset, effect = "individual", model = "within")
FE_ind_ind6 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = dataset, effect = "individual", model = "within")
#FE_ind_ind7 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = dataset, effect = "individual", model = "within")
#FE_ind_ind8 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = dataset, effect = "individual", model = "within")
#FE_ind_ind9 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = dataset, effect = "individual", model = "within")
#FE_ind_ind10 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = dataset, effect = "individual", model = "within")
#FE_ind_ind11 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = dataset, effect = "individual", model = "within")
FE_ind_ind12 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "individual", model = "within")
FE_ind_ind13 <- plm(df$mean_nok ~ df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "individual", model = "within")
FE_ind_ind14 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "individual", model = "within")

# Summary for OLS models
#OLS_model_list <- list(FE_ind_ind0, FE_ind_ind1, FE_ind_ind2, FE_ind_ind3, FE_ind_ind4, FE_ind_ind5,FE_ind_ind6,FE_ind_ind7,FE_ind_ind8,FE_ind_ind9,FE_ind_ind10,FE_ind_ind11,FE_ind_ind12, FE_ind_ind13)
FE_model_list <- list(FE_ind_ind0, FE_ind_ind1, FE_ind_ind2, FE_ind_ind6, FE_ind_ind14, FE_ind_ind12, FE_ind_ind13)

FE_regression_table <- stargazer(FE_model_list,
                                  title = "FE Regression Results",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)


# Save the table to a text file
capture.output(FE_regression_table, file = "FE_ind_effect_output.txt")






# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_twoway_ind1 <- plm(df$mean_nok ~ df$union_density, data = dataset, effect = "twoway", model = "within")
FE_twoway_ind0 <- plm(df$mean_nok ~ df$union_density, data = dataset, effect = "twoway", model = "within")
FE_twoway_ind1 <- plm(df$mean_nok ~ df$union_density + df$collective_rate, data = dataset, effect = "twoway", model = "within")
FE_twoway_ind2 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = dataset, effect = "twoway", model = "within")
FE_twoway_ind3 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = dataset, effect = "twoway", model = "within")
#FE_twoway_ind4 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = dataset, effect = "twoway", model = "within")
#FE_twoway_ind5 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = dataset, effect = "twoway", model = "within")
FE_twoway_ind6 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = dataset, effect = "twoway", model = "within")
#FE_twoway_ind7 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = dataset, effect = "twoway", model = "within")
#FE_twoway_ind8 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = dataset, effect = "twoway", model = "within")
#FE_twoway_ind9 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = dataset, effect = "twoway", model = "within")
#FE_twoway_ind10 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = dataset, effect = "twoway", model = "within")
#FE_twoway_ind11 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = dataset, effect = "twoway", model = "within")
FE_twoway_ind12 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "twoway", model = "within")
FE_twoway_ind13 <- plm(df$mean_nok ~ df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "twoway", model = "within")
FE_twoway_ind14 <- plm(df$mean_nok ~ df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "twoway", model = "within")

# Summary for OLS models
#OLS_model_list <- list(FE_twoway_ind0, FE_twoway_ind1, FE_twoway_ind2, FE_twoway_ind3, FE_twoway_ind4, FE_twoway_ind5,FE_twoway_ind6,FE_twoway_ind7,FE_twoway_ind8,FE_twoway_ind9,FE_twoway_ind10,FE_twoway_ind11,FE_twoway_ind12, FE_twoway_ind13)
FE_model_list <- list(FE_twoway_ind0, FE_twoway_ind1, FE_twoway_ind2, FE_twoway_ind6, FE_twoway_ind14, FE_twoway_ind12, FE_twoway_ind13)

FE_regression_table <- stargazer(FE_model_list,
                                 title = "FE Regression Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)


# Save the table to a text file
capture.output(FE_regression_table, file = "FE_twoway_effect_output.txt")




# Merge the two tables
merged_table <- paste(OLS_regression_table, FE_regression_table)

# Save the merged table to a text file
capture.output(merged_table, file = "combined_regression_table_output.txt")


###In this section I will regress first lower quartile, then median, then upper quartile


FE_reg_low <- plm(logdataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate
                     , data = dataset, effect = "twoways", model = "within")

FE_reg_med <- plm(logdataset$median_nok ~ dataset$union_density + dataset$collective_rate
                  , data = dataset, effect = "twoways", model = "within")

FE_reg_upper <- plm(logdataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate
                  , data = dataset, effect = "twoways", model = "within")
FE_distribution_list <- list(FE_reg_low, FE_reg_med, FE_reg_upper)

FE_distrib_reg_table <- stargazer(FE_distribution_list,
                                 title = "Fixed Effects Regression on wage quartiles Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

capture.output(FE_distrib_reg_table, file = "distribution_regression.txt")

FE_reg_low_control <- plm(logdataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate
                  + dataset$male_ratio + dataset$has.education.bachelor + dataset$has.education.master 
                  + dataset$has.education.doctor, data = dataset, effect = "twoways", model = "within")

FE_reg_med_control <- plm(logdataset$median_nok ~ dataset$union_density + dataset$collective_rate
                          + dataset$male_ratio + dataset$has.education.bachelor + dataset$has.education.master 
                          + dataset$has.education.doctor, data = dataset, effect = "twoways", model = "within")

FE_reg_upper_control <- plm(logdataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate
                            + dataset$male_ratio + dataset$has.education.bachelor + dataset$has.education.master 
                            + dataset$has.education.doctor, data = dataset, effect = "twoways", model = "within")

FE_distribution_list_control <- list(FE_reg_low_control, FE_reg_med_control, FE_reg_upper_control)

FE_distrib_reg_table_control <- stargazer(FE_distribution_list_control,
                                  title = "Fixed Effects Regression on log(wage) quartiles Results",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE,
                                  covariate.labels = c("Union Density", "Collective Bargaining", 
                                                       "Male prevalence", "Bachelor degree prevalence",
                                                       "Master degree prevalence", "Doctor degree prevalence"),
                                  column.labels = c("Fixed Effects"), 
                                  add.lines = list(c("Entity"), c("Time")), 
                                  single.row = TRUE, 
                                  header = FALSE, 
                                  digits = 2)


capture.output(FE_distrib_reg_table_control, file = "distribution_regression_control.txt")

####quantile regression



# Run plm with dependent variable "mean_nok" and independent variable uniondensity
QR_lowq_ind0 <- rq(lower_quartile_nok ~ union_density, data = logdataset, tau=0.25)
QR_lowq_ind1 <- rq(lower_quartile_nok ~ union_density + collective_rate, data = logdataset, tau=0.25)
QR_lowq_ind2 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio, data = logdataset, tau=0.25)
QR_lowq_ind3 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio + has.education.finished.hs, data = logdataset, tau=0.25)
QR_lowq_ind6 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio + has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor, data = logdataset, tau=0.25)
QR_lowq_ind12 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.25)
QR_lowq_ind13 <- rq(lower_quartile_nok ~ collective_rate + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.25)
QR_lowq_ind14 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.25)
QR_lowq_ind15 <- rq(lower_quartile_nok ~ union_density + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.25)


QR_median_ind0 <- rq(median_nok ~ union_density, data = logdataset, tau=0.5)
QR_median_ind1 <- rq(median_nok ~ union_density + collective_rate, data = logdataset, tau=0.5)
QR_median_ind2 <- rq(median_nok ~ union_density + collective_rate + male_ratio, data = logdataset, tau=0.5)
QR_median_ind3 <- rq(median_nok ~ union_density + collective_rate + male_ratio + has.education.finished.hs, data = logdataset, tau=0.5)
QR_median_ind6 <- rq(median_nok ~ union_density + collective_rate + male_ratio + has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor, data = logdataset, tau=0.5)
QR_median_ind12 <- rq(median_nok ~ union_density + collective_rate + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.5)
QR_median_ind13 <- rq(median_nok ~ collective_rate + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.5)
QR_median_ind14 <- rq(median_nok ~ union_density + collective_rate + male_ratio +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.5)
QR_median_ind15 <- rq(median_nok ~ union_density + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.5)


QR_upperq_ind0 <- rq(upper_quartile_nok ~ union_density, data = logdataset, tau=0.75)
QR_upperq_ind1 <- rq(upper_quartile_nok ~ union_density + collective_rate, data = logdataset, tau=0.75)
QR_upperq_ind2 <- rq(upper_quartile_nok ~ union_density + collective_rate + male_ratio, data = logdataset, tau=0.75)
QR_upperq_ind3 <- rq(upper_quartile_nok ~ union_density + collective_rate + male_ratio + has.education.finished.hs, data = logdataset, tau=0.75)
QR_upperq_ind6 <- rq(upper_quartile_nok ~ union_density + collective_rate + male_ratio + has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor, data = logdataset, tau=0.75)
QR_upperq_ind12 <- rq(upper_quartile_nok ~ union_density + collective_rate + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.75)
QR_upperq_ind13 <- rq(upper_quartile_nok ~ collective_rate + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.75)
QR_upperq_ind14 <- rq(upper_quartile_nok ~ union_density + collective_rate + male_ratio +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.75)
QR_upperq_ind15 <- rq(upper_quartile_nok ~ union_density + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties, data = logdataset, tau=0.75)



# create a list of lowerquartile
model_lowq_list <- list(QR_lowq_ind0, QR_lowq_ind1, QR_lowq_ind2, QR_lowq_ind3,
                   QR_lowq_ind6, QR_lowq_ind12, QR_lowq_ind13, QR_lowq_ind14,
                   QR_lowq_ind15)

#stargazer lower quartile
QR_lowq_table <- stargazer(model_lowq_list,
                                  title = "Lower Quartile Regression Table",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)

capture.output(QR_lowq_table, file = "lower_quartile_results.txt")

# create a list of your models
model_median_list <- list(QR_median_ind0, QR_median_ind1, QR_median_ind2, QR_median_ind3,
                   QR_median_ind6, QR_median_ind12, QR_median_ind13, QR_median_ind14,
                   QR_median_ind15)

#stargazer median
QR_median_table <- stargazer(model_median_list,
                                  title = "Median Quartile Regression Table",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)

capture.output(QR_median_table, file = "Median_results.txt")

# create a list of your models
model_upperq_list <- list(QR_upperq_ind0, QR_upperq_ind1, QR_upperq_ind2, QR_upperq_ind3,
                   QR_upperq_ind6, QR_upperq_ind12, QR_upperq_ind13, QR_upperq_ind14,
                   QR_upperq_ind15)

#stargazer upper quartile
QR_upperq_table <- stargazer(model_upperq_list,
                                  title = "Upper Quartile Regression Table",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)

capture.output(QR_upperq_table, file = "upper_quartile_results.txt")




# Run plm with dependent variable "mean_nok" and independent variable uniondensity
QR_lowq_ind0 <- rq(lower_quartile_nok ~ union_density +  factor(industryparentname) + factor(year), tau=0.25, data=logdataset)
QR_lowq_ind1 <- rq(lower_quartile_nok ~ union_density + collective_rate+  factor(industryparentname) + factor(year), tau=0.25, data=logdataset)
QR_lowq_ind2 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio, data = logdataset, tau=0.25)
QR_lowq_ind3 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio + has.education.finished.hs, data = logdataset, tau=0.25)
QR_lowq_ind6 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio + has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor+  factor(industryparentname) + factor(year), tau=0.25, data=logdataset)
QR_lowq_ind12 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties+  factor(industryparentname) + factor(year), tau=0.25, data=logdataset)
QR_lowq_ind13 <- rq(lower_quartile_nok ~ collective_rate + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties+  factor(industryparentname) + factor(year), tau=0.25, data=logdataset)
QR_lowq_ind14 <- rq(lower_quartile_nok ~ union_density + collective_rate + male_ratio +  teenager + twenties + thirties + fourties + fifties + sixties+  factor(industryparentname) + factor(year), tau=0.25, data=logdataset)
QR_lowq_ind15 <- rq(lower_quartile_nok ~ union_density + male_ratio +has.education.finished.hs + has.education.bachelor + has.education.master +has.education.doctor +  teenager + twenties + thirties + fourties + fifties + sixties+  factor(industryparentname) + factor(year), tau=0.25, data=logdataset)


# create a list of lowerquartile
model_lowq_list <- list(QR_lowq_ind1, QR_lowq_ind2, QR_lowq_ind3,
                        QR_lowq_ind6, QR_lowq_ind12, QR_lowq_ind13, QR_lowq_ind14,
                        QR_lowq_ind15)
#stargazer lower quartile
QR_lowq_table <- stargazer(model_lowq_list,
                           title = "Lower Quartile Regression Table",
                           align = TRUE,
                           type = "text",
                           model.names = TRUE,
                           dep.var.labels.include = TRUE)

capture.output(QR_lowq_table, file = "lower_quartile_results.txt")


OLS_ff_ind0 <- plm(df$mean_nok ~ df$union_density_manuf, data = df)
OLS_ff_ind1 <- plm(df$mean_nok ~ df$union_density_manuf+union_density, data = df)
OLS_ff_ind2 <- plm(df$mean_nok ~ df$union_density_manuf +df$union_density + df$collective_rate + , data = df)
OLS_ff_ind3 <- plm(df$mean_nok ~ df$union_density_manuf+ df$union_density + df$collective_rate +  + df$male_ratio, data = df)
OLS_ff_ind4 <- plm(df$mean_nok ~ df$union_density_manuf+ df$union_density + df$collective_rate +  + df$male_ratio + df$has.education.finished.hs, data = df)

#front-runner
OLS_ff_ind0 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density, data = dataset, effect = "individual", model = "within")
OLS_ff_ind1 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate, data = dataset, effect = "individual", model = "within")
OLS_ff_ind2 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio, data = dataset, effect = "individual", model = "within")
OLS_ff_ind3 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = dataset, effect = "individual", model = "within")
OLS_ff_ind4 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = dataset, effect = "individual", model = "within")
OLS_ff_ind5 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = dataset, effect = "individual", model = "within")
OLS_ff_ind6 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = dataset, effect = "individual", model = "within")
OLS_ff_ind7 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = dataset, effect = "individual", model = "within")
OLS_ff_ind8 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = dataset, effect = "individual", model = "within")
OLS_ff_ind9 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = dataset, effect = "individual", model = "within")
OLS_ff_ind10 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = dataset, effect = "individual", model = "within")
OLS_ff_ind11 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = dataset, effect = "individual", model = "within")
OLS_ff_ind12 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "individual", model = "within")
OLS_ff_ind13 <- plm(logdataset$mean_nok ~ union_density_manuf+df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "individual", model = "within")
OLS_ff_ind14 <- plm(logdataset$mean_nok ~ union_density_manuf+df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = dataset, effect = "individual", model = "within")
OLS_ff_ind15 <- plm(logdataset$mean_nok ~ union_density_manuf, data = dataset, effect = "individual", model = "within")



model_ff_list <- list(OLS_ff_ind0,OLS_ff_ind1,OLS_ff_ind2,OLS_ff_ind3,OLS_ff_ind4,OLS_ff_ind5,OLS_ff_ind6,OLS_ff_ind7,OLS_ff_ind8,
                      OLS_ff_ind9,OLS_ff_ind10,OLS_ff_ind11,OLS_ff_ind12,OLS_ff_ind13,OLS_ff_ind14,OLS_ff_ind15)

OLS_ff_table <- stargazer(model_ff_list,
                          title = "Lower Quartile Regression Table",
                          align = TRUE,
                          type = "text",
                          model.names = TRUE,
                          dep.var.labels.include = TRUE)

capture.output(OLS_ff_table, file = "ff_results.txt")
