#FE methodology

# Methodology. Run data.R first

# Selecting variables of interest for FE (and testing)
ds <- df %>%
  select(year, parentcode_indus, industryparentname, union_density, mean_nok)

#making log of wage for later
dataset <- dataset %>%
  mutate_if(is.numeric, log)
# Dataset for panel data
dataset <- pdata.frame(df, index = c("year", "industryparentname", "parentcode_indus"))

#time fixed effects
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "time", model = "within")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = df)
FE_reg_ind0 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "time", model = "within")
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate, data = dataset, effect = "time", model = "within")
FE_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = dataset, effect = "time", model = "within")
FE_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = dataset, effect = "time", model = "within")
#FE_reg_ind4 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = dataset, effect = "time", model = "within")
#FE_reg_ind5 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = dataset, effect = "time", model = "within")
FE_reg_ind6 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = dataset, effect = "time", model = "within")
#FE_reg_ind7 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = dataset, effect = "time", model = "within")
#FE_reg_ind8 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = dataset, effect = "time", model = "within")
#FE_reg_ind9 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = dataset, effect = "time", model = "within")
#FE_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = dataset, effect = "time", model = "within")
#FE_reg_ind11 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = dataset, effect = "time", model = "within")
FE_reg_ind12 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "time", model = "within")
FE_reg_ind13 <- plm(dataset$mean_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "time", model = "within")
FE_reg_ind14 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "time", model = "within")

# Summary for FE models
#FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind3, FE_reg_ind4, FE_reg_ind5,FE_reg_ind6,FE_reg_ind7,FE_reg_ind8,FE_reg_ind9,FE_reg_ind10,FE_reg_ind11,FE_reg_ind12, FE_reg_ind13)
FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind6, FE_reg_ind14, FE_reg_ind12, FE_reg_ind13)

FE_regression_table <- stargazer(FE_model_list,
                                  title = "FE Regression Results",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(FE_regression_table, file = "FE_regression_table_output.txt")
write.csv(FE_regression_table, "FE_regression_table.csv", row.names = FALSE)


#two-way (time and individual) fixed effects
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "twoway", model = "within")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = df)
FE_reg_ind0 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "twoway", model = "within")
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate, data = dataset, effect = "twoway", model = "within")
FE_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = dataset, effect = "twoway", model = "within")
FE_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind4 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind5 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = dataset, effect = "twoway", model = "within")
FE_reg_ind6 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind7 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind8 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind9 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind11 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = dataset, effect = "twoway", model = "within")
FE_reg_ind12 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "within")
FE_reg_ind13 <- plm(dataset$mean_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "within")
FE_reg_ind14 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "within")

# Summary for FE models
#FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind3, FE_reg_ind4, FE_reg_ind5,FE_reg_ind6,FE_reg_ind7,FE_reg_ind8,FE_reg_ind9,FE_reg_ind10,FE_reg_ind11,FE_reg_ind12, FE_reg_ind13)
FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind6, FE_reg_ind14, FE_reg_ind12, FE_reg_ind13)

FE_regression_table <- stargazer(FE_model_list,
                                 title = "FE Regression Results - twoway",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(FE_regression_table, file = "FE_regression_table_output.txt")
write.csv(FE_regression_table, "FE_regression_table.csv", row.names = FALSE)


#industry fixed effects
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "individual", model = "within")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = df)
FE_reg_ind0 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "individual", model = "within")
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate, data = dataset, effect = "individual", model = "within")
FE_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = dataset, effect = "individual", model = "within")
FE_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = dataset, effect = "individual", model = "within")
#FE_reg_ind4 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = dataset, effect = "individual", model = "within")
#FE_reg_ind5 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = dataset, effect = "individual", model = "within")
FE_reg_ind6 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = dataset, effect = "individual", model = "within")
#FE_reg_ind7 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = dataset, effect = "individual", model = "within")
#FE_reg_ind8 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = dataset, effect = "individual", model = "within")
#FE_reg_ind9 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = dataset, effect = "individual", model = "within")
#FE_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = dataset, effect = "individual", model = "within")
#FE_reg_ind11 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = dataset, effect = "individual", model = "within")
FE_reg_ind12 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "individual", model = "within")
FE_reg_ind13 <- plm(dataset$mean_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "individual", model = "within")
FE_reg_ind14 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "individual", model = "within")

# Summary for FE models
#FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind3, FE_reg_ind4, FE_reg_ind5,FE_reg_ind6,FE_reg_ind7,FE_reg_ind8,FE_reg_ind9,FE_reg_ind10,FE_reg_ind11,FE_reg_ind12, FE_reg_ind13)
FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind6, FE_reg_ind14, FE_reg_ind12, FE_reg_ind13)

FE_regression_table <- stargazer(FE_model_list,
                                 title = "FE Regression Results - individual (industry)",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(FE_regression_table, file = "FE_regression_table_output.txt")
write.csv(FE_regression_table, "FE_regression_table.csv", row.names = FALSE)

#two-way (time and individual) fixed effects
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "twoway", model = "within")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = df)
FE_reg_ind0 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "twoway", model = "within")
FE_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate, data = dataset, effect = "twoway", model = "within")
FE_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = dataset, effect = "twoway", model = "within")
FE_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind4 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind5 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = dataset, effect = "twoway", model = "within")
FE_reg_ind6 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind7 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind8 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind9 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = dataset, effect = "twoway", model = "within")
#FE_reg_ind11 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = dataset, effect = "twoway", model = "within")
FE_reg_ind12 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "within")
FE_reg_ind13 <- plm(dataset$mean_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "within")
FE_reg_ind14 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "within")

# Summary for FE models
#FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind3, FE_reg_ind4, FE_reg_ind5,FE_reg_ind6,FE_reg_ind7,FE_reg_ind8,FE_reg_ind9,FE_reg_ind10,FE_reg_ind11,FE_reg_ind12, FE_reg_ind13)
FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind6, FE_reg_ind14, FE_reg_ind12, FE_reg_ind13)

FE_regression_table <- stargazer(FE_model_list,
                                 title = "FE Regression Results - twoway",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)




#FE lower quartile

FE_lowq_ind0 <- plm(dataset$lower_quartile_nok ~ dataset$union_density, data = df)
FE_lowq_ind1 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate, data = df)
FE_lowq_ind2 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = df)
FE_lowq_ind3 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = df)
FE_lowq_ind4 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = df)
FE_lowq_ind5 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = df)
FE_lowq_ind6 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = df)
FE_lowq_ind7 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = df)
FE_lowq_ind8 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = df)
FE_lowq_ind9 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = df)
FE_lowq_ind10 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = df)
FE_lowq_ind11 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = df)
FE_lowq_ind12 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
FE_lowq_ind13 <- plm(dataset$lower_quartile_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
FE_lowq_ind14 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)

# Summary for lower quartile FE models
#FE_lowq_list <- list(FE_lowq_ind0, FE_lowq_ind1, FE_lowq_ind2, FE_lowq_ind3, FE_lowq_ind4, FE_lowq_ind5,FE_lowq_ind6,FE_lowq_ind7,FE_lowq_ind8,FE_lowq_ind9,FE_lowq_ind10,FE_lowq_ind11,FE_lowq_ind12, FE_lowq_ind13)
FE_lowq_list <- list(FE_lowq_ind0, FE_lowq_ind1, FE_lowq_ind2, FE_lowq_ind6, FE_lowq_ind14, FE_lowq_ind12, FE_lowq_ind13)
FE_lowq_table <- stargazer(FE_lowq_list,
                            title = "FE Regression Results",
                            align = TRUE,
                            type = "text",
                            model.names = TRUE,
                            dep.var.labels.include = TRUE)
capture.output(FE_lowq_table, file = "FE_lowq_table_output.txt")

#FE upper quartile

FE_highq_ind0 <- plm(dataset$upper_quartile_nok ~ dataset$union_density, data = df)
FE_highq_ind1 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate, data = df)
FE_highq_ind2 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = df)
FE_highq_ind3 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = df)
FE_highq_ind4 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = df)
FE_highq_ind5 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = df)
FE_highq_ind6 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = df)
FE_highq_ind7 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = df)
FE_highq_ind8 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = df)
FE_highq_ind9 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = df)
FE_highq_ind10 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = df)
FE_highq_ind11 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = df)
FE_highq_ind12 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
FE_highq_ind13 <- plm(dataset$upper_quartile_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
FE_highq_ind14 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)

# Summary for lower quartile FE models
#FE_highq_list <- list(FE_highq_ind0, FE_highq_ind1, FE_highq_ind2, FE_highq_ind3, FE_highq_ind4, FE_highq_ind5,FE_highq_ind6,FE_highq_ind7,FE_highq_ind8,FE_highq_ind9,FE_highq_ind10,FE_highq_ind11,FE_highq_ind12, FE_highq_ind13, FE_highq_ind14)
FE_highq_list <- list(FE_highq_ind0, FE_highq_ind1, FE_highq_ind2, FE_highq_ind6, FE_highq_ind14, FE_highq_ind12, FE_highq_ind13)

FE_highq_table <- stargazer(FE_highq_list,
                             title = "FE Regression Results",
                             align = TRUE,
                             type = "text",
                             model.names = TRUE,
                             dep.var.labels.include = TRUE)
capture.output(FE_highq_table, file = "FE_highq_table_output.txt")

#FE median

FE_median_ind0 <- plm(dataset$median_nok ~ dataset$union_density, data = df)
FE_median_ind1 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate, data = df)
FE_median_ind2 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = df)
FE_median_ind3 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = df)
FE_median_ind4 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = df)
FE_median_ind5 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = df)
FE_median_ind6 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = df)
FE_median_ind7 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = df)
FE_median_ind8 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = df)
FE_median_ind9 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = df)
FE_median_ind10 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = df)
FE_median_ind11 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = df)
FE_median_ind12 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
FE_median_ind13 <- plm(dataset$median_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
FE_median_ind14 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)

# Summary for lower quartile FE models
#FE_median_list <- list(FE_median_ind0, FE_median_ind1, FE_median_ind2, FE_median_ind3, FE_median_ind4, FE_median_ind5,FE_median_ind6,FE_median_ind7,FE_median_ind8,FE_median_ind9,FE_median_ind10,FE_median_ind11,FE_median_ind12, FE_median_ind13)
FE_median_list <- list(FE_median_ind0, FE_median_ind1, FE_median_ind2, FE_median_ind6, FE_median_ind14, FE_median_ind12, FE_median_ind13)
FE_median_table <- stargazer(FE_median_list,
                              title = "FE Regression Results",
                              align = TRUE,
                              type = "text",
                              model.names = TRUE,
                              dep.var.labels.include = TRUE)
capture.output(FE_median_table, file = "FE_median_table_output.txt")

#combining stargazer for all FEs

FE_total_list <- list(FE_median_list, FE_highq_list, FE_lowq_list, FE_model_list)
FE_total_table <- stargazer(FE_total_list,
                             title = "FE Regression Results",
                             align = TRUE,
                             type = "text",
                             model.names = TRUE,
                             dep.var.labels.include = TRUE)
capture.output(FE_total_table, file = "FE_total_table_output.txt")

