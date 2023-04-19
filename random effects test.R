#random methodology

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
R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "time", model = "random")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = df)
R_reg_ind0 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "time", model = "random")
R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate, data = dataset, effect = "time", model = "random")
R_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = dataset, effect = "time", model = "random")
R_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = dataset, effect = "time", model = "random")
#R_reg_ind4 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = dataset, effect = "time", model = "random")
#R_reg_ind5 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = dataset, effect = "time", model = "random")
R_reg_ind6 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = dataset, effect = "time", model = "random")
#R_reg_ind7 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = dataset, effect = "time", model = "random")
#R_reg_ind8 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = dataset, effect = "time", model = "random")
#R_reg_ind9 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = dataset, effect = "time", model = "random")
#R_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = dataset, effect = "time", model = "random")
#R_reg_ind11 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = dataset, effect = "time", model = "random")
R_reg_ind12 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "time", model = "random")
R_reg_ind13 <- plm(dataset$mean_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "time", model = "random")
R_reg_ind14 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "time", model = "random")

# Summary for FE models
#R_model_list <- list(R_reg_ind0, R_reg_ind1, R_reg_ind2, R_reg_ind3, R_reg_ind4, R_reg_ind5,R_reg_ind6,R_reg_ind7,R_reg_ind8,R_reg_ind9,R_reg_ind10,R_reg_ind11,R_reg_ind12, R_reg_ind13)
R_model_list <- list(R_reg_ind0, R_reg_ind1, R_reg_ind2, R_reg_ind6, R_reg_ind14, R_reg_ind12, R_reg_ind13)

R_regression_table <- stargazer(R_model_list,
                                 title = "FE Regression Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(R_regression_table, file = "R_regression_table_output.txt")
write.csv(R_regression_table, "R_regression_table.csv", row.names = FALSE)


#two-way (time and individual) fixed effects
R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "twoway", model = "random")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = df)
R_reg_ind0 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "twoway", model = "random")
R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate, data = dataset, effect = "twoway", model = "random")
R_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = dataset, effect = "twoway", model = "random")
R_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = dataset, effect = "twoway", model = "random")
#R_reg_ind4 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = dataset, effect = "twoway", model = "random")
#R_reg_ind5 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = dataset, effect = "twoway", model = "random")
R_reg_ind6 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = dataset, effect = "twoway", model = "random")
#R_reg_ind7 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = dataset, effect = "twoway", model = "random")
#R_reg_ind8 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = dataset, effect = "twoway", model = "random")
#R_reg_ind9 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = dataset, effect = "twoway", model = "random")
#R_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = dataset, effect = "twoway", model = "random")
#R_reg_ind11 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = dataset, effect = "twoway", model = "random")
R_reg_ind12 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "random")
R_reg_ind13 <- plm(dataset$mean_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "random")
R_reg_ind14 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "random")

# Summary for FE models
#R_model_list <- list(R_reg_ind0, R_reg_ind1, R_reg_ind2, R_reg_ind3, R_reg_ind4, R_reg_ind5,R_reg_ind6,R_reg_ind7,R_reg_ind8,R_reg_ind9,R_reg_ind10,R_reg_ind11,R_reg_ind12, R_reg_ind13)
R_model_list <- list(R_reg_ind0, R_reg_ind1, R_reg_ind2, R_reg_ind6, R_reg_ind14, R_reg_ind12, R_reg_ind13)

R_regression_table <- stargazer(R_model_list,
                                 title = "FE Regression Results - twoway",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(R_regression_table, file = "R_regression_table_output.txt")
write.csv(R_regression_table, "R_regression_table.csv", row.names = FALSE)


#industry fixed effects
R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "individual", model = "random")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = df)
R_reg_ind0 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "individual", model = "random")
R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate, data = dataset, effect = "individual", model = "random")
R_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = dataset, effect = "individual", model = "random")
R_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = dataset, effect = "individual", model = "random")
#R_reg_ind4 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = dataset, effect = "individual", model = "random")
#R_reg_ind5 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = dataset, effect = "individual", model = "random")
R_reg_ind6 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = dataset, effect = "individual", model = "random")
#R_reg_ind7 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = dataset, effect = "individual", model = "random")
#R_reg_ind8 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = dataset, effect = "individual", model = "random")
#R_reg_ind9 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = dataset, effect = "individual", model = "random")
#R_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = dataset, effect = "individual", model = "random")
#R_reg_ind11 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = dataset, effect = "individual", model = "random")
R_reg_ind12 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "individual", model = "random")
R_reg_ind13 <- plm(dataset$mean_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "individual", model = "random")
R_reg_ind14 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "individual", model = "random")

# Summary for FE models
#R_model_list <- list(R_reg_ind0, R_reg_ind1, R_reg_ind2, R_reg_ind3, R_reg_ind4, R_reg_ind5,R_reg_ind6,R_reg_ind7,R_reg_ind8,R_reg_ind9,R_reg_ind10,R_reg_ind11,R_reg_ind12, R_reg_ind13)
R_model_list <- list(R_reg_ind0, R_reg_ind1, R_reg_ind2, R_reg_ind6, R_reg_ind14, R_reg_ind12, R_reg_ind13)

R_regression_table <- stargazer(R_model_list,
                                 title = "FE Regression Results - individual (industry)",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(R_regression_table, file = "R_regression_table_output.txt")
write.csv(R_regression_table, "R_regression_table.csv", row.names = FALSE)

#two-way (time and individual) fixed effects
R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "twoway", model = "random")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density, data = df)
R_reg_ind0 <- plm(dataset$mean_nok ~ dataset$union_density, data = dataset, effect = "twoway", model = "random")
R_reg_ind1 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate, data = dataset, effect = "twoway", model = "random")
R_reg_ind2 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = dataset, effect = "twoway", model = "random")
R_reg_ind3 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = dataset, effect = "twoway", model = "random")
#R_reg_ind4 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = dataset, effect = "twoway", model = "random")
#R_reg_ind5 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = dataset, effect = "twoway", model = "random")
R_reg_ind6 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = dataset, effect = "twoway", model = "random")
#R_reg_ind7 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = dataset, effect = "twoway", model = "random")
#R_reg_ind8 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = dataset, effect = "twoway", model = "random")
#R_reg_ind9 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = dataset, effect = "twoway", model = "random")
#R_reg_ind10 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = dataset, effect = "twoway", model = "random")
#R_reg_ind11 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = dataset, effect = "twoway", model = "random")
R_reg_ind12 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "random")
R_reg_ind13 <- plm(dataset$mean_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "random")
R_reg_ind14 <- plm(dataset$mean_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = dataset, effect = "twoway", model = "random")

# Summary for FE models
#R_model_list <- list(R_reg_ind0, R_reg_ind1, R_reg_ind2, R_reg_ind3, R_reg_ind4, R_reg_ind5,R_reg_ind6,R_reg_ind7,R_reg_ind8,R_reg_ind9,R_reg_ind10,R_reg_ind11,R_reg_ind12, R_reg_ind13)
R_model_list <- list(R_reg_ind0, R_reg_ind1, R_reg_ind2, R_reg_ind6, R_reg_ind14, R_reg_ind12, R_reg_ind13)

R_regression_table <- stargazer(R_model_list,
                                 title = "FE Regression Results - twoway",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)




#FE lower quartile

R_lowq_ind0 <- plm(dataset$lower_quartile_nok ~ dataset$union_density, data = df)
R_lowq_ind1 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate, data = df)
R_lowq_ind2 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = df)
R_lowq_ind3 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = df)
R_lowq_ind4 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = df)
R_lowq_ind5 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = df)
R_lowq_ind6 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = df)
R_lowq_ind7 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = df)
R_lowq_ind8 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = df)
R_lowq_ind9 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = df)
R_lowq_ind10 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = df)
R_lowq_ind11 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = df)
R_lowq_ind12 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
R_lowq_ind13 <- plm(dataset$lower_quartile_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
R_lowq_ind14 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)

# Summary for lower quartile FE models
#R_lowq_list <- list(R_lowq_ind0, R_lowq_ind1, R_lowq_ind2, R_lowq_ind3, R_lowq_ind4, R_lowq_ind5,R_lowq_ind6,R_lowq_ind7,R_lowq_ind8,R_lowq_ind9,R_lowq_ind10,R_lowq_ind11,R_lowq_ind12, R_lowq_ind13)
R_lowq_list <- list(R_lowq_ind0, R_lowq_ind1, R_lowq_ind2, R_lowq_ind6, R_lowq_ind14, R_lowq_ind12, R_lowq_ind13)
R_lowq_table <- stargazer(R_lowq_list,
                           title = "FE Regression Results",
                           align = TRUE,
                           type = "text",
                           model.names = TRUE,
                           dep.var.labels.include = TRUE)
capture.output(R_lowq_table, file = "R_lowq_table_output.txt")

#FE upper quartile

R_highq_ind0 <- plm(dataset$upper_quartile_nok ~ dataset$union_density, data = df)
R_highq_ind1 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate, data = df)
R_highq_ind2 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = df)
R_highq_ind3 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = df)
R_highq_ind4 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = df)
R_highq_ind5 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = df)
R_highq_ind6 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = df)
R_highq_ind7 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = df)
R_highq_ind8 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = df)
R_highq_ind9 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = df)
R_highq_ind10 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = df)
R_highq_ind11 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = df)
R_highq_ind12 <- plm(dataset$upper_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
R_highq_ind13 <- plm(dataset$upper_quartile_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
R_highq_ind14 <- plm(dataset$lower_quartile_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)

# Summary for lower quartile FE models
#R_highq_list <- list(R_highq_ind0, R_highq_ind1, R_highq_ind2, R_highq_ind3, R_highq_ind4, R_highq_ind5,R_highq_ind6,R_highq_ind7,R_highq_ind8,R_highq_ind9,R_highq_ind10,R_highq_ind11,R_highq_ind12, R_highq_ind13, R_highq_ind14)
R_highq_list <- list(R_highq_ind0, R_highq_ind1, R_highq_ind2, R_highq_ind6, R_highq_ind14, R_highq_ind12, R_highq_ind13)

R_highq_table <- stargazer(R_highq_list,
                            title = "FE Regression Results",
                            align = TRUE,
                            type = "text",
                            model.names = TRUE,
                            dep.var.labels.include = TRUE)
capture.output(R_highq_table, file = "R_highq_table_output.txt")

#FE median

R_median_ind0 <- plm(dataset$median_nok ~ dataset$union_density, data = df)
R_median_ind1 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate, data = df)
R_median_ind2 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio, data = df)
R_median_ind3 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs, data = df)
R_median_ind4 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor, data = df)
R_median_ind5 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master , data = df)
R_median_ind6 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor, data = df)
R_median_ind7 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio + dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor + dataset$teenager, data = df)
R_median_ind8 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties, data = df)
R_median_ind9 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties, data = df)
R_median_ind10 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties, data = df)
R_median_ind11 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties, data = df)
R_median_ind12 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
R_median_ind13 <- plm(dataset$median_nok ~ dataset$collective_rate + dataset$male_ratio +dataset$has.education.finished.hs + dataset$has.education.bachelor + dataset$has.education.master +dataset$has.education.doctor +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)
R_median_ind14 <- plm(dataset$median_nok ~ dataset$union_density + dataset$collective_rate + dataset$male_ratio +  dataset$teenager + dataset$twenties + dataset$thirties + dataset$fourties + dataset$fifties + dataset$sixties, data = df)

# Summary for lower quartile FE models
#R_median_list <- list(R_median_ind0, R_median_ind1, R_median_ind2, R_median_ind3, R_median_ind4, R_median_ind5,R_median_ind6,R_median_ind7,R_median_ind8,R_median_ind9,R_median_ind10,R_median_ind11,R_median_ind12, R_median_ind13)
R_median_list <- list(R_median_ind0, R_median_ind1, R_median_ind2, R_median_ind6, R_median_ind14, R_median_ind12, R_median_ind13)
R_median_table <- stargazer(R_median_list,
                             title = "FE Regression Results",
                             align = TRUE,
                             type = "text",
                             model.names = TRUE,
                             dep.var.labels.include = TRUE)
capture.output(R_median_table, file = "R_median_table_output.txt")

#combining stargazer for all FEs

R_total_list <- list(R_median_list, R_highq_list, R_lowq_list, R_model_list)
R_total_table <- stargazer(R_total_list,
                            title = "FE Regression Results",
                            align = TRUE,
                            type = "text",
                            model.names = TRUE,
                            dep.var.labels.include = TRUE)
capture.output(R_total_table, file = "R_total_table_output.txt")

