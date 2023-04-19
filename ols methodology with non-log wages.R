#ols methodology with non-log wages

#OLS methodology

# Methodology. Run data.R first

# Selecting variables of interest for OLS (and testing)
ds <- df %>%
  select(year, parentcode_indus, industryparentname, union_density, mean_nok)

#making log of wage for later
logdataset <- dataset %>%
  mutate_if(is.numeric, log)
# Dataset for panel data
dataset <- pdata.frame(df, index = c("year", "industryparentname", "parentcode_indus"))

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

OLS_lowq_ind0 <- plm(df$lower_quartile_nok ~ df$union_density, data = df)
OLS_lowq_ind1 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate, data = df)
OLS_lowq_ind2 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = df)
OLS_lowq_ind3 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = df)
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

OLS_highq_ind0 <- plm(df$upper_quartile_nok ~ df$union_density, data = df)
OLS_highq_ind1 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate, data = df)
OLS_highq_ind2 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = df)
OLS_highq_ind3 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = df)
OLS_highq_ind4 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor, data = df)
OLS_highq_ind5 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master , data = df)
OLS_highq_ind6 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor, data = df)
OLS_highq_ind7 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor + df$teenager, data = df)
OLS_highq_ind8 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties, data = df)
OLS_highq_ind9 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties, data = df)
OLS_highq_ind10 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties, data = df)
OLS_highq_ind11 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties, data = df)
OLS_highq_ind12 <- plm(df$upper_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_highq_ind13 <- plm(df$upper_quartile_nok ~ df$collective_rate + df$male_ratio +df$has.education.finished.hs + df$has.education.bachelor + df$has.education.master +df$has.education.doctor +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)
OLS_highq_ind14 <- plm(df$lower_quartile_nok ~ df$union_density + df$collective_rate + df$male_ratio +  df$teenager + df$twenties + df$thirties + df$fourties + df$fifties + df$sixties, data = df)

# Summary for lower quartile OLS models
#OLS_highq_list <- list(OLS_highq_ind0, OLS_highq_ind1, OLS_highq_ind2, OLS_highq_ind3, OLS_highq_ind4, OLS_highq_ind5,OLS_highq_ind6,OLS_highq_ind7,OLS_highq_ind8,OLS_highq_ind9,OLS_highq_ind10,OLS_highq_ind11,OLS_highq_ind12, OLS_highq_ind13, OLS_highq_ind14)
OLS_highq_list <- list(OLS_highq_ind0, OLS_highq_ind1, OLS_highq_ind2, OLS_highq_ind6, OLS_highq_ind14, OLS_highq_ind12, OLS_highq_ind13)

OLS_highq_table <- stargazer(OLS_highq_list,
                             title = "OLS Regression Results",
                             align = TRUE,
                             type = "text",
                             model.names = TRUE,
                             dep.var.labels.include = TRUE)
capture.output(OLS_highq_table, file = "OLS_highq_table_output.txt")

#OLS median

OLS_median_ind0 <- plm(df$median_nok ~ df$union_density, data = df)
OLS_median_ind1 <- plm(df$median_nok ~ df$union_density + df$collective_rate, data = df)
OLS_median_ind2 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio, data = df)
OLS_median_ind3 <- plm(df$median_nok ~ df$union_density + df$collective_rate + df$male_ratio + df$has.education.finished.hs, data = df)
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

#combining stargazer for all OLSs

OLS_total_list <- list(OLS_median_list, OLS_highq_list, OLS_lowq_list, OLS_model_list)
OLS_total_table <- stargazer(OLS_total_list,
                             title = "OLS Regression Results",
                             align = TRUE,
                             type = "text",
                             model.names = TRUE,
                             dep.var.labels.include = TRUE)
capture.output(OLS_total_table, file = "OLS_total_table_output.txt")
