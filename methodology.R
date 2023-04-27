# Methodology. Run data.R first

# Selecting variables of interest for OLS (and testing)
ds <- df %>%
  select(year, parentcode_indus, industryparentname, union_density, mean_nok)


logdf <- df %>%
  mutate(mean_nok = log(mean_nok))%>%
  mutate(median_nok = log(median_nok))%>%
  mutate(lower_quartile_nok = log(lower_quartile_nok))%>%
  mutate(upper_quartile_nok = log(upper_quartile_nok))

#Panel Data Set
logdataset <- pdata.frame(logdf, index = c("industryparentname", "year"))


# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#OLS_reg_ind1 <- plm(logdf$mean_nok ~ logdf$union_density, data = logdf)
OLS_reg_ind0 <- plm(logdf$mean_nok ~ logdf$union_density, data = logdf, model = "pooling")
OLS_reg_ind1 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate, data = logdf)
OLS_reg_ind2 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio, data = logdf)
OLS_reg_ind3 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs, data = logdf)
#OLS_reg_ind4 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor, data = logdf)
#OLS_reg_ind5 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master , data = logdf)
OLS_reg_ind6 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor, data = logdf)
#OLS_reg_ind7 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor + logdf$teenager, data = logdf)
#OLS_reg_ind8 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties, data = logdf)
#OLS_reg_ind9 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties, data = logdf)
#OLS_reg_ind10 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties, data = logdf)
#OLS_reg_ind11 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties, data = logdf)
OLS_reg_ind12 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_reg_ind13 <- plm(logdf$mean_nok ~ logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_reg_ind14 <- lm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_reg_ind15 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)

# Summary for OLS models
#OLS_model_list <- list(OLS_reg_ind0, OLS_reg_ind1, OLS_reg_ind2, OLS_reg_ind3, OLS_reg_ind4, OLS_reg_ind5,OLS_reg_ind6,OLS_reg_ind7,OLS_reg_ind8,OLS_reg_ind9,OLS_reg_ind10,OLS_reg_ind11,OLS_reg_ind12, OLS_reg_ind13)
OLS_model_list <- list(OLS_reg_ind0, OLS_reg_ind1, OLS_reg_ind2, OLS_reg_ind6, OLS_reg_ind14, OLS_reg_ind12, OLS_reg_ind13, OLS_reg_ind15)

OLS_regression_table <- stargazer(OLS_model_list,
                                  title = "OLS Regression Results",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(OLS_regression_table, file = "results/OLS_regression_table_output.txt")


#Pooled OLS
# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#OLS_pool_ind1 <- plm(logdataset$mean_nok ~ logdataset$union_density, data = logdataset)
OLS_pool_ind0 <- plm(logdataset$mean_nok ~ logdataset$union_density, data = logdataset, model = "pooling")
OLS_pool_ind1 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate, data = logdataset, model = "pooling")
OLS_pool_ind2 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio, data = logdataset, model = "pooling")
OLS_pool_ind3 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs, data = logdataset, model = "pooling")
#OLS_pool_ind4 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs + logdataset$has.education.bachelor, data = logdataset, model = "pooling")
#OLS_pool_ind5 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master , data = logdataset, model = "pooling")
OLS_pool_ind6 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor, data = logdataset, model = "pooling")
#OLS_pool_ind7 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor + logdataset$teenager, data = logdataset, model = "pooling")
#OLS_pool_ind8 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties, data = logdataset, model = "pooling")
#OLS_pool_ind9 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties, data = logdataset, model = "pooling")
#OLS_pool_ind10 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties, data = logdataset, model = "pooling")
#OLS_pool_ind11 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties, data = logdataset, model = "pooling")
OLS_pool_ind12 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, model = "pooling")
OLS_pool_ind13 <- plm(logdataset$mean_nok ~ logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, model = "pooling")
OLS_pool_ind14 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, model = "pooling")
OLS_pool_ind15 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, model = "pooling")

# Summary for OLS models
#OLS_model_list <- list(OLS_pool_ind0, OLS_pool_ind1, OLS_pool_ind2, OLS_pool_ind3, OLS_pool_ind4, OLS_pool_ind5,OLS_pool_ind6,OLS_pool_ind7,OLS_pool_ind8,OLS_pool_ind9,OLS_pool_ind10,OLS_pool_ind11,OLS_pool_ind12, OLS_pool_ind13)
OLS_model_list <- list(OLS_pool_ind0, OLS_pool_ind1, OLS_pool_ind2, OLS_pool_ind6, OLS_pool_ind14, OLS_pool_ind12, OLS_pool_ind13, OLS_pool_ind15)

OLS_regression_table <- stargazer(OLS_model_list,
                                  title = "Pooled OLS Regression Results",
                                  out = "results/Pooled_ols_regression_table_output.html",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(OLS_regression_table, file = "results/Pooled_OLS_regression_table_output.txt")


#OLS lower quartile

OLS_lowq_ind0 <- plm(logdf$lower_quartile_nok ~ logdf$union_density, data = logdf)
OLS_lowq_ind1 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate, data = logdf)
OLS_lowq_ind2 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio, data = logdf)
OLS_lowq_ind3 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs, data = logdf)
OLS_lowq_ind4 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor, data = logdf)
OLS_lowq_ind5 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master , data = logdf)
OLS_lowq_ind6 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor, data = logdf)
OLS_lowq_ind7 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor + logdf$teenager, data = logdf)
OLS_lowq_ind8 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties, data = logdf)
OLS_lowq_ind9 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties, data = logdf)
OLS_lowq_ind10 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties, data = logdf)
OLS_lowq_ind11 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties, data = logdf)
OLS_lowq_ind12 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_lowq_ind13 <- plm(logdf$lower_quartile_nok ~ logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_lowq_ind14 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_lowq_ind15 <- plm(logdf$lower_quartile_nok ~ logdf$union_density + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)

# Summary for lower quartile OLS models
#OLS_lowq_list <- list(OLS_lowq_ind0, OLS_lowq_ind1, OLS_lowq_ind2, OLS_lowq_ind3, OLS_lowq_ind4, OLS_lowq_ind5,OLS_lowq_ind6,OLS_lowq_ind7,OLS_lowq_ind8,OLS_lowq_ind9,OLS_lowq_ind10,OLS_lowq_ind11,OLS_lowq_ind12, OLS_lowq_ind13)
OLS_lowq_list <- list(OLS_lowq_ind0, OLS_lowq_ind1, OLS_lowq_ind2, OLS_lowq_ind6, OLS_lowq_ind14, OLS_lowq_ind12, OLS_lowq_ind13, OLS_lowq_ind15)
OLS_lowq_table <- stargazer(OLS_lowq_list,
                                  title = "OLS Regression Results",
                                  align = TRUE,
                                  type = "text",
                                  model.names = TRUE,
                                  dep.var.labels.include = TRUE)
capture.output(OLS_lowq_table, file = "results/OLS_lowq_table_output.txt")

#OLS upper quartile

OLS_upperq_ind0 <- plm(logdf$upper_quartile_nok ~ logdf$union_density, data = logdf)
OLS_upperq_ind1 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate, data = logdf)
OLS_upperq_ind2 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio, data = logdf)
OLS_upperq_ind3 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs, data = logdf)
OLS_upperq_ind4 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor, data = logdf)
OLS_upperq_ind5 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master , data = logdf)
OLS_upperq_ind6 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor, data = logdf)
OLS_upperq_ind7 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor + logdf$teenager, data = logdf)
OLS_upperq_ind8 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties, data = logdf)
OLS_upperq_ind9 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties, data = logdf)
OLS_upperq_ind10 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties, data = logdf)
OLS_upperq_ind11 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties, data = logdf)
OLS_upperq_ind12 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_upperq_ind13 <- plm(logdf$upper_quartile_nok ~ logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_upperq_ind14 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_upperq_ind15 <- plm(logdf$upper_quartile_nok ~ logdf$union_density + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)


# Summary for lower quartile OLS models
#OLS_upperq_list <- list(OLS_upperq_ind0, OLS_upperq_ind1, OLS_upperq_ind2, OLS_upperq_ind3, OLS_upperq_ind4, OLS_upperq_ind5,OLS_upperq_ind6,OLS_upperq_ind7,OLS_upperq_ind8,OLS_upperq_ind9,OLS_upperq_ind10,OLS_upperq_ind11,OLS_upperq_ind12, OLS_upperq_ind13, OLS_upperq_ind14)
OLS_upperq_list <- list(OLS_upperq_ind0, OLS_upperq_ind1, OLS_upperq_ind2, OLS_upperq_ind6, OLS_upperq_ind14, OLS_upperq_ind12, OLS_upperq_ind13, OLS_upperq_ind15)

OLS_upperq_table <- stargazer(OLS_upperq_list,
                            title = "OLS Regression Results",
                            align = TRUE,
                            type = "text",
                            model.names = TRUE,
                            dep.var.labels.include = TRUE)
capture.output(OLS_upperq_table, file = "results/OLS_upperq_table_output.txt")

#OLS median

OLS_median_ind0 <- plm(logdf$median_nok ~ logdf$union_density, data = logdf)
OLS_median_ind1 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate, data = logdf)
OLS_median_ind2 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio, data = logdf)
OLS_median_ind3 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs, data = logdf)
OLS_median_ind4 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor, data = logdf)
OLS_median_ind5 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master , data = logdf)
OLS_median_ind6 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor, data = logdf)
OLS_median_ind7 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor + logdf$teenager, data = logdf)
OLS_median_ind8 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties, data = logdf)
OLS_median_ind9 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties, data = logdf)
OLS_median_ind10 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties, data = logdf)
OLS_median_ind11 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties, data = logdf)
OLS_median_ind12 <- plm(logdf$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_median_ind13 <- plm(logdf$median_nok ~ logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_median_ind14 <- plm(logdataset$median_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)
OLS_median_ind15 <- plm(logdf$median_nok ~ logdf$union_density + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdf)

# Summary for lower quartile OLS models
#OLS_median_list <- list(OLS_median_ind0, OLS_median_ind1, OLS_median_ind2, OLS_median_ind3, OLS_median_ind4, OLS_median_ind5,OLS_median_ind6,OLS_median_ind7,OLS_median_ind8,OLS_median_ind9,OLS_median_ind10,OLS_median_ind11,OLS_median_ind12, OLS_median_ind13)
OLS_median_list <- list(OLS_median_ind0, OLS_median_ind1, OLS_median_ind2, OLS_median_ind6, OLS_median_ind14, OLS_median_ind12, OLS_median_ind13,OLS_median_ind15)
OLS_median_table <- stargazer(OLS_median_list,
                             title = "OLS Regression Results",
                             align = TRUE,
                             type = "text",
                             model.names = TRUE,
                             dep.var.labels.include = TRUE)
capture.output(OLS_median_table, file = "results/OLS_median_table_output.txt")

# Estimate the fixed effects model (twoway)

FE_reg_ind0 <- plm(logdataset$mean_nok ~ logdataset$union_density, effect = "twoway", data = logdataset, model = "within")
FE_reg_ind1 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate, data = logdataset, effect = "twoway", model = "within")
FE_reg_ind2 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio, data = logdataset, effect = "twoway", model = "within")
FE_reg_ind3 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs, data = logdataset, effect = "twoway", model = "within")
FE_reg_ind6 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor, data = logdataset, effect = "twoway", model = "within")
FE_reg_ind12 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "twoway", model = "within")
FE_reg_ind13 <- plm(logdataset$mean_nok ~ logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "twoway", model = "within")
FE_reg_ind14 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "twoway", model = "within")
FE_reg_ind15 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "twoway", model = "within")

FE_model_list <- list(FE_reg_ind0, FE_reg_ind1, FE_reg_ind2, FE_reg_ind6, FE_reg_ind14, FE_reg_ind12, FE_reg_ind13,FE_reg_ind15)

# Create a stargazer table for the fixed effects model
FE_regression_table <- stargazer(FE_model_list,
                                 title = "Two way Fixed Effects Regression Results",
                                 out = "results/FE_regression_output.html",
                                 
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(FE_regression_table, file = "results/FE_regression_output.txt")

# Run plm with dependent variable "mean_nok" and independent variable uniondensity
FE_time_ind0 <- plm(logdataset$mean_nok ~ logdataset$union_density, effect = "time", data = logdataset, model = "within")
FE_time_ind1 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate, data = logdataset, effect = "time", model = "within")
FE_time_ind2 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio, data = logdataset, effect = "time", model = "within")
FE_time_ind3 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs, data = logdataset, effect = "time", model = "within")
FE_time_ind6 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor, data = logdataset, effect = "time", model = "within")
FE_time_ind12 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "within")
FE_time_ind13 <- plm(logdataset$mean_nok ~ logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "within")
FE_time_ind14 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "within")
FE_time_ind15 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "within")

FE_model_list <- list(FE_time_ind0, FE_time_ind1, FE_time_ind2, FE_time_ind6, FE_time_ind14, FE_time_ind12, FE_time_ind13,FE_time_ind15)


FE_regression_table <- stargazer(FE_model_list,
                                 title = "Time FE Regression Results",
                                 out = "results/FE_time_effect_output.html",
                                 
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)


# Save the table to a text file
capture.output(FE_regression_table, file = "results/FE_time_effect_output.txt")



# Run plm with dependent variable "mean_nok" and independent variable uniondensity
FE_entity_ind0 <- plm(logdataset$mean_nok ~ logdataset$union_density, effect = "individual", data = logdataset, model = "within")
FE_entity_ind1 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate, data = logdataset, effect = "individual", model = "within")
FE_entity_ind2 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio, data = logdataset, effect = "individual", model = "within")
FE_entity_ind3 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs, data = logdataset, effect = "individual", model = "within")
FE_entity_ind6 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor, data = logdataset, effect = "individual", model = "within")
FE_entity_ind12 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "individual", model = "within")
FE_entity_ind13 <- plm(logdataset$mean_nok ~ logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "individual", model = "within")
FE_entity_ind14 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "individual", model = "within")
FE_entity_ind15 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "individual", model = "within")

FE_model_list <- list(FE_entity_ind0, FE_entity_ind1, FE_entity_ind2, FE_entity_ind6, FE_entity_ind14, FE_entity_ind12, FE_entity_ind13,FE_entity_ind15)


FE_regression_table <- stargazer(FE_model_list,
                                 title = "Entity FE Regression Results",
                                 out = "results/FE_entity_effect_output.html",
                                 
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)


# Save the table to a text file
capture.output(FE_regression_table, file = "results/FE_entity_effect_output.txt")



#Here we make a regression for one specification but with different models
specification_list <- list(OLS_pool_ind12, FE_entity_ind12, FE_time_ind12, FE_reg_ind12)


regression_table <- stargazer(specification_list,
                                 title = "Regression Results",
                              out = "results/specification_output.html",
                              column.labels = c("Pooled OLS", "Industry Fixed Effects", "Time Fixed Effects", "Two-way Fixed Effects"),
                              covariate.labels = c("D - Labour Union Denisty", "C - Collective Coverage Rate",
                                                   "V - Gender - Male-Ratio",
                                                   "V - Education - High School","V - Education - Bachelor"
                                                   ,"V - Education - Master","V - Education - Doctor",
                              "V - Age-Group (15-19)","V - Age-Group (20-29)","V - Age-Group (30-39)","V - Age-Group (40-49)",
                              "V - Age-Group (50-59)", "V - Age-Group (60-69)"),
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                              model.numbers = TRUE,
                                 dep.var.labels.include = TRUE,
                              add.lines = list(c("Time FE", "No", "No", "Yes", "Yes"),
                                               c("Industry FE", "No", "Yes", "No", "Yes")))



# Estimate the fixed effects model
FE_ind_ind1 <- plm(logdataset$mean_nok ~ logdataset$union_density, data = logdataset, effect = "individual", model = "within")
FE_ind_ind2 <- plm(logdataset$mean_nok ~ logdataset$union_density +
                     logdataset$collective_rate, data = logdataset, effect = "individual", model = "within")
FE_ind_ind3 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate +
                     logdataset$male_ratio, data = logdataset, effect = "individual", model = "within")
FE_ind_ind10 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "individual", model = "within")

FE_model_list <- list(FE_ind_ind1,FE_ind_ind2,FE_ind_ind3, FE_ind_ind10)
# Create a stargazer table for the fixed effects model
FE_regression_table <- stargazer(FE_model_list,
                                 title = "Fixed Effects Regression Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

# Save the table to a text file
capture.output(FE_regression_table, file = "results/FE_ind_effect_output.txt")
write.csv(FE_regression_table, "FE_regression_table.csv", row.names = FALSE)


# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_ind_ind1 <- plm(logdf$mean_nok ~ logdf$union_density, data = logdataset, effect = "individual", model = "within")
FE_ind_ind0 <- plm(logdf$mean_nok ~ logdf$union_density, data = logdataset, effect = "individual", model = "within")
FE_ind_ind1 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate, data = logdataset, effect = "individual", model = "within")
FE_ind_ind2 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio, data = logdataset, effect = "individual", model = "within")
FE_ind_ind3 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs, data = logdataset, effect = "individual", model = "within")
#FE_ind_ind4 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor, data = logdataset, effect = "individual", model = "within")
#FE_ind_ind5 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master , data = logdataset, effect = "individual", model = "within")
FE_ind_ind6 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor, data = logdataset, effect = "individual", model = "within")
#FE_ind_ind7 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor + logdf$teenager, data = logdataset, effect = "individual", model = "within")
#FE_ind_ind8 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties, data = logdataset, effect = "individual", model = "within")
#FE_ind_ind9 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties, data = logdataset, effect = "individual", model = "within")
#FE_ind_ind10 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties, data = logdataset, effect = "individual", model = "within")
#FE_ind_ind11 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties, data = logdataset, effect = "individual", model = "within")
FE_ind_ind12 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "individual", model = "within")
FE_ind_ind13 <- plm(logdf$mean_nok ~ logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "individual", model = "within")
FE_ind_ind14 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "individual", model = "within")

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
capture.output(FE_regression_table, file = "results/FE_ind_effect_output.txt")






# Run plm with dependent variable "mean_nok" and independent variable uniondensity
#FE_twoway_ind1 <- plm(logdf$mean_nok ~ logdf$union_density, data = logdataset, effect = "twoway", model = "within")
FE_twoway_ind0 <- plm(logdf$mean_nok ~ logdf$union_density, data = logdataset, effect = "twoway", model = "within")
FE_twoway_ind1 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate, data = logdataset, effect = "twoway", model = "within")
FE_twoway_ind2 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio, data = logdataset, effect = "twoway", model = "within")
FE_twoway_ind3 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs, data = logdataset, effect = "twoway", model = "within")
#FE_twoway_ind4 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor, data = logdataset, effect = "twoway", model = "within")
#FE_twoway_ind5 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master , data = logdataset, effect = "twoway", model = "within")
FE_twoway_ind6 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor, data = logdataset, effect = "twoway", model = "within")
#FE_twoway_ind7 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor + logdf$teenager, data = logdataset, effect = "twoway", model = "within")
#FE_twoway_ind8 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties, data = logdataset, effect = "twoway", model = "within")
#FE_twoway_ind9 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties, data = logdataset, effect = "twoway", model = "within")
#FE_twoway_ind10 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties, data = logdataset, effect = "twoway", model = "within")
#FE_twoway_ind11 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties, data = logdataset, effect = "twoway", model = "within")
FE_twoway_ind12 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "twoway", model = "within")
FE_twoway_ind13 <- plm(logdf$mean_nok ~ logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "twoway", model = "within")
FE_twoway_ind14 <- plm(logdf$mean_nok ~ logdf$union_density + logdf$collective_rate + logdf$male_ratio +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "twoway", model = "within")

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
capture.output(FE_regression_table, file = "results/FE_twoway_effect_output.txt")








# Merge the two tables
merged_table <- paste(OLS_regression_table, FE_regression_table)

# Save the merged table to a text file
capture.output(merged_table, file = "results/combined_regression_table_output.txt")


###In this section I will regress first lower quartile, then median, then upper quartile


FE_reg_low <- plm(logdataset$lower_quartile_nok ~ logdataset$union_density + logdataset$collective_rate
                     , data = logdataset, effect = "twoways", model = "within")

FE_reg_med <- plm(logdataset$median_nok ~ logdataset$union_density + logdataset$collective_rate
                  , data = logdataset, effect = "twoways", model = "within")

FE_reg_upper <- plm(logdataset$upper_quartile_nok ~ logdataset$union_density + logdataset$collective_rate
                  , data = logdataset, effect = "twoways", model = "within")
FE_distribution_list <- list(FE_reg_low, FE_reg_med, FE_reg_upper)

FE_distrib_reg_table <- stargazer(FE_distribution_list,
                                 title = "Fixed Effects Regression on wage quartiles Results",
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

capture.output(FE_distrib_reg_table, file = "results/distribution_regression.txt")

FE_reg_low_control <- plm(logdataset$lower_quartile_nok ~ logdataset$union_density + logdataset$collective_rate
                  + logdataset$male_ratio + logdataset$has.education.bachelor + logdataset$has.education.master 
                  + logdataset$has.education.doctor, data = logdataset, effect = "twoways", model = "within")

FE_reg_med_control <- plm(logdataset$median_nok ~ logdataset$union_density + logdataset$collective_rate
                          + logdataset$male_ratio + logdataset$has.education.bachelor + logdataset$has.education.master 
                          + logdataset$has.education.doctor, data = logdataset, effect = "twoways", model = "within")

FE_reg_upper_control <- plm(logdataset$upper_quartile_nok ~ logdataset$union_density + logdataset$collective_rate
                            + logdataset$male_ratio + logdataset$has.education.bachelor + logdataset$has.education.master 
                            + logdataset$has.education.doctor, data = logdataset, effect = "twoways", model = "within")

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


capture.output(FE_distrib_reg_table_control, file = "results/distribution_regression_control.txt")

