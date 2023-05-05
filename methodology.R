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
#   OLS_pool_vif0 <- vif(OLS_pool_ind0) #vif requires multiple independent variables
OLS_pool_vif1 <- vif(OLS_pool_ind1)
OLS_pool_vif2 <- vif(OLS_pool_ind2)
OLS_pool_vif3 <- vif(OLS_pool_ind3)
OLS_pool_vif6 <- vif(OLS_pool_ind6)
OLS_pool_vif12 <- vif(OLS_pool_ind12)
OLS_pool_vif13 <- vif(OLS_pool_ind13)
OLS_pool_vif14 <- vif(OLS_pool_ind14)
OLS_pool_vif15 <- vif(OLS_pool_ind15)
OLS_vif_list <- list(OLS_pool_vif1, OLS_pool_vif2, OLS_pool_vif6, OLS_pool_vif14, OLS_pool_vif12, OLS_pool_vif13, OLS_pool_vif15)

OLS_vif_table <- stargazer(OLS_vif_list,
                                  title = "Pooled OLS VIF Results"
                                ,  out = "results/Pooled_ols_vif_table_output.html"
                                ,  align = TRUE
                                ,  type = "text"
                                # , model.names = TRUE
                                # , dep.var.labels.include = TRUE
)

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
#This is found in results 5.10
specification_list <- list(OLS_pool_ind12, FE_entity_ind12, FE_time_ind12, FE_reg_ind12)


FE_12 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset,  model = "within")


# Fixed effects transformation
#logdataset_transformed <- demean(logdataset, index = "your_index_variable")
#
# OLS regression on transformed variables
#FE_12_ols <- lm(mean_nok ~ union_density + collective_rate + male_ratio +
#                  has.education.finished.hs + has.education.bachelor + has.education.master +
#                  has.education.doctor + teenager + twenties + thirties +
#                  fourties + fifties + sixties,
#                data = logdataset_transformed)
#
# Calculate VIF
#FE_vif12 <- vif(FE_12_ols)
#
#
#
#FE_12 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset,  model = "within")
#FE_vif12 <- vif(FE_12)



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


#Next we make a regression table for Results 5.2

##Median
#pooled ols
OLS_pool_median <- plm(logdataset$median_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, model = "pooling")
#entity fixed effects
FE_entity_median <- plm(logdataset$median_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "individual", model = "within")
#time fixed effects
FE_time_median <- plm(logdataset$median_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "within")
#twoway fixed effects
FE_reg_median <- plm(logdataset$median_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "twoway", model = "within")



specification_list <- list(OLS_pool_median, FE_entity_median, FE_time_median, FE_reg_median)

FE_regression_table <- stargazer(specification_list,
                                 title = "Median Regression results",
                                 out = "results/Median_output.html",
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
                                                  c("Industry FE", "No", "Yes","No","Yes")))

##Lower Quartile
#pooled ols
OLS_pool_lowq <- plm(logdataset$lower_quartile_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, model = "pooling")
#entity fixed effects
FE_entity_lowq <- plm(logdataset$lower_quartile_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "individual", model = "within")
#time fixed effects
FE_time_lowq <- plm(logdataset$lower_quartile_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "within")
#twoway fixed effects
FE_reg_lowq <- plm(logdataset$lower_quartile_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "twoway", model = "within")



specification_list <- list(OLS_pool_lowq, FE_entity_lowq, FE_time_lowq, FE_reg_lowq)

FE_regression_table <- stargazer(specification_list,
                                 title = "Lower Quartile Regression results",
                                 out = "results/Lowq_output.html",
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
                                                  c("Industry FE", "No", "Yes","No","Yes")))

##Upper Quartile
#pooled ols
OLS_pool_upperq <- plm(logdataset$upper_quartile_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, model = "pooling")
#entity fixed effects
FE_entity_upperq <- plm(logdataset$upper_quartile_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "individual", model = "within")
#time fixed effects
FE_time_upperq <- plm(logdataset$upper_quartile_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "within")
#twoway fixed effects
FE_reg_upperq <- plm(logdataset$upper_quartile_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "twoway", model = "within")



specification_list <- list(OLS_pool_upperq, FE_entity_upperq, FE_time_upperq, FE_reg_upperq)



FE_regression_table <- stargazer(specification_list,
                               title = "Upper Quartile Regression results",
                               out = "results/Upperq_output.html",
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
                                                c("Industry FE", "No", "Yes","No","Yes")))



#Wage Distribution regression table

specification_list <- list(OLS_pool_median, FE_time_median, 
                           OLS_pool_lowq, FE_time_lowq, 
                           OLS_pool_upperq, FE_time_upperq)

FE_regression_table <- stargazer(specification_list,
                                 title = "Quartile Regression results",
                                 out = "results/Quartile_output.html",
                                 
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)

vregression_table <- stargazer(specification_list,
                               title = "Regression Results across Quartiles",
                               out = "results/Quartile_output.html",
                               column.labels = c("Pooled OLS", "Time Fixed Effects", "Pooled OLS", "Time Fixed Effects", "Pooled OLS", "Time Fixed Effects"),
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
                               add.lines = list(c("Time FE", "No", "Yes", "No", "Yes","No", "Yes"),
                                                c("Industry FE", "No", "No","No","No","No","No")))


#Lets try random effects for the fun of it



# Estimate the fixed effects model (twoway)

random_ind0 <- plm(logdataset$mean_nok ~ logdataset$union_density, effect = "time", data = logdataset, model = "random")
random_ind1 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate, data = logdataset, effect = "time", model = "random")
random_ind2 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio, data = logdataset, effect = "time", model = "random")
random_ind3 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs, data = logdataset, effect = "time", model = "random")
random_ind6 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio + logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor, data = logdataset, effect = "time", model = "random")
random_ind12 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "random")
random_ind13 <- plm(logdataset$mean_nok ~ logdataset$collective_rate + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "random")
random_ind14 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$collective_rate + logdataset$male_ratio +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "random")
random_ind15 <- plm(logdataset$mean_nok ~ logdataset$union_density + logdataset$male_ratio +logdataset$has.education.finished.hs + logdataset$has.education.bachelor + logdataset$has.education.master +logdataset$has.education.doctor +  logdataset$teenager + logdataset$twenties + logdataset$thirties + logdataset$fourties + logdataset$fifties + logdataset$sixties, data = logdataset, effect = "time", model = "random")

FE_model_list <- list(random_ind0, random_ind1, random_ind2, random_ind6, random_ind14, random_ind12, random_ind13,random_ind15)

# Create a stargazer table for the fixed effects model
FE_regression_table <- stargazer(FE_model_list,
                                 title = "Random Time Effects Regression Results",
                                 out = "results/Random_output.html",
                                 
                                 align = TRUE,
                                 type = "text",
                                 model.names = TRUE,
                                 dep.var.labels.include = TRUE)


plm::pdim(FE_reg_ind12)

