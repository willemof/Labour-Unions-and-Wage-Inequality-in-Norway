
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

capture.output(QR_lowq_table, file = "results/lower_quartile_results.txt")

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

capture.output(QR_median_table, file = "results/Median_results.txt")

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

capture.output(QR_upperq_table, file = "results/upper_quartile_results.txt")




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

capture.output(QR_lowq_table, file = "results/lower_quartile_results.txt")


OLS_ff_ind0 <- plm(logdf$mean_nok ~ logdf$union_density_manuf, data = logdf)
OLS_ff_ind1 <- plm(logdf$mean_nok ~ logdf$union_density_manuf+union_density, data = logdf)
OLS_ff_ind2 <- plm(logdf$mean_nok ~ logdf$union_density_manuf +logdf$union_density + logdf$collective_rate + , data = logdf)
OLS_ff_ind3 <- plm(logdf$mean_nok ~ logdf$union_density_manuf+ logdf$union_density + logdf$collective_rate +  + logdf$male_ratio, data = logdf)
OLS_ff_ind4 <- plm(logdf$mean_nok ~ logdf$union_density_manuf+ logdf$union_density + logdf$collective_rate +  + logdf$male_ratio + logdf$has.education.finished.hs, data = logdf)

#front-runner
OLS_ff_ind0 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind1 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind2 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind3 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind4 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind5 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master , data = logdataset, effect = "individual", model = "within")
OLS_ff_ind6 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind7 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio + logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor + logdf$teenager, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind8 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind9 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind10 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind11 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind12 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind13 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$collective_rate + logdf$male_ratio +logdf$has.education.finished.hs + logdf$has.education.bachelor + logdf$has.education.master +logdf$has.education.doctor +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind14 <- plm(logdataset$mean_nok ~ union_density_manuf+logdf$union_density + logdf$collective_rate + logdf$male_ratio +  logdf$teenager + logdf$twenties + logdf$thirties + logdf$fourties + logdf$fifties + logdf$sixties, data = logdataset, effect = "individual", model = "within")
OLS_ff_ind15 <- plm(logdataset$mean_nok ~ union_density_manuf, data = logdataset, effect = "individual", model = "within")



model_ff_list <- list(OLS_ff_ind0,OLS_ff_ind1,OLS_ff_ind2,OLS_ff_ind3,OLS_ff_ind4,OLS_ff_ind5,OLS_ff_ind6,OLS_ff_ind7,OLS_ff_ind8,
                      OLS_ff_ind9,OLS_ff_ind10,OLS_ff_ind11,OLS_ff_ind12,OLS_ff_ind13,OLS_ff_ind14,OLS_ff_ind15)

OLS_ff_table <- stargazer(model_ff_list,
                          title = "Lower Quartile Regression Table",
                          align = TRUE,
                          type = "text",
                          model.names = TRUE,
                          dep.var.labels.include = TRUE)

capture.output(OLS_ff_table, file = "results/ff_results.txt")