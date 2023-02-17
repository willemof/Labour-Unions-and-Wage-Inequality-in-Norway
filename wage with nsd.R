#Load data

ind19wage_ds_expand<- read_csv(file = "csv/ssb/ind19wage_ds_expand.csv")
x_agg_vis <- read_csv(file="csv/ssb/x_agg_vis.csv")

ind19wage_ds_filter <- ind19wage_ds_expand %>%
  filter(year == 2013 | year == 2014) %>%
  filter(industry_sic2007 != "Total") %>%
  filter(sex == "Both sexes")

x_agg_vis <- x_agg_vis %>%
  filter(year == 2013 | year == 2014)  %>%
  filter(parentcode_indus != "U") %>%
  filter(parentcode_indus != "T") 

ind19wage_ds_filter <- ind19wage_ds_filter %>%
  mutate(industryparentname = parentname, .keep = "unused")


ind19wage_ds_expand$year<- as.character(ind19wage_ds_expand$year)
agemedianwage <- left_join(ind19wage_ds_filter, x_agg_vis)



g <- agemedianwage
g <- g %>%
  select(parentname, median_nok, sex, alder_aar, date) 



g <- pivot_wider(g, 
                 id_expand = FALSE,
                 names_from = sex,
                 values_from = median_nok)
g <- g %>% clean_names()





g <- g %>%
  filter(year==2014)

gg <- ggplot(data = g,
             mapping = aes(x=g$alder_aar,y=g$males)+geom_point())

gg


fig <- ggplotly(gg) 
fig <- style(fig,                 
             hovertext = paste0(g$industry_sic2007,"", g$kjonn, formatC(g$median_nok, format="f", big.mark =" ",digits=0)," NOK)."),
             hoverinfo = "text")
fig



h <- ind19wage_ds_filter_gender


gg <- ggplot(data = h,
             mapping = aes(x=g$alder_aar,y=g$mean_nok))+geom_point()

gg


fig <- ggplotly(gg) 
fig <- style(fig,                 
             hovertext = paste0(g$industry_sic2007,"",  formatC(g$median_nok, format="f", big.mark =" ",digits=0)," NOK)."),
             hoverinfo = "text")
fig




