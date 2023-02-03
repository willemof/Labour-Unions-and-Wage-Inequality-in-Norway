#GG plots
library(ggplot2)
library(plotly)

##plotly plots


fig <- plot_ly(data=nsdssb_2016,
               x= nsdssb_2016$is.union,
               y= nsdssb_2016$average_monthly_earnings_nok)

colnames(nsdssb_2016)

##after getting nsdssb filter for 2016 graphs


nsdssb_2016 <- nsdssb  %>%
  filter(year==2016) 


nsdssb_2016 <- nsdssb_2016 %>%
  mutate(number=1:NROW(nsdssb_2016))


#ggplotly
unionvearningsbyindustry <- ggplot(data=nsdssb_2016,
       aes(x= is.union,
           y= average_monthly_earnings_nok,
           colour=shortname
           ))+
  geom_point()    +theme(legend.position="none")
unionvearningsbyindustry <- unionvearningsbyindustry +
  ggtitle("Labour Union Density and Monthly Wages Across 88 Industries in 2016")+
  xlab("Labour Union Density")+
  ylab("Average Monthly Wage (NOK)")

fig <- ggplotly(unionvearningsbyindustry) 
fig <- style(fig,                 
      hovertext = paste0(nsdssb_2016$parentcode, nsdssb_2016$number,"--",
        nsdssb_2016$industry, " (", formatC(nsdssb_2016$is.union, digits=3),", ", formatC(nsdssb_2016$average_monthly_earnings_nok, format="f", big.mark =" ",digits=0)," NOK)."),
      hoverinfo = "text")
fig
unionvearningsbyindustry

##vignette("ggplot2-specs")
