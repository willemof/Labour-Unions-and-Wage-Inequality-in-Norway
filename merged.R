#NSD Merged with SSB

x_agg$industry <- as.character(x_agg$industry)


nsdssb <- inner_join(x_agg, ssb)

