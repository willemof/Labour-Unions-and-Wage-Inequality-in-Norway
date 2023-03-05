x_agg_vis_16 <- read_csv(file="csv/ssb/x_agg_vis.csv")

monthlywage15_22_expand <- read_csv(file = ("csv/monthlywage15_22_expand.csv"))

monthlywage15 <- monthlywage15_22_expand %>%
  filter("Both sexes" == sex) %>%
  filter( %in%  )
