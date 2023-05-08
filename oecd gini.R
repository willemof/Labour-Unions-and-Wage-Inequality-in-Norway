gini <- read_csv(file = ("csv/DP_LIVE_07052023192031824.csv"))
                 # Filter the data for the selected countries
selected_countries <- c("NOR", "USA", "GBR", "DEU",
                        "DNK", "SWE", "ISR", "OTO")
gini_filtered <- gini %>%
  filter(LOCATION %in% selected_countries) %>%
  filter(SUBJECT %in% c("GINI"))

graph1 <- ggplot(gini_filtered, aes(x = TIME, y = Value, color = LOCATION)) +
  geom_line(size = 1, show.legend = TRUE) +
  labs(title = "Gini from 2002 to 2020 Across Selected OECD Countries",
       x = "Year",
       y = "(%)",
       color = "Country") +
  scale_color_manual(values = c("NOR" = "red", "USA" = "gold", "GBR" = "turquoise", "DNK" = "lightgreen",
                                "SWE" = "pink","ISR" = "blue", "OTO" = "black","DEU" = "grey"))+
  theme_minimal()

graph1

ggsave(paste0("visualisations/","Unionization OVer Year",".svg"), graph1, width = 11, height = 8.5, units = "in")

ggplotly(graph1)
