oecd <- read_csv(file = ("csv/CBC_07052023172649150.csv"))

# Filter the data for the selected countries
selected_countries <- c("Norway", "United States", "United Kingdom", "Germany",
                        "Denmark", "Sweden", "Israel", "India")
oecd_filtered <- oecd %>%
  filter(Country %in% selected_countries)

# Convert the "Year" variable to numeric
oecd_filtered$Year <- as.numeric(oecd_filtered$Year)

graph2<- ggplot(oecd_filtered, aes(x = Year, y = Value, color = Country)) +
  geom_line(size = 1) +
  labs(title = "Collective Bargaining Coverage Over Time",
       x = "Year",
       y = "Collective Bargaining Rate (%)",
       color = "Country") +
  theme_minimal()

ggsave(paste0("visualisations/","Collective Bargaining Coverage Over Time",".svg"), graph2, width = 11, height = 8.5, units = "in")
