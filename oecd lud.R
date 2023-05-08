oecd <- read_csv(file = ("csv/TUD_07052023173204619.csv"))
# Filter the data for the selected countries
selected_countries <- c("Norway", "United States", "United Kingdom", "Germany",
                        "Denmark", "Sweden", "Israel", "OECD - Total")
oecd_filtered <- oecd %>%
  filter(Country %in% selected_countries)%>%
  mutate(Year = Time)



# Convert the "Year" variable to numeric
oecd_filtered$Year <- as.numeric(oecd_filtered$Year)

graph1 <- ggplot(oecd_filtered, aes(x = Year, y = Value, color = Country)) +
  geom_line(size = 1, show.legend = FALSE) +
  labs(title = "Labour Union Density Over Year",
       x = "Year",
       y = "Unionization Rate (%)",
       color = "Country") +
  scale_color_manual(values = c("Norway" = "red", "United States" = "gold", "United Kingdom" = "turquoise", "Denmark" = "lightgreen",
                                "Sweden" = "pink","Israel" = "blue", "OECD - Total" = "black","Germany" = "grey"))+
  theme_minimal()

ggsave(paste0("visualisations/","Unionization OVer Year",".svg"), graph, width = 11, height = 8.5, units = "in")



oecd2 <- read_csv(file = ("csv/CBC_07052023172649150.csv"))

# Filter the data for the selected countries

oecd_filtered2 <- oecd2 %>%
  filter(Country %in% selected_countries) 

# Convert the "Year" variable to numeric
oecd_filtered2$Year <- as.numeric(oecd_filtered2$Year)

graph2<- ggplot(oecd_filtered2, aes(x = Year, y = Value, color = Country)) +
  geom_line(size = 1) +
  labs(title = "Collective Bargaining Coverage Over Year",
       x = "Year",
       y = "Collective Bargaining Rate (%)",
       color = "Country") +  
  scale_color_manual(values = c("Norway" = "red", "United States" = "gold", "United Kingdom" = "turquoise", "Denmark" = "lightgreen",
                                "Sweden" = "pink","Israel" = "blue", "OECD - Total" = "black","Germany" = "grey")) +
  theme_minimal()

ggsave(paste0("visualisations/","Collective Bargaining Coverage Over Year",".svg"), graph2, width = 11, height = 8.5, units = "in")

combined_graph <- grid.arrange(graph1, graph2, ncol = 2)
combined_graph <- plot_grid(graph1, graph2, align = "vh", ncol = 2)

ggsave(paste0("visualisations/","Collective Bargaining and LUD Coverage Over Year",".svg"), combined_graph, width = 11, height = 8.5, units = "in")


graph1 <- ggplot(oecd_filtered, aes(x = Year, y = Value, color = Country)) +
  geom_line(size = 3) +
  labs(title = "Labour Union Density Over Year",
       x = "Year",
       y = "Unionization Rate (%)",
       color = "Country") +
  scale_color_manual(values = c("Norway" = "red", "United States" = "gold", "United Kingdom" = "turquoise", "Denmark" = "lightgreen",
                                "Sweden" = "pink","Israel" = "blue", "OECD - Total" = "black","Germany" = "grey")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box.background = element_rect(fill = "transparent", size = 0, linetype = "solid"),
        plot.margin = margin(5.5, 10, 5.5, 5.5, "pt"))

graph2 <- ggplot(oecd_filtered2, aes(x = Year, y = Value, color = Country)) +
  geom_line(size = 3) +
  labs(title = "Collective Bargaining Coverage Over Year",
       x = "Year",
       y = "Collective Bargaining Rate (%)",
       color = "Country") +
  scale_color_manual(values = c("Norway" = "red", "United States" = "gold", "United Kingdom" = "turquoise", "Denmark" = "lightgreen",
                                "Sweden" = "pink","Israel" = "blue", "OECD - Total" = "black","Germany" = "grey")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box.background = element_rect(fill = "transparent", size = 0, linetype = "solid"),
        plot.margin = margin(5.5, 40, 5.5, 5.5, "pt"))


custom_legend <- cowplot::get_legend(graph2)

graph1 <- graph1 + theme(legend.position = "none")
graph2 <- graph2 + theme(legend.position = "right")

graph1 <- graph1 + theme(
  plot.title = element_text(size = 18),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 14)
)

graph2 <- graph2 + theme(
  plot.title = element_text(size = 18),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 14)
)


combined_graph <- plot_grid(graph1, graph2, ncol = 2,rel_widths = c(0.85, 1))
combined_graph


ggsave(paste0("visualisations/", "Final_Graph.svg"), combined_graph, width = 22, height = 8.5, units = "in")


