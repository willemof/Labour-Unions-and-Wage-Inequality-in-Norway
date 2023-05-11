## visualizing distribution of industries
#microdata <- x_m


# Sampled proportions by year
sampled_proportions <- microdata %>%
  count(year, industryparentname, parentcode_indus) %>%
  group_by(year) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  mutate(data_type = "Sampled") %>%
  rename(weighted_n = n) %>%
  filter(year == 2017) %>%
  filter(!(parentcode_indus %in% c("U", "T", "00.0")))

# Weighted proportions by year
weighted_proportions <- df %>%
  group_by(year, industryparentname, parentcode_indus) %>%
  filter(year == 2017) %>%
  group_by(year) %>%
  ungroup() %>%
  mutate(data_type = "Weighted") %>%
  filter(!(parentcode_indus %in% c("U", "T", "00.0")))

df <- df %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - "))

#weighted_proportions <- weighted_proportions %>%
#  select(colnames(sampled_proportions))

# Combine the two data sets
#combined_proportions <- rbind(sampled_proportions, weighted_proportions)



# Filter the data to include only the year 2017

# Filter the weighted_proportions data to include only the year 2016
weighted_proportions_2016 <- df %>%
  filter(year == 2016) #%>%
# filter(!(parentcode_indus %in% c("U", "T", "00.0")))


#this is not the end of the data processing part



# Industry names
industry_names <- c(
  "A - Agriculture, forestry and fishing",
  "B - Mining and quarrying",
  "C - Manufacturing",
  "D - Electricity, gas, steam and air conditioning supply",
  "E - Water supply; sewerage, waste management and remediation activities",
  "F - Construction",
  "G - Wholesale and retail trade; repair of motor vehicles and motorcycles",
  "H - Transportation and storage",
  "I - Accommodation and food service activities",
  "J - Information and communication",
  "K - Financial and insurance activities",
  "L - Real estate activities",
  "M - Professional, scientific and technical activities",
  "N - Administrative and support service activities",
  "O - Public administration and defence; compulsory social security",
  "P - Education",
  "Q - Human health and social work activities",
  "R - Arts, entertainment and recreation",
  "S - Other service activities"
)

# Custom color palette
custom_palette <- c(
  "#000000", "#FF0000", "#00FF00", "#0000FF",
  "#FFFF00", "#FF00FF", "#00FFFF", "#F99999",
  "#008000", "#000080", "#808000", "#800080",
  "#008080", "#8F6999", "#808080", "#FFA500",
  "#A52A2A", "#C0C0C0", "#2E8B57", "#dfffff",
  "#ddd333", "#603666"
)

# Create a named color vector
named_color_vector <- setNames(custom_palette, industry_names)
#This is what decides -> scale_fill_manual(values = named_color_vector)


year_data_filtered <- df %>%
  filter(year == 2016)

year_data_filtered <- year_data_filtered %>%
  mutate(proportion = population_count / sum(population_count))


# Calculate breaks for the primary y-axis
primary_breaks <- scales::pretty_breaks()(year_data_filtered$proportion)


# Calculate breaks for the secondary y-axis
total_employees <- sum(year_data_filtered$population_count)
secondary_breaks <- primary_breaks * total_employees

# Function to generate custom labels for secondary y-axis
custom_labels <- function(breaks) {
  labels <- scales::number(breaks / 1000, accuracy = 1, scale = 1, big.mark = ",", label.padding = 0.5)
  return(labels)
}

# Create the bar plot
bar_plot <- ggplot(year_data_filtered, aes(x = parentcode_indus, y = proportion, fill = industry_label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = named_color_vector) + #NAMED COLOUR VECTOR DECIDES THE COLOURRRRR
  scale_y_continuous(
    labels = scales::percent_format(),
    sec.axis = sec_axis(
      trans = ~. * total_employees,
      name = "Number of Employees (per 1,000)",
      labels = custom_labels,
      breaks = secondary_breaks
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", y = "Proportion of Labor Force", title = "Proportion of Labor Force by Industry in 2016") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1))

# Print the plot
print(bar_plot)


# Create the pie chart
pie_chart <- ggplot(year_data_filtered, aes(x = "", y = population_count, fill = industry_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = named_color_vector) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(fill = "Industry", title = "Proportioin of Labour Force by Industry in 2016")

# Print the pie chart
print(pie_chart)

# Create the pie chart without a legend
pie_chart <- ggplot(year_data_filtered, aes(x = "", y = population_count, fill = industry_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = named_color_vector) +
  theme_void() +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )  +
  guides(fill = FALSE)


# Modify the pie chart to make it smaller
pie_chart <- pie_chart +
  theme(plot.margin = margin(0, 0, 0, 0)) +
  coord_polar("y", start = 0, clip = "off") +
  theme(plot.background = element_blank())

# Place the pie chart inside the bar plot
combined_plot <- ggdraw(bar_plot) +
  draw_plot(pie_chart, x = 0.75, y = 0.75, width = 0.25, height = 0.25)

# Print the combined plot
print(combined_plot)


# Export the plot as a PDF
ggsave(paste0("visualisations/","Proportioin of Labour Force by Industry in 2016",".svg"), combined_plot, width = 11, height = 8.5, units = "in")


####Second section


full_merged_ds_year <- full_merged_ds_year %>%
  filter(!(parentcode_indus %in% c("U", "T", "00.0")))%>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - "))%>%
  filter(year == 2016)

# Create the crossbar plot
crossbar_plot <- ggplot(full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = full_merged_ds_year$industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok, width = 0.7), position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok, width = 0.9), position = position_dodge(0.9), size = 1) +
  geom_point(aes(y = median_nok), color = "white", size = 2, position = position_dodge(0.9)) +
  scale_fill_manual(values = named_color_vector) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", y = "Monthly Wage (NOK)", title = "Distribution of Median, Lower & Upper Quartile Wages (NOK) by Industry in 2016") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1))

# Print the crossbar plot
print(crossbar_plot)




# Create the crossbar plot with unionization rates and collective bargaining rates
crossbar_plot_union_collective <- ggplot(full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = full_merged_ds_year$industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok, width = 0.7), position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok, width = 0.9), position = position_dodge(0.9), size = 1) +
  geom_point(aes(y = median_nok), color = "white", size = 2, position = position_dodge(0.9)) +
  geom_line(aes(y = union_density * 100000, group = 1, color = "Unionization Rate"), size = 1) +
  geom_line(aes(y = collective_rate * 100000, group = 2, color = "Collective Bargaining Rate"), size = 1) +
  scale_fill_manual(values = named_color_vector) +
  scale_y_continuous(name = "Monthly Wage (NOK)", sec.axis = sec_axis(~./100000, name = "Unionization Rate (%) & Collective Bargaining Rate (%)")) +
  scale_color_manual(values = c("Unionization Rate" = "darkblue", "Collective Bargaining Rate" = "orange")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", title = "Distribution of Median, Lower & Upper Quartile Wages (NOK), Unionization Rates & Collective Bargaining Rates by Industry in 2016") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1), color = guide_legend(title = NULL))

# Print the crossbar plot with unionization rates and collective bargaining rates
print(crossbar_plot_union_collective)


# Sort the dataset by union_density in ascending order
sorted_full_merged_ds_year <- full_merged_ds_year[order(full_merged_ds_year$union_density),]
sorted_full_merged_ds_year
# Reorder the factor levels of the parentcode_indus column based on the sorted dataset
sorted_full_merged_ds_year$parentcode_indus <- factor(sorted_full_merged_ds_year$parentcode_indus, levels = unique(sorted_full_merged_ds_year$parentcode_indus))




# Now recreate the crossbar plot with the sorted x-axis
crossbar_plot_union_sorted <- ggplot(sorted_full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok), width = 0.7, position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok), width = 0.9, position = position_dodge(0.9), size = 1) +
  geom_point(aes(y = median_nok), color = "white", size = 2, position = position_dodge(0.9)) +
  geom_line(aes(y = union_density * 100000, group = 1, color = "Unionization Rate"), size = 1) +
  scale_fill_manual(values = named_color_vector) +
  scale_y_continuous(name = "Monthly Wage (NOK)", sec.axis = sec_axis(~./100000, name = "Unionization Rate (%)")) +
  scale_color_manual(values = c("Unionization Rate" = "darkblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", title = "Distribution of Median, Lower & Upper Quartile Wages (NOK) & Unionization Rates by Industry in 2016 (Sorted by Unionization Rate)") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1), color = guide_legend(title = NULL))

crossbar_plot_union_sorted

ggsave(paste0("visualisations/","Distribution of Median, Lower & Upper Quartile Wages (NOK) & Unionization Rates by Industry in 2016 (Sorted by Unionization Rate)",".svg"), crossbar_plot_union_sorted, width = 11, height = 8.5, units = "in")



crossbar_plot_union_sorted <- ggplot(sorted_full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok), width = 0.7, position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok), width = 0.9, position = position_dodge(0.9), size = 1) +
  geom_point(aes(y = median_nok), color = "white", size = 2, position = position_dodge(0.9)) +
  geom_line(aes(y = union_density * 100000, group = 1, color = "Unionization Rate"), size = 1) +
  geom_line(aes(y = collective_rate * 100000, group = 2, color = "Collective Bargaining Rate"), size = 1) +
  scale_fill_manual(values = named_color_vector)  +
  scale_y_continuous(name = "Monthly Wage (NOK)", labels = scales::label_number(big.mark = " "), sec.axis = sec_axis(~./100000, name = "Unionization Rate (%) & Collective Bargaining Rate (%)", labels = scales::percent)) + 
  scale_color_manual(values = c("Unionization Rate" = "darkblue", "Collective Bargaining Rate" = "orange")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(x = "Industry Code", title = "Distribution of Median, Lower, Upper Quartile Wages (NOK), Unionization &\nCollective Bargaining Rates (%) by Industry in 2016 (Sorted by Ascending Unionization Rate)") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1), color = guide_legend(title = NULL))

crossbar_plot_union_sorted


ggsave(paste0("visualisations/","Distribution of Median, Lower & Upper Quartile Wages (NOK) & Unionization and Collective Bargaining Rates)",".svg"), crossbar_plot_union_sorted, width = 11, height = 8.5, units = "in")


#
#Wage vs Labour Union Density, scatterplot


# Filter away data for the year 2015
full_merged_ds_filtered <- full_merged_ds %>%
  filter(!is.na(union_density))


# Create the scatterplot
scatter_plot <- ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = factor(year))) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(
    x = "Unionization Rate",
    y = "Mean Monthly Wage (NOK)",
    title = "Scatterplot of Mean Monthly Wage (NOK) vs. Labor Union Density by Year (excluding 2015)"
  )

# Print the scatterplot
print(scatter_plot)
ggsave(paste0("visualisations/","Scatterplot of Mean Monthly Wage (NOK) vs. Labor Union Density by Year (excluding 2015)",".svg"), scatter_plot, width = 11, height = 8.5, units = "in")


# Create the scatterplot with black industry code labels inside colored points
scatter_plot <- ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = factor(year))) +
  geom_point(size = 3.5, alpha = 0.7) +
  geom_text(aes(label = parentcode_indus), color = "black", size = 3, fontface = "bold") +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  ) +
  labs(
    x = "Unionization Rate",
    y = "Mean Monthly Wage (NOK)",
    title = "Scatterplot of Mean Monthly Wage (NOK) vs. Labor Union Density by Year and Industry (excluding 2015)"
  )

# Print the scatterplot
print(scatter_plot)
ggsave(paste0("visualisations/","Scatterplot of Mean Monthly Wage (NOK) vs. Labor Union Density by Year and Industry (excluding 2015)",".svg"), scatter_plot, width = 11, height = 8.5, units = "in")


# Regression
scatter_plot <-  ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = as.factor(year), label = parentcode_indus)) +
  geom_point(size = 4) +
  geom_text(aes(label = parentcode_indus), color = "black", size = 4, check_overlap = FALSE) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = c("2013" = "pink", "2014" = "yellow", "2016" = "turquoise", "2017" = "lightgreen")) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean Wage (NOK)", color = "Year", title = "Mean Wage vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Year")) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

scatter_plot


ggsave(paste0("visualisations/","Mean Wage vs. Union Density by Industry and Year)",".svg"), scatter_plot, width = 11, height = 8.5, units = "in")

# Create a new variable for the mean-median gap
full_merged_ds_filtered <- full_merged_ds_filtered %>%
  mutate(mean_median_gap = mean_nok - median_nok)

# Create a scatterplot with the mean-median gap on the y-axis
ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = as.factor(year), label = parentcode_indus)) +
  geom_point(size = 3) +
  geom_text(aes(label = parentcode_indus), color = "black", size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = c("2013" = "red", "2014" = "blue", "2016" = "green", "2017" = "purple")) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Year", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Year"))



# Create a scatterplot with the mean-median gap on the y-axis
full_merged_ds_filtered <- full_merged_ds_filtered %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - "))

ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = industry_label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = full_merged_ds_filtered$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Industry", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))

#no white text but good legend

# Create a scatterplot with the mean-median gap on the y-axis


legend_labels <- full_merged_ds_filtered %>%
  select(parentcode_indus, industry_label) %>%
  distinct() %>%
  arrange(parentcode_indus)

ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = industry_label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = legend_labels$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Industry", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))


#White text on point, mean median gap labour union density regression
full_merged_ds_filtered <- full_merged_ds_filtered %>%
  mutate(industry_label = paste(parentcode_indus, industryparentname, sep = " - "),
         year_label = as.character(year - 2010)) 
filtered_data <- full_merged_ds_filtered %>%
  filter(industry_label != "00.0 - Unspecified or unidentifiable industry")

legend_labels <- legend_labels %>%
  filter(industry_label != "00.0 - Unspecified or unidentifiable industry")

filtered_palette <- custom_palette[filtered_legend_labels$industry_label]


mean_median_vis <- ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = industry_label)) +
  geom_point(size = 3, show.legend = TRUE) +
  geom_text(aes(label = year_label), color = "white", size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = legend_labels$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Industry", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))

ggsave(paste0("visualisations/","Mean-Median Wage Gap vs. Union Density by Industry and Year",".svg"), mean_median_vis, width = 11, height = 8.5, units = "in")


x <- ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = industry_label)) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_text(aes(label = year_label), color = "white", size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = legend_labels$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean Wage (NOK)", color = "Industry", title = "Mean Wage vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))


# Export the plot as a PDF
ggsave(paste0("visualisations/","Mean Wage vs. Union Density by Industry and Year",".svg"), x, width = 11, height = 8.5, units = "in")


file = (paste0("visualisations/","Mean Wage vs. Union Density by Industry and Year"))


docu <- df %>%
  select(union_density, collective_rate ,industry_label, year)

write_csv(docu, (file = ("csv/ludcolbargperindustry.csv")))
