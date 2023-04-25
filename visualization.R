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
combined_proportions <- rbind(sampled_proportions, weighted_proportions)

library(viridis)

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

# Calculate breaks for the primary y-axis
primary_breaks <- scales::pretty_breaks()(year_data_filtered$population_count)

# Calculate breaks for the secondary y-axis
secondary_breaks <- primary_breaks * sum(year_data_filtered$population_count) / 1000


# Create the bar plot
bar_plot <- ggplot(year_data_filtered, aes(x = parentcode_indus, y = population_count, fill = industry_label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = named_color_vector) +
  scale_y_continuous(
    breaks = primary_breaks,
    sec.axis = sec_axis(
      trans = ~. * sum(year_data_filtered$population_count) / 1000,
      name = "Number of Employees (1 000)",
      labels = scales::number_format(accuracy = 1, scale = 1, big.mark = ",", label.padding = 0.5),
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



bar_plot <- ggplot(year_data_filtered, aes(x = parentcode_indus, y = population_count , fill = industry_label )) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = named_color_vector) +
  scale_y_continuous(
    breaks = primary_breaks,
    sec.axis = sec_axis(
      trans = ~. * sum(year_data_filtered$population_count) / 1000,
      name = "Number of Employees (1 000)",
      labels = scales::number_format(accuracy = 1, scale = 1, big.mark = ",", label.padding = 0.5),
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

#
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
  labs(fill = "Industry", title = "Proportion of Labor Force by Industry in 2016")

# Print the pie chart
print(pie_chart)


###Second section


full_merged_ds_year <- full_merged_ds_year %>%
  filter(!(parentcode_indus %in% c("U", "T", "00.0")))

# Create the crossbar plot
crossbar_plot <- ggplot(full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
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

# Create the crossbar plot with unionization rates
crossbar_plot_union <- ggplot(full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
  geom_crossbar(aes(ymin = lower_quartile_nok, ymax = upper_quartile_nok, width = 0.7), position = position_dodge(0.9)) +
  geom_linerange(aes(ymin = median_nok, ymax = median_nok, width = 0.9), position = position_dodge(0.9), size = 1) +
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
  labs(x = "Industry Code", title = "Distribution of Median, Lower & Upper Quartile Wages (NOK) & Unionization Rates by Industry in 2016") +
  guides(fill = guide_legend(title = "Industry", nrow = NULL, ncol = 1), color = guide_legend(title = NULL))

# Print the crossbar plot with unionization rates
print(crossbar_plot_union)


# First, calculate the median union_density by parentcode_indus
median_union_density <- aggregate(full_merged_ds_year$union_density, by = list(full_merged_ds_year$parentcode_indus), FUN = median)

# Rename the columns of median_union_density
colnames(median_union_density) <- c("parentcode_indus", "median_union_density")

# Merge the median_union_density with the full_merged_ds_year dataframe
full_merged_ds_year <- merge(full_merged_ds_year, median_union_density, by = "parentcode_indus")

# Reorder the parentcode_indus factor levels by ascending median_union_density
full_merged_ds_year$parentcode_indus <- factor(
  full_merged_ds_year$parentcode_indus,
  levels = median_union_density[order(median_union_density$median_union_density), "parentcode_indus"]
)

# Now recreate the crossbar plot with the sorted x-axis
crossbar_plot_union_sorted <- ggplot(full_merged_ds_year, aes(x = parentcode_indus, y = median_nok, fill = industry_label)) +
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

# Regression
ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = as.factor(year), label = parentcode_indus)) +
  geom_point(size = 3) +
  geom_text(aes(label = parentcode_indus), color = "black", size = 3, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = c("2013" = "pink", "2014" = "lightblue", "2016" = "green", "2017" = "purple")) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean Wage (NOK)", color = "Year", title = "Mean Wage vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Year"))

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

ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_median_gap, color = industry_label)) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_text(aes(label = year_label), color = "white", size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = legend_labels$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean-Median Wage Gap (NOK)", color = "Industry", title = "Mean-Median Wage Gap vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))



ggplot(full_merged_ds_filtered, aes(x = union_density, y = mean_nok, color = industry_label)) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_text(aes(label = year_label), color = "white", size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1, color = "black") +
  scale_color_manual(values = custom_palette, labels = legend_labels$industry_label) +
  theme_minimal() +
  labs(x = "Union Density", y = "Mean Wage (NOK)", color = "Industry", title = "Mean Wage vs. Union Density by Industry and Year") +
  guides(color = guide_legend(title = "Industry", nrow = NULL, ncol = 1))


