
url <- str_trim( str_remove_all("


https://data.ssb.no/api/v0/en/table/13860/




                      ", "\n"))
  
  
  data.tmp <- '
{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "1",
          "2"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "A",
          "B",
          "C",
          "D",
          "E",
          "F",
          "G",
          "H",
          "I",
          "J",
          "K",
          "L",
          "M",
          "N",
          "O",
          "P",
          "Q",
          "R",
          "S",
          "T",
          "U",
          "A-Z",
          "Z"
        ]
      }
    },
    {
      "code": "Desiler",
      "selection": {
        "filter": "vs:Desiler",
        "values": [
          "01",
          "02",
          "03",
          "04",
          "05",
          "06",
          "07",
          "08",
          "09",
          "10"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
  d.tmp <- POST(url , body = data.tmp, encode = "json", verbose())
  
  decilewage16_22 <- fromJSONstat(content(d.tmp, "text"))
  decilewage16_22 <- clean_names(decilewage16_22)
  decilewage16_22 <- tibble(decilewage16_22) 

  x<- decilewage16_22
  #This series of if statements converts years, quarters and months into dates using zoo
  if("month" %in% colnames(x)) {
    x<- separate(x, month, into = c("year", "month"), sep = "M")
    x$date  <- paste0(x$year,"-", x$month) 
    x$date <- as.Date(as.yearmon(x$date), frac = 1)
  }else{ if("quarter" %in% colnames(x)) {
    x<- separate(x, quarter, into = c("year", "quarter"), sep = "K")
    x$date  <- paste0(x$year," Q", x$quarter) 
    x$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",x$date)),frac = 1)
  }else{ x$date  <- paste0(x$year," Q4") 
  x$date= as.Date(as.yearqtr(gsub("(\\d)(Q)(\\d{1,})","\\3 Q\\1",x$date)),frac = 1)
  }
  }
  
  decilewage16_22 <- x %>%
    select(date, everything())

  write_csv(decilewage16_22, file = ("csv/decilewage16_22.csv"))
  
  decilewage16_22 <- read_csv(file = ("csv/decilewage16_22.csv"))

  decilewage16_22_expand <- pivot_wider(decilewage16_22, 
                                             id_expand = FALSE,
                                             names_from = contents,
                                             values_from = c("value"))
  decilewage16_22 <- separate(decilewage16_22, decil_group, c("trash", "decile"), sep = " ")
  decilewage16_22 <- decilewage16_22 %>%
    select(-trash)
  decilewage16_22_expand <- pivot_wider(decilewage16_22, 
                                        id_expand = FALSE,
                                        names_from = contents,
                                        values_from = c("value"))
  
  
  decilewage16_22_expand<- decilewage16_22_expand %>%
    filter(!(decilewage16_22_expand$industry_sic2007 %in% c("All industries", "Unknown industry")))
  
  
  decilewage16_22_expand<- decilewage16_22_expand %>%
    filter((decilewage16_22_expand$year %in% c("2016")))
  
  # Filter out rows with NA in any column
  decilewage16_22_expand <- decilewage16_22_expand[complete.cases(decilewage16_22_expand), ]
 
  
  
  
  decilewage16_22_expand <- decilewage16_22_expand %>%
    arrange(year, industry_sic2007, decile) %>%
    group_by(year, industry_sic2007) %>%
    mutate(Lower_Boundary = lag(`Boundaries monthly earnings (NOK)`)) %>%
    ungroup() %>%
    mutate(Lower_Boundary = ifelse(is.na(Lower_Boundary), 0, Lower_Boundary),
           mid_point = (Lower_Boundary + `Boundaries monthly earnings (NOK)`) / 2)
  
  head(decilewage16_22_expand)
  
  
  # there add parentcodes where missing
  
  
  old_to_new_names <- c(
    "Accommodation and food service activities" = "Accommodation and food service activities",
    "Administrative and support service activities" = "Administrative and support service activities",
    "Agriculture, forestry and fishing" = "Agriculture, forestry and fishing",
    "Arts, entertainment and recreation" = "Arts, entertainment and recreation",
    "Construction" = "Construction",
    "Education" = "Education",
    "Electricity, gas and steam" = "Electricity, gas, steam and air conditioning supply",
    "Financial and insurance activities" = "Financial and insurance activities",
    "Human health and social work activities" = "Human health and social work activities",
    "Information and communication" = "Information and communication",
    "Manufacturing" = "Manufacturing",
    "Mining and quarrying" = "Mining and quarrying",
    "Other service activities" = "Other service activities",
    "Professional, scientific and technical activities" = "Professional, scientific and technical activities",
    "Public administration and defence" = "Public administration and defence; compulsory social security",
    "Real estate activities" = "Real estate activities",
    "Transportation and storage" = "Transportation and storage",
    "Water supply, sewerage, waste" = "Water supply; sewerage, waste management and remediation activities",
    "Wholesale and retail trade: repair of motor vehicles and motorcycles" = "Wholesale and retail trade; repair of motor vehicles and motorcycles"
  )
  
  decilewage16_22_expand <- decilewage16_22_expand %>%
    mutate(industry_sic2007 = old_to_new_names[industry_sic2007])
  current_to_new_names <- c(
    "Agriculture, forestry and fishing" = "A - Agriculture, forestry and fishing",
    "Mining and quarrying" = "B - Mining and quarrying",
    "Manufacturing" = "C - Manufacturing",
    "Electricity, gas, steam and air conditioning supply" = "D - Electricity, gas, steam and air conditioning supply",
    "Water supply; sewerage, waste management and remediation activities" = "E - Water supply; sewerage, waste management and remediation activities",
    "Construction" = "F - Construction",
    "Wholesale and retail trade; repair of motor vehicles and motorcycles" = "G - Wholesale and retail trade; repair of motor vehicles and motorcycles",
    "Transportation and storage" = "H - Transportation and storage",
    "Accommodation and food service activities" = "I - Accommodation and food service activities",
    "Information and communication" = "J - Information and communication",
    "Financial and insurance activities" = "K - Financial and insurance activities",
    "Real estate activities" = "L - Real estate activities",
    "Professional, scientific and technical activities" = "M - Professional, scientific and technical activities",
    "Administrative and support service activities" = "N - Administrative and support service activities",
    "Public administration and defence; compulsory social security" = "O - Public administration and defence; compulsory social security",
    "Education" = "P - Education",
    "Human health and social work activities" = "Q - Human health and social work activities",
    "Arts, entertainment and recreation" = "R - Arts, entertainment and recreation",
    "Other service activities" = "S - Other service activities"
  )
  
  decilewage16_22_expand <- decilewage16_22_expand %>%
    mutate(industry_sic2007 = current_to_new_names[industry_sic2007])
  
  
 industry_names_and_code <- c(
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
named_color_vector <- setNames(custom_palette, industry_names_and_code)
#This is what decides -> scale_fill_manual(values = named_color_vector)


  
  binwidth <- 2000
  
  
  ggplot(decilewage16_22_expand, aes(x = mid_point, fill = industry_sic2007)) +
    geom_histogram(binwidth = 5000, position = "identity", alpha = 0.5) +
    scale_fill_manual(values = named_color_vector) +
    labs(title = "Wage Distribution Across Industries",
         x = "Wage",
         y = "Frequency") +
    theme_minimal() +
    scale_fill_discrete(name = "Industry")
  
  # Create a stacked density plot with custom color palette
  stacked_density_plot <- ggplot(decilewage16_22_expand, aes(x = `Boundaries monthly earnings (NOK)`, fill = industry_sic2007)) +
    geom_density(alpha = 0.5, position = "stack") +
    scale_fill_manual(values = named_color_vector) +
    theme_minimal() +
    labs(x = "Wage Boundary", y = "Density", title = "Stacked Wage Distribution by Industry") +
    guides(fill = guide_legend(title = "Industry"))
  
  # Display the stacked density plot
  stacked_density_plot
  
  
  # Calculate deciles
  deciles <- quantile(decilewage16_22_expand$`Boundaries monthly earnings (NOK)`, probs = seq(0.1, 0.9, by = 0.1))
  

  # Create a data frame for decile labels
  decile_labels_data <- data.frame(x = deciles, y = 0, label = paste0("Decile ", 1:9))
  
 
  
  # Create the stacked histogram with reordered industries, custom color palette, and population percentage
  stacked_histogram_percentage <- ggplot(decilewage16_22_expand, aes(x = `Boundaries monthly earnings (NOK)`, y = ..count.. / sum(..count..) * 100, fill = industry_sic2007)) +
    geom_histogram(binwidth = 1000, position = "stack") +
    scale_fill_manual(values = named_color_vector) +
    theme_minimal() +
    labs(x = "Wage Boundary", y = "Population Percentage", title = "Stacked Wage Distribution by Industry (Ordered) as Population Percentage with Deciles") +
    guides(fill = guide_legend(title = "Industry"))
  
  # Add vertical decile lines
  stacked_histogram_percentage_with_decile_lines <- stacked_histogram_percentage +
    geom_vline(xintercept = deciles, color = "black", linetype = "dashed", size = 0.5)
  
  # Add decile labels
  stacked_histogram_percentage_with_decile_lines_and_labels <- stacked_histogram_percentage_with_decile_lines +
    geom_text(data = decile_labels_data, aes(x = x, y = y, label = label), inherit.aes = FALSE, angle = 90, hjust = -0.5, size = 3)
  
  # Display the stacked histogram with population percentage, vertical decile lines, and decile labels
  stacked_histogram_percentage_with_decile_lines_and_labels
  
  ##quartiles and median
  
  # Calculate lower quartile, upper quartile and median
  lower_quartile <- quantile(decilewage16_22_expand$`Boundaries monthly earnings (NOK)`, probs = 0.25)
  upper_quartile <- quantile(decilewage16_22_expand$`Boundaries monthly earnings (NOK)`, probs = 0.75)
  median_value <- quantile(decilewage16_22_expand$`Boundaries monthly earnings (NOK)`, probs = 0.5)
  
  # Create a data frame for quartile and median labels
  quartile_median_labels_data <- data.frame(x = c(lower_quartile, median_value, upper_quartile),
                                            y = 0, label = c("Lower Quartile", "Median", "Upper Quartile"))
  # Add vertical lines for lower quartile, median, and upper quartile
  stacked_histogram_percentage_with_quartile_median_lines <- stacked_histogram_percentage +
    geom_vline(xintercept = lower_quartile, color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median_value, color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = upper_quartile, color = "black", linetype = "dashed", size = 1)
  
  # Add labels for lower quartile, median, and upper quartile
  stacked_histogram_percentage_with_quartile_median_lines_and_labels <- stacked_histogram_percentage_with_quartile_median_lines +
    geom_text(data = quartile_median_labels_data, aes(x = x, y = y, label = label), inherit.aes = FALSE, angle = 90, hjust = -0.5, size = 3)
  
  # Display the stacked histogram with population percentage, vertical quartile and median lines, and labels
  stacked_histogram_percentage_with_quartile_median_lines_and_labels

  
  # Add vertical lines for lower quartile, median, and upper quartile to the stacked density plot
  stacked_density_plot_with_quartile_median_lines <- stacked_density_plot +
    geom_vline(xintercept = lower_quartile, color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median_value, color = "black", linetype = "dashed", size = 1.5) +
    geom_vline(xintercept = upper_quartile, color = "black", linetype = "dashed", size = 1)
  
  # Add labels for lower quartile, median, and upper quartile to the stacked density plot
  stacked_density_plot_with_quartile_median_lines_and_labels <- stacked_density_plot_with_quartile_median_lines +
    geom_text(data = quartile_median_labels_data, aes(x = x, y = 0, label = label), inherit.aes = FALSE, angle = 90, hjust = -0.5, size = 3)
  
  # Display the stacked density plot with vertical quartile and median lines, and labels
  stacked_density_plot_with_quartile_median_lines_and_labels
  
  
  
  # Update the text label position relative to the dotted lines
  stacked_density_plot_with_quartile_median_lines_and_labels <- stacked_density_plot_with_quartile_median_lines +
    geom_text(data = quartile_median_labels_data, aes(x = x, y = 0, label = label), inherit.aes = FALSE, angle = 90, hjust = -1, size = 3)
  
  # Display the updated stacked density plot with adjusted text labels
  stacked_density_plot_with_quartile_median_lines_and_labels
  
  # Create a data frame for decile and quartile labels
  decile_quartile_labels_data <- data.frame(x = c(deciles, lower_quartile, upper_quartile), y = 0, label = c(paste0("Decile ", 1:9), "Lower Quartile", "Upper Quartile"))
  
  # Create the stacked density plot with deciles and quartiles
  stacked_density_plot_with_deciles_quartiles <- stacked_density_plot +
    geom_vline(xintercept = c(deciles, lower_quartile, upper_quartile), color = "black", linetype = "dashed", size = 0.5) +
    geom_text(data = decile_quartile_labels_data, aes(x = x, y = 0, label = label), inherit.aes = FALSE, angle = 90, hjust = -1, size = 3)
  
  # Display the stacked density plot with deciles and quartiles
  stacked_density_plot_with_deciles_quartiles
  
  # Update the decile labels data frame to include "Decile 5 (Median)"
  decile_quartile_labels_data <- data.frame(x = c(deciles, lower_quartile, upper_quartile), y = 0, 
                                            label = c(paste0("Decile ", 1:4), "Decile 5 (Median)", paste0("Decile ", 6:9), "Lower Quartile", "Upper Quartile"))
  
  # Create the stacked density plot with updated decile and quartile labels
  stacked_density_plot_with_deciles_quartiles_updated_labels <- stacked_density_plot +
    geom_vline(xintercept = c(deciles, lower_quartile, upper_quartile), color = "black", linetype = "dashed", size = 0.5) +
    geom_text(data = decile_quartile_labels_data, aes(x = x, y = 0, label = label), inherit.aes = FALSE, angle = 90, hjust = -1, size = 3)
  
  # Display the stacked density plot with updated decile and quartile labels
  stacked_density_plot_with_deciles_quartiles_updated_labels
  
  
  library(ineq) # for the Gini function
  
  # Calculate the Gini coefficient for each industry
  gini_by_industry <- decilewage16_22_expand %>%
    group_by(industry_sic2007) %>%
    summarise(gini_coefficient = Gini(`Boundaries monthly earnings (NOK)`)) %>%
    arrange(desc(gini_coefficient))
  
  # Reorder industries based on the Gini coefficients
  decilewage16_22_expand <- decilewage16_22_expand %>%
    mutate(industry_sic2007 = factor(industry_sic2007, levels = gini_by_industry$industry_sic2007))
  
  # Create the stacked density plot with reordered industries based on inequality
  stacked_density_plot_with_reordered_industries <- ggplot(decilewage16_22_expand, aes(x = `Boundaries monthly earnings (NOK)`, fill = industry_sic2007)) +
    geom_density(alpha = 0.5, position = "stack") +
    scale_fill_manual(values = named_color_vector) +
    theme_minimal() +
    geom_vline(xintercept = lower_quartile, color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median_value, color = "black", linetype = "dashed", size = 1.5) +
    geom_vline(xintercept = upper_quartile, color = "black", linetype = "dashed", size = 1) +
    labs(x = "Monthly Wage", y = "Density of Employees", title = "Stacked Norwegian Wage Distribution by Industry in 2016 (Ordered by Inequality)") +
    guides(fill = guide_legend(title = "Industry"))
  
  # Add labels for lower quartile, median, and upper quartile to the stacked density plot
  stacked_density_plot_with_reordered_industries <- stacked_density_plot_with_reordered_industries +
    geom_text(data = quartile_median_labels_data, aes(x = x, y = 0, label = label), inherit.aes = FALSE, angle = 90, nudge_x = -2000, hjust = 0, size = 4)
  

  
  
  # Display the stacked density plot with reordered industries
  stacked_density_plot_with_reordered_industries
  
  
  # Calculate the national mean wage
  national_mean_wage <- mean(decilewage16_22_expand$`Boundaries monthly earnings (NOK)`)
  
  # Add the national mean wage as a dotted line to the stacked density plot
  stacked_density_plot_with_deciles_quartiles_mean <- stacked_density_plot_with_reordered_industries +
    geom_vline(xintercept = national_mean_wage, color = "black", linetype = "dotted", size = 1) +
    annotate("text", x = national_mean_wage, y = 0, label = "Mean Wage", angle = 90, hjust = -1, color = "black", size = 3)
  
  # Display the stacked density plot with deciles, quartiles, and mean wage
  stacked_density_plot_with_deciles_quartiles_mean
  
  
  # Create the stacked histogram with reordered industries, custom color palette, and population percentage
  stacked_histogram_percentage <- ggplot(decilewage16_22_expand, aes(x = `Boundaries monthly earnings (NOK)`, y = ..count.. / sum(..count..) * 100, fill = industry_sic2007)) +
    geom_histogram(binwidth = 1000, position = "stack") +
    scale_fill_manual(values = named_color_vector) +
    theme_minimal() +
    labs(x = "Wage Boundary", y = "Population Percentage", title = "Stacked Wage Distribution by Industry (Ordered) as Population Percentage with Quartiles, and Mean Wage") +
    guides(fill = guide_legend(title = "Industry")) +
    theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dashed")) # Add major y grid lines
  
  # Add vertical quartile lines
  stacked_histogram_percentage_with_quartile_lines <- stacked_histogram_percentage +
    geom_vline(xintercept = lower_quartile, color = "black", linetype = "dashed", size = 1) +
    geom_vline(xintercept = median_value, color = "black", linetype = "dashed", size = 1.5) +
    geom_vline(xintercept = upper_quartile, color = "black", linetype = "dashed", size = 1)
  
  # Add quartile labels
  stacked_histogram_percentage_with_quartile_lines_and_labels <- stacked_histogram_percentage_with_quartile_lines +
    geom_text(data = quartile_median_labels_data, aes(x = x, y = 0, label = label), inherit.aes = FALSE, angle = 90, nudge_x = -2000, hjust = 0, size = 4)
  
  # Add the national mean wage as a dotted line
  stacked_histogram_percentage_with_quartiles_mean <- stacked_histogram_percentage_with_quartile_lines_and_labels +
    geom_vline(xintercept = national_mean_wage, color = "black", linetype = "dotted", size = 1) +
    annotate("text", x = national_mean_wage, y = 0, label = "Mean Wage", angle = 90, hjust = -7, color = "black", size = 3)
  
  # Display the stacked histogram with population percentage, quartiles, and mean wage
  stacked_histogram_percentage_with_quartiles_mean
  
  