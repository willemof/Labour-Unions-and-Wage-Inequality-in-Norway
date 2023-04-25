
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

  write_csv(decilewage16_22, file = ("csv/decilewage16_22.csv"))
  
  decilewage16_22 <- read_csv(file = ("csv/decilewage16_22.csv"))
  decilewage16_22 <- x %>%
    select(date, everything())
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
  
  # Custom color palette
  custom_palette <- c(
    "#000000", "#FF0000", "#00FF00", "#0000FF",
    "#FFFF00", "#FF00FF", "#00FFFF", "#F99999",
    "#008000", "#000080", "#808000", "#800080",
    "#008080", "#8F6999", "#808080", "#FFA500",
    "#A52A2A", "#C0C0C0", "#2E8B57", "#dfffff",
    "#ddd333", "#603666"
  )
  
  
  binwidth <- 2000
  
  
  ggplot(decilewage16_22_expand, aes(x = mid_point, fill = industry_sic2007)) +
    geom_histogram(binwidth = 5000, position = "identity", alpha = 0.5) +
    scale_fill_manual(values = custom_palette) +
    labs(title = "Wage Distribution Across Industries",
         x = "Wage",
         y = "Frequency") +
    theme_minimal() +
    scale_fill_discrete(name = "Industry")
  
  # Create a stacked density plot with custom color palette
  stacked_density_plot <- ggplot(decilewage16_22_expand, aes(x = `Boundaries monthly earnings (NOK)`, fill = industry_sic2007)) +
    geom_density(alpha = 0.5, position = "stack") +
    scale_fill_manual(values = custom_palette) +
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
    scale_fill_manual(values = custom_palette) +
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
  
  