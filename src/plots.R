################################################################################
# 0. Libraries
################################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(forcats)
library(stringr)
library(ggsci)      
library(purrr)      
library(broom) 

################################################################################
# 1. Input
################################################################################

data <- read_excel("~/Desktop/medicalErrors3/manuscript/Supplementary Material 1.xlsx")
regression_results <- read_excel("~/Desktop/medicalErrors3/manuscript/Supplementary Material 2.xlsx")
hierarchical_results <- read_excel("~/Desktop/medicalErrors3/manuscript/Supplementary Material 3.xlsx")




################################################################################
### 2. Regressions plot ####
################################################################################


#===============================================================================
# 2.1 INDICATOR MAPPING AND CATEGORY GROUPING
#===============================================================================

# Mapping: code to clean indicator names
indicator_rename <- c(
  "Current.health.expenditure....of.GDP." = "Current health expenditure (% of GDP)",
  "Death.rate..crude..per.1.000.people." = "Death rate, crude (per 1,000 people)",
  "Hospital.beds..per.1.000.people." = "Hospital beds (per 1,000 people)",
  "Mortality.rate..neonatal..per.1.000.live.births." = "Mortality rate, neonatal (per 1,000 live births)",
  "Mortality.rate..under.5..per.1.000.live.births." = "Mortality rate, under-5 (per 1,000 live births)",
  "Mortality.rate..infant..per.1.000.live.births." = "Mortality rate, infant (per 1,000 live births)",
  "Mortality.rate..adult..female..per.1.000.female.adults." = "Mortality rate, adult, female (per 1,000 female adults)",
  "Mortality.rate..adult..male..per.1.000.male.adults." = "Mortality rate, adult, male (per 1,000 male adults)",
  "Out.of.pocket.expenditure.per.capita..current.US.." = "Out-of-pocket expenditure per capita (current US$)",
  "Physicians..per.1.000.people." = "Physicians (per 1,000 people)",
  "Risk.of.catastrophic.expenditure.for.surgical.care....of.people.at.risk." = "Risk of catastrophic expenditure for surgical care (% of people at risk)",
  "Risk.of.impoverishing.expenditure.for.surgical.care....of.people.at.risk." = "Risk of impoverishing expenditure for surgical care (% of people at risk)",
  "Charges.for.the.use.of.intellectual.property..payments..BoP..current.US.." = "Charges for the use of intellectual property, payments (BoP, current US$)",
  "Charges.for.the.use.of.intellectual.property..receipts..BoP..current.US.." = "Charges for the use of intellectual property, receipts (BoP, current US$)",
  "Research.and.development.expenditure....of.GDP." = "Research and development expenditure (% of GDP)",
  "Researchers.in.R.D..per.million.people." = "Researchers in R&D (per million people)"
)

# Group indicators into categories as appropriate:
category_map <- tibble::tibble(
  indicator_name = unname(indicator_rename),
  category = c(
    # Health System
    "Health System",   # Current health expenditure (% of GDP)
    "Mortality",       # Death rate, crude (per 1,000 people)
    "Health System",   # Hospital beds (per 1,000 people)
    "Mortality",       # Mortality rate, neonatal (per 1,000 live births)
    "Mortality",       # Mortality rate, under-5 (per 1,000 live births)
    "Mortality",       # Mortality rate, infant (per 1,000 live births)
    "Mortality",       # Mortality rate, adult, female (per 1,000 female adults)
    "Mortality",       # Mortality rate, adult, male (per 1,000 male adults)
    "Health System",   # Out-of-pocket expenditure per capita (current US$)
    "Health System",   # Physicians (per 1,000 people)
    "Financial Risk",  # Risk of catastrophic expenditure for surgical care
    "Financial Risk",  # Risk of impoverishing expenditure for surgical care
    "R&D/Innovation",  # Charges for the use of intellectual property, payments
    "R&D/Innovation",  # Charges for the use of intellectual property, receipts
    "R&D/Innovation",  # Research and development expenditure (% of GDP)
    "R&D/Innovation"   # Researchers in R&D (per million people)
  )
)

#===============================================================================
# 2.2 DATA PREPARATION
#===============================================================================

# Assume regression_results is already loaded as in your example

df <- regression_results

# Determine which variable to plot (the non-'publications' one)
df <- df %>%
  mutate(
    plot_var = if_else(independent_var == "publications", dependent_var, independent_var),
    variable_role = if_else(independent_var == "publications", "dependent", "independent")
  ) %>%
  filter(plot_var %in% names(indicator_rename))

# Map to clean indicator names
df <- df %>%
  mutate(indicator_name = indicator_rename[plot_var])

# Add category
df <- df %>%
  left_join(category_map, by = "indicator_name")

# Add significance stars based on adjusted_p_value
df <- df %>%
  mutate(significance = case_when(
    adjusted_p_value < 0.001 ~ "***",
    adjusted_p_value < 0.01  ~ "**",
    adjusted_p_value < 0.05  ~ "*",
    TRUE ~ ""
  ))

# Add a marker for dependent variables (superscript d)
df <- df %>%
  mutate(
    indicator_label = if_else(variable_role == "dependent",
                              paste0(indicator_name, " ᵈ"),
                              indicator_name)
  )

# Order income groups
income_order = c("HIC", "UMC", "LMC", "LIC")
df$income_group <- factor(df$income_group, levels = income_order)

# Order indicators within categories as they appear in your category_map
indicator_ordered_levels <- df %>%
  distinct(category, indicator_label) %>%
  arrange(match(category, unique(category_map$category)), indicator_label) %>%
  pull(indicator_label)
df$indicator_ordered <- factor(df$indicator_label, levels = indicator_ordered_levels)

# Rank coefficients by actual value (not absolute value) within each indicator
df <- df %>%
  group_by(indicator_label) %>%
  mutate(
    # Rank by actual coefficient value (considering sign)
    coef_rank = rank(coefficient, ties.method = "min"),
    # Assign category labels based on number of unique ranks
    n_ranks = n_distinct(coef_rank),
    rank_category = factor(
      coef_rank,
      levels = sort(unique(coef_rank)),
      labels = c("Lowest", "Low", "High", "Highest")[1:n_distinct(coef_rank)]
    ),
    direction = if_else(coefficient >= 0, "Positive", "Negative")
  ) %>%
  ungroup()

#===============================================================================
# 2.3 PLOTTING
#===============================================================================

plot <- ggplot() +
  # Positive coefficients as filled tiles
  geom_tile(
    data = subset(df, direction == "Positive"),
    aes(x = income_group, y = indicator_ordered, fill = rank_category),
    color = "white", size = 0.5
  ) +
  # Negative coefficients as outlined circles
  geom_point(
    data = subset(df, direction == "Negative"),
    aes(x = income_group, y = indicator_ordered, fill = rank_category),
    color = "white", size = 6, shape = 21
  ) +
  # Significance stars
  geom_text(
    data = df,
    aes(x = income_group, y = indicator_ordered, label = significance),
    size = 2.5
  ) +
  scale_fill_brewer(palette = "RdBu", direction = -1, na.value = "grey90") +
  scale_shape_manual(values = c("Positive" = 22, "Negative" = 21)) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "Country Income Classification",
    y = NULL,
    fill = "Coefficient\nMagnitude",
    shape = "Direction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(size = 9, face = "bold", angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold", hjust = 0),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.box = "vertical"
  )

print(plot)





################################################################################
### 3. Indicator plots ####
################################################################################

# Create a mapping from column names to clean titles
indicator_titles <- c(
  "Health.researchers..in.full.time.equivalent...as.a.proportion.of.all.researchers" = 
    "Health researchers (in full-time equivalent), as a proportion of all researchers",
  "Number.of.grants.for.biomedical.research.by.funder..type.of.grant..duration.and.recipients..World.RePORT." = 
    "Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)",
  "Official.development.assistance..ODA..for.medical.research.and.basic.health.sectors.per.capita..by.recipient.country" = 
    "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country",
  "Cause.of.death..by.non.communicable.diseases....of.total." = 
    "Cause of death, by non-communicable diseases (% of total)",
  "Cause.of.death..by.communicable.diseases.and.maternal..prenatal.and.nutrition.conditions....of.total." = 
    "Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)",
  "Current.health.expenditure....of.GDP." = 
    "Current health expenditure (% of GDP)",
  "Death.rate..crude..per.1.000.people." = 
    "Death rate, crude (per 1,000 people)",
  "Hospital.beds..per.1.000.people." = 
    "Hospital beds (per 1,000 people)",
  "Mortality.rate..neonatal..per.1.000.live.births." = 
    "Mortality rate, neonatal (per 1,000 live births)",
  "Mortality.rate..under.5..per.1.000.live.births." = 
    "Mortality rate, under-5 (per 1,000 live births)",
  "Mortality.rate..infant..per.1.000.live.births." = 
    "Mortality rate, infant (per 1,000 live births)",
  "Mortality.rate..adult..female..per.1.000.female.adults." = 
    "Mortality rate, adult, female (per 1,000 female adults)",
  "Mortality.rate..adult..male..per.1.000.male.adults." = 
    "Mortality rate, adult, male (per 1,000 male adults)",
  "Out.of.pocket.expenditure.per.capita..current.US.." = 
    "Out-of-pocket expenditure per capita (current US$)",
  "Physicians..per.1.000.people." = 
    "Physicians (per 1,000 people)",
  "Risk.of.catastrophic.expenditure.for.surgical.care....of.people.at.risk." = 
    "Risk of catastrophic expenditure for surgical care (% of people at risk)",
  "Risk.of.impoverishing.expenditure.for.surgical.care....of.people.at.risk." = 
    "Risk of impoverishing expenditure for surgical care (% of people at risk)",
  "Specialist.surgical.workforce..per.100.000.population." = 
    "Specialist surgical workforce (per 100,000 population)",
  "Charges.for.the.use.of.intellectual.property..payments..BoP..current.US.." = 
    "Charges for the use of intellectual property, payments (BoP, current US$)",
  "Charges.for.the.use.of.intellectual.property..receipts..BoP..current.US.." = 
    "Charges for the use of intellectual property, receipts (BoP, current US$)",
  "Research.and.development.expenditure....of.GDP." = 
    "Research and development expenditure (% of GDP)",
  "Researchers.in.R.D..per.million.people." = 
    "Researchers in R&D (per million people)"
)

# Function to create a combined line plot with embedded trend information
generate_combined_plot <- function(data, indicator_col) {
  # Get the clean title for the indicator
  indicator_title <- indicator_titles[indicator_col]
  
  # Extract y-label from title
  unit_match <- regmatches(indicator_title, regexpr("\\([^)]+\\)$", indicator_title))
  if (length(unit_match) > 0) {
    y_label <- gsub("[()]", "", unit_match)
  } else {
    y_label <- indicator_title
  }
  
  # Create the summary data (mean per year and income group)
  data_summary <- data %>%
    filter(!is.na(!!sym(indicator_col))) %>%
    group_by(income_level_iso3c, date) %>%
    summarize(value = mean(!!sym(indicator_col), na.rm = TRUE), .groups = "drop")
  
  # Calculate trends for each income group
  trend_data <- data_summary %>%
    group_by(income_level_iso3c) %>%
    arrange(date) %>%
    filter(n() >= 2) %>% # Need at least two points for trend
    summarize(
      first_year = first(date),
      last_year = last(date),
      first_value = first(value),
      last_value = last(value),
      pct_change = (last_value - first_value) / first_value * 100,
      years_span = last_year - first_year,
      .groups = "drop"
    ) %>%
    # Add color and icon based on direction
    mutate(
      direction_icon = ifelse(pct_change >= 0, "▲", "▼"),
      direction_color = ifelse(pct_change >= 0, "#00A087FF", "#E64B35FF"),
      label_text = sprintf("%s %s", direction_icon, percent(abs(pct_change/100), accuracy = 0.1))
    )
  
  # Get the last data point for each income group for annotation positioning
  annotation_points <- data_summary %>%
    group_by(income_level_iso3c) %>%
    filter(date == max(date)) %>%
    left_join(trend_data, by = "income_level_iso3c")
  
  # Generate the plot with embedded trend information
  p <- ggplot() +
    # Add a subtle area under the curve - WITH show.legend=FALSE to avoid legend artifacts
    geom_area(
      data = data_summary,
      aes(x = date, y = value, fill = income_level_iso3c),
      alpha = 0.2,
      show.legend = FALSE  # This prevents the "a" in the legend
    ) +
    # The line plot
    geom_line(
      data = data_summary, 
      aes(x = date, y = value, color = income_level_iso3c),
      size = 0.6, lineend = "round"
    ) +
    # Add a small point at the end of each line
    geom_point(
      data = annotation_points,
      aes(x = date, y = value, color = income_level_iso3c),
      size = 1.5
    ) +
    # Add trend annotations next to the end of each line
    geom_richtext(
      data = annotation_points,
      aes(
        x = date + 0.5, 
        y = value,
        label = label_text,
        color = income_level_iso3c
      ),
      hjust = 0, 
      vjust = 0.5,
      size = 3,
      fill = NA,
      label.color = NA,
      show.legend = FALSE  # No legend for annotations
    ) +
    labs(
      title = indicator_title,
      x = NULL,
      y = NULL,
      color = NULL,
      fill = NULL
    ) +
    # Extend x-axis slightly to accommodate annotations
    scale_x_continuous(
      breaks = sort(unique(c(
        seq(min(data_summary$date, na.rm = TRUE), 
            max(data_summary$date, na.rm = TRUE), 
            by = 5),
        max(data_summary$date, na.rm = TRUE)
      ))),
      limits = c(NA, max(data_summary$date, na.rm = TRUE) + 3),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      minor_breaks = NULL
    ) +
    scale_color_npg() +
    scale_fill_npg() +
    # Override guides to ensure clean legend
    guides(
      color = guide_legend(override.aes = list(fill = NA))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8, face = "bold"),
      axis.line.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dashed", color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      axis.ticks.x = element_line(size = 0.5),
      axis.ticks.length.x = unit(2, "pt"),
      axis.ticks.y = element_blank(),
      plot.margin = margin(10, 25, 10, 10)  # Add right margin for annotations
    )
  
  return(p)
}

# Get list of indicator columns (excluding metadata and bibliometric columns)
indicator_cols <- names(data)[6:27]  # Columns 6-27 based on your colnames output

# Create a list to store the combined plots
combined_plots <- list()

# Generate all combined plots
for (indicator in indicator_cols) {
  # Generate combined plot
  combined_plots[[indicator]] <- generate_combined_plot(data, indicator)
  
  # Print completion message
  cat("Generated combined plot for:", indicator_titles[indicator], "\n")
}

# Display the first plot to start
print(combined_plots[[1]])

# Function to display all combined plots sequentially
display_all_combined_plots <- function() {
  for (i in seq_along(combined_plots)) {
    cat("Displaying plot", i, "of", length(combined_plots), ":", names(combined_plots)[i], "\n")
    print(combined_plots[[i]])
    # Uncomment below if you want to pause between plots
    # readline(prompt = "Press [Enter] to see next plot")
  }
}

display_all_combined_plots()






################################################################################
# ### 4. Hierarchical results plot ####
################################################################################

library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)
library(scales)
library(grid)
library(gridExtra)

# Clean variable names for better display (same as before)
hierarchical_results <- hierarchical_results %>%
  mutate(
    dep_clean = case_when(
      str_detect(dependent_var, "Current.health.expenditure") ~ "Current health expenditure (% of GDP)",
      str_detect(dependent_var, "Death.rate") ~ "Death rate (per 1,000 people)",
      str_detect(dependent_var, "Hospital.beds") ~ "Hospital beds (per 1,000 people)",
      str_detect(dependent_var, "Mortality.rate..neonatal") ~ "Mortality rate, neonatal (per 1,000 live births)",
      str_detect(dependent_var, "Mortality.rate..under.5") ~ "Mortality rate, under-5 (per 1,000 live births)",
      str_detect(dependent_var, "Mortality.rate..infant") ~ "Mortality rate, infant (per 1,000 live births)",
      str_detect(dependent_var, "Mortality.rate..adult..female") ~ "Mortality rate, female adults (per 1,000)",
      str_detect(dependent_var, "Mortality.rate..adult..male") ~ "Mortality rate, male adults (per 1,000)",
      str_detect(dependent_var, "Out.of.pocket") ~ "Out-of-pocket expenditure (current US$)",
      str_detect(dependent_var, "catastrophic") ~ "Risk of catastrophic expenditure for surgical care (%)",
      str_detect(dependent_var, "impoverishing") ~ "Risk of impoverishing expenditure for surgical care (%)",
      str_detect(dependent_var, "intellectual.property..payments") ~ "IP payments (BoP, current US$)",
      str_detect(dependent_var, "intellectual.property..receipts") ~ "IP receipts (BoP, current US$)",
      dependent_var == "publications" ~ "Scientific publications",
      TRUE ~ dependent_var
    ),
    
    indep_clean = case_when(
      independent_var == "publications" ~ "Scientific publications",
      str_detect(independent_var, "Physicians") ~ "Physicians (per 1,000 people)",
      str_detect(independent_var, "Research.and.d") ~ "R&D expenditure (% of GDP)",
      str_detect(independent_var, "Researchers.in") ~ "Researchers in R&D (per million)",
      TRUE ~ independent_var
    ),
    
    # Create model label showing what predicts what
    model_label = ifelse(
      independent_var == "publications",
      paste("Publications →", dep_clean),
      paste(indep_clean, "→ Publications")
    ),
    
    # Create UPDATED category groupings based on user's specifications
    category = case_when(
      # Health System
      str_detect(dependent_var, "Current.health.expenditure") | 
        str_detect(dependent_var, "Hospital.beds") |
        str_detect(dependent_var, "Out.of.pocket") |
        (independent_var != "publications" & str_detect(independent_var, "Physicians")) ~ "Health System",
      
      # Mortality
      str_detect(dependent_var, "Death.rate") |
        str_detect(dependent_var, "Mortality") ~ "Mortality",
      
      # Financial Risk
      str_detect(dependent_var, "catastrophic") | 
        str_detect(dependent_var, "impoverishing") ~ "Financial Risk",
      
      # R&D/Innovation
      str_detect(dependent_var, "intellectual.property") |
        str_detect(dependent_var, "Research.and.development") |
        str_detect(dependent_var, "Researchers.in.R.D") |
        (independent_var != "publications" & str_detect(independent_var, "Research.and.d")) |
        (independent_var != "publications" & str_detect(independent_var, "Researchers.in")) ~ "R&D/Innovation",
      
      # Default
      TRUE ~ "Other"
    ),
    
    # Set a specific order for categories
    category = factor(category, levels = c("Financial Risk", "Health System", "Mortality",  "R&D/Innovation")),
    
    # Determine significance levels for coloring
    significance = case_when(
      main_effect_p_adj < 0.001 ~ "p < 0.001",
      main_effect_p_adj < 0.01 ~ "p < 0.01",
      main_effect_p_adj < 0.05 ~ "p < 0.05",
      TRUE ~ "Not significant"
    ),
    
    # Calculate 95% confidence intervals
    ci_lower = main_effect - 1.96 * main_effect_se,
    ci_upper = main_effect + 1.96 * main_effect_se
  )

# Order by category and significance
hierarchical_results <- hierarchical_results %>%
  arrange(category, desc(abs(main_effect))) %>%
  mutate(model_label = factor(model_label, levels = rev(unique(model_label))))

# Format effect sizes and CIs for display
hierarchical_results <- hierarchical_results %>%
  mutate(
    # Format effect size and CI for display
    effect_text = case_when(
      abs(main_effect) < 0.001 ~ sprintf("%.2e", main_effect),
      abs(main_effect) < 0.01 ~ sprintf("%.5f", main_effect),
      abs(main_effect) < 1 ~ sprintf("%.4f", main_effect),
      abs(main_effect) < 10 ~ sprintf("%.3f", main_effect),
      abs(main_effect) < 1000 ~ sprintf("%.2f", main_effect),
      abs(main_effect) < 1000000 ~ sprintf("%.1f", main_effect),
      TRUE ~ sprintf("%.2e", main_effect)
    ),
    
    # Format CI
    ci_text = case_when(
      # For very small/large values, use scientific notation
      abs(main_effect) < 0.001 | abs(main_effect) > 1000000 ~ 
        sprintf("(%.2e, %.2e)", ci_lower, ci_upper),
      
      # For small values, use more decimal places
      abs(main_effect) < 0.01 ~ 
        sprintf("(%.5f, %.5f)", ci_lower, ci_upper),
      
      # For medium values
      abs(main_effect) < 1 ~ 
        sprintf("(%.4f, %.4f)", ci_lower, ci_upper),
      
      # For standard values
      abs(main_effect) < 10 ~ 
        sprintf("(%.3f, %.3f)", ci_lower, ci_upper),
      
      # For larger values
      abs(main_effect) < 1000 ~ 
        sprintf("(%.2f, %.2f)", ci_lower, ci_upper),
      
      # For much larger values
      abs(main_effect) < 1000000 ~ 
        sprintf("(%.1f, %.1f)", ci_lower, ci_upper),
      
      # Default to scientific notation for very large values
      TRUE ~ sprintf("(%.2e, %.2e)", ci_lower, ci_upper)
    ),
    
    # Complete display text
    display_text = paste0(effect_text, " ", ci_text)
  )

# Create a function for symmetric log transformation (handles negative values)
symlog_trans <- function(base = 10, threshold = 1, scale = 1) {
  trans <- function(x) {
    sign(x) * log10(1 + abs(x)/threshold) * scale
  }
  
  inv <- function(x) {
    sign(x) * (base^(abs(x)/scale) - 1) * threshold
  }
  
  scales::trans_new(
    name = paste0("symlog-", format(threshold)),
    transform = trans,
    inverse = inv,
    domain = c(-Inf, Inf),
    breaks = scales::extended_breaks(),
    format = scales::format_format(scientific = FALSE)
  )
}

# Find a good threshold for the symlog transformation
min_magnitude <- min(abs(hierarchical_results$main_effect[hierarchical_results$main_effect != 0]))
threshold <- min_magnitude / 10

# Create the forest plot with symmetric log scale
p <- ggplot(hierarchical_results, 
            aes(y = model_label, x = main_effect, xmin = ci_lower, xmax = ci_upper)) +
  # Add vertical line at x=0
  geom_vline(xintercept = 0, color = "black", size = 0.7) +
  # Add light reference lines
  geom_vline(xintercept = c(-1000000, -10000, -100, -1, 1, 100, 10000, 1000000), 
             color = "gray90", size = 0.5, linetype = "dashed") +
  # Add horizontal lines for the CIs
  geom_errorbarh(height = 0.3, color = "gray30") +
  # Add points colored by significance
  geom_point(aes(color = significance, 
                 size = ifelse(main_effect_p_adj < 0.05, 3, 2))) +
  # Add labels for model type
  geom_text(aes(label = distribution), hjust = -0.2, vjust = -0.5, 
            size = 2.8, color = "gray50") +
  # Facet by category
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  # Use a colorblind-friendly palette for significance
  scale_color_manual(values = c(
    "p < 0.001" = "#0072B2",  # Blue
    "p < 0.01" = "#009E73",   # Green
    "p < 0.05" = "#D55E00",   # Orange
    "Not significant" = "#000000" # Gray
  )) +
  scale_size_identity() +
  # Use symmetric log scale for x-axis with CAREFULLY SELECTED BREAKS
  scale_x_continuous(
    trans = symlog_trans(threshold = threshold),
    breaks = c(-10^7, -10^5, -10^3, -10, -1, -0.1, 0, 0.1, 1, 10, 10^3, 10^5, 10^7),
    labels = function(x) {
      ifelse(x == 0, "0", 
             ifelse(abs(x) < 0.001 | abs(x) > 999, 
                    scales::scientific(x), 
                    scales::comma(x)))
    }
  ) +
  labs(
    #title = "Hierarchical Regression Results: Publications and Health Indicators",
    #subtitle = "Points show regression coefficients with 95% confidence intervals. The black vertical line at zero indicates no effect.",
    x = "Effect size (symmetric log scale)",
    y = NULL,
    color = "Significance"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.spacing = unit(1, "lines")
  )

# Create a table for the right side
# First, prepare the table data
table_data <- hierarchical_results %>%
  select(model_label, display_text, category) %>%
  arrange(match(model_label, levels(hierarchical_results$model_label)))

# Create a ggplot for the text table
p_table <- ggplot(table_data, aes(x = 1, y = model_label)) +
  geom_text(aes(label = display_text), hjust = 0, size = 2.8) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(1, 2), expand = c(0, 0)) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 10, r = 5, b = 10, l = 0),
    # Align the text with the main plot's y-axis positioning
    axis.text.y = element_blank()
  )

# Create a title for the table
p_table_title <- ggplot() +
  annotate("text", x = 1, y = 0.5, label = "Effect (95% CI)", size = 3.5, fontface = "bold") +
  theme_void() +
  theme(
    plot.margin = margin(t = 18, r = 5, b = 0, l = 0)  # Adjust top margin to align with main plot title
  )

# Combine into a single display
# First combine the table title with table
table_combined <- gridExtra::grid.arrange(p_table_title, p_table, 
                                          ncol = 1, heights = c(0.05, 0.95))

# Then combine the main plot with the table
final_plot <- gridExtra::grid.arrange(p, table_combined, 
                                      ncol = 2, widths = c(0.8, 0.2))

# Display the final combined plot
print(final_plot)



# export table_data as CSV

#write.csv(table_data, "~/Desktop/medicalErrors3/manuscript/hierarchical_results_table_95CI.csv", row.names = FALSE)
