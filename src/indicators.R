###############################################################################
# LOAD LIBRARIES
###############################################################################
library(dplyr)
library(tidyr)
library(readxl)
library(pheatmap)
library(ggplot2)
library(ggsci)
library(metafor)
library(wbstats)

###############################################################################
# HELPER FUNCTIONS
###############################################################################

# Function to replace income level strings with abbreviations
replace_income_levels <- function(df) {
  df %>%
    mutate(income_level_iso3c = case_when(
      income_level_iso3c == "Low income" ~ "LIC",
      income_level_iso3c == "Lower middle income" ~ "LMC",
      income_level_iso3c == "High income" ~ "HIC",
      income_level_iso3c == "Upper middle income" ~ "UMC",
      TRUE ~ income_level_iso3c  # Keep non-matching values
    ))
}

# Function to pivot indicator data (columns starting with "X" represent years)
process_indicator_df <- function(df) {
  df %>%
    pivot_longer(
      cols = starts_with("X"),            # Select year columns
      names_to = "date",                  # New column name for years
      values_to = "value"                 # New column name for values
    ) %>%
    mutate(date = as.numeric(sub("X", "", date)))  # Remove the "X" and convert to numeric
}

###############################################################################
# 1. INPUT INDICATORS FROM Global Observatory on Health R&D (NO API)
###############################################################################

# Define income groups of interest
income_groups <- c("HIC", "LIC", "LMC", "UMC")

# --- Read the CSV files maintaining original column names ---

# (a) Health researchers (in full-time equivalent), as a proportion of all researchers
df_health_researchers <- read.csv(
  "~/Desktop/MedicalErrors3/data/indicators/Health researchers (in full-time equivalent), as a proportion of all researchers.csv", 
  header = TRUE, stringsAsFactors = FALSE
)
# Rename column and update income levels
df_health_researchers <- df_health_researchers %>%
  rename(income_level_iso3c = X) %>%
  replace_income_levels() %>%
  process_indicator_df()

# (b) Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)
df_grants <- read.csv(
  "~/Desktop/MedicalErrors3/data/indicators/Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT).csv", 
  header = TRUE, stringsAsFactors = FALSE
)
df_grants <- df_grants %>%
  rename(income_level_iso3c = Income.Group) %>%
  replace_income_levels() %>%
  process_indicator_df()

# (c) Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country
df_oda <- read.csv(
  "~/Desktop/MedicalErrors3/data/indicators/Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country.csv", 
  header = TRUE, stringsAsFactors = FALSE
)
df_oda <- df_oda %>%
  rename(income_level_iso3c = Income.Group) %>%
  replace_income_levels() %>%
  process_indicator_df()

###############################################################################
# 2. INPUT MAIN DATASET AND CREATE SUMMARY DATASET (data_summary)
###############################################################################

# Read the main dataset from Excel
data <- read_excel("~/Desktop/MedicalErrors3/data/data.xlsx", col_types = "text")

# Convert selected columns to numeric and filter rows with a valid 'Country by region'
data <- data %>%
  mutate_at(c(1, 4, 5), as.numeric) %>%
  filter(!is.na(`Country by region`))

# Replace "-" with NA in the "Cuartil" column
data$Cuartil <- na_if(data$Cuartil, "-")

# Create a summary dataset grouped by Country Income and Publication Year
data_summary <- data %>%
  group_by(`Country by Income`, `año de publicación`) %>%
  summarise(
    citations    = sum(`Número de citaciones`, na.rm = TRUE),
    publications = n(),
    Hindex       = mean(`Índice H`, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  rename(
    income_level_iso3c = `Country by Income`,
    date = `año de publicación`
  ) %>%
  replace_income_levels()

###############################################################################
# 3. MERGE INDICATOR DATA (Global Observatory) INTO data_summary
###############################################################################

# Create a named list of the indicator data frames
indicator_dataframes <- list(
  "Health researchers (in full-time equivalent), as a proportion of all researchers" = df_health_researchers,
  "Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)" = df_grants,
  "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country" = df_oda
)

# Merge each indicator into data_summary using left_join (by income level and date)
for (indicator_name in names(indicator_dataframes)) {
  indicator_df <- indicator_dataframes[[indicator_name]] %>%
    dplyr::select(income_level_iso3c, date, value)
  data_summary <- left_join(data_summary, indicator_df, by = c("income_level_iso3c", "date"))
  # Rename the joined column (which is named "value") to the indicator's name
  data_summary <- data_summary %>% rename(!!indicator_name := value)
  message(paste("Added data to data_summary for indicator:", indicator_name))
}

###############################################################################
# 4. INPUT INDICATORS USING WORLD BANK API
###############################################################################

# Define the World Bank indicator codes to query
wb_indicators <- c("SH.DTH.NCOM.ZS", "SH.DTH.COMM.ZS", "SH.XPD.CHEX.GD.ZS", "SP.DYN.CDRT.IN",
                   "SH.MED.BEDS.ZS", "SH.DYN.NMRT", "SH.DYN.MORT", "SP.DYN.IMRT.IN", "SP.DYN.AMRT.FE",
                   "SP.DYN.AMRT.MA", "SH.XPD.OOPC.PC.CD", "SH.MED.PHYS.ZS", "SH.SGR.CRSK.ZS",
                   "SH.SGR.IRSK.ZS", "SH.MED.SAOP.P5", "BM.GSR.ROYL.CD", "BX.GSR.ROYL.CD", "GB.XPD.RSDV.GD.ZS",
                   "SP.POP.SCIE.RD.P6")  # Add or remove codes as needed

# Get country information and filter for the income groups of interest
countries_info <- wb_countries()
countries_income <- countries_info %>%
  filter(income_level_iso3c %in% income_groups) %>%
  dplyr::select(iso3c, income_level_iso3c)

# Initialize a vector to store the indicator labels (names)
all_indicators <- c()

# Loop over each indicator code from the World Bank
for (code in wb_indicators) {
  # 1. Download indicator data
  indicator_data <- wb_data(indicator = code, return_wide = TRUE)
  
  # 2. Extract the descriptive label of the indicator
  indicator_label <- attr(indicator_data[[code]], "label")
  
  # 3. Merge with countries_income to add income level
  indicator_data <- merge(indicator_data, countries_income, by = "iso3c")
  
  # 4. Rename the column with the indicator code to "value"
  colnames(indicator_data)[which(colnames(indicator_data) == code)] <- "value"
  
  # 5. Select relevant columns
  indicator_data <- indicator_data[, c("country", "date", "value", "last_updated", "income_level_iso3c")]
  
  # 6. Aggregate by income level and date (mean of the indicator values)
  indicator_data <- aggregate(value ~ date + income_level_iso3c, data = indicator_data, FUN = mean, na.rm = TRUE)
  
  # 7. Reorder columns: income_level_iso3c, date, value
  indicator_data <- indicator_data[, c("income_level_iso3c", "date", "value")]
  
  # 8. Merge the indicator into data_summary via a left join
  data_summary <- left_join(data_summary, indicator_data, by = c("income_level_iso3c", "date"))
  # The joined column will be named "value"; rename it to the indicator label
  data_summary <- data_summary %>% rename(!!indicator_label := value)
  
  # 9. Append the indicator label to the all_indicators vector
  all_indicators <- c(all_indicators, indicator_label)
  
  # 10. Create an object in the environment with the indicator label as its name (for later use)
  assign(indicator_label, indicator_data)
  
  message(paste("Processed and added indicator to data_summary:", indicator_label))
}

# Prepend the names of the three original indicators to all_indicators
all_indicators <- c(
  "Health researchers (in full-time equivalent), as a proportion of all researchers",
  "Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)",
  "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country",
  all_indicators
)
