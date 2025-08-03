################################################################################
#### 0. LIBRARIES ####
################################################################################

library(readxl)
library(dplyr)
library(openxlsx)


################################################################################
#### 1. INPUT ####
################################################################################

SM1 = read_excel("~/data/Supplementary Material 2.xlsx")
SM2 = read_excel("~/data/Supplementary Material 3.xlsx")



################################################################################
#### 2. Additional data for SM2 ####
################################################################################

# Add interpretable coefficients and confidence intervals based on model type
# This code creates 4 new columns:
# 1. effect_size: The interpretable coefficient value (OR, IRR, or raw β)
# 2. effect_type: The type of effect measure ("OR", "IRR", or "Coefficient")
# 3. ci_lower: Lower bound of 95% confidence interval
# 4. ci_upper: Upper bound of 95% confidence interval

SM1 <- SM1 %>%
  mutate(
    # Calculate raw confidence intervals first
    raw_ci_lower = case_when(
      model_type == "linear" ~ coefficient - qt(0.975, n_observations - 2) * std_error,
      TRUE ~ coefficient - qnorm(0.975) * std_error
    ),
    raw_ci_upper = case_when(
      model_type == "linear" ~ coefficient + qt(0.975, n_observations - 2) * std_error,
      TRUE ~ coefficient + qnorm(0.975) * std_error
    ),
    
    # Generate appropriate effect size based on model type
    effect_size = case_when(
      model_type == "linear" ~ coefficient,
      model_type %in% c("poisson", "negbin") ~ exp(coefficient),
      model_type == "beta" ~ exp(coefficient),
      TRUE ~ NA_real_
    ),
    
    # Specify the effect type based on model
    effect_type = case_when(
      model_type == "linear" ~ "Coefficient",
      model_type %in% c("poisson", "negbin") ~ "IRR",
      model_type == "beta" ~ "OR",
      TRUE ~ NA_character_
    ),
    
    # Calculate transformed confidence intervals for interpretable coefficients
    ci_lower = case_when(
      model_type == "linear" ~ raw_ci_lower,
      TRUE ~ exp(raw_ci_lower)
    ),
    
    ci_upper = case_when(
      model_type == "linear" ~ raw_ci_upper,
      TRUE ~ exp(raw_ci_upper)
    )
  ) %>%
  # Remove temporary columns used for calculations
  select(-raw_ci_lower, -raw_ci_upper)






################################################################################
#### 3. Additional data for SM3 ####
################################################################################

# Add interpretable coefficients and confidence intervals for hierarchical models
# This code creates 4 new columns:
# 1. effect_size: The interpretable effect value (OR, IRR, or raw coefficient)
# 2. effect_type: The type of effect measure ("OR", "IRR", or "Coefficient")
# 3. ci_lower: Lower bound of 95% confidence interval for interpretable effect
# 4. ci_upper: Upper bound of 95% confidence interval for interpretable effect


SM2 <- SM2 %>%
  mutate(
    # Calculate raw confidence intervals first
    raw_ci_lower = case_when(
      distribution == "gaussian" ~ main_effect - qt(0.975, n_observations - n_groups - 1) * main_effect_se,
      TRUE ~ main_effect - qnorm(0.975) * main_effect_se
    ),
    raw_ci_upper = case_when(
      distribution == "gaussian" ~ main_effect + qt(0.975, n_observations - n_groups - 1) * main_effect_se,
      TRUE ~ main_effect + qnorm(0.975) * main_effect_se
    ),
    
    # Generate appropriate effect size based on distribution
    effect_size = case_when(
      distribution == "gaussian" ~ main_effect,
      distribution %in% c("poisson", "nbinom2") ~ exp(main_effect),
      distribution %in% c("beta", "gaussian_logit") ~ exp(main_effect),
      TRUE ~ NA_real_
    ),
    
    # Specify the effect type based on distribution
    effect_type = case_when(
      distribution == "gaussian" ~ "Coefficient",
      distribution %in% c("poisson", "nbinom2") ~ "IRR",
      distribution %in% c("beta", "gaussian_logit") ~ "OR",
      TRUE ~ NA_character_
    ),
    
    # Calculate transformed confidence intervals for interpretable effects
    ci_lower = case_when(
      distribution == "gaussian" ~ raw_ci_lower,
      TRUE ~ exp(raw_ci_lower)
    ),
    
    ci_upper = case_when(
      distribution == "gaussian" ~ raw_ci_upper,
      TRUE ~ exp(raw_ci_upper)
    )
  ) %>%
  # Remove temporary columns used for calculations
  select(-raw_ci_lower, -raw_ci_upper)








################################################################################
#### 4. Add a column for plotting the effect size ####
################################################################################

# For SM1 dataframe
SM1 <- SM1 %>%
  mutate(
    effect_size_for_plotting = case_when(
      effect_type == "Coefficient" ~ effect_size,
      TRUE ~ effect_size - 1
    )
  )

# For SM2 dataframe
SM2 <- SM2 %>%
  mutate(
    effect_size_for_plotting = case_when(
      effect_type == "Coefficient" ~ effect_size,
      TRUE ~ effect_size - 1
    )
  )


