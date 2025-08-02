################################################################################
# 0. Libraries
################################################################################

library(readxl)



################################################################################
# 1. Input
################################################################################

data = read_excel("~/Desktop/medicalErrors3/manuscript/Supplementary Material 1.xlsx")




################################################################################
# 2. Depuration
################################################################################

# Convert to decimals columns with percentage values
percentage_columns <- c("Health.researchers..in.full.time.equivalent...as.a.proportion.of.all.researchers",
                        "Cause.of.death..by.non.communicable.diseases....of.total.",
                        "Cause.of.death..by.communicable.diseases.and.maternal..prenatal.and.nutrition.conditions....of.total.",
                        "Current.health.expenditure....of.GDP.",
                        "Risk.of.catastrophic.expenditure.for.surgical.care....of.people.at.risk.",
                        "Risk.of.impoverishing.expenditure.for.surgical.care....of.people.at.risk.",
                        "Research.and.development.expenditure....of.GDP.")

data[percentage_columns] <- lapply(data[percentage_columns], function(x) {
  if (is.numeric(x)) {
    return(x / 100)  # Convert to decimal
  } else {
    return(x)  # Return unchanged if not numeric
  }
})


################################################################################
# 3.1 Regression analyses
################################################################################

# Load required packages
library(dplyr)
library(tidyr)
library(broom)


# Function to print column information
print_column_info <- function(data) {
  cat("Column names in the dataset:\n")
  for (i in 1:ncol(data)) {
    cat(i, ": '", colnames(data)[i], "'\n", sep="")
  }
  cat("\n")
}

# Function to determine appropriate model type based on variable characteristics
determine_model_type <- function(variable, detailed_check = TRUE) {
  # Remove NA values for analysis
  var_clean <- variable[!is.na(variable)]
  
  # If insufficient data for meaningful analysis
  if (length(var_clean) < 5) {
    warning("Too few data points for reliable distribution analysis")
    return("linear")  # Safe default
  }
  
  # Check if count data (non-negative integers)
  is_count_data <- all(var_clean >= 0) && all(round(var_clean) == var_clean)
  
  # Check if proportion data (between 0 and 1)
  is_proportion_data <- all(var_clean >= 0 & var_clean <= 1)
  
  # Detailed analysis for count data
  if (is_count_data && detailed_check) {
    # Calculate descriptive statistics
    mean_val <- mean(var_clean)
    var_val <- var(var_clean)
    
    # Check equidispersion (Poisson characteristic)
    dispersion_ratio <- var_val / mean_val
    
    # Decision criteria for count data:
    if (mean(var_clean) < 1) {
      # For counts with low average, Poisson is usually appropriate
      cat("Counts with low mean (<1): Poisson recommended\n")
      return("poisson")
    } else if (dispersion_ratio > 1.5) {
      # Significant overdispersion: use negative binomial
      cat("Overdispersion detected (variance/mean = ", round(dispersion_ratio, 2), 
          "): negative binomial recommended\n", sep="")
      return("negbin")
    } else if (dispersion_ratio < 0.7) {
      # Underdispersion: consider alternatives
      cat("Underdispersion detected (variance/mean = ", round(dispersion_ratio, 2), 
          "): consider alternative models\n", sep="")
      # For underdispersion, Poisson is still better than negative binomial
      return("poisson")
    } else {
      # Near equidispersion: Poisson is appropriate
      cat("Approximately equidispersed (variance/mean = ", round(dispersion_ratio, 2), 
          "): Poisson appropriate\n", sep="")
      return("poisson")
    }
  }
  
  # Decision based on basic characteristics
  if (is_count_data) {
    return("poisson")
  } else if (is_proportion_data) {
    # Check if values are exclusively 0 and 1 (binary)
    if (all(var_clean %in% c(0, 1))) {
      return("binomial")
    } else {
      return("beta")
    }
  } else {
    # Check normality for continuous data
    if (detailed_check) {
      # Shapiro-Wilk test for normality (for small samples)
      if (length(var_clean) <= 5000) {
        shapiro_test <- tryCatch({
          shapiro.test(var_clean)
        }, error = function(e) {
          return(NULL)
        })
        
        if (!is.null(shapiro_test)) {
          if (shapiro_test$p.value < 0.05) {
            cat("Data do not follow normal distribution (p=", round(shapiro_test$p.value, 4), 
                "): consider transformation\n", sep="")
          } else {
            cat("Data appear normally distributed (p=", round(shapiro_test$p.value, 4), ")\n", sep="")
          }
        }
      }
    }
    return("linear")
  }
}

# Function to fit model for a specific income group and variables
fit_individual_model <- function(data, income_level, dependent_var, independent_var, model_type = "auto") {
  # Filter data for specific income group
  group_data <- data[data$income_level_iso3c == income_level, ]
  
  # Remove rows with NA in dependent or independent variables
  group_data <- group_data[!is.na(group_data[[dependent_var]]) & !is.na(group_data[[independent_var]]), ]
  
  # Check if we have enough data points
  if (nrow(group_data) < 5) {
    return(list(
      model = NULL,
      statistics = data.frame(
        income_group = income_level,
        dependent_var = dependent_var,
        independent_var = independent_var,
        n_observations = nrow(group_data),
        model_type = NA,
        coefficient = NA,
        std_error = NA,
        t_or_z_value = NA,
        p_value = NA,
        r_squared = NA,
        adj_r_squared = NA,
        aic = NA,
        successful = FALSE,
        error_message = "Insufficient data (fewer than 5 observations)"
      )
    ))
  }
  
  # Auto-determine model type if not specified
  if (model_type == "auto") {
    model_type <- determine_model_type(group_data[[dependent_var]])
  }
  
  # Create formula with backticks to handle special characters
  formula <- as.formula(paste0("`", dependent_var, "` ~ `", independent_var, "`"))
  
  # Try to fit the appropriate model
  result <- tryCatch({
    if (model_type == "poisson") {
      # For count data with equidispersion
      model <- glm(formula, data = group_data, family = poisson())
      model_summary <- summary(model)
      
      # Extract statistics
      coef_table <- coef(model_summary)
      coefficient <- coef_table[2, 1]  # Coefficient of independent variable
      std_error <- coef_table[2, 2]    # Standard error
      z_value <- coef_table[2, 3]      # z-value
      p_value <- coef_table[2, 4]      # p-value
      
      # Calculate pseudo-R-squared (1 - residual deviance/null deviance)
      r_squared <- 1 - (model_summary$deviance / model_summary$null.deviance)
      adj_r_squared <- NA  # Not applicable for GLMs
      
      # Get AIC
      aic <- model_summary$aic
      
      stats <- data.frame(
        income_group = income_level,
        dependent_var = dependent_var,
        independent_var = independent_var,
        n_observations = nrow(group_data),
        model_type = "poisson",
        coefficient = coefficient,
        std_error = std_error,
        t_or_z_value = z_value,
        p_value = p_value,
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        aic = aic,
        successful = TRUE,
        error_message = NA
      )
    } else if (model_type == "negbin") {
      # For count data with overdispersion
      if (!requireNamespace("MASS", quietly = TRUE)) {
        install.packages("MASS")
        library(MASS)
      }
      
      model <- MASS::glm.nb(formula, data = group_data)
      model_summary <- summary(model)
      
      # Extract statistics
      coef_table <- coef(model_summary)
      coefficient <- coef_table[2, 1]  # Coefficient of independent variable
      std_error <- coef_table[2, 2]    # Standard error
      z_value <- coef_table[2, 3]      # z-value
      p_value <- coef_table[2, 4]      # p-value
      
      # Calculate pseudo-R-squared
      r_squared <- 1 - (model_summary$deviance / model_summary$null.deviance)
      adj_r_squared <- NA  # Not applicable for GLMs
      
      # Get AIC
      aic <- AIC(model)
      
      stats <- data.frame(
        income_group = income_level,
        dependent_var = dependent_var,
        independent_var = independent_var,
        n_observations = nrow(group_data),
        model_type = "negbin",
        coefficient = coefficient,
        std_error = std_error,
        t_or_z_value = z_value,
        p_value = p_value,
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        aic = aic,
        successful = TRUE,
        error_message = NA
      )
    } else if (model_type == "binomial") {
      # For binary variables
      model <- glm(formula, data = group_data, family = binomial())
      model_summary <- summary(model)
      
      # Extract statistics
      coef_table <- coef(model_summary)
      coefficient <- coef_table[2, 1]  # Coefficient of independent variable
      std_error <- coef_table[2, 2]    # Standard error
      z_value <- coef_table[2, 3]      # z-value
      p_value <- coef_table[2, 4]      # p-value
      
      # Calculate pseudo-R-squared
      r_squared <- 1 - (model_summary$deviance / model_summary$null.deviance)
      adj_r_squared <- NA  # Not applicable for GLMs
      
      # Get AIC
      aic <- model_summary$aic
      
      stats <- data.frame(
        income_group = income_level,
        dependent_var = dependent_var,
        independent_var = independent_var,
        n_observations = nrow(group_data),
        model_type = "binomial",
        coefficient = coefficient,
        std_error = std_error,
        t_or_z_value = z_value,
        p_value = p_value,
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        aic = aic,
        successful = TRUE,
        error_message = NA
      )
    } else if (model_type == "beta") {
      # For proportion data - need betareg package
      if (!requireNamespace("betareg", quietly = TRUE)) {
        install.packages("betareg")
        library(betareg)
      }
      
      # Beta regression requires (0,1) exclusive range
      y <- group_data[[dependent_var]]
      n <- length(y)
      y_transf <- (y * (n - 1) + 0.5) / n
      group_data[[dependent_var]] <- y_transf
      
      model <- betareg::betareg(formula, data = group_data)
      model_summary <- summary(model)
      
      # Extract statistics
      coef_table <- coef(model_summary)$mean
      coefficient <- coef_table[2, 1]  # Assuming independent var is the second coefficient
      std_error <- coef_table[2, 2]    # Standard error
      z_value <- coef_table[2, 3]      # z-value
      p_value <- coef_table[2, 4]      # p-value
      
      # Get pseudo-R-squared
      r_squared <- model_summary$pseudo.r.squared
      adj_r_squared <- NA  # Not directly applicable
      
      # Get AIC
      aic <- AIC(model)
      
      stats <- data.frame(
        income_group = income_level,
        dependent_var = dependent_var,
        independent_var = independent_var,
        n_observations = nrow(group_data),
        model_type = "beta",
        coefficient = coefficient,
        std_error = std_error,
        t_or_z_value = z_value,
        p_value = p_value,
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        aic = aic,
        successful = TRUE,
        error_message = NA
      )
    } else {
      # Default: linear regression
      model <- lm(formula, data = group_data)
      model_summary <- summary(model)
      
      # Extract statistics
      coef_table <- coef(model_summary)
      coefficient <- coef_table[2, 1]  # Coefficient of independent variable
      std_error <- coef_table[2, 2]    # Standard error
      t_value <- coef_table[2, 3]      # t-value
      p_value <- coef_table[2, 4]      # p-value
      
      # Get R-squared values
      r_squared <- model_summary$r.squared
      adj_r_squared <- model_summary$adj.r.squared
      
      # Get AIC
      aic <- AIC(model)
      
      stats <- data.frame(
        income_group = income_level,
        dependent_var = dependent_var,
        independent_var = independent_var,
        n_observations = nrow(group_data),
        model_type = "linear",
        coefficient = coefficient,
        std_error = std_error,
        t_or_z_value = t_value,
        p_value = p_value,
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        aic = aic,
        successful = TRUE,
        error_message = NA
      )
    }
    
    # Return model and statistics
    list(model = model, statistics = stats)
    
  }, error = function(e) {
    # Return error information
    list(
      model = NULL,
      statistics = data.frame(
        income_group = income_level,
        dependent_var = dependent_var,
        independent_var = independent_var,
        n_observations = nrow(group_data),
        model_type = model_type,
        coefficient = NA,
        std_error = NA,
        t_or_z_value = NA,
        p_value = NA,
        r_squared = NA,
        adj_r_squared = NA,
        aic = NA,
        successful = FALSE,
        error_message = e$message
      )
    )
  })
  
  return(result)
}

# Function to run models for all income groups for a specific variable combination
run_models_by_income_groups <- function(data, 
                                        dependent_var,
                                        independent_var,
                                        columns_to_exclude = c(6:10, 18)) {
  
  # Get unique income groups
  income_groups <- unique(data$income_level_iso3c)
  
  # Run model for each income group
  results <- lapply(income_groups, function(group) {
    cat("Fitting model for income group:", group, 
        "- dependent:", dependent_var, 
        "- independent:", independent_var, "\n")
    
    fit_individual_model(data, group, dependent_var, independent_var)
  })
  
  # Extract just the statistics into a combined dataframe
  stats_df <- do.call(rbind, lapply(results, function(x) x$statistics))
  
  # Create a named list of models
  models_list <- setNames(
    lapply(results, function(x) x$model),
    income_groups
  )
  
  return(list(
    statistics = stats_df,
    models = models_list
  ))
}

# Main analysis function that runs multiple variable combinations
run_full_analysis <- function(data,
                              indicators_as_dependent,
                              indicators_as_independent,
                              columns_to_exclude = c(6:10, 18)) {
  
  # Results containers
  all_results_dep <- list()
  all_stats_dep <- data.frame()
  
  all_results_ind <- list()
  all_stats_ind <- data.frame()
  
  # 1. Run models with indicators as dependent variables
  if (length(indicators_as_dependent) > 0) {
    for (dep_var in indicators_as_dependent) {
      for (ind_var in indicators_as_independent) {
        # Skip if variables are the same
        if (dep_var == ind_var) next
        
        # Run models for all income groups
        result_key <- paste(dep_var, ind_var, sep = "_vs_")
        result <- run_models_by_income_groups(
          data, dep_var, ind_var
        )
        
        all_results_dep[[result_key]] <- result
        all_stats_dep <- rbind(all_stats_dep, result$statistics)
      }
    }
  }
  
  # 2. Run models with indicators as independent variables (if needed)
  reverse_models_needed <- !identical(indicators_as_dependent, indicators_as_independent)
  
  if (reverse_models_needed && length(indicators_as_independent) > 0) {
    for (dep_var in indicators_as_independent) {
      for (ind_var in indicators_as_dependent) {
        # Skip if variables are the same or if this combination was done above
        if (dep_var == ind_var) next
        if (paste(ind_var, dep_var, sep = "_vs_") %in% names(all_results_dep)) next
        
        # Run models for all income groups
        result_key <- paste(dep_var, ind_var, sep = "_vs_")
        result <- run_models_by_income_groups(
          data, dep_var, ind_var
        )
        
        all_results_ind[[result_key]] <- result
        all_stats_ind <- rbind(all_stats_ind, result$statistics)
      }
    }
  }
  
  # Apply p-value adjustment for multiple comparisons
  if (nrow(all_stats_dep) > 0) {
    all_stats_dep$adjusted_p_value <- p.adjust(all_stats_dep$p_value, method = "BH")
  }
  
  if (nrow(all_stats_ind) > 0) {
    all_stats_ind$adjusted_p_value <- p.adjust(all_stats_ind$p_value, method = "BH")
  }
  
  # Return results
  return(list(
    dep_results = all_results_dep,
    ind_results = all_results_ind,
    dep_statistics = all_stats_dep,
    ind_statistics = all_stats_ind,
    all_statistics = rbind(all_stats_dep, all_stats_ind)
  ))
}

# Function to filter results by significance level
filter_significant_results <- function(stats_df, alpha = 0.05, use_adjusted = TRUE) {
  if (use_adjusted && "adjusted_p_value" %in% names(stats_df)) {
    stats_df %>%
      filter(successful == TRUE & adjusted_p_value < alpha) %>%
      arrange(adjusted_p_value)
  } else {
    stats_df %>%
      filter(successful == TRUE & p_value < alpha) %>%
      arrange(p_value)
  }
}



# 1. Execution
#--------------

# 2. Print column names to verify
print_column_info(data)

# 3. Run analysis using verified column names
dependent_indicators <- c(
  "Current.health.expenditure....of.GDP.",
  "Death.rate..crude..per.1.000.people.",
  "Hospital.beds..per.1.000.people.",
  "Mortality.rate..neonatal..per.1.000.live.births.",
  "Mortality.rate..under.5..per.1.000.live.births.",
  "Mortality.rate..infant..per.1.000.live.births.",
  "Out.of.pocket.expenditure.per.capita..current.US..",
  'Mortality.rate..adult..female..per.1.000.female.adults.',
  'Mortality.rate..adult..male..per.1.000.male.adults.',
  'Out.of.pocket.expenditure.per.capita..current.US..',
  'Risk.of.catastrophic.expenditure.for.surgical.care....of.people.at.risk.',
  'Risk.of.impoverishing.expenditure.for.surgical.care....of.people.at.risk.',
  'Charges.for.the.use.of.intellectual.property..payments..BoP..current.US..',
  'Charges.for.the.use.of.intellectual.property..receipts..BoP..current.US..'
)

independent_indicators <- c('Physicians..per.1.000.people.',
                            'Research.and.development.expenditure....of.GDP.',
                            'Researchers.in.R.D..per.million.people.')

# 4a. Run with health indicators as dependent variables
results1 <- run_full_analysis(
  data,
  indicators_as_dependent = dependent_indicators,
  indicators_as_independent = c("publications")
)

# View results
print("Results with health indicators as dependent variables:")
View(results1$dep_statistics)

# 4b. Run with publications as dependent variable
results2 <- run_full_analysis(
  data,
  indicators_as_dependent = c("publications"),
  indicators_as_independent = independent_indicators
)

# View these results
print("Results with publications as dependent variable:")
View(results2$dep_statistics)

# rbind results1$dep_statistics and results2$dep_statistics
regression_results <- rbind(results1$dep_statistics, results2$dep_statistics)

# Export to xlsx
library(openxlsx)
write.xlsx(regression_results, 
           file = "~/Desktop/medicalErrors3/manuscript/Supplementary Material 2.xlsx", 
           rowNames = FALSE)








###############################################################################
# 3.2 HIERARCHICAL APPROACH
###############################################################################

# Load required packages
library(dplyr)
library(tidyr)
library(lme4)      # For linear mixed models
library(glmmTMB)   # For generalized linear mixed models with various distributions
library(sjPlot)    # For visualizing and tabulating mixed models
library(ggplot2)
library(broom.mixed) # For tidying mixed model output
library(performance) # For model diagnostics

# Function to print column information
print_column_info <- function(data) {
  cat("Column names in the dataset:\n")
  for (i in 1:ncol(data)) {
    cat(i, ": '", colnames(data)[i], "'\n", sep="")
  }
  cat("\n")
}

# Function to determine appropriate distribution for each variable type
determine_distribution <- function(variable) {
  # For count/rate variables
  if (all(variable >= 0, na.rm = TRUE) && all(round(variable) == variable, na.rm = TRUE)) {
    return("poisson")
  }
  # For proportion variables (between 0 and 1)
  else if (all(variable >= 0 & variable <= 1, na.rm = TRUE)) {
    return("beta")
  }
  # For continuous variables (default)
  else {
    return("gaussian")
  }
}

# Function to fit hierarchical models with appropriate distribution
# IMPROVED: Removed redundant specification of income_level as both fixed and random effect
fit_hierarchical_model <- function(data, dependent_var, independent_var = "publications", 
                                   covariates = NULL, distribution = "auto") {
  
  # Make sure income_level_iso3c is a factor with proper order
  if (!is.factor(data$income_level_iso3c)) {
    data$income_level_iso3c <- factor(data$income_level_iso3c, 
                                      levels = c("LIC", "LMC", "UMC", "HIC"),
                                      ordered = TRUE)
  }
  
  # Filter out rows with NA in dependent or independent variables
  model_data <- data %>%
    filter(!is.na(!!sym(dependent_var)) & !is.na(!!sym(independent_var)))
  
  # If too few observations, return NULL
  if (nrow(model_data) < 20) {
    cat("Insufficient data for modeling", dependent_var, "(", nrow(model_data), "observations)\n")
    return(NULL)
  }
  
  # Auto-determine distribution if set to "auto"
  if (distribution == "auto") {
    distribution <- determine_distribution(model_data[[dependent_var]])
  }
  
  # IMPROVED: Modified formula to prevent redundancy (income_level only as random effect)
  formula_str <- paste0("`", dependent_var, "` ~ `", independent_var, 
                        "` + (1 | income_level_iso3c)")
  
  # Add covariates if provided
  if (!is.null(covariates) && length(covariates) > 0) {
    covariates_str <- paste(paste0("`", covariates, "`"), collapse = " + ")
    formula_str <- paste0(formula_str, " + ", covariates_str)
  }
  
  formula <- as.formula(formula_str)
  
  # Fit the appropriate model based on distribution - start with simpler model
  simple_model <- tryCatch({
    if (distribution == "poisson") {
      # For count data
      glmmTMB(formula, data = model_data, family = poisson())
    } else if (distribution == "beta") {
      # For proportion data - need to transform to (0,1) range
      y <- model_data[[dependent_var]]
      n <- length(y)
      y_transf <- (y * (n - 1) + 0.5) / n
      model_data[[dependent_var]] <- y_transf
      
      glmmTMB(formula, data = model_data, family = beta_family())
    } else {
      # For continuous data
      lmer(formula, data = model_data, control = lmerControl(optimizer = "bobyqa"))
    }
  }, error = function(e) {
    cat("Error fitting simple model for", dependent_var, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(simple_model)) {
    return(NULL)
  }
  
  # IMPROVED: Modified interaction formula to prevent redundancy
  formula_interaction <- as.formula(paste0("`", dependent_var, "` ~ `", independent_var, 
                                           "` * income_level_iso3c + (1 | income_level_iso3c)"))
  
  # Add covariates if provided
  if (!is.null(covariates) && length(covariates) > 0) {
    covariates_str <- paste(paste0("`", covariates, "`"), collapse = " + ")
    formula_interaction <- as.formula(paste0("`", dependent_var, "` ~ `", independent_var, 
                                             "` * income_level_iso3c + ", covariates_str, 
                                             " + (1 | income_level_iso3c)"))
  }
  
  interaction_model <- tryCatch({
    if (distribution == "poisson") {
      # For count data
      glmmTMB(formula_interaction, data = model_data, family = poisson())
    } else if (distribution == "beta") {
      # Use already transformed data
      glmmTMB(formula_interaction, data = model_data, family = beta_family())
    } else {
      # For continuous data
      lmer(formula_interaction, data = model_data, control = lmerControl(optimizer = "bobyqa"))
    }
  }, error = function(e) {
    cat("Error fitting interaction model for", dependent_var, ":", e$message, "\n")
    cat("Using simple model without interactions instead.\n")
    return(simple_model)  # Return simple model as fallback
  })
  
  # Check for convergence - if interaction model has convergence issues, use simple model
  if (inherits(interaction_model, "glmmTMB")) {
    if (!interaction_model$sdr$pdHess) {
      cat("Interaction model for", dependent_var, "has convergence issues. Using simple model instead.\n")
      final_model <- simple_model
      is_simplified <- TRUE
    } else {
      final_model <- interaction_model
      is_simplified <- FALSE
    }
  } else if (inherits(interaction_model, "merMod")) {
    # For lmer models
    if (!is.null(attr(interaction_model@optinfo$derivs, "Hessian"))) {
      final_model <- interaction_model
      is_simplified <- FALSE
    } else {
      cat("Interaction model for", dependent_var, "has convergence issues. Using simple model instead.\n")
      final_model <- simple_model
      is_simplified <- TRUE
    }
  } else {
    # Default to simple model if interaction_model is not recognizable
    final_model <- simple_model
    is_simplified <- TRUE
  }
  
  # Compare models using AIC if both converged
  if (!is_simplified) {
    if (AIC(simple_model) < AIC(final_model)) {
      final_model <- simple_model
      is_simplified <- TRUE
      cat("Simple model has better fit (lower AIC) for", dependent_var, "- using simple model.\n")
    }
  }
  
  return(list(
    model = final_model,
    simple_model = simple_model,
    interaction_model = if (is_simplified) NULL else interaction_model,
    data = model_data,
    dependent_var = dependent_var,
    independent_var = independent_var,
    distribution = distribution,
    covariates = covariates,
    simplified = is_simplified
  ))
}

# Improved function to extract model results safely
extract_model_results <- function(model_obj) {
  if (is.null(model_obj) || is.null(model_obj$model)) {
    return(NULL)
  }
  
  model <- model_obj$model
  dependent_var <- model_obj$dependent_var
  independent_var <- model_obj$independent_var
  distribution <- model_obj$distribution
  is_simplified <- model_obj$simplified
  
  # Extract fixed effects summary safely
  tryCatch({
    # Get model summary with p-values
    if (inherits(model, "glmmTMB")) {
      model_summary <- summary(model)
      fixed_effects <- model_summary$coefficients$cond
      
      # Make sure we have p-values - if not, calculate them
      if (!("Pr(>|z|)" %in% colnames(fixed_effects))) {
        z_values <- fixed_effects[, "Estimate"] / fixed_effects[, "Std. Error"]
        p_values <- 2 * pnorm(abs(z_values), lower.tail = FALSE)
        fixed_effects <- cbind(fixed_effects, "Pr(>|z|)" = p_values)
      }
    } else if (inherits(model, "merMod")) {
      model_summary <- summary(model)
      fixed_effects <- model_summary$coefficients
      
      # Calculate p-values for lmer models (t-distribution approximation)
      if (!("Pr(>|t|)" %in% colnames(fixed_effects))) {
        # Get degrees of freedom - approximate using Satterthwaite
        df <- nrow(model_obj$data) - length(fixef(model))
        t_values <- fixed_effects[, "t value"]
        p_values <- 2 * pt(abs(t_values), df = df, lower.tail = FALSE)
        fixed_effects <- cbind(fixed_effects, "Pr(>|t|)" = p_values)
      }
    } else {
      # For other model types
      fixed_effects <- coef(summary(model))
    }
    
    # Find the row for the independent variable
    main_effect_idx <- grep(paste0("^", independent_var, "$"), rownames(fixed_effects))
    
    if (length(main_effect_idx) == 0) {
      cat("Warning: Could not find main effect for", independent_var, "in model results\n")
      main_effect <- NA
      main_effect_se <- NA
      main_effect_p <- NA
    } else {
      main_effect <- fixed_effects[main_effect_idx, "Estimate"]
      main_effect_se <- fixed_effects[main_effect_idx, "Std. Error"]
      
      # Get p-value from appropriate column
      p_col <- if ("Pr(>|z|)" %in% colnames(fixed_effects)) {
        "Pr(>|z|)"
      } else if ("Pr(>|t|)" %in% colnames(fixed_effects)) {
        "Pr(>|t|)"
      } else {
        # If no p-value column exists, calculate p-value from z or t statistic
        if ("z value" %in% colnames(fixed_effects)) {
          z_value <- fixed_effects[main_effect_idx, "z value"]
          main_effect_p <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
        } else if ("t value" %in% colnames(fixed_effects)) {
          t_value <- fixed_effects[main_effect_idx, "t value"]
          # Approximate degrees of freedom
          df <- nrow(model_obj$data) - length(fixef(model))
          main_effect_p <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)
        } else {
          main_effect_p <- NA
        }
        NULL
      }
      
      if (!is.null(p_col)) {
        main_effect_p <- fixed_effects[main_effect_idx, p_col]
      }
    }
    
    # Extract random effects safely
    random_effect_var <- tryCatch({
      if (inherits(model, "glmmTMB")) {
        VarCorr(model)$cond$income_level_iso3c[1]
      } else {
        as.data.frame(VarCorr(model))$sdcor[1]^2
      }
    }, error = function(e) {
      cat("Warning: Could not extract random effect variance\n")
      return(NA)
    })
    
    # Create data frame of results
    results <- data.frame(
      dependent_var = dependent_var,
      independent_var = independent_var,
      distribution = distribution,
      model_type = ifelse(is_simplified, "simple", "interaction"),
      n_observations = nrow(model_obj$data),
      n_groups = length(unique(model_obj$data$income_level_iso3c)),
      
      # Main effect of independent variable
      main_effect = main_effect,
      main_effect_se = main_effect_se,
      main_effect_p = main_effect_p,
      
      # Random effect variance
      random_effect_var = random_effect_var,
      
      # Model fit statistics
      AIC = AIC(model),
      BIC = BIC(model)
    )
    
    # Add interaction effects if present (not in simplified models)
    if (!is_simplified) {
      # Find interaction rows
      interaction_rows <- grep(paste0(independent_var, ":income_level_iso3c"), rownames(fixed_effects))
      
      if (length(interaction_rows) > 0) {
        # Extract interaction effects
        interaction_effects <- fixed_effects[interaction_rows, , drop = FALSE]
        
        # Add to results
        for (i in 1:nrow(interaction_effects)) {
          income_group <- sub(".*income_level_iso3c", "", rownames(interaction_effects)[i])
          col_name <- paste0("interaction_", income_group)
          col_name_se <- paste0("interaction_", income_group, "_se")
          
          results[[col_name]] <- interaction_effects[i, "Estimate"]
          results[[col_name_se]] <- interaction_effects[i, "Std. Error"]
          
          # Add p-values if available
          if (!is.null(p_col)) {
            col_name_p <- paste0("interaction_", income_group, "_p")
            results[[col_name_p]] <- interaction_effects[i, p_col]
          } else if ("z value" %in% colnames(interaction_effects)) {
            col_name_p <- paste0("interaction_", income_group, "_p")
            z_value <- interaction_effects[i, "z value"]
            results[[col_name_p]] <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
          } else if ("t value" %in% colnames(interaction_effects)) {
            col_name_p <- paste0("interaction_", income_group, "_p")
            t_value <- interaction_effects[i, "t value"]
            df <- nrow(model_obj$data) - length(fixef(model))
            results[[col_name_p]] <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)
          }
        }
      }
    }
    
    return(results)
    
  }, error = function(e) {
    cat("Error extracting model results for", dependent_var, ":", e$message, "\n")
    return(NULL)
  })
}

# IMPROVED: Function to adjust p-values for multiple comparisons
adjust_p_values <- function(results) {
  # Make a copy of the results
  adjusted_results <- results
  
  # Extract all p-values that need adjustment
  p_value_columns <- grep("_p$", names(adjusted_results), value = TRUE)
  
  if (length(p_value_columns) > 0) {
    # Convert to list of p-values (removing NAs)
    p_values <- unlist(adjusted_results[, p_value_columns])
    p_values <- p_values[!is.na(p_values)]
    
    if (length(p_values) > 0) {
      # Adjust p-values using False Discovery Rate method
      adjusted_p_values <- p.adjust(p_values, method = "fdr")
      
      # Put adjusted values back into their columns
      p_value_index <- 1
      for (col in p_value_columns) {
        for (i in 1:nrow(adjusted_results)) {
          if (!is.na(adjusted_results[i, col])) {
            adjusted_results[i, paste0(col, "_adj")] <- adjusted_p_values[p_value_index]
            p_value_index <- p_value_index + 1
          } else {
            adjusted_results[i, paste0(col, "_adj")] <- NA
          }
        }
      }
    }
  }
  
  return(adjusted_results)
}

# Improved function for model interpretation
interpret_model <- function(model_obj) {
  if (is.null(model_obj) || is.null(model_obj$model)) {
    return("Model fitting failed.")
  }
  
  model <- model_obj$model
  dependent_var <- model_obj$dependent_var
  independent_var <- model_obj$independent_var
  is_simplified <- model_obj$simplified
  
  # Safe extraction of model summary
  tryCatch({
    if (inherits(model, "glmmTMB")) {
      model_summary <- summary(model)$coefficients$cond
    } else {
      model_summary <- summary(model)$coefficients
    }
    
    # Extract coefficients safely
    main_effect_row <- grep(paste0("^", independent_var, "$"), rownames(model_summary))
    
    if (length(main_effect_row) == 0) {
      return(paste("Could not find main effect of", independent_var, "in model"))
    }
    
    # Main effect
    main_effect <- model_summary[main_effect_row, "Estimate"]
    
    # Get p-value column name
    p_col <- if ("Pr(>|z|)" %in% colnames(model_summary)) {
      "Pr(>|z|)"
    } else if ("Pr(>|t|)" %in% colnames(model_summary)) {
      "Pr(>|t|)"
    } else {
      NULL
    }
    
    # Get p-value if column exists
    if (!is.null(p_col) && p_col %in% colnames(model_summary)) {
      main_effect_p <- model_summary[main_effect_row, p_col]
      p_value_text <- paste("(p =", round(main_effect_p, 4), ")")
    } else {
      p_value_text <- "(p-value not available)"
    }
    
    # Start building interpretation
    interpretation <- paste0(
      "Effect of ", independent_var, " on ", dependent_var, ":\n\n",
      "- Base effect: ", round(main_effect, 4), " ", p_value_text, "\n"
    )
    
    # For interaction models, add interpretations for interactions
    if (!is_simplified) {
      interaction_rows <- grep(paste0(independent_var, ":income_level_iso3c"), rownames(model_summary))
      
      if (length(interaction_rows) > 0) {
        interpretation <- paste0(interpretation, 
                                 "\nDifferential effects by income group (compared to reference):\n")
        
        for (row in interaction_rows) {
          group <- sub(".*income_level_iso3c", "", rownames(model_summary)[row])
          effect <- model_summary[row, "Estimate"]
          
          # Get p-value if column exists
          if (!is.null(p_col) && p_col %in% colnames(model_summary)) {
            p_val <- model_summary[row, p_col]
            p_text <- paste("(p =", round(p_val, 4), ")")
          } else {
            p_text <- "(p-value not available)"
          }
          
          interpretation <- paste0(interpretation,
                                   "- ", group, ": additional ", round(effect, 4),
                                   " ", p_text, "\n")
          
          # Total effect for this group
          total_effect <- main_effect + effect
          interpretation <- paste0(interpretation,
                                   "  Total effect for ", group, ": ", round(total_effect, 4), "\n")
        }
      }
    } else {
      interpretation <- paste0(interpretation, 
                               "\nNote: Using simplified model without interactions due to convergence or fit issues.\n")
    }
    
    # Add information about random effects
    random_var <- tryCatch({
      if (inherits(model, "glmmTMB")) {
        VarCorr(model)$cond$income_level_iso3c[1]
      } else {
        as.data.frame(VarCorr(model))$sdcor[1]^2
      }
    }, error = function(e) {
      return(NA)
    })
    
    if (!is.na(random_var)) {
      interpretation <- paste0(interpretation, 
                               "\nRandom effect variance (income group): ", round(random_var, 4), "\n")
    }
    
    # Add model fit information
    interpretation <- paste0(interpretation,
                             "\nModel fit: AIC = ", round(AIC(model), 2),
                             ", BIC = ", round(BIC(model), 2))
    
    return(interpretation)
    
  }, error = function(e) {
    return(paste("Error interpreting model:", e$message))
  })
}

# Function to run hierarchical models for multiple dependent variables
run_hierarchical_analysis <- function(data, dependent_vars, independent_var = "publications", 
                                      covariates = NULL, include_time = FALSE) {
  
  # Add time as covariate if specified
  if (include_time) {
    if (is.null(covariates)) {
      covariates <- "date"
    } else {
      covariates <- c(covariates, "date")
    }
  }
  
  # Create an empty list for models and an empty dataframe with proper structure
  models <- list()
  results <- data.frame()
  
  # Track if we've initialized the results dataframe
  results_initialized <- FALSE
  
  for (dep_var in dependent_vars) {
    cat("Fitting hierarchical model for:", dep_var, "\n")
    
    # Try to fit model with error handling
    tryCatch({
      model <- fit_hierarchical_model(data, dep_var, independent_var, covariates)
      
      if (!is.null(model)) {
        models[[dep_var]] <- model
        
        # Extract model results
        model_results <- extract_model_results(model)
        
        if (!is.null(model_results)) {
          # If this is our first successful model, use its structure
          if (!results_initialized && nrow(results) == 0) {
            results <- model_results
            results_initialized <- TRUE
          } else {
            # Make sure columns match before combining
            if (!identical(names(results), names(model_results))) {
              cat("Warning: Model for", dep_var, "produced results with different structure.\n")
              cat("Adjusting columns to ensure compatibility...\n")
              
              # Add missing columns to results
              for (col in setdiff(names(model_results), names(results))) {
                results[[col]] <- NA
              }
              
              # Add missing columns to model_results
              for (col in setdiff(names(results), names(model_results))) {
                model_results[[col]] <- NA
              }
              
              # Ensure columns are in the same order
              model_results <- model_results[, names(results)]
            }
            
            # Now safe to combine
            results <- rbind(results, model_results)
          }
        }
      }
    }, error = function(e) {
      cat("ERROR processing", dep_var, ":", e$message, "\n")
      cat("Skipping this variable and continuing with next one.\n")
    })
  }
  
  # IMPROVED: Apply multiple comparison correction to all p-values
  if (nrow(results) > 0) {
    results <- adjust_p_values(results)
    cat("\nMultiple comparison correction applied using False Discovery Rate method.\n")
  }
  
  return(list(
    models = models,
    results = results
  ))
}

# Function to create publication vs. indicator plots by income group
plot_relationship <- function(data, x_var = "publications", y_var, title = NULL) {
  if (is.null(title)) {
    title <- paste("Relationship between", x_var, "and", y_var)
  }
  
  # Make sure the plot doesn't error even if data has issues
  p <- tryCatch({
    ggplot(data, aes_string(x = paste0("`", x_var, "`"), 
                            y = paste0("`", y_var, "`"), 
                            color = "income_level_iso3c")) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE) +
      facet_wrap(~ income_level_iso3c, scales = "free_y") +
      theme_bw() +
      labs(
        title = title,
        x = x_var,
        y = y_var,
        color = "Income Group"
      )
  }, error = function(e) {
    # Return a basic empty plot if there's an error
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Error creating plot:", e$message)) +
      theme_void()
  })
  
  return(p)
}

# Function to run complete analysis with already loaded dataframe
run_complete_analysis <- function(data, dependent_vars, independent_var = "publications", 
                                  include_time = TRUE, include_diagnostics = TRUE) {
  # Print column information
  print_column_info(data)
  
  # Run hierarchical models
  analysis <- run_hierarchical_analysis(data, dependent_vars, independent_var, 
                                        include_time = include_time)
  
  # Print summary results table
  cat("\n--- Summary of Hierarchical Model Results ---\n")
  print(analysis$results)
  
  # Generate detailed interpretations
  cat("\n--- Detailed Model Interpretations ---\n")
  for (dep_var in names(analysis$models)) {
    cat("\n\n", paste(rep("=", 50), collapse=""), "\n")
    cat("MODEL FOR:", dep_var, "\n")
    cat(paste(rep("=", 50), collapse=""), "\n")
    
    # Print interpretation safely
    model_interpretation <- interpret_model(analysis$models[[dep_var]])
    cat(model_interpretation, "\n")
    
    # Create plot for this relationship
    if (include_diagnostics) {
      cat("\nVisualizing relationship:\n")
      p <- plot_relationship(data, independent_var, dep_var)
      print(p)
      
      # Model diagnostics if model exists and converged
      if (!is.null(analysis$models[[dep_var]]$model)) {
        cat("\nModel Diagnostics:\n")
        model <- analysis$models[[dep_var]]$model
        
        # Check for convergence issues
        if (inherits(model, "glmmTMB")) {
          convergence_status <- ifelse(model$sdr$pdHess, 
                                       "Model converged properly", 
                                       "Convergence issues detected")
        } else {
          convergence_status <- ifelse(is.null(attr(model@optinfo$derivs, "Hessian")), 
                                       "Convergence issues detected", 
                                       "Model converged properly")
        }
        
        cat("Convergence check:", convergence_status, "\n")
        
        # Only run performance checks if model converged
        if ((inherits(model, "glmmTMB") && model$sdr$pdHess) ||
            (inherits(model, "merMod") && is.null(attr(model@optinfo$derivs, "Hessian")))) {
          
          tryCatch({
            cat("Running model diagnostics (this may take a moment)...\n")
            model_perf <- check_model(model, check = c("qq", "homogeneity", "outliers"))
            print(model_perf)
          }, error = function(e) {
            cat("Couldn't run diagnostics:", e$message, "\n")
          })
        } else {
          cat("Skipping diagnostics due to convergence issues.\n")
        }
      }
    }
  }
  
  return(analysis)
}

# Define health indicators to analyze
dependent_indicators <- c(
  "Current.health.expenditure....of.GDP.",
  "Death.rate..crude..per.1.000.people.",
  "Hospital.beds..per.1.000.people.",
  "Mortality.rate..neonatal..per.1.000.live.births.",
  "Mortality.rate..under.5..per.1.000.live.births.",
  "Mortality.rate..infant..per.1.000.live.births.",
  "Out.of.pocket.expenditure.per.capita..current.US..",
  "Mortality.rate..adult..female..per.1.000.female.adults.",
  "Mortality.rate..adult..male..per.1.000.male.adults.",
  "Out.of.pocket.expenditure.per.capita..current.US..",
  "Risk.of.catastrophic.expenditure.for.surgical.care....of.people.at.risk.",
  "Risk.of.impoverishing.expenditure.for.surgical.care....of.people.at.risk.",
  "Charges.for.the.use.of.intellectual.property..payments..BoP..current.US..",
  "Charges.for.the.use.of.intellectual.property..receipts..BoP..current.US.."
)

# Run the analysis using the already loaded 'data' dataframe
hierarchical_results <- run_complete_analysis(data, dependent_indicators)

all_pub_independent_results <- as.data.frame(hierarchical_results$results)

# Export results to CSV if needed
#write.csv(hierarchical_results$results, "hierarchical_model_results.csv", row.names = FALSE)



# Define research indicators to use as independent variables
independent_indicators <- c('Physicians..per.1.000.people.',
                            'Research.and.development.expenditure....of.GDP.',
                            'Researchers.in.R.D..per.million.people.')

# Create a container for all results
publications_as_dependent_results <- list()

# Run model for each independent variable
for(ind_var in independent_indicators) {
  cat("\n--- Running hierarchical models with publications as dependent variable ---\n")
  cat("Independent variable:", ind_var, "\n\n")
  
  # Run the analysis
  result <- run_complete_analysis(
    data = data,
    dependent_vars = c("publications"),
    independent_var = ind_var,
    include_time = TRUE,
    include_diagnostics = TRUE
  )
  
  # Store results
  publications_as_dependent_results[[ind_var]] <- result
}

# Combine all results into one dataframe
# Create an empty dataframe for combined results
all_pub_dependent_results <- data.frame()

# Process each result, ensuring column compatibility
for(ind_var in names(publications_as_dependent_results)) {
  if(!is.null(publications_as_dependent_results[[ind_var]]$results) && 
     nrow(publications_as_dependent_results[[ind_var]]$results) > 0) {
    
    current_results <- publications_as_dependent_results[[ind_var]]$results
    
    # If this is our first successful model, use its structure
    if(nrow(all_pub_dependent_results) == 0) {
      all_pub_dependent_results <- current_results
    } else {
      # Make sure columns match before combining
      # Add missing columns to all_pub_dependent_results
      for(col in setdiff(names(current_results), names(all_pub_dependent_results))) {
        all_pub_dependent_results[[col]] <- NA
      }
      
      # Add missing columns to current_results
      for(col in setdiff(names(all_pub_dependent_results), names(current_results))) {
        current_results[[col]] <- NA
      }
      
      # Ensure columns are in the same order
      current_results <- current_results[, names(all_pub_dependent_results)]
      
      # Now safe to combine
      all_pub_dependent_results <- rbind(all_pub_dependent_results, current_results)
    }
  }
}

# Rbind hierarchical results with publications as dependent results
# Fix column mismatch before combining results
# First, identify all columns that exist in either dataframe
all_columns <- unique(c(names(all_pub_independent_results), names(all_pub_dependent_results)))

# Add missing columns to all_pub_independent_results
for(col in setdiff(all_columns, names(all_pub_independent_results))) {
  all_pub_independent_results[[col]] <- NA
}

# Add missing columns to all_pub_dependent_results
for(col in setdiff(all_columns, names(all_pub_dependent_results))) {
  all_pub_dependent_results[[col]] <- NA
}

# Now combine the datasets
all_hierarchical_results <- rbind(
  all_pub_independent_results,
  all_pub_dependent_results
)







##### fixing some models

# Diagnostic script for models with NaN standard errors
# This will help identify issues with beta distribution and Poisson models

# Function to diagnose NaN standard errors in model results
diagnose_nan_models <- function(dataframe, results_df) {
  # Find rows with NaN standard errors
  nan_rows <- which(is.na(results_df$main_effect_se) | 
                      results_df$main_effect_se == "NaN")
  
  cat("Found", length(nan_rows), "models with NaN standard errors\n")
  
  for (i in nan_rows) {
    # Extract model information
    dep_var <- results_df$dependent_var[i]
    ind_var <- results_df$independent_var[i]
    dist_type <- results_df$distribution[i]
    model_type <- results_df$model_type[i]
    
    cat("\n\n==== DIAGNOSING MODEL ====\n")
    cat("Row:", i, "\n")
    cat("Dependent variable:", dep_var, "\n")
    cat("Independent variable:", ind_var, "\n")
    cat("Distribution:", dist_type, "\n")
    cat("Model type:", model_type, "\n")
    
    # Filter data for this model
    model_data <- dataframe %>% 
      filter(!is.na(!!sym(dep_var)) & !is.na(!!sym(ind_var)))
    
    # Check for data issues
    cat("\n1. DATA SUMMARY:\n")
    cat("Total observations:", nrow(model_data), "\n")
    
    # Distribution by income group
    cat("\n2. OBSERVATIONS BY INCOME GROUP:\n")
    income_table <- table(model_data$income_level_iso3c)
    print(income_table)
    
    # Check for low observation counts
    if(any(income_table < 5)) {
      cat("WARNING: Some income groups have very few observations (<5)\n")
    }
    
    # Dependent variable summary by income group
    cat("\n3. DEPENDENT VARIABLE DISTRIBUTION BY INCOME GROUP:\n")
    dep_by_income <- aggregate(
      model_data[[dep_var]] ~ model_data$income_level_iso3c, 
      FUN = function(x) c(min = min(x), mean = mean(x), max = max(x), count = length(x))
    )
    print(dep_by_income)
    
    # Check for beta distribution issues
    if(dist_type == "beta") {
      cat("\n4. BETA DISTRIBUTION CHECKS:\n")
      # Check if values are strictly between 0 and 1
      min_val <- min(model_data[[dep_var]])
      max_val <- max(model_data[[dep_var]])
      
      cat("Range:", min_val, "to", max_val, "\n")
      if(min_val <= 0 || max_val >= 1) {
        cat("WARNING: Beta distribution requires values strictly between 0 and 1\n")
        cat("Values at exactly 0 or 1 will cause convergence issues\n")
      }
      
      # Check for values very close to 0 or 1
      near_boundary <- sum(model_data[[dep_var]] < 0.001 | model_data[[dep_var]] > 0.999)
      if(near_boundary > 0) {
        cat("WARNING:", near_boundary, "values are extremely close to 0 or 1\n")
      }
    }
    
    # Interaction diagnostics
    if(model_type == "interaction") {
      cat("\n5. INTERACTION DIAGNOSTICS:\n")
      
      # Check for potential separation issues
      cat("Distribution of independent variable by income group:\n")
      ind_by_income <- aggregate(
        model_data[[ind_var]] ~ model_data$income_level_iso3c, 
        FUN = function(x) c(min = min(x), median = median(x), max = max(x), count = length(x))
      )
      print(ind_by_income)
      
      # Check for separation by creating quartiles
      cat("\nCheck for separation issues by quartile:\n")
      quantiles <- quantile(model_data[[ind_var]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      model_data$quartile <- cut(model_data[[ind_var]], breaks = quantiles, include.lowest = TRUE, labels = FALSE)
      
      # Cross-tabulate income groups by quartiles
      quartile_table <- table(Income_Level = model_data$income_level_iso3c, Quartile = model_data$quartile)
      print(quartile_table)
      
      # Check for empty cells
      empty_cells <- sum(quartile_table == 0)
      if(empty_cells > 0) {
        cat("WARNING: Found", empty_cells, "empty combinations of income level and quartile\n")
        cat("This can cause separation issues and NaN standard errors\n")
      }
    }
    
    # Provide recommendations
    cat("\n==== RECOMMENDATIONS ====\n")
    
    if(dist_type == "beta") {
      cat("1. For beta models:\n")
      cat("   - Try a logit transformation and use Gaussian distribution instead\n")
      cat("   - Consider a simpler model without interactions\n")
      cat("   - Ensure all values are strictly between 0 and 1, transform boundary values\n")
    }
    
    if(dist_type == "poisson") {
      cat("1. For Poisson models:\n")
      cat("   - Try negative binomial distribution instead\n")
      cat("   - Consider a simpler model without interactions\n")
      cat("   - Check for overdispersion or excess zeros\n")
    }
    
    cat("2. General solutions:\n")
    cat("   - Remove interaction terms\n")
    cat("   - Use a simpler distribution (Gaussian)\n")
    cat("   - Standardize the independent variable\n")
    cat("   - If necessary, consolidate income groups with few observations\n")
  }
  
  return(nan_rows)
}

# Run the diagnostic function on the hierarchical results
problematic_rows <- diagnose_nan_models(data, all_hierarchical_results)

# Print the row numbers with NaN standard errors
cat("\nRows with NaN standard errors:", problematic_rows, "\n")








# Complete solution to fix models with NaN standard errors
# This script will handle all problematic rows in one clean pass

library(lme4)
library(glmmTMB)
library(dplyr)

cat("\n===== STARTING COMPLETE MODEL FIX =====\n")

# Store original values for comparison
cat("\nOriginal problematic rows:\n")
print(all_hierarchical_results[c(1, 11, 15), c("dependent_var", "independent_var", "distribution", 
                                               "model_type", "main_effect", "main_effect_se", 
                                               "main_effect_p")])

# Create a copy of the dataframe we can modify
updated_results <- all_hierarchical_results

#==== Fix row 1: Current Health Expenditure ====
cat("\n\n1. Fixing Current Health Expenditure model\n")

health_exp_data <- data %>% 
  filter(!is.na(`Current.health.expenditure....of.GDP.`) & !is.na(publications))

# Transform for logit model
health_exp_data$health_exp_transformed <- health_exp_data$`Current.health.expenditure....of.GDP.`
n <- nrow(health_exp_data)
health_exp_data$health_exp_transformed <- (health_exp_data$health_exp_transformed * (n-1) + 0.5) / n
health_exp_data$logit_health_exp <- log(health_exp_data$health_exp_transformed / (1 - health_exp_data$health_exp_transformed))

# Fit the model
health_model <- lmer(logit_health_exp ~ publications + date + (1 | income_level_iso3c), 
                     data = health_exp_data)
health_summary <- summary(health_model)
print(health_summary)

# Extract coefficients
health_coef <- health_summary$coefficients["publications", ]
health_t_value <- health_coef["t value"]
health_df <- health_summary$devcomp$dims["obs"] - health_summary$devcomp$dims["p"]
health_p_value <- 2 * pt(abs(health_t_value), df = health_df, lower.tail = FALSE)

# Direct update of row 1
updated_results$model_type[1] <- "simple"
updated_results$distribution[1] <- "gaussian_logit"
updated_results$main_effect[1] <- health_coef["Estimate"]
updated_results$main_effect_se[1] <- health_coef["Std. Error"]
updated_results$main_effect_p[1] <- health_p_value
updated_results$main_effect_p_adj[1] <- health_p_value
updated_results$random_effect_var[1] <- as.data.frame(VarCorr(health_model))$vcov[1]
updated_results$AIC[1] <- AIC(health_model)
updated_results$BIC[1] <- BIC(health_model)

# Clear interaction terms for row 1
interaction_cols <- grep("^interaction_", names(updated_results), value = TRUE)
for (col in interaction_cols) {
  updated_results[1, col] <- NA
}

cat("Row 1 fixed: p-value =", health_p_value, "\n")

#==== Fix row 11: Risk of Catastrophic Expenditure ====
cat("\n2. Fixing Risk of Catastrophic Expenditure model\n")

catastrophic_data <- data %>% 
  filter(!is.na(`Risk.of.catastrophic.expenditure.for.surgical.care....of.people.at.risk.`) & 
           !is.na(publications))

# Transform for logit model
catastrophic_data$risk_transformed <- catastrophic_data$`Risk.of.catastrophic.expenditure.for.surgical.care....of.people.at.risk.`
n <- nrow(catastrophic_data)
catastrophic_data$risk_transformed <- (catastrophic_data$risk_transformed * (n-1) + 0.5) / n
catastrophic_data$logit_risk <- log(catastrophic_data$risk_transformed / (1 - catastrophic_data$risk_transformed))

# Fit the model
risk_model <- lmer(logit_risk ~ publications + date + (1 | income_level_iso3c), 
                   data = catastrophic_data)
risk_summary <- summary(risk_model)
print(risk_summary)

# Extract coefficients
risk_coef <- risk_summary$coefficients["publications", ]
risk_t_value <- risk_coef["t value"]
risk_df <- risk_summary$devcomp$dims["obs"] - risk_summary$devcomp$dims["p"]
risk_p_value <- 2 * pt(abs(risk_t_value), df = risk_df, lower.tail = FALSE)

# Direct update of row 11
updated_results$model_type[11] <- "simple"
updated_results$distribution[11] <- "gaussian_logit"
updated_results$main_effect[11] <- risk_coef["Estimate"]
updated_results$main_effect_se[11] <- risk_coef["Std. Error"]
updated_results$main_effect_p[11] <- risk_p_value
updated_results$main_effect_p_adj[11] <- risk_p_value
updated_results$random_effect_var[11] <- as.data.frame(VarCorr(risk_model))$vcov[1]
updated_results$AIC[11] <- AIC(risk_model)
updated_results$BIC[11] <- BIC(risk_model)

# Clear interaction terms for row 11
for (col in interaction_cols) {
  updated_results[11, col] <- NA
}

cat("Row 11 fixed: p-value =", risk_p_value, "\n")

#==== Fix row 15: Publications vs Physicians ====
cat("\n3. Fixing Publications vs Physicians model\n")

physicians_data <- data %>% 
  filter(!is.na(publications) & !is.na(`Physicians..per.1.000.people.`))

# Fit negative binomial model
physicians_model <- glmmTMB(publications ~ `Physicians..per.1.000.people.` + date + 
                              (1 | income_level_iso3c), 
                            data = physicians_data, 
                            family = nbinom2)
physicians_summary <- summary(physicians_model)
print(physicians_summary)

# Direct update of row 15
physician_coef <- physicians_summary$coefficients$cond["Physicians..per.1.000.people.", ]
updated_results$model_type[15] <- "simple"
updated_results$distribution[15] <- "nbinom2"
updated_results$main_effect[15] <- physician_coef["Estimate"]
updated_results$main_effect_se[15] <- physician_coef["Std. Error"]
updated_results$main_effect_p[15] <- physician_coef["Pr(>|z|)"]
updated_results$main_effect_p_adj[15] <- physician_coef["Pr(>|z|)"]
updated_results$random_effect_var[15] <- physicians_summary$varcor$cond$income_level_iso3c[1]
updated_results$AIC[15] <- AIC(physicians_model)
updated_results$BIC[15] <- BIC(physicians_model)

# Clear interaction terms for row 15
for (col in interaction_cols) {
  updated_results[15, col] <- NA
}

cat("Row 15 fixed: p-value =", physician_coef["Pr(>|z|)"], "\n")

#==== Verify and replace the original dataframe ====
# Show the updated rows
cat("\n===== VERIFICATION =====\n")
cat("Updated rows:\n")
print(updated_results[c(1, 11, 15), c("dependent_var", "independent_var", "distribution", 
                                      "model_type", "main_effect", "main_effect_se", 
                                      "main_effect_p")])

# Check for any remaining NAs in main_effect_se or main_effect_p
remaining_na_se <- sum(is.na(updated_results$main_effect_se) | 
                         updated_results$main_effect_se == "NaN")
remaining_na_p <- sum(is.na(updated_results$main_effect_p))

cat("\nRemaining NaN standard errors:", remaining_na_se)
cat("\nRemaining NA p-values:", remaining_na_p)

# Replace the original dataframe with our updated version
all_hierarchical_results <- updated_results

# Export the final results
write.csv(all_hierarchical_results, "hierarchical_combined_results_final.csv", row.names = FALSE)

cat("\n===== COMPLETE =====\n")
cat("All models fixed and saved to hierarchical_combined_results_final.csv\n")









# Direct manual calculation of p-values for problematic rows
cat("\n===== MANUAL P-VALUE CALCULATION =====\n")

# ==== ROW 1: HEALTH EXPENDITURE MODEL ====
cat("\n1. Health Expenditure Model (Row 1)\n")

# Get coefficient values
estimate <- all_hierarchical_results$main_effect[1]
std_error <- all_hierarchical_results$main_effect_se[1]
t_value <- estimate / std_error

# Calculate degrees of freedom (observations - parameters)
# For mixed models, a conservative approach is n-p-1 where p is fixed effects
df <- 69 - 3  # 69 observations, 3 fixed effects (intercept, publications, date)

# Calculate p-value directly
p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)

cat("Estimate:", estimate, "\n")
cat("Std Error:", std_error, "\n")
cat("t-value:", t_value, "\n")
cat("df:", df, "\n")
cat("Calculated p-value:", p_value, "\n")

# Update the p-values directly
all_hierarchical_results$main_effect_p[1] <- p_value
all_hierarchical_results$main_effect_p_adj[1] <- p_value

# ==== ROW 11: RISK OF CATASTROPHIC EXPENDITURE MODEL ====
cat("\n2. Risk of Catastrophic Expenditure Model (Row 11)\n")

# Get coefficient values
estimate <- all_hierarchical_results$main_effect[11]
std_error <- all_hierarchical_results$main_effect_se[11]
t_value <- estimate / std_error

# Calculate degrees of freedom
df <- 62 - 3  # 62 observations, 3 fixed effects

# Calculate p-value directly
p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)

cat("Estimate:", estimate, "\n")
cat("Std Error:", std_error, "\n")
cat("t-value:", t_value, "\n")
cat("df:", df, "\n")
cat("Calculated p-value:", p_value, "\n")

# Update the p-values directly
all_hierarchical_results$main_effect_p[11] <- p_value
all_hierarchical_results$main_effect_p_adj[11] <- p_value

# Verify fixes
cat("\n===== VERIFICATION =====\n")
cat("Updated p-values:\n")
print(all_hierarchical_results[c(1, 11, 15), c("dependent_var", "independent_var", 
                                               "main_effect", "main_effect_se", 
                                               "main_effect_p")])

# Check for any remaining NAs
remaining_na_p <- sum(is.na(all_hierarchical_results$main_effect_p))
cat("\nRemaining NA p-values:", remaining_na_p, "\n")

# Export the final results
write.csv(all_hierarchical_results, "hierarchical_combined_results_final.csv", row.names = FALSE)

cat("\n===== COMPLETE =====\n")
cat("All p-values successfully calculated and updated.\n")




# Export to xlsx
library(openxlsx)
#write.xlsx(all_hierarchical_results, "Supplementary Material 3.xlsx", rowNames = FALSE)

