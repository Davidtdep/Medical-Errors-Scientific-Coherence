This repository contains the R scripts used for the analysis of medical errors across World Bank income groups.

## Repository Structure

The repository is organized as follows:

- **src/** - R scripts for analysis
  - indicators.R - Script for retrieving the indicators
  - Main_analysis.R - Core statistical analyses
  - plots.R - Focused analysis on data visualization
  - complement.R - Calculation of interpretable coefficients (i.e., OR, IRR) and their 95% CIs
- **data/** - Supplementary materials and datasets
  - Supplementary Material 1.xlsx - Aggreagated bibliometric and indicator data by income level and year 
  - Supplementary Material 2.xlsx - Regression results
  - Supplementary Material 3.xlsx - Hierarchical mixed-effects models results

  
## Study Overview

This project analyzes 2,639 research articles on medical errors diseases published between 1995-2024, exploring relationships between scientific output and indicators on mortality, health system, and and R&D/innovation domains, across countries with different income levels.

## Data Sources

- Bibliometric data from systematic literature search on Medical Errors
- 22 country-level indicators from public databases
- Countries classified by World Bank income groups (HIC, UMIC, LMIC, LIC)

## Usage

1. Install required R packages
2. Place input data in appropriate locations (paths may need adjustment)
3. Run scripts in order: indicators.R → Main_analysis.R → plots.R

For detailed information about the methodology and results, please refer to the associated publication.

## Data Availability
This project uses openly available data from public databases. The medical error bibliometric dataset used is **available upon reasonable request**.
  
## License
This repository is licensed under the **Creative Commons Attribution 4.0 International (CC BY 4.0) License**, allowing free use, modification, and distribution with appropriate attribution. See the `LICENSE` file for more details.
