# 11_Rate_Ratio_Analysis_Rads.R  

# Alternative to comparing age adjusted rates between WA and Kitsap using PHSKC Rads package


# Purpose --------------------------------------------------------------------

# This script enables users to quickly generate rate ratios from Age-Adjusted Rates using only their point estimates and respective CIs. This script makes use of the propagate uncertainty function developed by Public Health, Seattle & King County, Assessment, Policy Development & Evaluation and is part of their R Automatic Data System (RADS) R package.

# RADS Package: https://github.com/PHSKC-APDE/rads

# For ease of use this script isolates the propagate uncertainty function from RADS and allows users to run the function without having to install the RADS package.

# For more detailed information on how this function works and its statistical methodology please see https://github.com/PHSKC-APDE/rads/wiki/propagate_uncertainty


# Statistical Note -------------------------------------------------------------------

# When comparing two rates with a rate ratio you need to combine the uncertainty from the two rates together to create the CI around the ratio. The CI around Age-Adjusted rates can often be asymmetric and right skewed, leading to inaccurate results when combining the uncertainty with traditional methods. The right-skewed nature of the uncertainty can be captured using the log normal approximation in most cases.

# To make the CI for Age-Adjusted Rate Ratios propagate uncertainty does the following: 
# 1. Generates thousands of random draws from the log normal distribution for your rates based on the point estimates and CIs on the log scale.
# 2. Each pair of draws is divided resulting in a distribution of rate ratios on the log scale.
# 3. The CI and P values are created from the empirical quantiles of the distribution of ratios.
# Results are exponentiated back to their original scale.


library(dplyr)
library(readr)
library(openxlsx)
library("remotes")
library(data.table)
library(knitr)
library(usethis)
library(lubridate)

#install.packages("devtools")
library(devtools)
library(rads.data)

#remotes::install_github("PHSKC-APDE/rads", auth_token = NULL) #use this code the first time you install rads
library("rads") 

# Load in completed age standardized rates for SUD Deaths to compare our rates to the rates of the state
# Read in the data from your environment variable path
file_path <- Sys.getenv("DEATHS_SUD_RATES")

# Read all sheets
df_kitsap <- read.xlsx(file_path, sheet = "Death Kitsap")
df_zip <- read.xlsx(file_path, sheet = "Death Zip Code")
df_state <- read.xlsx(file_path, sheet = "Death State")

# View structure to confirm column names
glimpse(df_kitsap)
glimpse(df_state)

# Create comparison dataset by year AND category
mortality_comparison <- data.table()

# Get unique combinations of year and category from Kitsap data
year_category_combos <- df_kitsap %>%
  distinct(dateofdeathyear, Category) %>%  
  arrange(dateofdeathyear, Category)

# Loop through each year-category combination
for(i in 1:nrow(year_category_combos)) {
  yr <- year_category_combos$dateofdeathyear[i]
  cat <- year_category_combos$Category[i]
  
  # Filter data for specific year and category
  kitsap_row <- df_kitsap %>% 
    filter(dateofdeathyear == yr, Category == cat)
  
  state_row <- df_state %>% 
    filter(dateofdeathyear == yr, Category == cat)
  
  # Only proceed if we have data for both geographies in this year-category combo
  if(nrow(kitsap_row) > 0 && nrow(state_row) > 0) {
    comparison_row <- data.table(
      dateofdeathyear = yr,
      Category = cat,
      comparison = paste("Kitsap vs WA -", yr, "-", cat),
      # Reference (State)
      state_rate = state_row$adj.rate[1],
      state_lower = state_row$adj.lci[1],
      state_upper = state_row$adj.uci[1],
      state_count = state_row$count[1],
      # Comparator (Kitsap)
      kitsap_rate = kitsap_row$adj.rate[1],
      kitsap_lower = kitsap_row$adj.lci[1],
      kitsap_upper = kitsap_row$adj.uci[1],
      kitsap_count = kitsap_row$count[1]
    )
    
    mortality_comparison <- rbind(mortality_comparison, comparison_row)
  }
}

# View the prepared comparison data
View(mortality_comparison)

# Running the propagate_uncertainty function for all year-category combinations
rate_ratio_results <- propagate_uncertainty(
  ph.estimates = mortality_comparison, 
  contrast_fn = function(x, y) x / y,  # Rate ratio
  dist = "lognormal",                   # Use with age-adjusted rates
  alpha = .05,                          # 95% CI
  comp_mean_col = "kitsap_rate",        # Kitsap is comparator
  comp_lower_col = "kitsap_lower", 
  comp_upper_col = "kitsap_upper",
  ref_mean_col = "state_rate",          # State is reference  
  ref_lower_col = "state_lower",
  ref_upper_col = "state_upper"
)

## After running propagate_uncertainty, rename the contrast columns
rate_ratio_results <- rate_ratio_results %>%
  rename(
    rr.estimate = contrast,
    rr.se = contrast_se,
    rr.lower = contrast_lower,
    rr.upper = contrast_upper,
    rr.pvalue = contrast_pvalue
  )

# Now view with the renamed columns
View(rate_ratio_results[, c("dateofdeathyear", "Category", "comparison", "rr.estimate", "rr.lower", "rr.upper", "rr.se", "rr.pvalue")])

# Update the summary results table with renamed columns
summary_results <- rate_ratio_results %>%
  select(dateofdeathyear, Category, comparison, 
         state_rate, state_lower, state_upper, state_count,
         kitsap_rate, kitsap_lower, kitsap_upper, kitsap_count,
         rr.estimate, rr.lower, rr.upper, rr.pvalue) %>%
  arrange(dateofdeathyear, Category)

View(summary_results)

# Update the significance summary with renamed columns
significant_differences <- summary_results %>%
  mutate(
    significantly_higher = ifelse(rr.lower > 1, "Yes", "No"),
    significantly_lower = ifelse(rr.upper < 1, "Yes", "No"),
    significance = case_when(
      rr.lower > 1 ~ "Kitsap significantly higher",
      rr.upper < 1 ~ "Kitsap significantly lower",
      TRUE ~ "No significant difference"
    )
  ) %>%
  select(dateofdeathyear, Category, kitsap_rate, state_rate, rr.estimate, 
         rr.lower, rr.upper, rr.pvalue, significance)

View(significant_differences)

# Export results (once we update the renviron script)
#output_folder <- 
#write.xlsx(summary_results, 
           #file = paste0(output_folder, "SUD_DeathRate_Ratio_Results_By_Year.xlsx"))

# Sample  visualization
library(ggplot2)

ggplot(rate_ratio_results, aes(x = dateofdeathyear, y = rr.estimate, color = Category)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = rr.lower, ymax = rr.upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Kitsap vs Washington State SUD Death Rate Ratios by Year",
       subtitle = "Rate Ratio > 1 indicates Kitsap rate is higher than State rate",
       x = "Year",
       y = "Rate Ratio (95% CI)") +
  theme_minimal()