### JOINPOINT ANALYSIS WITH SIMPLIFIED WORKFLOW ----
### Following best practices from example script
### Adapted for Kitsap SUD mortality dataset

library(ljr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(openxlsx)

### STEP 1: LOAD AND EXPLORE DATA ----

file_path <- Sys.getenv("DEATHS_SUD_RATES")
df_kitsap <- read.xlsx(file_path, sheet = "Death Kitsap")

# Get a sense of your data first
summary(df_kitsap)
head(df_kitsap)
unique(df_kitsap$Category)

# Plot raw data to identify patterns BEFORE modeling
# This helps you develop intuition about where joinpoints might be
ggplot(df_kitsap, aes(x = dateofdeathyear, y = crude.rate, color = Category)) +
  geom_point(size = 2, shape = 16, alpha = 0.6) +
  geom_smooth(alpha = 0) +
  theme_classic() +
  xlab("Year") +
  ylab("Age-Adjusted Mortality Rate (per 100,000)") +
  scale_color_jco()

# Log-transformed version (matches the logistic transformation in ljr models)
ggplot(df_kitsap, aes(x = dateofdeathyear, y = adj.rate, color = Category)) +
  geom_point(size = 2, shape = 16, alpha = 0.6) +
  geom_smooth(alpha = 0) +
  theme_classic() +
  xlab("Year") +
  ylab("Age-Adjusted Mortality Rate (per 100,000)") +
  scale_color_jco() +
  scale_y_log10()

### STEP 2: DATA SETUP ----
# IMPORTANT: ljr functions require COUNTS and POPULATION, not rates!
# The dataset already has these; we just need to reshape properly

kitsap_wide <- df_kitsap %>%
  # Ensure year is numeric
  mutate(year = as.numeric(dateofdeathyear)) %>%
  # Select only relevant columns
  select(year, Category, count, pop) %>%
  # Reshape so each category has its own count and population columns
  pivot_wider(
    names_from = "Category",
    values_from = c("count", "pop"),
    names_glue = "{Category}.{.value}"
  )

head(kitsap_wide)

### STEP 3: DEFINE CAUSES OF INTEREST ----

# Identify your substance-related death categories
causes <- c("Opioids", "Stimulant", "Polysubstance", "Any drugs")

# Check which categories are actually in your data
available_causes <- unique(df_kitsap$Category)
print(available_causes)

# Adjust causes to match your actual data:
causes <- intersect(causes, available_causes)

### STEP 4: HELPER FUNCTIONS ----

# Clean data for a specific cause
clean_data <- function(cause, data) {
  count_col <- paste0(cause, ".count")
  pop_col <- paste0(cause, ".pop")
  
  data %>%
    filter(!is.na(.data[[count_col]]) & !is.na(.data[[pop_col]]) &
             is.finite(.data[[count_col]]) & is.finite(.data[[pop_col]])) %>%
    # Remove zero counts (log transformation can't handle these)
    filter(.data[[count_col]] > 0)
}

# Main joinpoint calculation function
# Fits 0, 1, 2, and 3 joinpoint models
jp.calc <- function(cause, data) {
  
  df <- clean_data(cause, data)
  
  count_col <- paste0(cause, ".count")
  pop_col <- paste0(cause, ".pop")
  
  counts <- df[[count_col]]
  pops <- df[[pop_col]]
  time <- df$year
  
  # Fit models with 0-3 joinpoints
  x0 <- ljr0(counts, pops, time)
  x1 <- ljr1(counts, pops, time, summ = TRUE)
  x2 <- ljrk(2, counts, pops, time)
  x3 <- ljrk(3, counts, pops, time)
  
  # Return as list for easy indexing
  list(
    cause = cause,
    zero_jp = x0,
    one_jp = x1,
    two_jp = x2,
    three_jp = x3
  )
}

### STEP 5: FIT ALL MODELS ----

kitsap_results <- lapply(causes, jp.calc, data = kitsap_wide)
names(kitsap_results) <- causes

# Each model automatically prints output!
# Review the printouts to understand the slope estimates (g0, g1, etc.)
# and joinpoint estimates (tau values)

### STEP 6: CREATE SUMMARY TABLES ----

# Extract joinpoint estimates into data frames for easy reference
make_jp_table <- function(res_list, causes_list) {
  
  # One-joinpoint model summary
  one_jp <- data.frame(
    Cause = causes_list,
    Joinpoint = sapply(causes_list, function(c) res_list[[c]]$one_jp$Joinpoint)
  )
  
  # Two-joinpoint model summary
  two_jp <- data.frame(
    Cause = causes_list,
    JP1 = sapply(causes_list, function(c) res_list[[c]]$two_jp$Joinpoints[1]),
    JP2 = sapply(causes_list, function(c) res_list[[c]]$two_jp$Joinpoints[2])
  )
  
  # Three-joinpoint model summary
  three_jp <- data.frame(
    Cause = causes_list,
    JP1 = sapply(causes_list, function(c) res_list[[c]]$three_jp$Joinpoints[1]),
    JP2 = sapply(causes_list, function(c) res_list[[c]]$three_jp$Joinpoints[2]),
    JP3 = sapply(causes_list, function(c) res_list[[c]]$three_jp$Joinpoints[3])
  )
  
  list(one_jp, two_jp, three_jp)
}

jp_summary <- make_jp_table(kitsap_results, causes)
cat("\n=== ONE-JOINPOINT MODEL ESTIMATES ===\n")
print(jp_summary[[1]])
cat("\n=== TWO-JOINPOINT MODEL ESTIMATES ===\n")
print(jp_summary[[2]])
cat("\n=== THREE-JOINPOINT MODEL ESTIMATES ===\n")
print(jp_summary[[3]])

### STEP 7: VISUALIZATION ----

# Create individual plots with all three model fits overlaid
plot_jp <- function(cause, data_original) {
  
  # Use original data for rates (includes adj.rate)
  df <- data_original %>%
    filter(Category == cause) %>%
    mutate(year = as.numeric(dateofdeathyear)) %>%
    # Remove zero or NA rates
    filter(!is.na(adj.rate) & adj.rate > 0)
  
  # Create base plot with log scale (matching model's logistic transform)
  gg <- ggplot(df, aes(x = year, y = adj.rate)) +
    geom_point(shape = 21, size = 3.5, color = "black", fill = "darkgrey", alpha = 0.7) +
    theme_classic() +
    xlab("Year") +
    ylab("Age-Adjusted Mortality Rate (per 100,000)") +
    scale_y_log10() +
    ggtitle(paste("Joinpoint Analysis:", toupper(cause)))
  
  # Add vertical lines for joinpoint estimates from each model
  # Color-coded: blue = 1JP, cyan = 2JP, orange = 3JP
  model_results <- kitsap_results[[cause]]
  
  if (!is.null(model_results$one_jp$Joinpoint)) {
    gg <- gg + geom_vline(xintercept = model_results$one_jp$Joinpoint, 
                          color = "#5C6BC0", linewidth = 1.2, 
                          linetype = "solid", alpha = 0.8)
  }
  
  if (!is.null(model_results$two_jp$Joinpoints)) {
    gg <- gg + geom_vline(xintercept = model_results$two_jp$Joinpoints, 
                          color = "#26C6DA", linewidth = 1.2, 
                          linetype = "dashed", alpha = 0.8)
  }
  
  if (!is.null(model_results$three_jp$Joinpoints)) {
    gg <- gg + geom_vline(xintercept = model_results$three_jp$Joinpoints, 
                          color = "#FFA726", linewidth = 1.2, 
                          linetype = "dotted", alpha = 0.8)
  }
  
  # Add legend for line types and models
  gg <- gg + 
    annotate("text", x = Inf, y = Inf, 
             label = "1-JP (solid, blue)\n2-JP (dashed, cyan)\n3-JP (dotted, orange)",
             hjust = 1.05, vjust = 1.05, size = 3.5, 
             bbox = list(boxstyle = "round", fill = "white", alpha = 0.8))
  
  return(gg)
}

# Generate plots for all causes
for (cause in causes) {
  print(plot_jp(cause, df_kitsap))
}

### STEP 8: DETAILED MODEL OUTPUT WITH CATEGORY LABELS ----

# Create a comprehensive summary of all model coefficients and joinpoints
print_model_summary <- function(cause, res_list) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("SUBSTANCE CATEGORY:", toupper(cause), "\n")
  cat(strrep("=", 70), "\n")
  
  # 1-Joinpoint Model
  cat("\n--- 1-JOINPOINT MODEL ---\n")
  cat("Model: y ~ Binom(n,p) where p = invlogit(eta)\n")
  cat("eta = b0 + g0*t\n\n")
  fit0 <- res_list[[cause]]$zero_jp
  cat("       Variables        Coef\n")
  cat("b0     Intercept", sprintf("%12.7f", fit0$Coef[1]), "\n")
  cat("g0             t", sprintf("%12.7f", fit0$Coef[2]), "\n")
  
  # 1-Joinpoint Model
  cat("\n--- 1-JOINPOINT MODEL ---\n")
  cat("Model: y ~ Binom(n,p) where p = invlogit(eta)\n")
  cat("eta = b0 + g0*t + g1*max(t-tau1,0)\n\n")
  fit1 <- res_list[[cause]]$one_jp
  cat("       Variables        Coef\n")
  cat("b0     Intercept", sprintf("%12.7f", fit1$Coef[1]), "\n")
  cat("g0             t", sprintf("%12.7f", fit1$Coef[2]), "\n")
  cat("g1 max(t-tau1,0)", sprintf("%12.7f", fit1$Coef[3]), "\n")
  cat("\nJoinpoints:\n")
  cat("tau1 =", fit1$Joinpoint, "\n")
  
  # 2-Joinpoint Model
  cat("\n--- 2-JOINPOINT MODEL ---\n")
  cat("Model: y ~ Binom(n,p) where p = invlogit(eta)\n")
  cat("eta = b0 + g0*t + g1*max(t-tau1,0) + g2*max(t-tau2,0)\n\n")
  fit2 <- res_list[[cause]]$two_jp
  cat("       Variables        Coef\n")
  cat("b0     Intercept", sprintf("%12.7f", fit2$Coef[1]), "\n")
  cat("g0             t", sprintf("%12.7f", fit2$Coef[2]), "\n")
  cat("g1 max(t-tau1,0)", sprintf("%12.7f", fit2$Coef[3]), "\n")
  cat("g2 max(t-tau2,0)", sprintf("%12.7f", fit2$Coef[4]), "\n")
  cat("\nJoinpoints:\n")
  cat("tau1 =", fit2$Joinpoints[1], "\n")
  cat("tau2 =", fit2$Joinpoints[2], "\n")
  
  # 3-Joinpoint Model
  cat("\n--- 3-JOINPOINT MODEL ---\n")
  cat("Model: y ~ Binom(n,p) where p = invlogit(eta)\n")
  cat("eta = b0 + g0*t + g1*max(t-tau1,0) + g2*max(t-tau2,0) + g3*max(t-tau3,0)\n\n")
  fit3 <- res_list[[cause]]$three_jp
  cat("       Variables        Coef\n")
  cat("b0     Intercept", sprintf("%12.7f", fit3$Coef[1]), "\n")
  cat("g0             t", sprintf("%12.7f", fit3$Coef[2]), "\n")
  cat("g1 max(t-tau1,0)", sprintf("%12.7f", fit3$Coef[3]), "\n")
  cat("g2 max(t-tau2,0)", sprintf("%12.7f", fit3$Coef[4]), "\n")
  cat("g3 max(t-tau3,0)", sprintf("%12.7f", fit3$Coef[5]), "\n")
  cat("\nJoinpoints:\n")
  cat("tau1 =", fit3$Joinpoints[1], "\n")
  cat("tau2 =", fit3$Joinpoints[2], "\n")
  cat("tau3 =", fit3$Joinpoints[3], "\n")
}

# Print detailed summaries for all causes with category labels
cat("\n\n", strrep("#", 80), "\n")
cat("# JOINPOINT REGRESSION ANALYSIS: KITSAP SUBSTANCE-RELATED DEATHS\n")
cat(strrep("#", 80), "\n")

for (cause in causes) {
  print_model_summary(cause, kitsap_results)
}

cat("\n", strrep("#", 80), "\n")
cat("# END OF ANALYSIS\n")
cat(strrep("#", 80), "\n")

### HOW TO INTERPRET OUTPUTS ----

# EXAMPLE INTERPRETATION 
# 
# SUBSTANCE CATEGORY: ANY DRUGS (Hypothetical 2-Joinpoint Model)
# 
# Model: y ~ Binom(n,p) where p = invlogit(eta)
# eta = b0 + g0*t + g1*max(t-tau1,0) + g2*max(t-tau2,0)
# 
# Coefficients:
#   b0 (Intercept):      -88.9029
#   g0 (t):               0.0397    <- baseline slope (2000-2020)
#   g1 max(t-tau1,0):     0.2691    <- slope change after tau1
#   g2 max(t-tau2,0):    -0.5289    <- slope change after tau2
# 
# Joinpoints:
#   tau1 = 2020.409
#   tau2 = 2022.797
#
# INTERPRETATION:
# - BASELINE TREND (before 2020.409): g0 = 0.0397
#   On the log scale, this means a 3.97% increase per year
#   (Drug deaths were rising steadily)
#
# - FIRST CHANGE (2020.409): g1 = 0.2691 added to g0
#   New slope = 0.0397 + 0.2691 = 0.3088
#   On the log scale, this means a 30.88% increase per year
#  
# - SECOND CHANGE (2022.797): g2 = -0.5289 added to previous slope
#   New slope = 0.3088 + (-0.5289) = -0.2201
#   On the log scale, this means a 22.01% DECREASE per year
#  
# 1. UNDERSTANDING MODEL OUTPUT (printed automatically):
#    - b0 (Intercept): Starting point of the fitted line (usually not interpreted directly)
#    - g0: Slope BEFORE the first joinpoint (rate of change on log scale)
#    - g1: CHANGE in slope at first joinpoint (added to g0)
#    - g2: CHANGE in slope at second joinpoint (added to g0+g1)
#    - g3: CHANGE in slope at third joinpoint (added to g0+g1+g2)
#    - tau: Year where the change occurs (can be decimal, e.g., 2020.409 = mid-April 2020)
#
#    KEY: Slopes are ADDITIVE! You add them together to get the slope for each segment
#    - Segment 1 (before tau1): slope = g0
#    - Segment 2 (between tau1 and tau2): slope = g0 + g1
#    - Segment 3 (between tau2 and tau3): slope = g0 + g1 + g2
#    - Segment 4 (after tau3): slope = g0 + g1 + g2 + g3
#
#    INTERPRETING SLOPES (on log scale):
#    - Positive slope = exponential increase in mortality
#    - Negative slope = exponential decrease in mortality
#    - Example: slope = 0.27 means ~27% increase per year on original scale
#    - Example: slope = -0.53 means ~53% decrease per year on original scale
#
# 2. COMPARING MODELS:
#    - 0JP: Overall trend (baseline)
#    - 1JP: One major point of change (most significant shift)
#    - 2JP: Two major points of change (identifies a secondary shift)
#    - 3JP: Three major points of change (use cautiously to avoid overfitting)
#
#    The joinpoint estimates MAY DIFFER between models because
#    they're estimated conditionally on other joinpoints.
#    This is NORMAL! For example, the 1JP model finds the MOST significant
#    change, but when you add a 2nd joinpoint, both may shift.
#
# 3. VISUAL INTERPRETATION:
#    - Points cluster around the fitted line if the model fits well
#    - Steep slopes indicate rapid change in mortality
#    - Gentle slopes indicate slow change or stable mortality rates
#    - Joinpoints appear where direction of trend changes
#    - In log scale, straight lines = exponential change in original scale
#
# 4. CHOOSING YOUR MODEL:
#    - Prefer simpler models (fewer joinpoints) unless data clearly warrants more
#    - Use domain knowledge: does the joinpoint coincide with known events?
#      (e.g., policy changes, drug availability changes, interventions)
#    - Look at visual fit: does adding more joinpoints actually improve fit?
#    - Avoid overfitting (too many joinpoints can fit noise, not signal)
#
# 5. PRACTICAL NOTES FOR YOUR DATA:
#    - The ljr package uses LOG-LINEAR modeling (exponential change)
#    - This is why we use log-scale plots to visualize
#    - Rates that increase exponentially show as straight lines on log scale
#    - Zero counts are filtered out automatically
#    - Small sample sizes can lead to unstable estimates
#    - Consider whether the joinpoint aligns with real-world events
#      (e.g., opioid crisis escalation, fentanyl introduction, policy changes)
#
# 6. REPORTING RESULTS:
#    - Report joinpoint year and slopes before/after
#    - Include visual plots with overlaid model fits
#    - Consider including uncertainty (confidence intervals if using ljrf)
#    - Contextual information: what happened at the joinpoint?