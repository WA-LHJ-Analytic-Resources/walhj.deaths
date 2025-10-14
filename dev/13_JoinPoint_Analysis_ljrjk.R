### JOINPOINT ANALYSIS WITH MONTE CARLO SIMULATIONS
### Following best practices from example GitHub script
### Adapted for Kitsap SUD mortality dataset

library(ljr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(openxlsx)

?ljrjk

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

file_path <- Sys.getenv("DEATHS_SUD_RATES")
df_kitsap <- read.xlsx(file_path, sheet = "Death Kitsap")

df_kitsap$dateofdeathyear <- as.integer(df_kitsap$dateofdeathyear)

# ============================================================================
# STEP 2: OPIOIDS ANALYSIS
# ============================================================================

df_kitsap_opioids <- df_kitsap %>% 
  filter(Category == "Opioids")

model_opioids <- ljrjk(0,1,
  y = df_kitsap_opioids$count, 
  n = df_kitsap_opioids$pop, 
  tm = df_kitsap_opioids$dateofdeathyear, 
  R = 1000, 
  alpha = 0.025
)

# Extract coefficients
opioids_coef <- model_opioids$Coef
opioids_slope <- opioids_coef[2]
opioids_apc <- (exp(opioids_slope) - 1) * 100

cat("\n=== OPIOIDS ===\n")
cat("Slope coefficient:", opioids_slope, "\n")
cat("APC:", opioids_apc, "%\n")

# ============================================================================
# STEP 3: ANY DRUGS ANALYSIS
# ============================================================================

df_kitsap_anydrugs <- df_kitsap %>%
  filter(Category == "Any drugs")

model_anydrugs <- ljrjk(0,1,
  y = df_kitsap_anydrugs$count, 
  n = df_kitsap_anydrugs$pop, 
  tm = df_kitsap_anydrugs$dateofdeathyear, 
  R = 1000, 
  alpha = 0.025
)

# Extract coefficients
anydrugs_coef <- model_anydrugs$Coef
anydrugs_slope <- anydrugs_coef[2]
anydrugs_apc <- (exp(anydrugs_slope) - 1) * 100

cat("\n=== ANY DRUGS ===\n")
cat("Slope coefficient:", anydrugs_slope, "\n")
cat("APC:", anydrugs_apc, "%\n")

# ============================================================================
# STEP 4: STIMULANTS ANALYSIS
# ============================================================================

df_kitsap_stimulants <- df_kitsap %>%
  filter(Category == "Stimulant")

model_stimulants <- ljrjk(0,1,
  y = df_kitsap_stimulants$count, 
  n = df_kitsap_stimulants$pop, 
  tm = df_kitsap_stimulants$dateofdeathyear, 
  R = 1000, 
  alpha = 0.025
)

# Extract coefficients
stimulants_coef <- model_stimulants$Coef
stimulants_slope <- stimulants_coef[2]
stimulants_apc <- (exp(stimulants_slope) - 1) * 100

cat("\n=== STIMULANTS ===\n")
cat("Slope coefficient:", stimulants_slope, "\n")
cat("APC:", stimulants_apc, "%\n")

# ============================================================================
# STEP 5: POLYSUBSTANCE ANALYSIS
# ============================================================================

df_kitsap_polysubstance <- df_kitsap %>%
  filter(Category == "Polysubstance")

model_polysubstance <- ljrjk(0,1,
  y = df_kitsap_polysubstance$count, 
  n = df_kitsap_polysubstance$pop, 
  tm = df_kitsap_polysubstance$dateofdeathyear, 
  R = 1000, 
  alpha = 0.025
)

# Extract coefficients
polysubstance_coef <- model_polysubstance$Coef
polysubstance_slope <- polysubstance_coef[2]
polysubstance_apc <- (exp(polysubstance_slope) - 1) * 100

cat("\n=== POLYSUBSTANCE ===\n")
cat("Slope coefficient:", polysubstance_slope, "\n")
cat("APC:", polysubstance_apc, "%\n")

# ============================================================================
# STEP 6: SUMMARY TABLE
# ============================================================================

summary_results <- data.frame(
  Category = c("Opioids", "Any drugs", "Stimulants", "Polysubstance"),
  Slope = c(opioids_slope, anydrugs_slope, stimulants_slope, polysubstance_slope),
  APC = c(opioids_apc, anydrugs_apc, stimulants_apc, polysubstance_apc)
)

cat("\n\n=== SUMMARY ===\n")
print(summary_results)

# ============================================================================
# STEP 7: VISUALIZATIONS (WORK IN PROGRESS DUE TO SOME MODELS HAVING 1 OR MORE JOINPOINTS)
# ============================================================================

ggplot(summary_results, aes(x = Category, y = APC, fill = Category)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Annual Percentage Change (APC) by Drug Category",
    x = "Drug Category",
    y = "APC (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("joinpoint_apc_comparison.png", width = 8, height = 6, dpi = 300)


# ============================================================================
# STEP 8: JOINPOINT OUTPUT SPECIFIC VISUALIZATIONS (WORK IN PROGRESS )
# ============================================================================

# Load in Model Estimates after running JoinPoint