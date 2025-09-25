# ==============================================================================
# scripts/05_combine_data.R - Combine all processed years
# ==============================================================================

# Get processed data path
processed_path <- Sys.getenv("DEATHS_PROCESSED_PATH")
if (processed_path == "") {
  stop("DEATHS_PROCESSED_PATH not found in .Renviron file")
}

# Find all processed CSV files
files_recode <- list.files(
  path = processed_path,
  pattern = '\\.csv$',
  full.names = TRUE
)

if (length(files_recode) == 0) {
  stop("No processed CSV files found")
}

message(paste("Found", length(files_recode), "processed files to combine"))

# Read all processed files
deaths_recode <- lapply(files_recode, function(file) {
  message(paste("Reading:", basename(file)))
  read_csv(file, col_types = cols(.default = "c"))
})

# Combine all processed data
deaths_comb <- map_df(deaths_recode, ~as.data.frame(.x), .id = "filename")


# Convert ageyears and all variables starting with kphd_ to numeric
deaths_comb <- deaths_comb %>%
  mutate(
    across(
      c("ageyears", starts_with("kphd_")), 
      as.numeric
    )
  )


# Add age categories for age adjustment
deaths_comb <- deaths_comb %>%
  mutate(
    agecat = case_when(
      ageyears < 1 ~ "0",
      ageyears >= 1 & ageyears < 5 ~ "1-4 years",
      ageyears >= 5 & ageyears < 15 ~ "5-14 years",
      ageyears >= 15 & ageyears < 25 ~ "15-24 years",
      ageyears >= 25 & ageyears < 35 ~ "25-34 years",
      ageyears >= 35 & ageyears < 45 ~ "35-44 years",
      ageyears >= 45 & ageyears < 55 ~ "45-54 years",
      ageyears >= 55 & ageyears < 65 ~ "55-64 years",
      ageyears >= 65 & ageyears < 75 ~ "65-74 years",
      ageyears >= 75 & ageyears < 85 ~ "75-84 years",
      ageyears >= 85 ~ "85+ years"
    )
  )

# Get year range for filename
min_year <- min(deaths_comb$dateofdeathyear, na.rm = TRUE)
max_year <- max(deaths_comb$dateofdeathyear, na.rm = TRUE)

# Save combined dataset
final_path <- Sys.getenv("DEATHS_FINAL_PATH")
if (final_path == "") {
  stop("DEATHS_FINAL_PATH not found in .Renviron file")
}

combined_dir <- file.path(final_path)
if (!dir.exists(combined_dir)) {
  dir.create(combined_dir, recursive = TRUE)
}

output_file <- file.path(combined_dir, paste0(min_year, "-", max_year, "_Deaths.csv"))
fwrite(deaths_comb, output_file)

message(paste("Combined dataset created:", nrow(deaths_comb), "total records"))
message(paste("Year range:", min_year, "to", max_year))
message(paste("File saved to:", output_file))

# Display age category distribution
message("Age category distribution:")
print(table(deaths_comb$agecat, useNA = "ifany"))