# ==============================================================================
# scripts/02_data_import.R - Import raw deaths data data
# ==============================================================================

# Get paths from environment variables
raw_data_path <- Sys.getenv("DEATHS_RAW_PATH")

if (raw_data_path == "") {
  stop("DEATHS_RAW_PATH not found in .Renviron file")
}

message(paste("Loading data from:", raw_data_path))

# Find all CSV files in the raw data directory
files <- list.files(
  path = raw_data_path,
  pattern = '\\.csv$',
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No CSV files found in the raw data directory")
}

message(paste("Found", length(files), "data files"))

# Import all files
deaths_data <- lapply(files, function(file) {
  message(paste("Reading:", basename(file)))
  read_csv(file, col_types = cols(.default = "c"))
})

# Standardize column names to lowercase
deaths_data <- lapply(deaths_data, function(x) {
  setNames(x, tolower(names(x)))
})

# Combine all data files
deaths_df <- map_df(deaths_data, ~as.data.frame(.x), .id = "filename")

# Clean column names
names(deaths_df) <- str_replace_all(names(deaths_df), '[^[:alnum:]]', "")
names(deaths_df) <- str_trim(names(deaths_df))

# Handle duplicate column names
duplicate_cols <- which(duplicated(names(deaths_df)))
if (length(duplicate_cols) > 0) {
  names(deaths_df)[duplicate_cols] <- paste0(names(deaths_df)[duplicate_cols], "_2")
  warning("Duplicate column names found and renamed")
}

message(paste("Data import complete:", nrow(deaths_df), "records loaded"))