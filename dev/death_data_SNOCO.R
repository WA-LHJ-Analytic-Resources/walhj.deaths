# 1_death_data.R
tictoc::tic("1_death_data.R")

# NOTE: If the R code errors out and says the file does not exist, open up the Assessment folder (to begin the connection with the Shared Drive and then rerun the code).

# Set Up -----

## Define Variables of Interest
death_data_vars <- c(
  "State File Number",
  "Date of Death",
  "Underlying COD Code",
  "Age Years",
  "Sex",
  "Race Summary Code",
  "Hispanic NCHS Bridge",
  "Residence State FIPS Code",
  "Residence County",
  "Residence City",
  "Death State",
  "Death County",
  "Death City"
)


# Load Death Data -----

death_dat_list <- list()

for (year in params$death_start_year:params$death_end_year) {
  year_string <- as.character(year)

  death_dat_list[[year_string]] <- read_csv(
    paste0(params$death_filepath, "/DeathStatF", year_string, ".csv"),
    col_select = all_of(death_data_vars),
    show_col_types = FALSE
  ) %>%
    janitor::clean_names()
}

## Bind File Years Together
death_dat <- bind_rows(death_dat_list, .id = "file_year")


# Clean Death Data -----

death_dat_clean <- death_dat %>%
  ### Clean DOB Information ###
  mutate(date_of_death = lubridate::mdy(date_of_death)) %>%

  # Extract Date Components
  mutate(
    Week = lubridate::week(date_of_death),
    Epi_Week = lubridate::epiweek(date_of_death),
    Month = lubridate::month(date_of_death),
    Month_Abbrev = lubridate::month(date_of_death, label = TRUE, abbr = TRUE),
    Quarter = lubridate::quarter(date_of_death),
    Semester = lubridate::semester(date_of_death),
    Year = lubridate::year(date_of_death)
  ) %>%

  ### Create Age Groups
  mutate(
    Age = as.numeric(age_years),
    Age = ifelse(Age < 0 | Age > 120, NA, Age)
  ) %>%
  create_custom_age_groups(df = .) %>%
  mutate(
    Age_Group = as.character(Age_Group)
  ) %>%

  ### Clean Demographic Information ###
  ## (ME) Race-Ethnicity
  mutate(
    Ethnicity = case_when(
      hispanic_nchs_bridge == 0 ~ "Non-Hispanic or Latino",
      hispanic_nchs_bridge %in% c(1:5) ~ "Hispanic or Latino",
      TRUE ~ "Unknown"
    ),
    Race = case_when(
      race_summary_code == 10 ~ "White",
      race_summary_code == 11 ~ "Black or African American",
      race_summary_code == 12 ~ "American Indian or Alaska Native",
      race_summary_code == 13 ~ "Asian",
      race_summary_code == 14 ~ "Native Hawaiian or Pacific Islander",
      race_summary_code == 99 ~ "Unknown",
      TRUE ~ "Multi-Race"
    ),
    Race_Ethnicity = case_when(
      # 1st - Classify by Hispanic/Latino Identity
      Ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
      # 2nd - If Non Hispanic or Unknown ethnicity --> race Category
      Race == "American Indian or Alaska Native" &
        Ethnicity != "Hispanic or Latino" ~
        "AIAN-NH",
      Race == "Asian" & Ethnicity != "Hispanic or Latino" ~ "Asian-NH",
      Race == "Black or African American" & Ethnicity != "Hispanic or Latino" ~
        "Black-NH",
      Race == "Multi-Race" & Ethnicity != "Hispanic or Latino" ~
        "Multi-Race-NH",
      Race == "Native Hawaiian or Pacific Islander" &
        Ethnicity != "Hispanic or Latino" ~
        "NHPI-NH",
      Race == "White" & Ethnicity != "Hispanic or Latino" ~ "White-NH",
      TRUE ~ "Unknown"
    ),
    Sex = case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      TRUE ~ "Unknown"
    )
  ) %>%
  ## (AOIC) Race-Ethnicity
  mutate(
    AIAN_01 = ifelse(
      race_summary_code %in%
        c(12, 21, 24, 27, 28, 30, 33, 34, 36, 37, 39, 40, 41, 43, 44, 50),
      1,
      0
    ),
    ASIAN_01 = ifelse(
      race_summary_code %in%
        c(13, 22, 25, 27, 29, 31, 33, 35, 36, 38, 39, 40, 42, 43, 44, 50),
      1,
      0
    ),
    BLACK_01 = ifelse(
      race_summary_code %in%
        c(11, 20, 24, 25, 26, 30, 31, 32, 36, 37, 38, 40, 41, 42, 44, 50),
      1,
      0
    ),
    NHPI_01 = ifelse(
      race_summary_code %in%
        c(14, 23, 26, 28, 29, 32, 34, 35, 37, 38, 39, 41, 42, 43, 44, 50),
      1,
      0
    ),
    WHITE_01 = ifelse(
      race_summary_code %in%
        c(10, 20, 21, 22, 23, 30, 31, 32, 33, 34, 35, 40, 41, 42, 43, 50),
      1,
      0
    ),
    OTHER_01 = 0, # No Other Race_Eth is Death Data
    HISP_01 = ifelse(Ethnicity == "Hispanic or Latino", 1, 0)
  ) %>%
  # Add Report Topic Indicators
  create_death_indicators(df = .) %>%
  # Reorder & Subset Variables
  select(
    file_year,
    state_file_number,
    date_of_death,
    underlying_cod_code,
    Age,
    Age_Group,
    Sex,
    Race,
    Ethnicity,
    Race_Ethnicity,
    residence_state = residence_state_fips_code,
    residence_county,
    residence_city,
    death_state,
    death_county,
    death_city,
    Week,
    Epi_Week,
    Month,
    Month_Abbrev,
    Quarter,
    Semester,
    Year,
    ends_with("_01")
  )

# Subset Death Data: Snohomish County Residents -----

death_all <- death_dat_clean %>%
  filter(residence_state == "WA" & residence_county == "SNOHOMISH")


# Subset Death Data: Report Topic -----

death_condition <- death_all %>%
  filter_death_indicators(df = ., topic = params$report_topic)

# (Optional) Subset Death Data: Age-Group Filtering -----

## Filter age groups in death_condition dataframe
death_condition <- filter_age_groups(death_condition)

## Filter age groups in death_all dataframe
death_all <- filter_age_groups(death_all)

# Save Data Extracts -----

death_condition %>%
  save_multiple_formats(
    .,
    xlsx = TRUE,
    rds = TRUE,
    filepath = paste0(
      "Data/",
      params$report_topic,
      "/Linelist/Death_Condition_Clean"
    )
  )

death_all %>%
  save_multiple_formats(
    .,
    xlsx = TRUE,
    rds = TRUE,
    filepath = paste0(
      "Data/",
      params$report_topic,
      "/Linelist/Death_All_Clean"
    )
  )

# Clean Up -----
rm(
  death_dat_list,
  death_dat,
  death_dat_clean,
  year,
  year_string,
  death_data_vars
)

tictoc::toc()
