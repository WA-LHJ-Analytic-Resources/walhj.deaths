# ==============================================================================
# scripts/04_data_cleaning.R - Clean and recode DEATHS data
# ==============================================================================

# Define geographic regions
source("scripts/utils/geographic_definitions.R")

# Filter data for specified year
deaths_yr <- deaths_df %>% 
  filter(dateofdeathyear == dbyr)

message(paste("Processing", nrow(deaths_yr), "deaths for year", dbyr))

# Data cleaning and variable creation
deaths_clean <- deaths_yr %>%
  # Remove leading and trailing spaces
  mutate(
    across(
      c(starts_with("recordaxiscode"), sex, racesummarycode, ageyears, 
        hispanicnchsbridge, underlyingcodcode, residencezipcode), 
      str_trim
    ),
    
    # Generate standardized sex variable
    kphd_sex = ifelse(tolower(sex) %in% c("m", "f"), sex, NA),
    
    # Convert variables to numeric
    recordaxiscode1 = underlyingcodcode,
    racesummarycode = as.numeric(racesummarycode),
    hispanicnchsbridge = as.numeric(hispanicnchsbridge),
    ageyears = as.numeric(ageyears),
    
    # Generate race/ethnicity variable
    # 1=White, 2=Black, 3=AIAN, 4=Asian, 5=NHOPI, 6=Multiple, 7=Hispanic, 9=Unknown
    kphd_raceeth = case_when(
      racesummarycode >= 10 & racesummarycode <= 14 & 
        (hispanicnchsbridge == 0 | hispanicnchsbridge == 9) ~ racesummarycode - 9,
      racesummarycode >= 20 & racesummarycode <= 50 & 
        (hispanicnchsbridge == 0 | hispanicnchsbridge == 9) ~ 6,
      hispanicnchsbridge >= 1 & hispanicnchsbridge <= 5 ~ 7,
      TRUE ~ 9
    ),
    
    # Generate regional variables
    kphd_kcregions = case_when(
      residencezipcode %in% bainbridge ~ 1,
      residencezipcode %in% bremerton ~ 2,
      residencezipcode %in% centralk ~ 3,
      residencezipcode %in% northk ~ 4,
      residencezipcode %in% southk ~ 5,
      residencezipcode == "99999" ~ 9,
      is.na(residencezipcode) ~ 9,
      TRUE ~ 7
    ),
    
    kphd_jcregions = case_when(
      residencezipcode %in% eastj ~ 1,
      residencezipcode %in% southj ~ 2,
      residencezipcode %in% porttownsend ~ 3,
      residencezipcode == "99999" ~ 9,
      is.na(residencezipcode) ~ 9,
      TRUE ~ 7
    ),
    
    kphd_ccregions = case_when(
      residencezipcode %in% centralc ~ 1,
      residencezipcode %in% eastc ~ 2,
      residencezipcode %in% westc ~ 3,
      residencezipcode == "99999" ~ 9,
      is.na(residencezipcode) ~ 9,
      TRUE ~ 7
    ),
    
    # Generate age groups
    kphd_ageg = case_when(
      ageyears >= 0 & ageyears <= 17 ~ 1,
      ageyears >= 18 & ageyears <= 34 ~ 2,
      ageyears >= 35 & ageyears <= 64 ~ 3,
      ageyears >= 65 ~ 4,
      TRUE ~ 9
    ),
    
    # Date processing
    dateofdeath = as.Date(dateofdeath, "%m/%d/%Y"),
    kphd_deathq = lubridate::quarter(dateofdeath, with_year = TRUE),
    
    # Preliminary data flag
    kphd_prelim = ifelse(str_detect(filename, regex("(?i)f"), negate = TRUE), 1, 0)
  )

# Generate ICD groupings using pattern matching
icd_clean <- deaths_clean %>%
  unite(merged, recordaxiscode1:recordaxiscode20, remove = FALSE) %>%
  unite(merged.2, recordaxiscode2:recordaxiscode20, remove = FALSE) %>%
  mutate(
    # Injury classifications
    kphd_injury = as.numeric(str_detect(merged, k_injury)),
    kphd_injury_unint = as.numeric(str_detect(merged, k_injury_unint)),
    kphd_injury_suicide = as.numeric(str_detect(merged, k_injury_suicide)),
    kphd_injury_homicide = as.numeric(str_detect(merged, k_injury_homicide)),
    kphd_injury_undetermined = as.numeric(str_detect(merged, k_injury_undetermined)),
    kphd_injury_legalwar = as.numeric(str_detect(merged, k_injury_legalwar)),
    
    # Drug classifications
    kphd_drug = as.numeric(str_detect(recordaxiscode1, k_drug)),
    kphd_opioid = as.numeric(str_detect(merged.2, k_opioid)),
    kphd_heroin = as.numeric(str_detect(merged.2, k_heroin)),
    kphd_synth_non_methadone = as.numeric(str_detect(merged.2, k_synth_non_methadone)),
    kphd_stimulant = as.numeric(str_detect(merged.2, k_stimulant)),
    kphd_cocaine = as.numeric(str_detect(merged.2, k_cocaine)),
    kphd_prescribedOp = as.numeric(str_detect(merged.2, k_prescribedOp)),
    kphd_otherOpioid = as.numeric(str_detect(merged.2, k_otherOpioid)),
    kphd_stim_non_cocaine = as.numeric(str_detect(merged.2, k_stim_non_cocaine)),
    kphd_sedative = as.numeric(str_detect(merged.2, k_sedative)),
    kphd_psychotropic = as.numeric(str_detect(merged.2, k_psychotropic)),
    kphd_alcohol = as.numeric(str_detect(merged.2, k_alcohol)),
    
    # Mechanism of injury
    kphd_fall = as.numeric(str_detect(merged, k_fall)),
    kphd_firearm = as.numeric(str_detect(merged, k_firearm)),
    kphd_transportation = as.numeric(str_detect(merged, k_transportation)),
    kphd_drowning = as.numeric(str_detect(merged, k_drowning))
  ) %>%
  
  # Other drugs calculation
  mutate(
    kphd_drugCatchCount_im = str_count(merged.2, k_drugCatch),
    kphd_drugKnownCount_im = str_count(merged.2, k_otherDrugs),
    kphd_sub_im = kphd_drugCatchCount_im - kphd_drugKnownCount_im,
    kphd_otherDrugs = as.numeric(kphd_sub_im >= 1)
  ) %>%
  
  # Remove intermediate variables
  select(-kphd_drugCatchCount_im, -kphd_drugKnownCount_im, -kphd_sub_im)

# Save processed data
processed_path <- Sys.getenv("DEATHS_PROCESSED_PATH")
if (processed_path == "") {
  stop("DEATHS_PROCESSED_PATH not found in .Renviron file")
}

output_file <- file.path(processed_path, paste0("Deaths", dbyr, "_Recode.csv"))
write.csv(icd_clean, output_file, row.names = FALSE)

# Create data object with year suffix
dfname <- paste0("d.", dbyr)
assign(dfname, icd_clean, envir = .GlobalEnv)

message(paste("Year", dbyr, "processing complete:", nrow(icd_clean), "records"))
message(paste("Data saved to:", output_file))