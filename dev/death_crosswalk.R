#death_crosswalk.R

# Packages -----
pacman::p_load(here, tidyverse)

# Load Data -----
folder = "C:/Users/SHDTJB/Downloads/Death_Data"

dat_2012 <- read_csv(here::here(folder, 'DeathStatF2012.csv')) %>%
  janitor::clean_names()

dat_2022 <- read_csv(here::here(folder, 'DeathStatF2022.csv')) %>%
  janitor::clean_names()

fips_crosswalk <- tidycensus::fips_codes %>%
  distinct(state, state_code) %>%
  select(fips = state_code, state)


sort(names(dat_2012))
sort(names(dat_2022))

# Convert Pre-2016 to 2016 Data Format -----

dat_2012_new <- dat_2012 %>%
  rename(
    ## Dates & Times
    date_of_birth = dob,
    date_of_death = dth_date,
    date_of_death_modifier = dod_mod,
    time_of_death = dth_time, # In 2016+ --> time_of_death Hour, Minutes
    ## Geographies & Facilities
    birthplace_state_fips_code = state_bir,
    birthplace_country = b_country,
    death_county_city_wa_code = cnty_city_occ,
    death_county_wa_code = cnty_occ,
    death_state = st_occ,
    death_zipcode = zip_occ,
    place_of_death_type = fac_type,
    death_facility = facility,
    ## Demographics
    armed_forces = arm_force,
    marital_status = married,
    education = educ,
    education_8_or_less = edu_le8,
    occupation = occ_lit,
    # occupationmilham = occ_sam # Couldn't find this...
    industry = ind_lit,
    informant_relationship = inform_rl,
    ## Race
    race_white = race_wht,
    race_black = race_blk,
    race_amer_indian_alaskan = race_ami,
    race_asian_indian = race_asi,
    race_chinese = race_chi,
    race_japanese = race_jap,
    race_korean = race_kor,
    race_vietnamese = race_vie,
    race_other_asian = race_oas,
    race_hawaiian = race_haw,
    race_guamanian_or_chamorro = race_gua,
    race_samoan = race_sam,
    race_other_pacific_islander = race_opi,
    race_other = race_oth,
    race_summary = sum_race, # No equivalent in 2022
    bridge_race = brg_race, # No equivalent in 2022
    ## Ethnicity
    hispanic_no = hisp_no,
    hispanic_mexican = hisp_mex,
    hispanic_puerto_rican = hisp_pr,
    hispanic_cuban = hisp_cub,
    hispanic_other = hisp_oth,
    hispanic_nchs_bridge = hisp,
    ## Residence Variables
    residence_city_wa_code = cnty_city_res,
    residence_county_wa_code = cnty_res,
    residence_state_fips_code = st_res,
    residence_zip_code = zipcode,
    residence_tribal_reservation_cod = res_trbc, # No equivalent in 2022
    disposition = disp_type,
    disposition_date = disp_date, # Break into disposition_date_year/month/day
    certifier_designation = att_class,
    # me_coroner_referred = referred  # Requires additional variable adjustment
    # Injury Codes
    acme_nature_of_injury_flag_1 = injflg1,
    acme_nature_of_injury_flag_2 = injflg2,
    acme_nature_of_injury_flag_3 = injflg3,
    acme_nature_of_injury_flag_4 = injflg4,
    acme_nature_of_injury_flag_5 = injflg5,
    acme_nature_of_injury_flag_6 = injflg6,
    acme_nature_of_injury_flag_7 = injflg7,
    acme_nature_of_injury_flag_8 = injflg8,
    acme_nature_of_injury_flag_9 = injflg9,
    acme_nature_of_injury_flag_10 = injflg10,
    acme_nature_of_injury_flag_11 = injflg11,
    acme_nature_of_injury_flag_12 = injflg12,
    acme_nature_of_injury_flag_13 = injflg13,
    acme_nature_of_injury_flag_14 = injflg14,
    acme_nature_of_injury_flag_15 = injflg15,
    acme_nature_of_injury_flag_16 = injflg16,
    acme_nature_of_injury_flag_17 = injflg17,
    acme_nature_of_injury_flag_18 = injflg18,
    acme_nature_of_injury_flag_19 = injflg19,
    acme_nature_of_injury_flag_20 = injflg20,
    underlying_cod_code = underly,
    record_axis_code_1 = mltcse1,
    record_axis_code_2 = mltcse2,
    record_axis_code_3 = mltcse3,
    record_axis_code_4 = mltcse4,
    record_axis_code_5 = mltcse5,
    record_axis_code_6 = mltcse6,
    record_axis_code_7 = mltcse7,
    record_axis_code_8 = mltcse8,
    record_axis_code_9 = mltcse9,
    record_axis_code_10 = mltcse10,
    record_axis_code_11 = mltcse11,
    record_axis_code_12 = mltcse12,
    record_axis_code_13 = mltcse13,
    record_axis_code_14 = mltcse14,
    record_axis_code_15 = mltcse15,
    record_axis_code_16 = mltcse16,
    record_axis_code_17 = mltcse17,
    record_axis_code_18 = mltcse18,
    record_axis_code_19 = mltcse19,
    record_axis_code_20 = mltcse20,
    autopsy_available = autop_ava,
    pregnancy = preg_stat,
    tobacco = tb_contri,
    manner = r_inj_caus,
    date_of_injury = inj_date,
    time_of_injury = inj_time,
    local_file_number = cnty_file,
    injury_place = inj_place,
    injury_city_wa_code = cnty_city_inj,
    injury_county_wa_code = cnty_inj,
    injury_state = st_inj,
    injury_zip_code = zip_inj_oc,
    injury_at_work = inj_at_wrk,
    date_received = rcv_ft,
    residence_length = res_auni,
    residence_length_type = res_lena,
    injury_transportation = trans_inj,
    injury_acme_place = inj_p_nchs,
    age_years = age
  ) %>%
  # Properly Format Date Variables
  mutate(across(
    .cols = c(
      "date_of_death",
      "date_of_birth",
      "date_of_injury",
      "disposition_date",
      "date_received"
    ),
    .fns = ~ .x |>
      as.character() |>
      {
        \(x) ifelse(str_length(x) > 8, NA, x)
      }() |>
      as.Date(format = "%Y%m%d")
  )) %>%
  # Properly Format Time Variables
  mutate(across(
    .cols = c(time_of_death, time_of_injury),
    .fns = list(
      hour = ~ str_sub(.x, 1, 2),
      minutes = ~ str_sub(.x, 3, 4)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  select(-time_of_death, -time_of_injury) %>%
  # Record Axis Codes
  mutate(across(
    .cols = starts_with("record_axis_code"),
    .fns = ~ as.character(.x)
  )) %>%
  # Tobacco Codes
  mutate(
    tobacco = case_when(
      tobacco == 1 ~ "Y",
      tobacco == 2 ~ "N",
      tobacco == 7 ~ "P",
      tobacco == 9 ~ "U",
      TRUE ~ "C" # DOUBLE CHECK THIS ... THIS "C" CODE WAS LEFT OUT OF THE CONVERSION... BUT WAS PRESENT IN 2022 DATA.
    )
  ) %>%
  # Armed Forces
  mutate(
    armed_forces = case_when(
      armed_forces == 1 ~ "Y",
      armed_forces == 2 ~ "N",
      armed_forces == 9 ~ "U"
    )
  ) %>%
  # Death Facility
  mutate(
    death_facility = ifelse(death_facility == "999", NA, death_facility)
  ) %>%
  # Marital Status
  mutate(
    marital_status = case_when(
      marital_status == 1 ~ "S",
      marital_status == 2 ~ "M",
      marital_status == 3 ~ "D",
      marital_status == 4 ~ "W",
      marital_status == 5 ~ "A",
      marital_status == 6 ~ "P",
      marital_status == 9 ~ "U"
    )
  ) %>%
  # Autopsy
  mutate(
    autopsy = case_when(
      autopsy == 1 ~ "Y",
      autopsy == 2 ~ "N",
      autopsy == 9 ~ "U",
      TRUE ~ as.character(autopsy) # Unsure what to convert 0 to...
    )
  ) %>%
  # State Variables
  select(contains("state"))


# TEST ------

# Create a named vector for mapping
state_map <- c(
  "AL" = "01",
  "AK" = "02",
  "AZ" = "03",
  "AR" = "04",
  "CA" = "05",
  "CO" = "06",
  "CT" = "07",
  "DE" = "08",
  "DISTRICT OF COLUMBIA" = "09",
  "FL" = "10",
  "GA" = "11",
  "HI" = "12",
  "ID" = "13",
  "IDAHO" = "13",
  "IL" = "14",
  "ILLINOIS" = "14",
  "IN" = "15",
  "INDIANA" = "15",
  "IA" = "16",
  "IOWA" = "16",
  "KS" = "17",
  "KANSAS" = "17",
  "KY" = "18",
  "KENTUCKY" = "18",
  "LA" = "19",
  "LOUISIANA" = "19",
  "ME" = "20",
  "MAINE" = "20",
  "MD" = "21",
  "MARYLAND" = "21",
  "MA" = "22",
  "MASSACHUSETTS" = "22",
  "MI" = "23",
  "MICHIGAN" = "23",
  "MN" = "24",
  "MINNESOTA" = "24",
  "MS" = "25",
  "MISSISSIPPI" = "25",
  "MO" = "26",
  "MISSOURI" = "26",
  "MT" = "27",
  "MONTANA" = "27",
  "NE" = "28",
  "NEBRASKA" = "28",
  "NV" = "29",
  "NEVADA" = "29",
  "NH" = "30",
  "NEW HAMPSHIRE" = "30",
  "NJ" = "31",
  "NEW JERSEY" = "31",
  "NM" = "32",
  "NEW MEXICO" = "32",
  "NY" = "33",
  "NEW YORK" = "33",
  "NC" = "34",
  "NORTH CAROLINA" = "34",
  "ND" = "35",
  "NORTH DAKOTA" = "35",
  "OH" = "36",
  "OHIO" = "36",
  "OK" = "37",
  "OKLAHOMA" = "37",
  "OR" = "38",
  "OREGON" = "38",
  "PA" = "39",
  "PENNSYLVANIA" = "39",
  "RI" = "40",
  "RHODE ISLAND" = "40",
  "SC" = "41",
  "SOUTH CAROLINA" = "41",
  "SD" = "42",
  "SOUTH DAKOTA" = "42",
  "TN" = "43",
  "TENNESSEE" = "43",
  "TX" = "44",
  "TEXAS" = "44",
  "UT" = "45",
  "UTAH" = "45",
  "VT" = "46",
  "VERMONT" = "46",
  "VA" = "47",
  "VIRGINIA" = "47",
  "WA" = "48",
  "WASHINGTON" = "48",
  "WV" = "49",
  "WEST VIRGINIA" = "49",
  "WI" = "50",
  "WISCONSIN" = "50",
  "WY" = "51",
  "WYOMING" = "51",
  "PUERTO RICO" = "52",
  "VIRGIN ISLANDS" = "53",
  "GUAM" = "54",
  "CANADA" = "55",
  "CUBA" = "56",
  "MEXICO" = "57",
  "ZZ" = "59",
  "AMERICAN SAMOA" = "60",
  "MARIANA ISLANDS" = "69",
  "NOT CLASSIFIABLE" = "99"
)

# Columns to recode
cols_to_recode <- c(
  "birthplace_state_fips_code",
  "residence_state_fips_code",
  "death_state",
  "injury_state"
)


# Apply the mapping
TEST <- dat_2012_new %>%
  mutate(across(all_of(cols_to_recode), ~ state_map[.], .names = "{.col}"))


table(dat_2012_new$residence_state_fips_code)
table(dat_2022$death_state)

sort(names(dat_2012_new))
sort(names(dat_2022))
