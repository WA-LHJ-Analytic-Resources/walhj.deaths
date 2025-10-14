# 10_LeadingCausesofDeath_Rads.R  
# Comprehensive causes of death analysis using PHSKC Rads package

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

# Load the most recent final combined database
file_path <- file.path(Sys.getenv("DEATHS_FINAL_PATH"), "2016-2025_Deaths.csv")

if (!file.exists(file_path)) {
  stop("Combined deaths file not found. Please run 00_source.R first to create combined dataset.")
}

deaths_comb <- read_csv(file_path, col_types = cols(.default = "c"))

#Filter to only Kitsap Residence Deaths

kitsap_deaths_comb <- deaths_comb %>%
  filter(residencecounty == "KITSAP")

# Categories of non-infant deaths
death_groups <- rads.data::icd_nchs113causes_raw[, .(cause = leading.cause.group.alias, 
                                                     causeid, 
                                                     cause_category = cause.category)]

print(death_groups)

# Categories of infant deaths
infdeath_groups <- rads.data::icd_nchs130causes_raw[, .(cause = leading.cause.group.alias, 
                                                        causeid, 
                                                        cause_category = cause.category)]
print(infdeath_groups)

## Non-infant deaths (ages >= 1)

#Must have the underlying cod code in this format to it to be called in the following
#functions

kitsap_deaths_comb <- kitsap_deaths_comb %>%
  rename(underlying_cod_code = underlyingcodcode)

#Use the icd_10 code clean function 

kitsap_deaths_comb$underlying_cod_code <- death_icd10_clean(kitsap_deaths_comb$underlying_cod_code)

# Do not specify`cause` or `causeids` arguments in order to get all causes of death
death_count <- death_113_count(
  ph.data = kitsap_deaths_comb, 
  icdcol = 'underlying_cod_code',
  group_by = c('sex', 'agecat'),
  kingco = FALSE 
)

# Ensure all rows have a causeid (will be missing when there was a zero count)
death_count <- merge(death_count[, causeid := NULL], 
                     rads::death_113(), 
                     by = 'cause.of.death', 
                     all.x= TRUE, all.y= FALSE)

# Add on the leading cause of death group identifier ----
death_count <- merge(death_count, 
                     death_groups, 
                     by = 'causeid', 
                     all.x= TRUE, all.y= FALSE)

# Fill in values for 'All causes' and COVID-19
death_count[cause.of.death %in% c('All causes', 'COVID-19 (U07.1)'), 
            cause := cause.of.death]
death_count[cause.of.death == 'All causes', cause_category := cause.of.death]
death_count[cause.of.death == 'COVID-19 (U07.1)', cause_category := 'Infectious disease']

# Delete if missing cause (these are cause.of.death == 'Missing/Unknown')
death_count <- death_count[!is.na(cause)]

# Delete if 'unspecified', 'not elsewhere classified', etc. 
death_count <- death_count[cause != '999']

# Delete if missing chi_sex because cannot merge with population
death_count <- death_count[!is.na(sex)]


# Sum deaths for each unique strata and keep select columns 
# This is necessary because a single `cause` can map to more than one `cause.of.death`
death_count <- death_count[ ,
                            .(deaths = sum(deaths)),
                            .(age_category = agecat,
                              gender = sex, 
                              cause, 
                              cause_category)]

table(death_count$cause_category)
###########################################################

# Calculating Age Standardized Rates using OFM Population Data

###########################################################

# King County Specific Examples (we do not current have functions set up to get our end as this is King County SQL dependent) 
# Get population for the years and demographic strata corresponding to your death counts
pop <- get_population(geo_type = "county",years = 2017:2021, ages = 1:100, group_by = c('genders', 'ages'))
pop <- pop[, .(pop, age, gender)]

# Merge the population data onto the non-infant death count data
combo <- merge(death_count, 
               pop, 
               by = c('age', 'gender'), 
               all.x= TRUE, all.y= TRUE)

# Get infant population from birth data - by sex (We do not current have functions set up to get our end as this is King County SQL dependent)
infpop <- rads::get_data_birth(cols = c('chi_sex'), year = 2017:2022, kingco = T)
infpop <- infpop[, .(pop = .N), .(gender = as.character(chi_sex))]
infpop <- infpop[!is.na(gender)]

# Merge the population (live birth) data onto the infant death count data
infcombo <- merge(infdeath_count, 
                  infpop, 
                  by = c('gender'), 
                  all.x= TRUE, all.y= TRUE)

##########################################################

#Kitsap County Data Integration for Age Standardization for Leading Causes of Death

#FIGURE OUT RAW OFM DATA FORM - CONFRIM FROM OFM IF WE CAN GET PRE MADE CSV PACKAGES FROM THEM

# For time being, insert the OFM population estimates from 2000 - 2024 for all genders by predefined age categories
# Filter for Kitsap Geography only for overall county estimates (it also contains relevant zip codes as well as WA state overall estimates)
pop <- read_csv(Sys.getenv("OFM_POPULATION_ALL"), col_types = cols(.default = "c")) %>%
filter(`Geography Name` == "Kitsap") %>%
  rename(age_category = agecat)

#Redo causes of death to be age category only because of population data limiations
# Do not specify`cause` or `causeids` arguments in order to get all causes of death
death_count <- death_113_count(
  ph.data = kitsap_deaths_comb, 
  icdcol = 'underlying_cod_code',
  group_by = c('agecat','dateofdeathyear'),
  kingco = FALSE 
)

# Ensure all rows have a causeid (will be missing when there was a zero count)
death_count <- merge(death_count[, causeid := NULL], 
                     rads::death_113(), 
                     by = 'cause.of.death', 
                     all.x= TRUE, all.y= FALSE)

# Add on the leading cause of death group identifier ----
death_count <- merge(death_count, 
                     death_groups, 
                     by = 'causeid', 
                     all.x= TRUE, all.y= FALSE)

# Fill in values for 'All causes' and COVID-19
death_count[cause.of.death %in% c('All causes', 'COVID-19 (U07.1)'), 
            cause := cause.of.death]
death_count[cause.of.death == 'All causes', cause_category := cause.of.death]
death_count[cause.of.death == 'COVID-19 (U07.1)', cause_category := 'Infectious disease']

# Delete if missing cause (these are cause.of.death == 'Missing/Unknown')
death_count <- death_count[!is.na(cause)]

# Delete if 'unspecified', 'not elsewhere classified', etc. 
death_count <- death_count[cause != '999']

# Sum deaths for each unique strata and keep select columns 
# This is necessary because a single `cause` can map to more than one `cause.of.death`
death_count <- death_count[ ,
                            .(deaths = sum(deaths)),
                            .(age_category = agecat,
                              Year = dateofdeathyear,
                              cause, 
                              cause_category)]

# Merge the population data onto the non-infant death count data

combo <- merge(death_count,
               pop,
               by = c('age_category','Year'),
               all.x = TRUE, all.y=TRUE)

# Calculate age standardized rates

#rename age-category to agecat, filter out deaths and pop that are NA
 

combo <- combo %>%
  rename(agecat = age_category) %>%
  filter(!is.na(pop)) %>%
  filter(!is.na(deaths))

# Make sure that pop is numeric

combo$pop <- as.numeric(combo$pop)

# Age standardized rates for 
rates <- age_standardize(
  ph.data = combo,
  ref.popname = "2000 U.S. Std Population (11 age groups)",
  collapse = FALSE,
  my.count = 'deaths',
  my.pop = 'pop',
  conf.level = 0.95,
  per = 100000,
  group_by = c('Year','cause','cause_category')
)

# Alter the naming conventions of the columns produced from the age_standardize function

rates <- rates[,.(Year,
                  ranking = NA_integer_,
                  cause_category,
                  cause,
                  count,
                  rate = adj.rate,
                  rate_lower = adj.lci,
                  rate_upper = adj.uci)]

ranking <- rates[, ranking := frank(-count, ties.method = 'dense')-1, .(Year)]

# Keep top 10 rankings
ranking <- ranking[ranking %in% 0:10]

# Sort the data by Year and ranking
setorder(ranking, Year, ranking)

#View the top 10 leading causes of death by year
print(ranking[count >= 10, .SD[1:11], Year])

###########################################################
# Mechanisms of intent injury matrix count
# a character vector of length 1 to 28. It specifies the mechanism of death that you want returned (E.g., "Cut/pierce", "Drowning", "Fall", "Firearm", etc.). "none" will ignore the mechanism and only return the intent of death.

#To see the complete list of mechanisms, type unique(rads.data::icd10_death_injury_matrix$mechanism) in your R console.

#NOTE You do not have to type the entire keyword for the mechanism, a partial string match is sufficient and is case insensitive. E.g., mechanism = c("cycl") would return both "Pedal cyclist" and "Motorcyclist".

#The default is '*', which selects all possible mechanism

mech.n.intent <- death_injury_matrix()[1:6]



# Kitsap Injury Related Deaths (every combination of intent and mechanism), stratified by age category
kitsap_mech_intent <- death_injury_matrix_count(ph.data = kitsap_deaths_comb,
                                                intent = "*",
                                                mechanism = "*",
                                                kingco = FALSE,
                                                group_by = 'agecat')[1:6]

# Kitsap Injury Related Deaths - Total injur related deaths (without regard to intent or mechanism)

kitsap_mech_intent_alt <- death_injury_matrix_count(ph.data = kitsap_deaths_comb,
                                                    intent = "none",
                                                    mechanism = "none",
                                                    kingco = FALSE,
                                                    group_by = "agecat")

# Kitsap county homicides (intent) by firearms (mechanism), stratified by sex

kitsap_mech_firearm <- death_injury_matrix_count(ph.data = kitsap_deaths_comb,
                                                 intent = "homicide",
                                                 mechanism = "firearm",
                                                 kingco = FALSE,
                                                 group_by = 'sex')

print(kitsap_mech_firearm)

# Kitsap county years of potential life lost due to firearm homicides
# Important note about ypll_age: Valid values are between 1 & 99 (inclusive), though 65 and 85 are the most common. 
# For example, ypll_age = 65 would sum the total number of years that could have been lived had everyone in the data lived to at least 65. 
# Note that this function returns the total number of YPLL. Additional processing is necessary to calculate rates per 100,000.

# Need to change the age variable to from age to chi_age to be calculated in the following function

kitsap_deaths_comb <- kitsap_deaths_comb %>%
rename(chi_age = ageyears)

kitsap_deaths_comb$chi_age <- as.numeric(kitsap_deaths_comb$chi_age)

kitsap_mech_firearm_ypll <- death_injury_matrix_count(ph.data = kitsap_deaths_comb,
                                                      intent = "homicide",
                                                      mechanism = "firearm",
                                                      kingco = FALSE,
                                                      death_age_col = "chi_age",
                                                      ypll_age = 65)

kitsap_mech_firearm_ypll

################################################################################

# Multicause Count (more in line with our KPHD traditional method of death categorization)
# Identifies underlying cause code and contributing cause codes
# As of now, this function only identifies opioid deaths, however, we can request
# PHSKC to add more specific causes of interest or we can attempt to alter the function itself
icd_10_causes <- data.table::copy(rads.data::icd10_multicause)

kable(icd_10_causes, format = "markdown")

rads::death_multicause()

kitsap_multicause_deaths <- kitsap_deaths_comb %>%
  # Step 1: Rename to match rads expected format
  rename_with(
    .fn = ~ gsub("recordaxiscode", "record_axis_code_", .x),
    .cols = starts_with("recordaxiscode")
  ) %>%
  # Step 2: Apply death_icd10_clean() to all those columns
  mutate(across(
    .cols = starts_with("record_axis_code_"),
    .fns  = death_icd10_clean
  )) %>%
  mutate(underlying_cod_code = death_icd10_clean(underlying_cod_code))


kitsap_opioid_deaths <- rads::death_multicause_count(ph.data = kitsap_multicause_deaths,
                             cause_name = "Opioid",
                             kingco = FALSE,
                             group_by = "dateofdeathyear")




kitsap_opioid_deaths <- kitsap_opioid_deaths[cause.of.death != 'All causes']

################################################################################

# Additional deaths not included in death_113_count() and death_130_count() functions

#List of other causes of death in this "death_other" function
death_other()
?death_other_count()

# Count of Drug overdose fatalities

kitsap_drug_deaths <- death_other_count(ph.data = kitsap_deaths_comb,
                                        cause = "Drug-overdose",
                                        kingco = FALSE,
                                        group_by = "dateofdeathyear")

kitsap_drug_deaths <- kitsap_drug_deaths[cause.of.death != 'All causes']

kitsap_drug_deaths

# Years lost to drug overdose per year

kitsap_drug_deaths_ypll <- death_other_count(ph.data = kitsap_deaths_comb,
                                             cause = "Drug-overdose",
                                             kingco = FALSE,
                                             death_age_col = "chi_age",
                                             group_by = "dateofdeathyear",
                                             ypll_age = 65)

kitsap_drug_deaths_ypll <- kitsap_drug_deaths_ypll[cause.of.death != 'All causes']

print(kitsap_drug_deaths_ypll)


# Example: Number of Cancer and Heart Disease Deaths per Year in Kitsap

kitsap_cancer_heart_deaths <- death_other_count(ph.data = kitsap_deaths_comb,
                                                cause = "cancer|heart",
                                                kingco = FALSE,
                                                death_age_col = "chi_age",
                                                group_by = "dateofdeathyear",
                                                ypll_age = 65)

kitsap_cancer_heart_deaths <- kitsap_cancer_heart_deaths[cause.of.death != 'All causes']

print(kitsap_cancer_heart_deaths)
###############################################################################

# Calculating life expectancies using the life_table() function
# This requires to update our Renviron script to put the filepath for OFM Population estimates into R


?life_table_prep

# Filter Kitsap death data to key variables of interest and make sure the date variables are in the date format

ex10 <- deaths_comb %>%
  select(dateofbirth, dateofdeath, dateofdeathyear) %>%
  mutate(
    dateofbirth = as.Date(dateofbirth, format = "%m/%d/%Y"),
    dateofdeath = as.Date(dateofdeath, format = "%Y-%m-%d")
  )


summary(ex10)

# Manually add 2000 U.S. Std Population (11 age groups) to the life table prep function as that is what our population data grouped as
dt10 <- life_table_prep(
  ph.data = ex10,
  cuts = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85),
  dobvar = "dateofbirth",
  dodvar = "dateofdeath",
  group_by = "dateofdeathyear"
)


# Relabel the age groups
dt10 <- dt10 %>%
  mutate(ages = case_when(
    ages == "0-1" ~ "0",
    ages == "1-5" ~ "1-4 years",
    ages == "5-15" ~ "5-14 years",
    ages == "15-25" ~ "15-24 years",
    ages == "25-35" ~ "25-34 years",
    ages == "35-45" ~ "35-44 years",
    ages == "45-55" ~ "45-54 years",
    ages == "55-65" ~ "55-64 years",
    ages == "65-75" ~ "65-74 years",
    ages == "75-85" ~ "75-84 years",
    ages == "85+" ~ "85+ years",
    TRUE ~ ages
  )) %>%
  rename(Year = dateofdeathyear)

# For time being, insert the OFM population estimates from 2000 - 2024 for all genders by predefined age categories
# Filter for Kitsap Geography only for overall county estimates (it also contains relevant zip codes as well as WA state overall estimates)
pop <- read_csv(Sys.getenv("OFM_POPULATION_ALL"), col_types = cols(.default = "c")) %>%
  filter(`Geography Name` == "Kitsap") %>%
  rename(ages = agecat)

# Merge the properly formatted death and population data together

dt10 <- merge(dt10,
              pop,
              by = c('Year','ages'),
              all = T)

# Convert ages to life_table format AFTER the merge
dt10 <- dt10 %>%
  mutate(ages = case_when(
    ages == "0" ~ "0-1",
    ages == "1-4 years" ~ "1-5",      # Changed to 1-5
    ages == "5-14 years" ~ "5-15",    # Changed to 5-15
    ages == "15-24 years" ~ "15-25",  # Changed to 15-25
    ages == "25-34 years" ~ "25-35",  # Changed to 25-35
    ages == "35-44 years" ~ "35-45",  # Changed to 35-45
    ages == "45-54 years" ~ "45-55",  # Changed to 45-55
    ages == "55-64 years" ~ "55-65",  # Changed to 55-65
    ages == "65-74 years" ~ "65-75",  # Changed to 65-75
    ages == "75-84 years" ~ "75-85",  # Changed to 75-85
    ages == "85+ years" ~ "85+",
    TRUE ~ ages
  ))


# Population must be a numeric variable before running analysis

dt10$pop <- as.numeric(dt10$pop)

dt10 <- dt10 %>%
  filter(!is.na(ages))

#Remove years of data that are not included (2000 - 2015, and 2025)
dt10 <- dt10 %>%
  filter(!is.na(deaths), !is.na(Year), !is.na(Geography))

# Now run the life table function
dt10.lifetable <- life_table(ph.data = dt10,
                             myages = "ages",
                             group_by = 'Year')

# How to interpret the life table output:

# To see detailed key of what the columns in the lifetable output mean run the following:

?life_table


  
#mx: age interval specific death rate

#qx: probability of dying in the age interval

#lx: # of (theoretical) persons alive at the start of the age interval
  
  #dx: # of deaths during the age interval
  
  #ax: average fraction of the interval lived by those who died in the interval

#Lx: total person years lived in the age interval

#Tx: total person years lived beyond the start of the age interval

#ex: expectation of life (a.k.a., life expectancy) at the start of the age interval. The value of ex for those under one year of age is typically referred to as 'Life Expectancy at Birth'.
