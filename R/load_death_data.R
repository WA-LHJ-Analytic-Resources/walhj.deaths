#' Load Death Data
#'
#' @description Load Death Certificate Files downloaded from Secure Access Washington (SAW) WA DOH CHS Data Files.
#'
#' @param folder  (String) The folder path where your organzation stores downloaded death certificate statistical files.
#' @param include_prelim (T|F) Option to load preliminary death certificate statistical files in addition to finalized files.
#' @param file_years (Optional; Numeric) Provide a list of death certificate statistical file years to load.
#' @return A list of data frames (labelled by file year) containing death certificate statistical files
#' @export

load_death_data <- function(folder, include_prelim, file_years = NULL) {
  # Step 0: Identify all Death Statistical Files in folder
  files <- fs::dir_ls(path = folder, recurse = FALSE) %>%
    fs::path_file() %>%
    list(files = .) %>%
    tibble::as_tibble(.) %>%
    # Filter to Death Statistical Files
    filter(str_detect(files, "DeathStat")) %>%
    # Deduplicate Death Statistical Files (if there are both .csv and .xlsx copies) --> preference for .csv files.
    tidyr::separate_wider_delim(
      files,
      delim = ".",
      names = c("file", "file_type")
    ) %>%
    mutate(priority = if_else(file_type == ".csv", 1, 2)) %>%
    slice_min(order_by = priority, with_ties = FALSE, by = file) %>%
    # Extract Year and Final File Version Information
    mutate(
      year = stringr::str_extract(file, "[:digit:]{4}"),
      year = stringr::str_sub(year, start = 1, end = 4),
      year = as.numeric(year),
      final = stringr::str_detect(file, pattern = "Q", negate = TRUE)
    ) %>%
    # Arrange by Year
    arrange(year) %>%
    # Create filename name
    mutate(filename = paste0(file, ".", file_type))

  # Step 1a: (Optional) Filter to final death certificate files
  if (include_prelim == FALSE) {
    files <- files %>%
      filter(final == TRUE)
  }

  # Step 1b: (Optional) Filter to certain years of death certificate data
  if (!is.null(file_years)) {
    files <- files %>%
      filter(year %in% file_years)
  }

  # Step 2: Extract Death File Nmames
  death_files <- files %>% pull(filename)

  # Step 3: Load in Death Data
  death_list <- list()

  for (file in death_files) {
    ## Extract File Year
    file_year <- stringr::str_extract(file, "\\d{4}")

    ## Print Loading Progress Message
    print(paste0(
      "Loading Death Certificate Statistical File for ",
      file_year,
      "..."
    ))

    ## Store Loaded Data
    death_list[[file_year]] <- rio::import(
      file = here::here(folder, file)
    ) %>%
      janitor::clean_names()
  }

  return(death_list)
}
