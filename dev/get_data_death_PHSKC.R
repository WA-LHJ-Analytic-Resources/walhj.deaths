# get_data_death.R
# Source: https://github.com/PHSKC-APDE/rads/blob/main/R/get_data.R

get_data_death <- function(
  cols = NA,
  year = NA,
  kingco = TRUE,
  version = 'final',
  topcode = TRUE,
  mykey = 'hhsaw',
  include_prelim = FALSE
) {
  # Visible bindings for data.table/check global variables ----
  chi_age <- chi_geo_kc <- chi_year <- NULL

  # Validate arguments other than mykey ----
  if (!(length(cols) == 1 && is.na(cols))) {
    if (!is.character(cols)) {
      stop(
        '\n\U0001f6d1 `cols` must specify a vector of variables or be NA (to get all possible columns).'
      )
    }
  }
  if (!(length(year) == 1 && is.na(year))) {
    if ((!is.numeric(year)) | sum(year %% 1) != 0) {
      stop(
        '\n\U0001f6d1 `year` must specify a vector of integers (e.g., c(2017, 2019)) or be NA (to get the most recent year).'
      )
    }
  }
  if (!is.logical(kingco) || length(kingco) != 1 || is.na(kingco)) {
    stop(
      '\n\U0001f6d1 `kingco` must be a logical (TRUE | FALSE, or equivalently, T | F).'
    )
  }
  if (
    !is.character(version) ||
      length(version) != 1 ||
      !(tolower(version) %in% c('final', 'stage'))
  ) {
    stop('\n\U0001f6d1 `version` must be either "final" or "stage".')
  }
  if (!is.logical(topcode) || length(topcode) != 1 || is.na(topcode)) {
    stop(
      '\n\U0001f6d1 `topcode` must be a logical (TRUE | FALSE, or equivalently, T | F).'
    )
  }
  if (
    !is.logical(include_prelim) ||
      length(include_prelim) != 1 ||
      is.na(include_prelim)
  ) {
    stop(
      '\n\U0001f6d1 `include_prelim` must be a logical (TRUE | FALSE, or equivalently, T | F).'
    )
  }

  # Validate mykey ----
  con <- validate_hhsaw_key(hhsaw_key = mykey)

  # create SQL table name
  mysqltable <- DBI::Id(schema = 'death', table = paste0(version, '_analytic'))

  # Get list of all colnames & years from SQL ----
  death.names <- tolower(names(DBI::dbGetQuery(
    con,
    glue::glue_sql("SELECT TOP (0) * FROM {`mysqltable`}", .con = con)
  )))
  if (isTRUE(include_prelim)) {
    death.years <- sort(unique(
      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT DISTINCT chi_year FROM {`mysqltable`}",
          .con = con
        )
      )$chi_year
    ))
  } else {
    death.years <- sort(unique(
      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT DISTINCT chi_year FROM {`mysqltable`} WHERE apde_file_status = 'F'",
          .con = con
        )
      )$chi_year
    ))
  }

  # Identify columns and years to pull from SQL ----
  cols <- tolower(cols)
  if (!all(is.na(cols))) {
    # convert user defined vector of columns to a SQL statement
    invalid.cols <- setdiff(cols, death.names)
    valid.cols <- intersect(death.names, cols)
    if (length(valid.cols) > 0) {
      cols <- glue::glue_sql_collapse(valid.cols, sep = ", ")
    }
    if (length(invalid.cols) > 0) {
      message(paste0(
        "The following column names do not exist in the death data and have not been extracted: ",
        paste0(invalid.cols, collapse = ", ")
      ))
    }
    if (length(valid.cols) == 0) {
      stop(
        "death data cannot be extracted because no valid column names have been submitted. To get all columns, use the argument 'cols = NA'"
      )
    }
  }

  if (all(is.na(cols))) {
    cols <- glue::glue_sql("*")
  }

  if (length(year) == 1 && is.na(year)) {
    year = max(death.years)
    message(paste0(
      "You did not specify a year so the most recent available year, ",
      max(death.years),
      ", was selected for you. Available years include ",
      format_time(death.years)
    ))
  }

  invalid.year <- setdiff(year, death.years)
  year <- intersect(year, death.years)
  if (length(year) == 0) {
    stop(paste0(
      "Death data cannot be extracted because no valid years have been provided. Valid years include: ",
      format_time(death.years)
    ))
  }
  if (length(invalid.year) > 0) {
    message(paste0(
      "The following years do not exist in the death data and have not been extracted: ",
      format_time(invalid.year)
    ))
  }

  # Pull columns and years from SQL ----
  validyears <- glue::glue_sql_collapse(year, sep = ", ")
  query.string <- glue_sql(
    'select {DBI::SQL(cols)} from {`mysqltable`} where chi_year in ({`validyears`*})',
    .con = con
  )

  if (isTRUE(kingco)) {
    query.string <- paste0(query.string, " AND chi_geo_kc = 'King County'")
  }

  if (isFALSE(include_prelim)) {
    query.string <- paste0(query.string, "AND apde_file_status = 'F'")
  }

  dat <- data.table::as.data.table(DBI::dbGetQuery(con, query.string))

  # Top code age (if wanted) ----
  if ('chi_age' %in% names(dat)) {
    dat[chi_age < 0, chi_age := NA] # cannot have a negative age (due to 9999 as year of birth)
    if (isTRUE(topcode)) {
      dat[chi_age > 100, chi_age := 100] # top code to 100 to match population data
    }
  }

  # Identify and format class == DATE ----
  datevars <- DBI::dbGetQuery(
    con,
    "select column_name FROM death.ref_column_list  WHERE table_name = 'analytic' AND column_type = 'DATE'"
  )[]$column_name
  datevars <- intersect(datevars, names(dat)) # names of all date variables that are in actual dataset

  if (length(datevars) > 0) {
    dat[,
      (datevars) := lapply(.SD, function(x) as.Date(as.character(x))),
      .SDcols = datevars
    ]
  }

  # Format string variables due to SQL import quirks ----
  # Clean string variables to handle SQL import quirks like extra whitespace and encoding issues
  string_clean(dat, stringsAsFactors = FALSE) # clean random white spaces

  # Return object ----
  return(dat)
}
