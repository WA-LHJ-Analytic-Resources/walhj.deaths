# ==============================================================================
# scripts/utils/check_environment.R - Environment validation
# ==============================================================================

validate_environment <- function() {
  required_vars <- c(
    "DEATHS_RAW_PATH",
    "DEATHS_PROCESSED_PATH", 
    "DEATHS_FINAL_PATH"
  )
  
  missing_vars <- character()
  
  for (var in required_vars) {
    if (Sys.getenv(var) == "") {
      missing_vars <- c(missing_vars, var)
    }
  }
  
  if (length(missing_vars) > 0) {
    stop(paste(
      "Missing required environment variables in .Renviron file:",
      paste(missing_vars, collapse = ", "),
      "\nPlease check your .Renviron file setup."
    ))
  }
  
  # Validate paths exist
  for (var in required_vars) {
    path <- Sys.getenv(var)
    if (!dir.exists(path)) {
      warning(paste("Path does not exist:", path, "for variable:", var))
    }
  }
  
  message("Environment validation complete - all required paths configured")
}