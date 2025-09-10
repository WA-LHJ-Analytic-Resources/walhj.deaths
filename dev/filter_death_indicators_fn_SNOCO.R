# filter_death_indicators.R

filter_death_indicators <- function(df, topic = params$report_topic) {
  if (topic == "Firearm Injury") {
    df_condition <- df %>%
      filter(firearm_injury_all_01 == 1)
  } else if (topic == "Falls") {
    df_condition <- df %>%
      filter(falls_all_01 == 1)
  } else if (topic == "Suicide") {
    df_condition <- df %>%
      filter(suicide_all_01 == 1)
  } else {
    stop("Error: Not a valid report topic!")
  }

  return(df_condition)
}
