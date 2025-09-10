# summarize_ed_data.R

summarize_death_data <- function(df, 
                                 column, 
                         start_date, end_date, 
                         factor_levels = NULL,
                         output_name = "n") {
  
  column_sym <- sym(column)
  
  # Load Data Frame (Filtered by Date Range)
  df_final <- df %>%
    filter(date_of_death >= start_date & date_of_death <= end_date) %>% 
    count(!!column_sym) %>%
    collect() %>%
    # Convert NA to "Unknown"
    mutate(!!column_sym := tidyr::replace_na(!!column_sym, "Unknown")) %>%
    # Define factor levels with "Unknown" included
    mutate(!!column_sym := factor(!!column_sym, levels = factor_levels)) %>%
    # Ensure 0 Count Categories are Included in Summaries
    complete(!!column_sym, fill = list(n = 0)) %>%
    # Rename Output Metric
    rename({{ output_name }} := n) %>%
    # Arrange Data by Column of Interest
    arrange(!!column_sym)
      
  return(df_final)
}