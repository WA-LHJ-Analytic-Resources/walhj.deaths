# create_death_indicators.R

create_death_indicators <- function(df) {
  df_death <- df %>%
    # Add Report Topic Indicators
    mutate(
      falls_all_01 = ifelse(
        str_detect(
          underlying_cod_code,
          pattern = "W(0[0-9]|1[0-9])|X80|Y01|Y30"
        ),
        1,
        0
      ),
      firearm_injury_all_01 = ifelse(
        str_detect(
          underlying_cod_code,
          pattern = "W3[2-4]|X7[2-4]|X9[3-5]|Y2[2-4]|Y350" # Y35.0 --> Y350 No decimals in this field
        ),
        1,
        0
      ),
      suicide_all_01 = ifelse(
        str_detect(
          underlying_cod_code,
          pattern = "U03|X(6[0-9]|7[0-9]|8[0-4])|Y870" # Y87.0 --> Y870 No decimals in this field
        ),
        1,
        0
      )
    )

  return(df_death)
}
