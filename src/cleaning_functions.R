# CALCULATE IMPUTATIONS FOR MISSING YEARS

library(tidyverse)

calc_latinoshares <- function(var){
  
  latinobarometer |> 
    # Spanish overseas territory
    filter(country != "Spain") |> 
    group_by(country, year) |> 
    mutate(n = n(), 
           mean = mean({{var}}, na.rm = TRUE)) |> 
    group_by(country, year, {{var}}, mean) |> 
    reframe(share = n()/n) |> 
    distinct() ->
    latinobarometer_shares
  
  return(latinobarometer_shares)
}

get_linear_imputation <- function(countryname, variable){
  #' Returns ccpc_vdem dataframe with added columns
  #' for linear imputation of chosen variable, number of missing observations
  #' and the change that has occured in the variable between last
  #' and next existing observation
  #' 
  #' Filters dataframe to country based on given countryname
  #' Imputes data for given variable

  suppressWarnings(warning("no non-missing arguments to max; returning -Inf"))
  
  # Filter to chosen country
  ccpc_vdem |> 
    filter(country == countryname) ->
    temp_df
  
  # Check region of country
  region <- temp_df |> 
    distinct(e_regiongeo) |> 
    pull(e_regiongeo)
  
  # Depending on region, the timeframe of the data is set
  if (region %in% c(1:4)){
    temp_df |> 
      filter(year > 1999) ->
      temp_df_time
    
    temp_df |> 
      filter(year < 2000) ->
      unused
  } else if (region %in% c(17:18)){
    temp_df |> 
      filter(year > 1994) ->
      temp_df_time
    
    temp_df |> 
      filter(year < 1995) ->
      unused
  } 
  
  # If Latinamerica or Europe & there are some observations for given variable
  # -> data is imputed
  # -> otherwise variables are returned NA
  if (region %in% c(1:4, 17:18) & any(!is.na(temp_df[[variable]]))){
    
    # to identify observations, row_id is added
    temp_df_time |> 
      mutate(
        row_id = row_number()
      ) ->
      temp_df_time
    
    # check which rows are not NA for the given variable
    temp_df_time |> 
      filter(!is.na(!!sym(variable))) |> 
      pull(row_id) ->
      rows_with_values
    
    # for each observation that is NA for the given variable
    temp_df_time %>%
      # choose the next smallest number from the observations that are not NA
      mutate(next_smallest = if_else(is.na(!!sym(variable)),
                                     map_dbl(row_id, ~max(rows_with_values[rows_with_values < .x], na.rm = TRUE)),
                                     NA),
             # and the next biggest
             next_biggest = if_else(is.na(!!sym(variable)),
                                    map_dbl(row_id, ~min(rows_with_values[rows_with_values > .x], na.rm = TRUE)),
                                    NA)) ->
      temp_df2
    
    # calculate how far the observation is from the last and next value
    temp_df2 |> 
      mutate(difference_last = row_id - next_smallest,
             difference_next = next_biggest - row_id) ->
      temp_df3
    
    get_value_by_row <- function(row_num, data, var_name) {
      #' returns the value of chosen variable, in chosen data for the chosen row
      if (is.na(row_num)) {
        return(NA)
      } else {
        return(data[[var_name]][row_num])
      }
    }
    
    out <- temp_df3 %>%
      # now using the helper function pick the last and next value
      mutate(
        lastvalue = if_else(!is.na(difference_last),
                            map_dbl(row_id, ~get_value_by_row(max(rows_with_values[rows_with_values < .], na.rm = TRUE), temp_df_time, variable)),
                            NA),
        nextvalue = if_else(!is.na(difference_next),
                            map_dbl(row_id, ~get_value_by_row(min(rows_with_values[rows_with_values > .], na.rm = TRUE), temp_df_time, variable)),
                            NA),
        # get the difference between the last and next observation
        differencerows = abs(next_biggest - next_smallest),
        # get how much the variable has changed in that time
        differencevalues = abs(lastvalue - nextvalue),
        # calculate how large one step is depending on the how much
        # the variable has changed and how many years are missing
        step = if_else(
          is.na(!!sym(variable)),
          differencevalues / differencerows,
          NA
        ),
        # add one step per observation
        # this is done multiplying the step with the 
        # difference to the last observation
        imputedvalue = case_when(
          !is.na(!!sym(variable)) ~ !!sym(variable),
          nextvalue > lastvalue ~ lastvalue + step*difference_last,
          nextvalue < lastvalue ~ lastvalue - step*difference_last
        )
      ) |> 
      select(-lastvalue, 
             -nextvalue, 
             -step,
             -difference_last,
             -difference_next,
             -next_smallest,
             -next_biggest,
             -row_id) |> 
      rename(!!paste0(variable, '_NAyears') := differencerows, 
             !!paste0(variable, '_NAchange') := differencevalues, 
             !!paste0(variable, '_linear_imp') := imputedvalue)
    
    
    
    out <- bind_rows(out, unused) |> 
      arrange(year)
    
  } else {
    
    temp_df |> 
      arrange(year) -> 
      out
    
  }
  
  return(out)
}


# CALCULATE INFORMATION FOR LAGGED YEARS EITHER AS SUM OR MEAN 

calculate_lagsummary <- function(var, intervall, func, by_government){
  #' calculates the chosen function for all lag of the chosen intervall
  #' function can be sum or mean
  #' can be done only by government (otherwise by country)
  
  if (by_government){
    ccpc_vdem |> 
      arrange(country, year) |> 
      group_by(country, interval_sen) |> 
      select(country, year, interval_sen, any_of(var)) ->
      ccpc_vdem_temp
  } else {
    ccpc_vdem |> 
      arrange(country, year) |> 
      group_by(country) |> 
      select(country, year, any_of(var)) ->
      ccpc_vdem_temp
  }
  
  for (i in 1:intervall) {
    lag_var_name <- paste0("lagged_", var, "_", i)
    
    ccpc_vdem_temp |> 
      mutate(!!lag_var_name := dplyr::lag(!!sym(var), i))  ->
      ccpc_vdem_temp
    
  }
  
  if (func == "sum"){
    
    ccpc_vdem_temp %>%
      rowwise() %>%
      mutate(sum_var_name := sum(across(starts_with("lagged")))) |> 
      ungroup() ->
      temp
    
    temp |> 
      select(country, year, last_col()) |> 
      rename_with(~ paste0(var, "_sum_", intervall), last_col()) ->
      out
    
    return(out)
  } else {
    varname <- paste0(var, "_mean_", intervall)
    
    ccpc_vdem_temp %>%
      rowwise() %>%
      mutate(!!varname := sum(across(starts_with("lag")))/intervall) %>%
      ungroup() |> 
      select(-any_of(var)) ->
      out
    
    return(out)
  }
  
}

