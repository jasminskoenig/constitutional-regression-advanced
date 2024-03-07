robust_ols <- function(formulaobject){
  
  robustols <- lm_robust(formulaobject,
                       clusters = country,
                       se_type = "stata",
                       data = df_final)
  
  return(robustols)
  
}

get_current_trust_levels <- function(){
  levels <- seq(
    min(df_final$lagged_trust_share_linear_imp_1, na.rm = TRUE),
    max(df_final$lagged_trust_share_linear_imp_1, na.rm = TRUE),
    0.05
  )
  
  return(levels)
}

marg_effects <- function(model) {
  
  marginaleffects(model,
    variables = "ruth_populism",
    newdata = pred_data
  ) |>
    mutate(
      cilower_95 = estimate - 1.96 * std.error,
      ciupper_95 = estimate + 1.96 * std.error,
      cilower_90 = estimate - 1.64 * std.error,
      ciupper_90 = estimate + 1.64 * std.error,
      cilower_99 = estimate - 3.58 * std.error,
      ciupper_99 = estimate + 3.58 * std.error
    ) |>
    pivot_longer(
      cols = starts_with("ci"),
      names_to = c("ci_type", "ci_size"),
      names_sep = "_",
      values_to = "value"
    ) |>
    pivot_wider(
      names_from = ci_type,
      values_from = value
    ) ->
  meff
  
  return(meff)
}


newbootstrap_2sls_predictions <- function(data, regressionformula, predictiondata, n_boot = 10) {
  set.seed(123)  # For reproducibility
  
  # Prepare a matrix to store bootstrap predictions
  coefficient <- predictiondata
  low <- predictiondata
  high <- predictiondata
  
  for (i in 1:n_boot) {
    
    # Resample data with replacement
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    
    print(regressionformula)
    # Fit model
    boot_model <- lm(jud_replace ~ fitted + ruth_populism + fitted*ruth_populism + evnt_sum_lag3 + executive + as.character(country),
                     data = boot_data)
    browser()
    pred_data <- expand.grid(fitted = seq(min(boot_data$fitted, na.rm = TRUE), 
                                                     max(boot_data$fitted, na.rm = TRUE), 
                                                     length.out = 100),
                          ruth_populism = unique(df_final$ruth_populism),
                          evnt_sum_lag3 = mean(df_final$evnt_sum_lag3),
                          executive = mean(df_final$executive),
                          country = unique(df_final$country))
    
    pred_data |> filter(country != "Iceland") -> pred_data
    # Use `predictions` from `marginaleffects` to get predicted values for new_data
    preds <- predictions(boot_model, newdata = pred_data)
    
    
    p <- predictions(boot_model, 
                     variables = list(fitted = seq(min(boot_data$fitted, na.rm = TRUE), 
                                                   max(boot_data$fitted, na.rm = TRUE), 
                                                   length.out = 100),
                                      ruth_populism = c(0,1)))
    
    # Store the predicted values in the matrix
    coefficient <- 
      bind_cols(coefficient, preds$estimate)
    low <- 
      bind_cols(low, preds$conf.low)
    high <- 
      bind_cols(high, preds$conf.high)
  }
  
  return_means <- function(predictedvalues){
    
    predictedvalues %>% 
      t() %>%  
      as.data.frame() %>% 
      filter(!row_number() %in% c(1, 2)) %>% 
      mutate(across(everything(), as.numeric)) ->
      temp
    
    means <- colMeans(temp)
    names(means) <- NULL
    predictiondata |> 
      bind_cols(means) ->
      predictiondata
    
    return(predictiondata)
  }
  
  predictiondata <- return_means(coefficient)
  predictiondata <- return_means(low)
  predictiondata <- return_means(high)
  
  predictiondata |> 
    rename("coefficient" = 3,
           "low" = 4,
           "high" = 5) ->
    predictiondata
  
  return(predictiondata)
}
