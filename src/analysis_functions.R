source("../../src/graphics.R")

set_basevars <- function(){
  
  indepv <<- c("trust_share_low_linear_imp_mean_3",
             "ruth_populism"
  )
  interact <<- c("trust_share_low_linear_imp_mean_3*ruth_populism")
  contr <<- c("surplus",
            "executive",
            "presidential",
            "gdp_growth_small_mean_3",
            "regime_age",
            "coalition")
  instr <<- c("v2juaccnt_mean_3",
            "v2juaccnt_mean_3*ruth_populism")
  fe <<- "country"
  
}

create_coefnames <- function() {
  # rename coefs
  coef_names <- c(
    "ruth_populism" = "Populist",
    "trust_share_low_linear_imp_mean_3" = "Trustmean3",
    "trust_hat" = "Trûst",
    "executive" = "Executive Power",
    "surplus" = "Surplus Seats",
    "presidential" = "Presidential System",
    "gdp_growth_small_mean_3" = "GDP",
    "v2juncind_mean_3" = "JI",
    "changelast5" = "Former Purges/Packing",
    "changelast3" = "Former Purges/Packing",
    "regime_age" = "Democracy Age",
    "coalition" = "Coalition Gov.",
    "trust_hat × ruth_populism" = "Trûst x Populist",
    "judicial_independence_mean_mean_3" = "Judicial Independence",
    "lagged_trust_share_low_linear_imp_1" = "Trust1",
    "trust_share_low_linear_imp_mean_5" = "Trustmean5",
    "trust_share_low_mean_3" = "Trustmean3",
    "trust_share_low_imp_lastv_mean_3" ="Trustmean3",
    "trust_share_high_linear_imp_mean_3" = "Trusthighmean3"
    
  )
}

select_vars <- function(){
  df |> 
    select(any_of(c(depv, indepv, contr, instr, fe)), year) |> 
    na.omit() ->
    df_final
  
  return(df_final)
}


robust_ols <- function(formulaobject){
  
  robustols <- lm_robust(formulaobject,
                       se_type = "HC1",
                       data = df_final)
  
  return(robustols)
  
}

get_current_trust_levels <- function(var){
  
    levels <- seq(
      min(df_final[[var]], na.rm = TRUE),
      max(df_final[[var]], na.rm = TRUE),
      0.05
    )
  
  return(levels)
}

marg_effects <- function(model,
                         variable = "ruth_populism") {
  
  slopes(model,
    variables = variable,
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

create_plot <- function(amedata, 
                        var, 
                        ytext = "AME of Populism",
                        appendix = TRUE){
  
  theme_set(theme_regression)
  
  if (depv == "jud_replace_cont"){
    title <- "Court Purges and Packing"
  } else if (depv == "v2jupoatck") {
    title <- "Attacks on Judiciary"
  }
  
  if (appendix) {
    yaxis <- c(-2,6)
    ybreaks <- seq(0, 6, by = 2)
  } else {
    yaxis <- c(-2,2)
    ybreaks <- seq(-1, 2, by = 1)
  }
  
  amedata |> 
    ggplot(aes(x = .data[[var]], 
               y = estimate)) +
    geom_hline(yintercept = 0, 
               color = "#C95D63", 
               linetype = "dashed") +
    geom_ribbon(aes(ymin = cilower, 
                    ymax = ciupper, 
                    alpha = ci_size), 
                fill = color_dark) +
    geom_line(aes()) +
    geom_line() +
    labs(x = "Trust in Judiciary",
         y = ytext,
         caption = element_blank(),
         title = title) +
    scale_x_percent(limits = c(0, 1),
                    expand = c(0,0))  +
    scale_y_continuous(limits = yaxis,
                    expand = c(0,0),
                    breaks = ybreaks)  +
    scale_alpha_manual(values = c(0.7, 0.5, 0.2),
                       name = "Confidence Intervall") +
    coord_cartesian(clip = "off") ->
    plot
  
  return(plot)
  
}

clean_coefs <- function(model){
  
  if ("plm" %in% class(model)) {
    coef(summary(model))[, c("Estimate", "Pr(>|z|)")] -> coefs
  } else {
    coef(summary(model))[, c("Estimate", "Pr(>|t|)")] -> coefs
  }
  
  name <- deparse(substitute(model))
  print(deparse(substitute(model)))
  coefs %>% 
    as.data.frame() %>% 
    rownames_to_column("variable") %>% 
    filter(!str_detect(variable, "^country|Intercept")) %>% 
    rename(coef = 2,
           p = 3) %>% 
    mutate(sig = if_else(p > 0.05, "No", "Yes")) %>% 
    mutate(variable = case_when(
      str_detect(variable, "trust") & str_detect(variable, "populism") ~ "Populism x Trust",
      str_detect(variable, "populism") ~ "Populism",
      str_detect(variable, "trust") ~ "Trust",
      str_detect(variable, "surplus") ~ "Surplus Seats",
      str_detect(variable, "executive") ~ "Executive Power",
      str_detect(variable, "presidential") ~ "Presidential System",
      str_detect(variable, "gdp") ~ "GDP Growth",
      str_detect(variable, "regime_age") ~ "Democracy Age",
      str_detect(variable, "judicial_independence") ~ "Judicial Independence",
      str_detect(variable, "coalition") ~ "Coalition Gov.",
      TRUE ~ variable
    )) %>% 
    mutate(modelname = name) ->
    cleaned_coefs
  
  return(cleaned_coefs)
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
