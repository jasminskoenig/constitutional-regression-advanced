library(sandwich)


### REGRESSION FUNCTIONS ----

linear_regression <- function(x, y, controls, interaction, interaction2,  dataset, returned){
  
  # calculates regressions with control variables gov_brief, threshold, court, covid and polticians
  # 5 models: base, base + covid, base + full controls, base + full controls + threshold, base + controls + interaction
  # input: x = dependent variable, y = indepenedent variable, c = controls, i = interaction, dataset = dataset
  # prints latex table of regression all regressions
  # returns model base + controls + interaction
  
  models <- list()
  
  col_x <- deparse(substitute(x))
  col_y <- deparse(substitute(y))

  # base
  formula_base <- reformulate(x, response = y)
  models$m_pr <- lm(formula_base, data = dataset)
  
  # controls
  formula_controls <- reformulate(c(x, controls), response = y)
  models$m_pr2 <- lm(formula_controls, data = dataset)
  
  
  # controls and interaction variable (without interacted)
  if (!missing(interaction)) {
    controls <- append(controls, interaction)
    formula_controls_interaction <- reformulate(c(x, controls), response = y)
    models$m_pr3 <- lm(formula_controls_interaction, data = dataset)
    
    # interaction
    formula_interaction <- reformulate(c(x, controls, paste(x, interaction, sep = ":")), response = y)
    models$m_pr4 <- lm(formula_interaction, data = dataset)
  }
  
  # controls and interaction variable (without interacted)
  if (!missing(interaction2)) {
    controls <- append(controls, interaction2)
    formula_controls_interaction <- reformulate(c(x, controls), response = y)
    models$m_pr3 <- lm(formula_controls_interaction, data = dataset)
    c <- append(c, interaction2)
    
    # interaction
    formula_interaction <- reformulate(c(x, controls, paste(x, interaction, interaction2, sep = ":")), response = y)
    models$m_pr4 <- lm(formula_interaction, data = dataset)
  }
  
  
  models <- Filter(function(x) !is.null(x), models)
  
  results <- stargazer(models, type = "latex")
  results
  
  return(models)
}

## Manual CSE ----

calculate_cse <- function(model, dataset, cluster){

  # Check if the cluster variable exists in the model's data
  if (!cluster %in% names(dataset)) {
    stop(paste("Cluster variable", cluster, "not found in the model's data"))
  }

  # extract coef
  coefs <- coef(summary(model))
  
  # clustered se
  se <- sqrt(diag(vcovHC(model, 
                                type = "HC1", 
                                cluster = "group", 
                                group = dataset[[cluster]])))
  
  # adapt t-values
  t_values <- coef(model) / se
  
  # adaept p-values manually
  p_values <- 2 * pt(-abs(t_values), 
                     df = df.residual(model))
  
  # Create a function to override the significance stars based on new p-values
  override_sig <- function(p_values) {
    sapply(p_values, function(p) {
      if (p < 0.01) return("***")
      else if (p < 0.05) return("**")
      else if (p < 0.1) return("*")
      else return("")
    })
  }
  
  # adapt stars
  stars <- override_sig(p_values)
  
  df <- as.data.frame(1:length(coefs), coefs, se, t_values, p_values, stars)
  
  return(df)

}



## FE ----

linear_regression_fe <- function(x, 
                                 y, 
                                 controls, 
                                 dataset, 
                                 fe){
  
  # calculates regressions with control variables gov_brief, threshold, court, covid and polticians
  # 5 models: base, base + covid, base + full controls, base + full controls + threshold, base + controls + interaction
  # input: x = dependent variable, y = indepenedent variable, c = controls, i = interaction, dataset = dataset
  # prints latex table of regression all regressions
  # returns model base + controls + interaction
  
  models <- list()
  
  col_x <- deparse(substitute(x))
  col_y <- deparse(substitute(y))
  
  # base
  formula_base <- reformulate(x, response = y)
  models$m_pr0 <- lm(formula_base, 
                     data = dataset)
  
  # fe
  formula_fe <- reformulate(c(x, fe), response = y)
  models$m_pr1 <- lm(formula_base, 
                             data = dataset)
  
  # controls
  formula_controls <- reformulate(c(x, fe, controls), response = y)
  models$m_pr2 <- lm(formula_controls, 
                             data = dataset)
  
  
  models <- Filter(function(x) !is.null(x), models)
  
  return(models)
}


linear_regression_fe_cse <- function(x, 
                                 y, 
                                 controls, 
                                 dataset, 
                                 fe){
  
  # calculates regressions with control variables gov_brief, threshold, court, covid and polticians
  # 5 models: base, base + covid, base + full controls, base + full controls + threshold, base + controls + interaction
  # input: x = dependent variable, y = indepenedent variable, c = controls, i = interaction, dataset = dataset
  # prints latex table of regression all regressions
  # returns model base + controls + interaction
  
  models <- list()
  
  col_x <- deparse(substitute(x))
  col_y <- deparse(substitute(y))
  
  # base
  formula_base <- reformulate(x, response = y)
  models$m_pr0 <- lm(formula_base, 
                            data = dataset)
  
  # fe
  formula_fe <- reformulate(c(x, fe), response = y)
  models$m_pr1 <- lm.cluster(formula_base, 
                            data = dataset,
                            cluster = fe)
  
  # controls
  formula_controls <- reformulate(c(x, fe, controls), response = y)
  models$m_pr2 <- lm.cluster(formula_controls, 
                             data = dataset,
                             cluster = fe)
  
  
  models <- Filter(function(x) !is.null(x), models)
  
  return(models)
}

## CLUSTERED SE ----

linear_regression_cse <- function(x, 
                              y, 
                              controls, 
                              interaction, 
                              interaction2,  
                              dataset){
  
  # calculates regressions with control variables gov_brief, threshold, court, covid and polticians
  # 5 models: base, base + covid, base + full controls, base + full controls + threshold, base + controls + interaction
  # input: x = dependent variable, y = indepenedent variable, c = controls, i = interaction, dataset = dataset
  # prints latex table of regression all regressions
  # returns model base + controls + interaction
  
  models <- list()
  
  col_x <- deparse(substitute(x))
  col_y <- deparse(substitute(y))
  
  # base
  formula_base <- reformulate(x, response = y)
  models$m_pr <- lm.cluster(formula_base, 
                            data = dataset, 
                            cluster = 'session_id')
  
  # controls
  formula_controls <- reformulate(c(x, controls), response = y)
  models$m_pr2 <- lm.cluster(formula_controls, 
                     data = dataset, 
                     cluster = 'session_id')
  
  
  # controls and interaction variable (without interacted)
  if (!missing(interaction)) {
    controls <- append(controls, interaction)
    formula_controls_interaction <- reformulate(c(x, controls), response = y)
    models$m_pr3 <- lm.cluster(formula_controls_interaction, 
                       data = dataset, 
                       cluster = 'session_id')
    
    # interaction
    formula_interaction <- reformulate(c(x, controls, paste(x, interaction, sep = ":")), response = y)
    models$m_pr4 <- lm.cluster(formula_interaction, 
                       data = dataset, 
                       cluster = 'session_id')
  }
  
  # controls and interaction variable (without interacted)
  if (!missing(interaction2)) {
    controls <- append(controls, interaction2)
    formula_controls_interaction <- reformulate(c(x, controls), response = y)
    models$m_pr3 <- lm.cluster(formula_controls_interaction, 
                       data = dataset, 
                       cluster = 'session_id')
    c <- append(c, interaction2)
    
    # interaction
    formula_interaction <- reformulate(c(x, controls, paste(x, interaction, interaction2, sep = ":")), response = y)
    models$m_pr4 <- lm.cluster(formula_interaction, 
                       data = dataset, 
                       cluster = 'session_id')
  }
  
  
  models <- Filter(function(x) !is.null(x), models)
  
  return(models)
}

## probit -----

probit_regression <- function(x, y, controls, interaction, interaction2,  dataset){
  
  # calculates regressions with control variables gov_brief, threshold, court, covid and polticians
  # 5 models: base, base + covid, base + full controls, base + full controls + threshold, base + controls + interaction
  # input: x = dependent variable, y = indepenedent variable, c = controls, i = interaction, dataset = dataset
  # prints latex table of regression all regressions
  # returns model base + controls + interaction
  
  models <- list()
  
  col_x <- deparse(substitute(x))
  col_y <- deparse(substitute(y))
  
  # base
  formula_base <- reformulate(x, response = y)
  models$m_pr <- glm(formula_base, data = dataset,
                    # cluster = 'session_id',
                     family = binomial(link = "probit"))
  
  # controls
  formula_controls <- reformulate(c(x, controls), response = y)
  models$m_pr2 <- glm(formula_controls, 
                              data = dataset, 
                            #  cluster = 'session_id',
                              family = binomial(link = "probit"))
  
  
  # controls and interaction variable (without interacted)
  if (!missing(interaction)) {
    controls <- append(controls, interaction)
    formula_controls_interaction <- reformulate(c(x, controls), response = y)
    models$m_pr3 <- glm(formula_controls_interaction, 
                        data = dataset,  
                       # cluster = 'session_id',
                        family = binomial(link = "probit"))
    
    # interaction
    formula_interaction <- reformulate(c(x, controls, paste(x, interaction, sep = ":")), response = y)
    models$m_pr4 <- glm(formula_interaction, 
                        data = dataset,  
                      #  cluster = 'session_id',
                        family = binomial(link = "probit"))
  }
  
  # controls and interaction variable (without interacted)
  if (!missing(interaction2)) {
    controls <- append(controls, interaction2)
    formula_controls_interaction <- reformulate(c(x, controls), response = y)
    models$m_pr3 <- glm.cluster(formula_controls_interaction, 
                        data = dataset,  
                        cluster = 'session_id',
                        family = binomial(link = "probit"))
    c <- append(c, interaction2)
    
    # interaction
    formula_interaction <- reformulate(c(x, controls, paste(x, interaction, interaction2, sep = ":")), response = y)
    models$m_pr4 <- glm.cluster(formula_interaction, 
                        data = dataset,  
                        cluster = 'session_id',
                        family = binomial(link = "probit"))
  }
  
  
  models <- Filter(function(x) !is.null(x), models)
  
  return(models)
}

## probit FE ----

probit_regression_fe <- function(x, 
                                 y, 
                                 controls, 
                                 dataset, 
                                 fe){
  
  # calculates regressions with control variables gov_brief, threshold, court, covid and polticians
  # 5 models: base, base + covid, base + full controls, base + full controls + threshold, base + controls + interaction
  # input: x = dependent variable, y = indepenedent variable, c = controls, i = interaction, dataset = dataset
  # prints latex table of regression all regressions
  # returns model base + controls + interaction
  
  models <- list()
  
  col_x <- deparse(substitute(x))
  col_y <- deparse(substitute(y))
  
  # base
  formula_base <- reformulate(x, response = y)
  models$m_pr0 <- glm(formula_base, 
                     data = dataset,
                     family = binomial(link = "probit"))
  
  # fe
  formula_fe <- reformulate(c(x, fe), response = y)
  models$m_pr1 <- glm.cluster(formula_base, 
                             data = dataset,
                             family = binomial(link = "probit"),
                             cluster = fe)
  
  # controls
  formula_controls <- reformulate(c(x, fe, controls), response = y)
  models$m_pr2 <- glm.cluster(formula_controls, 
                             data = dataset,
                             family = binomial(link = "probit"),
                             cluster = fe)
  
  
  models <- Filter(function(x) !is.null(x), models)
  
  return(models)
}

### PLOTTING FUNCTION ----

plotter <- function(model){
  
  model |> 
    # calculate marginal effects
    margins(
      variables = "threshold",
      at = list(share_attacks_break = levels)
    ) |> 
    summary() |> 
    # calculate 3 confidence intervals
    mutate(lower = AME - 1.96 * SE, 
           upper = AME + 1.96 * SE,
           lower_90 = AME - 1.64 * SE, 
           upper_90 = AME + 1.64 * SE,
           lower_99 = AME - 3.58 * SE, 
           upper_99 = AME + 3.58 * SE) ->
    meff
  
  modeltext = deparse(substitute(model))
  
  if(str_detect(modeltext, "reg_pr")){
    ylab = "AME of Attacks on..."
    xlab = ""
    title = "Press Release"
  } else if(str_detect(modeltext, "reg_ph")){
    ylab = ""
    xlab = ""
    title = "Public Hearing"
  } else if(str_detect(modeltext, "reg_pa")){
    ylab = ""
    xlab = ""
    title = "Public Announcement"
  } else {
    ylab = "AME of Attacks on Invalidation"
    xlab = ""
    title = ""
  }
  
  
  plot <- meff_corr |> 
    ggplot(aes(x = as.factor(threshold), y = AME)) +
    geom_linerange(aes(ymin = lower, 
                       ymax = upper, 
                       linewidth = ci,
                       alpha = ci), 
                   color = color_dark) +
    geom_point(aes(y = AME), 
               size = 4,
               color = color_dark) +
    geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
    labs(x = xlab,
         y = ylab,
         title = title,
         alpha = "Confidence Interval",
         linewidth = "Confidence Interval")  +
    ylim(-0.3, 0.3) +
    scale_linewidth_manual(values = c(3, 2, 0.5)) +
    scale_alpha_manual(values = c(1, 0.8, 0.6)) +
    scale_x_discrete(breaks = c(0, 1),
                     labels = c("Before\nThreshold", "After\nThreshold"))
  return(plot)
  
}

plotter_cse <- function(model, dataset){
  
  model |> 
    # calculate marginal effects
    margins(
      variables = "share_attacks_break",
      at = list(threshold = c(0,1)),
      data = dataset
    ) |> 
    summary() |> 
    # calculate 3 confidence intervals
    mutate(lower = AME - 1.96 * SE, 
           upper = AME + 1.96 * SE,
           lower_90 = AME - 1.64 * SE, 
           upper_90 = AME + 1.64 * SE,
           lower_99 = AME - 3.58 * SE, 
           upper_99 = AME + 3.58 * SE) ->
    meff
  
  meff |>  
    pivot_longer(cols = c(starts_with("lower"), 
                          starts_with("upper")), 
                 names_to = "ci", 
                 values_to = "value") |> 
    mutate(ci = if_else(!str_detect(ci, "\\d"), 
                        paste0(ci, "_95"), 
                        ci)) |> 
    separate(ci, c("direction", "ci")) |>  
    pivot_wider(names_from = direction, 
                values_from = value) -> 
    meff_corr
  
  modeltext = deparse(substitute(model))
  
  if(str_detect(modeltext, "reg_pr")){
    ylab = "AME of Attacks"
    xlab = ""
    title = "Press Release"
  } else if(str_detect(modeltext, "reg_ph")){
    ylab = ""
    xlab = ""
    title = "Public Hearing"
  } else if(str_detect(modeltext, "reg_pa")){
    ylab = ""
    xlab = ""
    title = "Public Announcement"
  } else {
    ylab = "AME of Attacks"
    xlab = ""
    title = ""
  }
  
  meff |>  
    pivot_longer(cols = c(starts_with("lower"), 
                          starts_with("upper")), 
                 names_to = "ci", 
                 values_to = "value") |> 
    mutate(ci = if_else(!str_detect(ci, "\\d"), 
                        paste0(ci, "_95"), 
                        ci)) |> 
    separate(ci, c("direction", "ci")) |>  
    pivot_wider(names_from = direction, 
                values_from = value) -> 
    meff_corr
  
  
  plot <- meff_corr |> 
    ggplot(aes(x = as.factor(threshold), y = AME)) +
    geom_linerange(aes(ymin = lower, 
                       ymax = upper, 
                       linewidth = ci,
                       alpha = ci), 
                   color = color_dark) +
    geom_point(aes(y = AME), 
               size = 4,
               color = color_dark) +
    geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
    labs(x = xlab,
         y = ylab,
         title = title,
         alpha = "Confidence Interval",
         linewidth = "Confidence Interval")  +
    ylim(-0.3, 0.3) +
    scale_linewidth_manual(values = c(3, 2, 0.5)) +
    scale_alpha_manual(values = c(1, 0.8, 0.6)) +
    scale_x_discrete(breaks = c(0, 1),
                     labels = c("Before\nThreshold", "After\nThreshold"))
  plot
  return(plot)
  
}
