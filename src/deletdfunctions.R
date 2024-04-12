library(ggrepel)
library(glueformula)

# ANALYSIS LIKELIHOOD CHANGE ----

reg_evnt_models <- function(dv, iv, moderator){
  
  # Runs base, mixed-effect and fixed-effect models to predict constitutional change
  # Returns list of regressions
  # Single regressions can be accessed like this: models$`Mixed Interaction` afterwards
  
  models <- list(
    "Base" = lm(lead(dv, 1) ~ iv, data = df4),
    "Mixed" = lmer(lead(dv, 1) ~ iv + (1 | country), data = df4),
    "Mixed Controls" = lmer(lead(dv, 1) ~ iv + moderator  + v2xnp_pres + (1 | country), data = df4),    
    "Mixed Interaction" = lmer(lead(dv, 1) ~ iv * moderator  + moderator   + (1 | country), data = df4)  
    #"Mixed Interaction Share" = lmer(lead(dv, 1) ~ iv * moderator + iv*lag_trust_share + moderator + lag_trust_share + v2xnp_pres + (1 | country), data = df4),    
    #"Mixed Dynamic" = lmer(lead(dv, 1) ~ iv*mean_of_trustlags3 + moderator  + mean_of_trustlags3 + v2xnp_pres + (1 | country), data = df4)
  )
  
  return(models)
  
}

reg_evnt_dummy_models <- function(iv, iv2, iv3){
  
  # Runs base, mixed-effect and fixed-effect models to predict constitutional change
  # Returns list of regressions
  # Single regressions can be accessed like this: models$`Mixed Interaction` afterwards
  
  models <- list(
    "Base" = lm(lead(evnt, 1) ~ iv + iv2 + iv3, data = df4)
  )
  
  return(models)
  
}

reg_evnt_jackknife <- function(ivname, moderatorname) {
  
  jack.reg <- df4$country |> 
    unique()
  
  jackedreg <- data.frame(country = character(), 
                          coefjack = numeric(), 
                          stringsAsFactors = FALSE)
  
  for (i in jack.reg) {
    
    print(i)
    df4 %>%
      filter(country != i) |> 
      rename(iv = ivname,
             moderator = moderatorname) ->
      df_temp
    
    # THIS IS WHERE DA BUG IS
    
    models <- list(
      "Base" = lm(lead(evnt, 1) ~ iv, data = df_temp),
      "Mixed" = lmer(lead(evnt, 1) ~ iv + (1 | country), data = df_temp),
      "Mixed Controls" = lmer(lead(evnt, 1) ~ iv + moderator + v2xnp_pres + lag_respect_con  + coalition + (1 | country), data = df_temp),
      "Mixed Interaction" = lmer(lead(evnt, 1) ~ iv * moderator + moderator + lag_respect_con + v2xnp_pres  + coalition + (1 | country), data = df_temp),
      "Fixed"     = glm(lead(evnt,1) ~ iv + country, data = df_temp),
      "Fixed Controls" = glm(lead(evnt,1) ~ iv + moderator + coalition + country, data = df_temp),
      "Fixed Interaction" = glm(lead(evnt,1) ~ iv + moderator + coalition + v2xnp_pres  + iv*moderator + country, data = df_temp)
    )
    
    newobs <- data.frame(country = i, coefjack = coef(models$`Fixed Interaction`)[2])
    
    jackedreg <- rbind(jackedreg, newobs)
    
  }
  
  return(jackedreg)
}

reg_evnt_table <- function(modellist){
  
  # Creates a table to compare all Models estimated in reg_evnt_models
  
  rows <- tribble(~term, ~Base, ~Mixed,  ~`Mixed Controls`, ~`Mixed Interaction`,  #~`Mixed Interaction Share`,  ~`Mixed Dynamic`, 
                  'MLM', 'No', 'Random Country Intercept', 'Random Country Intercept', 'Random Country Intercept') # 'Random Country Intercept', 'Random Country Intercept')
  
  attr(rows, 'position') <- 9
  
  table <- modelsummary(
    modellist,
    fmt = 1,
    estimate  = "{estimate}{stars}",
    statistic = 'conf.int',
    coef_omit = "(Intercept|^country)",
    add_rows = rows,
    coef_rename = c("iv" = "Populism Score", 
                    "moderator" = "High Court Trust (Lag, 1)",
                    "v2xnp_pres" = "Presidentialism Score",
                    "lagged_v2x_libdem" = "Lagged Democray (2)",
                    "coalition" = "Coalition",
                    "surplus" = "Surplus Seats",
                    "ivxmoderator" = "Populism x High Court Trust",
                    "no_govparties" = "No Gov Parties",
                    "lag_respect_con" = "Respect Constitution (Lag, 1)",
                    "mean_of_demlags" = "Mean of Lag Liberal Democracy - 5",
                    "mean_of_trustlegs" = "Mean of Lag Share Trust in High Court - ",
                    "trust_share" = "Share Trust in High Court",
                    "lag_trust_mean" = "High Court Trust (Lag, 1)"))
  
  return(table)
  
}

reg_mod_levels <- function(regression_model, interaction1) {
  
  # calculate levels of moderating variable
  # for these levels the predicitions of iv will be calculated
  
  if (all(interaction1 %in% c(0, 1) | is.na(interaction1))) {
    levels <- c(0, 1)
  } else if (!all(interaction1 %in% c(0, 1) | is.na(interaction1))) {
    levels <- seq(
      min(interaction1, na.rm = TRUE),
      max(interaction1, na.rm = TRUE),
      0.05
    )
  }  
  return(levels)
}

reg_tidy_effects <- function(regression_model, levels, ivname, moderatorname, label0, label1) {  
  
  # calculate marginal effects and confidence intervals
  # returns a df with AME, SE and predictions at 90, 95 and 99 CI
  
  # we need to specify which variable we're working with at the moment (ivname & moderatorname)
  
  # to make the plots prettier, we need to rename binary variables
  # label0 for moderator == 0, label1 for moderator == 1
  
  df4 |> 
    mutate(iv = ivname,
           moderator = moderatorname) ->
    df4
  
  model_fe %>%
    # calculate marginal effects
    margins(
      variables = "iv",
      at = list(moderator = levels)
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
  
  if (all(df4$moderator %in% c(0, 1) | is.na(df4$moderator))){
    meff |> 
      mutate(moderator = if_else(moderator == 0, label0, label1)) ->
      meff
  }
  
  return(meff)
}

reg_plot <- function(effects){
  
  if ("populismscore" %in% names(effects) & !all(effects$populismscore %in% c("Non-Populist", "Populist"))){
    
    # Three-way Interaction plot for continuous populism score
    
    effects |> 
      ggplot(aes(x = populismscore, y = AME)) +
      geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
      geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, fill = "darkslategrey") +
      geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, fill = "darkslategrey") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkslategrey") +
      geom_line(aes(y = AME)) +
      geom_line() +
      facet_grid(~ moderator) +
      labs(x = "Government Populism Score",
           y = "",
           caption = "Average Marginal Effect of Constitutional Change Conditioned By Ideology & Government Populism Score.") +
      scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        limits = c(0, 1)
      ) +
      ylim(-0.15, 0.15) 
  } else if ("populismscore" %in% names(effects) & all(effects$populismscore %in% c("Non-Populist", "Populist"))) {
    
    # Three way Interaction plot for binary populism score
    
    effects |> 
      ggplot(aes(x = populismscore, y = AME)) +
      geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
      geom_linerange(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, color = "darkslategrey") +
      geom_linerange(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, color = "darkslategrey") +
      geom_linerange(aes(ymin = lower, ymax = upper), alpha = 0.3, color = "darkslategrey") +
      geom_point(aes(y = AME)) +
      facet_grid(~ moderator) +
      labs(x = "Government Populism Score",
           y = "",
           caption = "Average Marginal Effect of Constitutional Change Conditioned By Ideology & Government Populism Score.") +
      ylim(-0.1, 0.1) 
    
  } else if (length(effects$moderator) == 2) {
    
    # Interaction plot for average marginal effect of populism score based on one binary moderator
    
    effects |> 
      ggplot(aes(y = moderator)) +
      geom_vline(xintercept = 0, color = "#C95D63", linetype = "dotted") +
      #xlim(-0.5, 0.5) +
      geom_linerange(aes(x = AME, xmin = lower, xmax = upper), alpha = 0.6, linewidth = 0.75 , color = "darkslategrey") +
      geom_linerange(aes(x = AME, xmin = lower_99, xmax = upper_99), alpha = 0.4, linewidth = 0.5, color = "darkslategrey") +
      geom_pointrange(aes(x = AME, xmin = lower_90, xmax = upper_90), alpha = 0.8, linewidth = 1, size= 0.3,  color = "darkslategrey") +
      labs(y = "",
           x = "Average Marginal Effect of Government Populism Score") 
    
  } else {
    
    effects |> 
      ggplot(aes(x = moderator, y = AME)) +
      geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
      geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, fill = "darkslategrey") +
      geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, fill = "darkslategrey") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkslategrey") +
      geom_line(aes(y = AME)) +
      geom_line() +
      labs(x = "High Court Trust (Lag, 1)",
           y = "Average Marginal Effect of Populism Score") +
      #caption = "Average Marginal Effect of Populism Score") +
      scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        limits = c(0, 1)
      )  
  }
  
}

# Analysis Impact on Democracy  ----

## Functions

reg_evnt_table_dem <- function(modellist){
  
  # Creates a table to compare all Models estimated in reg_evnt_models
  
  
  table <- modelsummary(
    modellist,
    fmt = 1,
    estimate  = "{estimate}{stars}",
    statistic = 'conf.int',
    coef_omit = "(Intercept|^country)",
    coef_rename = c("populismscore" = "Populism Score", 
                    "moderator" = "Left-Wing",
                    "evnt" = "Constitutional Change",
                    "v2xnp_pres" = "Presidentialism Score",
                    "lag_democracyscore" = "Lagged Democray (2)",
                    "coalition" = "Coalition",
                    "surplus" = "Surplus Seats",
                    "ivxmoderator" = "Populism x Left-Wing",
                    "no_govparties" = "No Gov Parties",
                    "lag_respect_con" = "Respect Constitution (Lag, 1)"))
  
  return(table)
  
}

reg_dem_jackknife <- function(democracyscorename, leadyears, populismscorename, moderatorname){
  
  jack.reg <- df4$country |> 
    unique()
  
  jackedreg <- data.frame(country = character(),
                          coefjack = numeric(),
                          stringsAsFactors = FALSE)
  
  for (i in jack.reg) {
    
    print(i)
    
    df4 %>%
      filter(country != i) |>
      rename(populismscore = populismscorename,
             democracyscore = democracyscorename,
             moderator = moderatorname) ->
      df_temp
    
    
    models = list(
      # mixed model predictor only populism
      "Populism" = lmer(lead(democracyscore, leadyears) ~ populismscore + (1 | country) + (1 | year), data = df_temp),
      # mixed model predictor only event
      "Event" = lmer(lead(democracyscore, leadyears) ~ evnt + (1 | country) + (1 | year), data = df_temp),
      # mixed model predictor only moderator
      "Moderator" = lmer(lead(democracyscore, leadyears) ~ moderator + (1 | country) + (1 | year), data = df_temp),
      # Controls
      "Controls" = lmer(lead(democracyscore, leadyears) ~ evnt + populismscore + moderator + mean_of_demlags + surplus + coalition +  (1 | country), data = df_temp),
      # mixed model with three-way interaction and controls
      "Interaction and Controls" = lmer(lead(democracyscore, leadyears) ~ evnt * populismscore * moderator + mean_of_demlags + surplus + coalition +  (1 | country), data = df_temp),
      # three way interaction with democracy lag instead of moderator, moderator as control
      "Dynamic" = lmer(lead(democracyscore, leadyears) ~ evnt * populismscore * mean_of_demlags + moderator + surplus + coalition +  (1 | country), data = df_temp),
      # three way interaction without controls
      "Interaction" = lmer(lead(democracyscore, leadyears) ~ evnt * populismscore * moderator + (1 | country) + (1 | year), data = df_temp),
      # Three way interaction with lagged democracy score and clustered standard errors
      "Clustered" = lm_robust(lead(democracyscore, leadyears)  ~ moderator * populismscore * evnt  + mean_of_demlags,
                              clusters = country,
                              fixed_effects = ~ country,
                              data = df_temp)
    )
    
    newobs <- data.frame(country = i, coefjack = fixef(safe$`Interaction and Controls`)[3])
    
    jackedreg <- rbind(jackedreg, newobs)
    
  }
  
  return(jack.reg)
}


reg_effects_multiinteraction <- function(regression_model, levels, levels2, moderatorname, moderatorname2, label0, label1) {  
  
  # calculate marginal effects and confidence intervals
  # returns a df with AME, SE and predictions at 90, 95 and 99 CI
  
  # we need to specify which variable we're working with at the moment (ivname & moderatorname)
  
  # to make the plots prettier, we need to rename binary variables
  # label0 for moderator == 0, label1 for moderator == 1
  
  rownos <- row.names(model.frame(regression_model))
  
  df4 |> 
    mutate(populismscore = moderatorname,
           moderator = moderatorname2) |> 
    filter(row_number() %in% rownos) ->
    df4
  
  regression_model %>%
    # calculate marginal effects
    margins(
      variables = "evnt",
      at = list(populismscore = levels_popul, moderator = levels_govleft)
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
  
  if (all(df4$populismscore %in% c(0, 1) | is.na(df4$populismscore))){
    meff |>
      mutate(populismscore = if_else(populismscore == 0, "Non-Populist", "Populist")) ->
      meff
  }
  
  if (all(df4$moderator %in% c(0, 1) | is.na(df4$moderator))){
    meff |>
      mutate(moderator = if_else(moderator == 0, label0, label1)) ->
      meff
  }
}
