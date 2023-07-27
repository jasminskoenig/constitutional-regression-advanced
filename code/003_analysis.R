# Set-up ----

## Libraries ----
library(tidyverse)
library(stargazer)
library(modelsummary)
library(kableExtra)
library(gt)
library(lme4)
library(gtsummary)
library(ggplot2)
library(hrbrthemes)
library(ggtext)
library(showtext)
# clustered standard errors
library(estimatr)
# matt kay kind of plotting
library(ggdist)
library(distributional)
library(modelr)
library(broom)
library(broom.mixed)
# for tilko kind of plotting
library(margins)

## Theme for Plots ----

theme_gridY <- theme_ipsum_rc(grid = "") +
  theme(
    axis.title.x = element_text(size = 30, family = "rajdhani"),
    axis.text.x = element_text(size = 30, family = "rajdhani"),
    axis.text.y = element_markdown(size = 30, family = "rajdhani"),
    axis.title.y.left = element_text(size = 30, family = "rajdhani"),
    strip.text = element_text(size = 34, family = "rajdhani"),
    plot.title = element_text(size = 31, family = "rajdhani"),
    plot.subtitle = element_text(size = 31, family = "rajdhani"),
    plot.caption = element_text(size = 28, family = "rajdhani", face = "plain", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 31, family = "rajdhani"),
    legend.title = element_text(size = 30, family = "rajdhani"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = grid::unit(c(0, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
    text = element_text(family = "rajdhani")
  )

# set theme
theme_set(theme_gridY)

# add font style
font_add_google(family = "rajdhani", name = "Rajdhani")
showtext_auto()


# Data ----

ccpc_vdem <- readRDS("data/ccpc_vdem_eu_la.rds") 


# Filter Data

ccpc_vdem|>
  # at time of analysis dataset only had data till 3030, 1990 democratization in ee
  filter(
    year > 1990,
    # exclude closed autocracies
    v2x_regime > 0,
    country != "Moldova"
  ) |>
  # year as factor for random intercepts
  mutate(year = as.factor(year)) ->
  ccpc_vdem

# filter data  frame to both continents
ccpc_vdem %>%
  filter(e_regiongeo %in% c(1:4, 17:18)) |>
  # create dummy for interaction effect
  mutate(latin = if_else(e_regiongeo %in% c(17, 18), 1, 0),
         lag_respect_con = lag(v2exrescon, 1)) |> 
  mutate(across(starts_with("v2x"), 
         .fns = ~ . - lag(.),
         .names = "lagged_{.col}")) ->
  df4


# Analysis ----

## Constitutional Change Likelihood by VParty----

### Functions ----

reg_evnt_models <- function(iv, moderator){
  
  # Runs base, mixed-effect and fixed-effect models to predict constitutional change
  # Returns list of regressions
  # Single regressions can be accessed like this: models$`Mixed Interaction` afterwards
  
  models <- list(
    "Base" = lm(lead(evnt, 1) ~ iv, data = df4),
    "Mixed" = lmer(lead(evnt, 1) ~ iv + (1 | country), data = df4),
    "Mixed Controls" = lmer(lead(evnt, 1) ~ iv + moderator + v2xnp_pres + lag_respect_con +lagged_v2x_libdem + coalition + (1 | country), data = df4),
    "Mixed Interaction" = lmer(lead(evnt, 1) ~ iv * moderator + moderator + lag_respect_con + v2xnp_pres + lagged_v2x_libdem + coalition + (1 | country), data = df4),
    "Fixed"     = glm(lead(evnt,1) ~ iv + country, data = df4),
    "Fixed Controls" = glm(lead(evnt,1) ~ iv + moderator + coalition + country, data = df4),
    "Fixed Interaction" = glm(lead(evnt,1) ~ iv + moderator + coalition + v2xnp_pres + lagged_v2x_libdem + iv*moderator + country, data = df4)
  )
  
  return(models)
  
}


reg_evnt_table <- function(modellist){
  
  # Creates a table to compare all Models estimated in reg_evnt_models
  
  rows <- tribble(~term, ~Base, ~Mixed,  ~`Mixed Controls`, ~`Mixed Interaction`, ~Fixed, ~`Fixed Controls`, ~`Fixed Interaction`,
                  'Country FE', 'No', 'Random Intercept', 'Random Intercept', 'Random Intercept', 'FE', 'FE', 'FE')
  
  attr(rows, 'position') <- 15
  
  table <- modelsummary(
    modellist,
    fmt = 1,
    estimate  = "{estimate}{stars}",
    statistic = 'conf.int',
    coef_omit = "(Intercept|^country)",
    add_rows = rows,
    coef_rename = c("iv" = "Populism Score", 
                    "moderator" = "Left-Wing",
                    "v2xnp_pres" = "Presidentialism Score",
                    "lagged_v2x_libdem" = "Lagged Democray (1)",
                    "coalition" = "Coalition",
                    "surplus" = "Surplus Seats",
                    "ivxmoderator" = "Populism x Left-Wing",
                    "no_govparties" = "No Gov Parties",
                    "lag_respect_con" = "Respect Constitution (Lag, 1)"))
  
  return(table)

}

# Example use of both functions
models <- reg_evnt_models(df4$gov_popul_weighted, df4$gov_left)

reg_evnt_table(models)

# Example how to pull one model
model_fe <- models$`Fixed Interaction`

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

# Example use of levels function
levels <- reg_mod_levels(model_fe, df4$gov_left)

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
  
  regression_model %>%
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
}

# Example use of effects function
effects_evnt <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$gov_popul_weighted, 
                            df4$gov_left, 
                            "Right", 
                            "Left")

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
        breaks = c(0, 0.35, 0.5, 0.75, 1),
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
    
  } else {
    
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
  }
  
}

# Example use of plots function
reg_plot(effects_evnt)

## Constitutional Change Likelihood by VParty----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$gov_popul_weighted, df4$gov_left)

# View Table to compare
reg_evnt_table(models)

# Pull fixed effects model
model_fe <- models$`Fixed Interaction`

# Calculate levels of moderating variable government left
levels <- reg_mod_levels(model_fe, df4$gov_left)


# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$gov_popul_weighted, 
                            df4$gov_left, 
                            "Right", 
                            "Left")

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)

## Constitutional Change Likelihood by Ruth Populism Score----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$ruth_populism, df4$gov_left)

# View Table to compare
reg_evnt_table(models)

# Pull fixed effects model
model_fe <- models$`Fixed Interaction`

# Calculate levels of moderating variable government left
levels <- reg_mod_levels(model_fe, df4$gov_left)


# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$ruth_populism, 
                            df4$gov_left, 
                            "Right", 
                            "Left")

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)

## Constitutional Change Likelihood by VParty Prime Minister----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$gov_popul_prime, df4$gov_left)

# View Table to compare
reg_evnt_table(models)

# Pull fixed effects model
model_fe <- models$`Fixed Interaction`

# Calculate levels of moderating variable government left
levels <- reg_mod_levels(model_fe, df4$gov_left)


# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$gov_popul_prime, 
                            df4$gov_left, 
                            "Right", 
                            "Left")

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)

# Analysis Impact on Democracy  ----

## Functions

reg_dem <- function(democracyscore, leadyears, populismscore, moderator){
  
  # calculates multiple mixed effects models for the likelyhood of democracy score
  # democracyscore, the number of leads for the dv, the populism score and a moderator can be chosen
  # the moderator is interacted with the populism score and evnt
  
  models = list(
  # mixed model predictor only populism
  "Populism" = lmer(lead(democracyscore, leadyears) ~ populismscore + (1 | country) + (1 | year), data = df4),
  # mixed model predictor only event
  "Event" = lmer(lead(democracyscore, leadyears) ~ evnt + (1 | country) + (1 | year), data = df4),
  # mixed model predictor only moderator
  "Moderator" = lmer(lead(democracyscore, leadyears) ~ moderator + (1 | country) + (1 | year), data = df4),
  # mixed model with three-way interaction and controls
  "Interaction & Controls" = lmer(lead(democracyscore, leadyears) ~ evnt * populismscore * moderator + lagged_v2x_libdem + surplus + coalition +  (1 | country), data = df4),
  # three way interaction with democracy lag instead of moderator, moderator as control
  "Dynamic" = lmer(lead(democracyscore, leadyears) ~ evnt * populismscore * lagged_v2x_libdem + moderator + surplus + coalition +  (1 | country), data = df4),
  # three way interaction without controls
  "Interaction" = lmer(lead(democracyscore, leadyears) ~ evnt * populismscore * moderator + (1 | country) + (1 | year), data = df4),
  # Three way interaction with lagged democracy score and clustered standard errors
  "Clustered" = lm_robust(lead(democracyscore, leadyears)  ~ moderator * populismscore * lag(v2x_libdem)  + moderator + lag(v2x_libdem),
                    clusters = country,
                    fixed_effects = ~ country,
                    data = df4)
  )
  
}

reg_effects_multiinteraction <- function(regression_model, levels, levels2, moderatorname, moderatorname2, label0, label1) {  
  
  # calculate marginal effects and confidence intervals
  # returns a df with AME, SE and predictions at 90, 95 and 99 CI
  
  # we need to specify which variable we're working with at the moment (ivname & moderatorname)
  
  # to make the plots prettier, we need to rename binary variables
  # label0 for moderator == 0, label1 for moderator == 1
  
  df4 |> 
    mutate(populismscore = moderatorname,
           moderator = moderatorname2) ->
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

## Government Weighted Populism Score Models -----

### Liberal Democracy by Weighted Populism & Left-Wing ----

models_libdem = reg_dem(df4$v2x_libdem, 1, df4$gov_popul_weighted, df4$gov_left)

model_libdem <- models_libdem$`Interaction`

levels_popul <- reg_mod_levels(model_libdem, df4$gov_popul_weighted)
levels_govleft <- reg_mod_levels(model_libdem, df4$gov_left)

effects_libdem <- reg_effects_multiinteraction(model_libdem, 
                             levels_libdem, levels_govleft, 
                             df4$gov_popul_weighted, df4$gov_left, 
                             "Right", "Left")

reg_plot(effects_libdem)

### Participation Democracy by Weighted Populism & Left-Wing ----

models_partip = reg_dem(df4$v2x_partip, 1, df4$gov_popul_weighted, df4$gov_left)

model_partip <- models_partip$`Interaction`

levels_partip <- reg_mod_levels(model_partip, df4$gov_popul_weighted)
levels_partip <- reg_mod_levels(model_partip, df4$gov_left)

effects_partip <- reg_effects_multiinteraction(model_partip, 
                                               levels_partip, levels_govleft, 
                                               df4$gov_popul_weighted, df4$gov_left, 
                                               "Right", "Left")

reg_plot(effects_partip)

### Civil Society Democracy by Weighted Populism & Left-Wing ----

models_cspart = reg_dem(df4$v2x_cspart, 1, df4$gov_popul_weighted, df4$gov_left)

model_cspart <- models_cspart$`Interaction and Controls`

levels_cspart <- reg_mod_levels(model_cspart, df4$gov_popul_weighted)
levels_cspart <- reg_mod_levels(model_cspart, df4$gov_left)

effects_cspart <- reg_effects_multiinteraction(model_cspart, 
                                               levels_cspart, levels_govleft, 
                                               df4$gov_popul_weighted, df4$gov_left, 
                                               "Right", "Left")

reg_plot(effects_cspart)


### Egalitarian Democracy by Weighted Populism & Left-Wing ----

models_egaldem = reg_dem(df4$v2x_egaldem, 1, df4$gov_popul_weighted, df4$gov_left)

model_egaldem <- models_egaldem$`Interaction and Controls`

levels_egaldem <- reg_mod_levels(model_egaldem, df4$gov_popul_weighted)
levels_egaldem <- reg_mod_levels(model_egaldem, df4$gov_left)

effects_egaldem <- reg_effects_multiinteraction(model_egaldem, 
                                               levels_egaldem, levels_govleft, 
                                               df4$gov_popul_weighted, df4$gov_left, 
                                               "Right", "Left")

reg_plot(effects_egaldem)

### Electoral Democracy by Weighted Populism & Left-Wing ----

models_polyarchy = reg_dem(df4$v2x_polyarchy, 1, df4$gov_popul_weighted, df4$gov_left)

model_polyarchy <- models_polyarchy$`Interaction and Controls`

levels_polyarchy <- reg_mod_levels(model_polyarchy, df4$gov_popul_weighted)
levels_polyarchy <- reg_mod_levels(model_polyarchy, df4$gov_left)

effects_polyarchy <- reg_effects_multiinteraction(model_polyarchy, 
                                                levels_polyarchy, levels_govleft, 
                                                df4$gov_popul_weighted, df4$gov_left, 
                                                "Right", "Left")

reg_plot(effects_polyarchy)

## Ruth Populism Models ----

### Liberal Democracy by Weighted Populism & Left-Wing ----

models_libdem = reg_dem(df4$v2x_libdem, 1, df4$ruth_populism, df4$gov_left)

model_libdem <- models_libdem$`Interaction`

levels_popul <- reg_mod_levels(model_libdem, df4$ruth_populism)
levels_govleft <- reg_mod_levels(model_libdem, df4$gov_left)

effects_libdem <- reg_effects_multiinteraction(model_libdem, 
                                               levels_popul, levels_govleft, 
                                               df4$ruth_populism, df4$gov_left,
                                               "Right", "Left")

reg_plot(effects_libdem)

### Participation Democracy by Weighted Populism & Left-Wing ----

models_partip = reg_dem(df4$v2x_partip, 1, df4$ruth_populism, df4$gov_left)

model_partip <- models_partip$`Interaction`

levels_partip <- reg_mod_levels(model_partip, df4$ruth_populism)
levels_partip <- reg_mod_levels(model_partip, df4$gov_left)

effects_partip <- reg_effects_multiinteraction(model_partip, 
                                               levels_partip, levels_govleft, 
                                               df4$ruth_populism, df4$gov_left, 
                                               "Right", "Left")

reg_plot(effects_partip)

### Civil Society Democracy by Weighted Populism & Left-Wing ----

models_cspart = reg_dem(df4$v2x_cspart, 1, df4$ruth_populism, df4$gov_left)

model_cspart <- models_cspart$`Interaction and Controls`

levels_cspart <- reg_mod_levels(model_cspart, df4$ruth_populism)
levels_cspart <- reg_mod_levels(model_cspart, df4$gov_left)

effects_cspart <- reg_effects_multiinteraction(model_cspart, 
                                               levels_cspart, levels_govleft, 
                                               df4$ruth_populism, df4$gov_left, 
                                               "Right", "Left")

reg_plot(effects_cspart)

### Egalitarian Democracy by Weighted Populism & Left-Wing ----

models_egaldem = reg_dem(df4$v2x_egaldem, 1, df4$ruth_populism, df4$gov_left)

model_egaldem <- models_egaldem$`Interaction and Controls`

levels_egaldem <- reg_mod_levels(model_egaldem, df4$ruth_populism)
levels_egaldem <- reg_mod_levels(model_egaldem, df4$gov_left)

effects_egaldem <- reg_effects_multiinteraction(model_egaldem, 
                                                levels_egaldem, levels_govleft, 
                                                df4$ruth_populism, df4$gov_left, 
                                                "Right", "Left")

reg_plot(effects_egaldem)

### Electoral Democracy by Weighted Populism & Left-Wing ----

models_polyarchy = reg_dem(df4$v2x_polyarchy, 1, df4$ruth_populism, df4$gov_left)

model_polyarchy <- models_polyarchy$`Interaction and Controls`

levels_polyarchy <- reg_mod_levels(model_polyarchy, df4$ruth_populism)
levels_polyarchy <- reg_mod_levels(model_polyarchy, df4$gov_left)

effects_polyarchy <- reg_effects_multiinteraction(model_polyarchy, 
                                                  levels_polyarchy, levels_govleft, 
                                                  df4$ruth_populism, df4$gov_left, 
                                                  "Right", "Left")

reg_plot(effects_polyarchy)

# Analysis Compliance


reg_jud_ind <- function(iv, moderator){
  
  # Runs base, mixed-effect and fixed-effect models to predict constitutional change
  # Returns list of regressions
  # Single regressions can be accessed like this: models$`Mixed Interaction` afterwards
  
  models <- list(
    "Constitution" = lm(v2exrescon ~ iv, data = df4),
    "Compliance Jud" = lm(v2jucomp ~ iv, data = df4),
    "Comliance High" = lm(v2juhccomp ~ iv, data = df4),
    "Independence" = lm(v2juhcind ~ iv, data = df4),
    "Index" = lm(v2x_jucon ~ iv, data = df4),
    "Constitution2" = lmer(v2exrescon ~ iv*moderator + (1 | country), data = df4),
    "Compliance Jud2" = lmer(v2jucomp ~ iv*moderator + (1 | country), data = df4),
    "Comliance High2" = lmer(v2juhccomp ~ iv*moderator + (1 | country), data = df4),
    "Independence2" = lmer(v2juhcind ~ iv*moderator + (1 | country), data = df4),
    "Index2" = lmer(v2x_jucon ~ iv*moderator + (1 | country), data = df4)
  )
  
  return(models)
  
}

models_jud_ind <- reg_jud_ind(df4$gov_popul_weighted, df4$gov_left)

table <- modelsummary(
  models_jud_ind,
  fmt = 1,
  estimate  = "{estimate}{stars}",
  statistic = 'conf.int',
  coef_omit = "(Intercept|^country)")
table

model_const_res <- models_jud_ind$`Index2`

levels_left <- reg_mod_levels(model_const_res, df4$gov_left)

effects_const_res <- reg_tidy_effects(model_const_res, 
                                      levels_left, 
                                      df4$gov_popul_weighted, df4$gov_left, 
                                      "Right", "Left")
reg_plot(effects_const_res)

