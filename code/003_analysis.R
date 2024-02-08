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
library(zoo)
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

ccpc_vdem <- readRDS("data/ccpc_vdem.rds") 


# filter data  frame to both continents
ccpc_vdem %>%
  filter(e_regiongeo %in% c(1:4, 17:18)) |>
  group_by(country) |> 
  # create dummy for interaction effect
  mutate(latin = if_else(e_regiongeo %in% c(17, 18), 1, 0),
         lag_trust_mean = lag(trust_mean),
         lag_trust_share = lag(trust_share),
         lag_judind = lag(v2juhcind),
         lag_respect_con = lag(v2exrescon, 1),
         lagged_v2x_libdem = lag(v2x_libdem, 2),
         #mean_of_demlags = mean(c(tail(v2x_libdem, 5), NA), na.rm = TRUE),
         #mean_of_trustlags = mean(c(tail(share_trust, 5), NA), na.rm = TRUE),
         across(starts_with("v2x"), 
         .fns = ~ . - lag(.),
         .names = "lagged_{.col}"),
         mean_of_demlags = rollmeanr(v2x_libdem, k = 5, fill = NA),
         mean_of_trustlags5 = rollmeanr(trust_mean, k = 5, fill = NA),
         mean_of_trustlags3 = rollmeanr(trust_mean, k = 3, fill = NA),
         mean_of_trustlags2 = rollmeanr(trust_mean, k = 2, fill = NA),
         share_of_trustlags5 = rollmeanr(trust_share, k = 5, fill = NA),
         share_of_trustlags3 = rollmeanr(trust_share, k = 3, fill = NA),
         share_of_trustlags2 = rollmeanr(trust_share, k = 2, fill = NA)) |>
  # at time of analysis dataset only had data till 3030, 1990 democratization in ee
  filter(
    year >= 2000,
    year <= 2020,
    # exclude closed autocracies
    v2x_regime > 0,
    country != "Moldova"
  ) |>
  # year as factor for random intercepts
  mutate(year = as.factor(year)) |> 
  ungroup() ->
  df4


# Analysis ----

## Constitutional Change Likelihood by VParty----

### Functions ----

reg_evnt_models <- function(dv, iv, moderator){
  
  # Runs base, mixed-effect and fixed-effect models to predict constitutional change
  # Returns list of regressions
  # Single regressions can be accessed like this: models$`Mixed Interaction` afterwards
  
  models <- list(
    "Base" = lm(lead(dv, 1) ~ iv, data = df4),
    "Mixed" = lmer(lead(dv, 1) ~ iv + (1 | country), data = df4),
    "Mixed Controls" = lmer(lead(dv, 1) ~ iv + moderator  + v2xnp_pres + (1 | country), data = df4),    
    "Mixed Interaction" = lmer(lead(dv, 1) ~ iv * moderator  + moderator  + v2xnp_pres + (1 | country), data = df4)  
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

# Example use of plots function
reg_plot(effects_evnt)

## Constitutional Change Likelihood by VParty----

### Weighted Populism & Left-Wing ----
models <- reg_evnt_models(df4$evnt, df4$gov_popul_weighted, df4$gov_left)

# View Table to compare
reg_evnt_table(models)

# Pull mixed effects model
model_fe <- models$`Mixed Dynamic`

stargazer(model_fe)

# Calculate levels of moderating variable government left
levels <- reg_mod_levels(model_fe, df4$gov_left)


# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$gov_popul_weighted, 
                            df4$gov_left,
                            "Right-Wing",
                            "Left-Wing")

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)

# Jackknife Model
jack.results <- reg_evnt_jackknife("gov_popul_weighted", "gov_left")

hist(jack.results$coefjack)

### Weighted Populism and Lagged Demoncracy

# Pull mixed effects model
model_fe <- models$`Mixed Dynamic`

stargazer(model_fe)

# Calculate levels of moderating variable government left
levels <- reg_mod_levels(df4$evnt, model_fe, df4$mean_of_demlags)


# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$gov_popul_weighted, 
                            df4$mean_of_demlags)

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)

# Jackknife Model XXX
jack.results <- reg_evnt_jackknife("gov_popul_weighted", "mean_of_demlags")

hist(jack.results$coefjack)

#### Dummy for junior, senior populist as iv ----

models_dum <- reg_evnt_dummy_models(df4$rooduijn_government_senior, 
                                    df4$gov_left,
                                    df4$solo)
summary(models_dum$Base)

## Constitutional Change Likelihood by Ruth Populism Score----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$evnt, df4$ruth_populism, df4$gov_left)

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
models <- reg_evnt_models(df4$evnt, df4$gov_popul_prime, df4$gov_left)

# View Table to compare
reg_evnt_table(models)

# Pull fixed effects model
model_fe <- models$`Mixed Interaction`

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

## Constitutional Change Likelihood by VParty Dummy----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$evnt, df4$vparty_populist, df4$gov_left)

# View Table to compare
reg_evnt_table(models)

## Constitutional Change Likelihood by VParty Senior Dummy----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$evnt, df4$vparty_populist_senior, df4$gov_left)

# View Table to compare
reg_evnt_table(models)

## Judicial Reform Likelihood by Weighted Populism Score----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$v2jureform_ord, df4$gov_popul_weighted, df4$lag_trust_share)

# View Table to compare
reg_evnt_table(models)

# Pull fixed effects model
model_fe <- models$`Mixed Interaction`

model_fe <- lmer(lead(v2jureform_ord, 1) ~ gov_popul_weighted * gov_left + gov_popul_weighted*lag_trust_mean  + gov_left + mean_of_demlags + lag_trust_mean + v2xnp_pres + (1 | country), data = df4)

# Calculate levels of moderating variable government left
levels <- reg_mod_levels(model_fe, df4$lag_trust_share)

# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$gov_popul_weighted, 
                            df4$lag_trust_share)

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)


## Judiciary Change Likelihood by Weighted Populism Score----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$jud_replace, df4$gov_popul_weighted, df4$lag_trust_share)

# View Table to compare
reg_evnt_table(models)


# Pull fixed effects model
model_fe <- models$`Mixed Interaction`


# Calculate levels of moderating variable government left
levels <- reg_mod_levels(model_fe, df4$lag_trust_share)

# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$gov_popul_weighted, 
                            df4$lag_trust_share)

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)


## Judiciary Change Likelihood by Weighted Populism Score----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$jud_replace_con, df4$gov_popul_weighted, df4$lag_trust_share)

# View Table to compare
reg_evnt_table(models)


# Pull fixed effects model
model_fe <- models$`Mixed Interaction`

# Calculate levels of moderating variable government left
levels <- reg_mod_levels(model_fe, df4$lag_trust_share)

# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$gov_popul_weighted, 
                            df4$lag_trust_share)

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)

## Judicial Change by Ruth Populism Score----

# Calculate fixed and mixed effects models
models <- reg_evnt_models(df4$jud_replace_con, 
                          df4$ruth_populism, 
                          df4$lag_trust_share)

# View Table to compare
reg_evnt_table(models) |> 
  save_kable("results/graphs/regression_replace.png")

# Pull fixed effects model
model_fe <- models$`Mixed Interaction`

# Calculate levels of moderating variable government left
levels <- reg_mod_levels(model_fe, df4$lag_trust_share)


# Calculate AMEs
effects <- reg_tidy_effects(model_fe, 
                            levels, 
                            df4$ruth_populism, 
                            df4$lag_trust_share)

# Plot AME of weighted populism score based on left- or right-wing government
reg_plot(effects)

ggsave("results/graphs/jud_replace.png", device = "png", height = 5, width = 10)

# Analysis Impact on Democracy  ----

## Functions

reg_dem <- function(democracyscore, leadyears, populismscore, moderator){
  
  # calculates multiple mixed effects models for the likelyhood of democracy score
  # democracyscore, the number of leads for the dv, the populism score and a moderator can be chosen
  # the moderator is interacted with the populism score and 
  
  # df4 |> 
  #   group_by(country) |> 
  #   mutate(lead_democracyscore = lead(democracyscore, 2),
  #          lag_democracyscore = lag(democracyscore, 2)) |> 
  #   ungroup() ->
  #   df4
  
  models = list(
  # mixed model predictor only populism
  "Populism" = lmer(lead_democracyscore ~ populismscore + (1 | country) , data = df4),
  # mixed model predictor only event
  "Event" = lmer(lead_democracyscore ~ evnt + (1 | country) , data = df4),
  # mixed model predictor only moderator
  "Moderator" = lmer(lead_democracyscore ~ moderator + (1 | country) , data = df4),
  # Controls
  "Controls" = lmer(lead_democracyscore ~ evnt + populismscore + moderator + lag_democracyscore  + surplus + coalition +  (1 | country), data = df4),
  # mixed model with three-way interaction and controls
  "Interaction and Controls" = lmer(lead_democracyscore ~ evnt * populismscore * moderator + lag_democracyscore + surplus + coalition +  (1 | country), data = df4),
  # three way interaction with democracy lag instead of moderator, moderator as control
  #"Dynamic" = lmer(lead_democracyscore ~ evnt * populismscore * lag_democracyscore + moderator + surplus + coalition +  (1 | country), data = df4),
  # # three way interaction without controls
  #"Interaction" = lmer(lead_democracyscore ~ evnt * populismscore * moderator + (1 | country) , data = df4),
  # Three way interaction with lagged democracy score and clustered standard errors
  "Clustered" = lm_robust(lead_democracyscore  ~ moderator * populismscore * evnt  + lag_democracyscore,
                    clusters = country,
                    fixed_effects = ~ country,
                    data = df4)
  )
  
}

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

reg_dem_jackknife("v2x_libdem", 1, "gov_popul_weighted", "gov_left")

test <- fixef(safe$`Interaction and Controls`)[11]

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

# DEBUGGING



## Government Weighted Populism Score Models -----

### Liberal Democracy by Weighted Populism & Left-Wing ----

df4 |> 
  group_by(country) |> 
  mutate(lead_democracyscore = lead(v2x_libdem, 4),
         lag_democracyscore = lag(v2x_libdem, 1)) |> 
  ungroup() ->
  df4

models_libdem = reg_dem(df4$v2x_libdem, 2, df4$gov_popul_weighted, df4$gov_left)

reg_evnt_table_dem(models_libdem)

model_libdem <- models_libdem$`Interaction and Controls`

levels_popul <- reg_mod_levels(model_libdem, df4$gov_popul_weighted)
levels_govleft <- reg_mod_levels(model_libdem, df4$gov_left)

effects_libdem <- reg_effects_multiinteraction(model_libdem, 
                             levels_libdem, levels_govleft, 
                             df4$gov_popul_weighted, df4$gov_left, 
                             "Right", "Left")

reg_plot(effects_libdem)

reg_dem_jackknife("v2x_libdem", 1, "gov_popul_weighted", "gov_left")

### Participation Democracy by Weighted Populism & Left-Wing ----

df4 |> 
  group_by(country) |> 
  mutate(lead_democracyscore = lead(v2x_partip, 1),
         lag_democracyscore = lag(v2x_partip, 1)) |> 
  ungroup() ->
  df4

models_partip = reg_dem(df4$v2x_partip, 1, df4$gov_popul_weighted, df4$gov_left)

model_partip <- models_partip$`Interaction`

levels_partip <- reg_mod_levels(model_partip, df4$gov_popul_weighted)
levels_gov_left <- reg_mod_levels(model_partip, df4$gov_left)

effects_partip <- reg_effects_multiinteraction(model_partip, 
                                               levels_partip, levels_govleft, 
                                               df4$gov_popul_weighted, df4$gov_left, 
                                               "Right", "Left")

reg_plot(effects_partip)

### Civil Society Democracy by Weighted Populism & Left-Wing ----

df4 |> 
  group_by(country) |> 
  mutate(lead_democracyscore = lead(v2x_cspart, 1),
         lag_democracyscore = lag(v2x_cspart, 1)) |> 
  ungroup() ->
  df4

models_cspart = reg_dem(df4$v2x_cspart, 1, df4$gov_popul_weighted, df4$gov_left)

reg_evnt_table_dem(models_cspart)

model_cspart <- models_cspart$`Interaction and Controls`

levels_cspart <- reg_mod_levels(model_cspart, df4$gov_popul_weighted)
levels_gov_left <- reg_mod_levels(model_cspart, df4$gov_left)

effects_cspart <- reg_effects_multiinteraction(model_cspart, 
                                               levels_cspart, levels_govleft, 
                                               df4$gov_popul_weighted, df4$gov_left, 
                                               "Right", "Left")

reg_plot(effects_cspart)


### Egalitarian Democracy by Weighted Populism & Left-Wing ----


df4 |> 
  group_by(country) |> 
  mutate(lead_democracyscore = lead(v2x_egaldem, 1),
         lag_democracyscore = lag(v2x_egaldem, 1)) |> 
  ungroup() ->
  df4

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


df4 |> 
  group_by(country) |> 
  mutate(lead_democracyscore = lead(v2x_polyarchy, 1),
         lag_democracyscore = lag(v2x_polyarchy, 1)) |> 
  ungroup() ->
  df4

models_polyarchy = reg_dem(df4$v2x_polyarchy, 1, df4$gov_popul_weighted, df4$gov_left)

model_polyarchy <- models_polyarchy$`Interaction and Controls`

levels_polyarchy <- reg_mod_levels(model_polyarchy, df4$gov_popul_weighted)
levels_polyarchy <- reg_mod_levels(model_polyarchy, df4$gov_left)

effects_polyarchy <- reg_effects_multiinteraction(model_polyarchy, 
                                                levels_polyarchy, levels_govleft, 
                                                df4$gov_popul_weighted, df4$gov_left, 
                                                "Right", "Left")

reg_plot(effects_polyarchy)

### v2juhccomp - Compliance with High Court ----

df4 |> 
  group_by(country) |> 
  mutate(lead_democracyscore = lead(v2juhccomp, 2),
         lag_democracyscore = lag(v2juhccomp, 2)) |> 
  ungroup() ->
  df4

models_compliance = reg_dem(df4$v2juhccomp, 1, df4$gov_popul_weighted, df4$gov_left)

models_compliance <- models_compliance$`Interaction and Controls`

levels_compliance <- reg_mod_levels(model_compliance, df4$gov_popul_weighted)
levels_gov_left <- reg_mod_levels(model_pcompliance, df4$gov_left)

effects_compliance <- reg_effects_multiinteraction(models_compliance, 
                                                  levels_compliance, levels_govleft, 
                                                  df4$gov_popul_weighted, df4$gov_left, 
                                                  "Right", "Left")

reg_plot(effects_compliance)

### v2juhcind - Independence of High Court ----

models_independence = reg_dem(df4$v2juhcind, 1, df4$gov_popul_weighted, df4$gov_left)

models_independence <- models_independence$`Interaction and Controls`

levels_independence <- reg_mod_levels(model_independence, df4$gov_popul_weighted)
levels_gov_left <- reg_mod_levels(model_independence, df4$gov_left)

effects_independence <- reg_effects_multiinteraction(models_independence, 
                                                   levels_independencee, levels_govleft, 
                                                   df4$gov_popul_weighted, df4$gov_left, 
                                                   "Right", "Left")

reg_plot(effects_independence)

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

### Blog ----

## Government Weighted Populism Score Models -----

### Liberal Democracy by Weighted Populism & Left-Wing ----

models_libdem_latin = reg_dem(df4$v2x_libdem, 1, df4$gov_popul_weighted, df4$latin)

model_libdem_latin <- models_libdem_latin$`Interaction`

levels_popul <- reg_mod_levels(model_libdem_latin, df4$gov_popul_weighted)
levels_govleft <- reg_mod_levels(model_libdem_latin, df4$latin)

effects_libdem <- reg_effects_multiinteraction(model_libdem_latin, 
                                               levels_libdem, levels_govleft, 
                                               df4$gov_popul_weighted, df4$latin, 
                                               "Europe", "Latin America")

reg_plot(effects_libdem)

reg_dem_jackknife("v2x_libdem", 1, "gov_popul_weighted", "gov_left")

### Participation Democracy by Weighted Populism & Left-Wing ----

models_partip = reg_dem(df4$v2x_partip, 1, df4$gov_popul_weighted, df4$latin)

model_partip <- models_partip$`Interaction`

levels_partip <- reg_mod_levels(model_partip, df4$gov_popul_weighted)
levels_partip <- reg_mod_levels(model_partip, df4$latin)

effects_partip <- reg_effects_multiinteraction(model_partip, 
                                               levels_partip, levels_govleft, 
                                               df4$gov_popul_weighted, df4$latin, 
                                               "Right", "Left")

reg_plot(effects_partip)