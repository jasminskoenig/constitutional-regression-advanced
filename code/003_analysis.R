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
library(patchwork)
library(hrbrthemes)
library(ggtext)
library(showtext)
library(zoo)
library(plm)
library(marginaleffects)

# Functions ----

source("src/analysischange_functions.R")

## Theme for Plots ----
textsize <- 12
source("src/graphics.R")
theme_set(theme_regression)

# Data ----

ccpc_vdem <- readRDS("data/ccpc_vdem.rds") 

# filter data  frame to both continents
ccpc_vdem %>%
  filter(e_regiongeo %in% c(1:4, 17:18)) |>
  filter(
    year <= 2020 & year > 1990,
    # exclude closed autocracies
    v2x_regime > 0,
    country != "Moldova",
    country != "Suriname"
  ) |> 
  arrange(country, year) |> 
  group_by(country) |> 
  mutate(latin = if_else(e_regiongeo %in% c(17:18), 1, 0),
         year = as.factor(year),
         lead_libdem = dplyr::lead(v2x_libdem, 1)) |> 
  ungroup() |> 
  mutate(ruth_populism_lr = relevel(factor(ruth_populism_lr), ref = "Non-Populist")) ->
  df4

# DISSERTATION MODELS ----

plmdata <- pdata.frame(
  df4,
  index = c(
    "country",
    "year"
  )
)

## LEFT-WING & WEIGHTED ----

### LIBDEM -----
mlds <- reg_dem(
  plmdata$v2x_libdem,
  1,
  plmdata$gov_popul_weighted,
  plmdata$gov_left
)
mld <- mlds$`Interaction and Controls`
summary(mld)

meff_mld <- calc_ame(mld)

plot_mld <- plot_ame(meff_mld,
  mld,
  add_histogram = TRUE
)
plot_mld


### POLYARCHY ----

mpos <- reg_dem(
  plmdata$v2x_polyarchy,
  1,
  plmdata$gov_popul_weighted,
  plmdata$gov_left
)

mpo <- mpos$`Interaction and Controls`
summary(mpo)

meff_mpo <- calc_ame(mpo)

plot_mpo <- plot_ame(meff_mpo,
  mpo,
  adapt_tag = TRUE
)
plot_mpo

### PARTIP ----
mpas <- reg_dem(
  plmdata$v2x_partip,
  1,
  plmdata$gov_popul_weighted,
  plmdata$gov_left
)

mpa <- mpas$`Interaction and Controls`
summary(mpa)

meff_mpa <- calc_ame(mpa)

plot_mpa <- plot_ame(meff_mpa, mpa)
plot_mpa

### EGAL ----

meds <- reg_dem(
  plmdata$v2x_egaldem,
  1,
  plmdata$gov_popul_weighted,
  plmdata$gov_left
)

med <- meds$`Interaction and Controls`
summary(med)

meff_med <- calc_ame(med)

plot_med <- plot_ame(meff_med, med)
plot_med

### CIVIL ----
mcss <- reg_dem(
  plmdata$v2x_cspart,
  1,
  plmdata$gov_popul_weighted,
  plmdata$gov_left
)

mcs <- mcss$`Interaction and Controls`
summary(mcs)

meff_mcs <- calc_ame(mcs)

plot_mcs <- plot_ame(meff_mcs, mcs)
plot_mcs

### DELIB----

mdds <- reg_dem(
  plmdata$v2x_delibdem,
  1,
  plmdata$gov_popul_weighted,
  plmdata$gov_left
)

mdd <- mdds$`Interaction and Controls`
summary(mdd)

meff_mdd <- calc_ame(mdd)

plot_mdd <- plot_ame(meff_mdd, mdd)
plot_mdd

### TABLE ----

coef_names <- c(
  "populismscore" = "Populism Score",
  "evnt" = "Constitutional Change",
  "moderator" = "Left-Wing",
  "surplus" = "Surplus Seats"
)

rows <- data.frame(
  "Coefficients" = "Country FE",
  "(1)" = "Yes",
  "(2)" = "Yes",
  "(3)" = "Yes",
  "(4)" = "Yes",
  "(5)" = "Yes"
)

mainmodellist <- list(mld, mpo, mpa, med, mcs)

table_mainmodels <- create_regressiontable(mainmodellist,
  add_row = TRUE,
  row_position = 17,
  row_data = rows,
  latex = TRUE
)

writeLines(table_mainmodels, "results/tables/constitutionalchange_main.tex")

### PLOT MAIN TEXT ----

plot_mld +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) ->
  plot1

plot_med +
  labs(y = element_blank(),
       x = element_blank()) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_blank()) ->
  plot2

plot_mpo +
  labs(y = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_blank()) ->
  plot3

plot1 + plot2 + plot3 + plot_layout(ncol = 1,
                                    nrow = 4,
                                    heights = c(0.3,1,1,1),
                                    guides = 'collect')

ggsave("results/graphs/change_effect.pdf",
       device = cairo_pdf,
       width = 7,
       height = 10)

### JACKKNIFE COUNTRIES ----

demtypes <- create_names()
democracytypes <- demtypes$long

jackknife_plots <- map(democracytypes, ~ plot_jackknife(.,
                                        1,
                                        "gov_popul_weighted",
                                        "gov_left"))

jackknife_plots[[1]] + jackknife_plots[[2]] + jackknife_plots[[3]] + 
  plot_layout(guides = "collect") ->
  jackknife_appendix

### JACKKNIFE LAGS ----

jackknife_lags <- map_dfr(democracytypes, ~ reg_dem_jackknifelag(.,
                                     1,
                                     "gov_popul_weighted",
                                     "gov_left")) 


leads <- c(1, 2, 3, 4)

tables_leads <- map(democracytypes, ~ create_regressiontable_leads(.x))

## DEMOCRATIC QUALITY AHEAD & WEIGHTED ----

models_lagged <- map(democracytypes, ~reg_main(plmdata[[.]],
                                               1,
                                               plmdata$gov_popul_weighted,
                                               lag(plmdata[[.]], 2)))

coef_names <- c(
  "populismscore" = "Populism Score",
  "evnt" = "Constitutional Change",
  "moderator" = "Lagged Democracy Score",
  "surplus" = "Surplus Seats"
)

table_dynamicmodels <- create_regressiontable(models_lagged,
  add_row = TRUE,
  row_position = 17,
  row_data = rows,
  latex = TRUE
)

writeLines(table_dynamicmodels, "results/tables/constitutionalchange_dynamic.tex")


## LEFT-WING & RUTH ----

models_ruth <- map(democracytypes, ~reg_main(plmdata[[.]],
                                               1,
                                               plmdata$ruth_populism_lr))

table_ruthmodels <- create_regressiontable(models_ruth,
                                              add_row = TRUE,
                                              row_position = 17,
                                              row_data = rows
)

table_ruthmodels

### END MAIN MODELS ###

# Blog ----

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

# Analysis ----

## Constitutional Change Likelihood by VParty----

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



## Government Weighted Populism Score Models -----

### Liberal Democracy by Weighted Populism & Left-Wing ----

models_libdem = reg_dem(df4$v2x_libdem, 2, df4$gov_popul_weighted, df4$gov_left)

reg_evnt_table_dem(models_libdem)

model_libdem <- models_libdem$`Interaction and Controls`

levels_popul <- reg_mod_levels(model_libdem, df4$gov_popul_weighted)
levels_govleft <- reg_mod_levels(model_libdem, df4$gov_left)

effects_libdem <- reg_effects_multiinteraction(model_libdem, 
                                               levels_libdem, 
                                               levels_govleft, 
                                               df4$gov_popul_weighted, 
                                               df4$gov_left, 
                                               "Right", 
                                               "Left")

reg_plot(effects_libdem)

reg_dem_jackknife("v2x_libdem", 1, "gov_popul_weighted", "gov_left")

### Participation Democracy by Weighted Populism & Left-Wing ----

models_partip = reg_dem(df4$v2x_partip, 1, df4$gov_popul_weighted, df4$gov_left)
reg_evnt_table_dem(models_partip)
model_partip <- models_partip$`Interaction`

levels_partip <- reg_mod_levels(model_partip, df4$gov_popul_weighted)
levels_gov_left <- reg_mod_levels(model_partip, df4$gov_left)

effects_partip <- reg_effects_multiinteraction(model_partip, 
                                               levels_partip, levels_govleft, 
                                               df4$gov_popul_weighted, df4$gov_left, 
                                               "Right", "Left")

reg_plot(effects_partip)

### Civil Society Democracy by Weighted Populism & Left-Wing ----

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

models_egaldem = reg_dem(df4$v2x_egaldem, 1, df4$gov_popul_weighted, df4$gov_left)
reg_evnt_table_dem(models_egaldem)
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
reg_evnt_table_dem(models_polyarchy)
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

### Test ----

df4 |> 
  mutate(ruth_populism_lr = relevel(factor(ruth_populism_lr), ref="Non-Populist")) ->
  df4

test <- lmer(lead(v2x_egaldem, 1) ~ ruth_populism_lr*evnt +  surplus + presidential + (1 | country), data = df4)  
stargazer(test)


### Liberal Democracy by Weighted Populism & Left-Wing ----

models_libdem = reg_dem(df4$v2x_libdem, 1, df4$ruth_populism, df4$gov_left)

model_libdem <- models_libdem$`Interaction`
stargazer(model_libdem, type = "text")

levels_popul <- c(0,1)
levels_govleft <- reg_mod_levels(model_libdem, df4$gov_left)

effects_libdem <- reg_effects_multiinteraction(model_libdem, 
                                               levels_popul, levels_govleft, 
                                               df4$ruth_populism, df4$gov_left,
                                               "Right", "Left")

reg_plot(effects_libdem)

### Participation Democracy by Weighted Populism & Left-Wing ----

models_partip = reg_dem(df4$v2x_partip, 1, df4$ruth_populism, df4$gov_left)

model_partip <- models_partip$`Interaction`
stargazer(model_partip, type = "text")

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
stargazer(model_cspart, type = "text")

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
stargazer(model_egaldem, type = "text")

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
stargazer(model_polyarchy, type = "text")

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
