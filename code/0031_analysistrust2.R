# ANALYSIS TRUST

# ANALYSIS TRUST

# Set-up ----

## Libraries ----
library(tidyverse)
library(stargazer)
library(patchwork)
library(ivreg)
library(glueformula)
library(lmtest)
library(sandwich)
library(plm)
library(modelsummary)
library(gt)
library(marginaleffects)
library(car)
library(scales)
library(kableExtra)
library(fixest)


## Graphics ----

source("src/graphics.R")
theme_set(theme_regression)

# Data ----

ccpc_vdem <- readRDS("data/ccpc_vdem.rds")


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
  mutate(latin = as.factor(if_else(e_regiongeo %in% c(17, 18), 1, 0)),
         country = as.factor(country),
         ruth_populism = case_when(
           ruth_populism == "Non-Populist" ~ 0,
           ruth_populism == "Populist" ~ 1,
           TRUE ~ NA
         ),
         poptrust = ruth_populism*lagged_trust_share_1,
         popcorr = ruth_populism*v2jucorrdc_mean_3,
         popacc = ruth_populism*v2juaccnt_mean_3,
         firstchangecon5 = if_else(jud_replace_con_lag5 > 0, 1, 0),
         firstchangecon3 = if_else(jud_replace_con_lag3 > 0, 1, 0),
         firstchange5 = if_else(jud_replace_lag5 > 0, 1, 0),
         firstchange3 = if_else(jud_replace_lag3 > 0, 1, 0)) |> 
  mutate(across(starts_with("v2x"), 
                .fns = ~ . - lag(.),
                .names = "lagged_{.col}")) |> 
  filter(!country %in% c("Norway", 
                         "Moldova", 
                         "Ukraine", 
                         "Bosnia and Herzegovina", 
                         "Switzlerand", 
                         "Belarus",
                         "Iceland",
                         "United Kingdom")) ->
  df4

# ANALYSIS JUDICIAL REPLACEMENT ----

## SET-UP

depv = "jud_replace_cont"
indepv = c("lagged_trust_share_linear_imp_1",
           "ruth_populism",
           "lagged_trust_share_linear_imp_1*ruth_populism"
           )
contr = c("surplus",
          "executive",
          "presidential",
          "gdp_log_lag")
instr = c("v2juaccnt_mean_3",
          "v2juaccnt_mean_3*ruth_populism")
fe = "country"

df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year) |> 
  na.omit() ->
  df_final

summary(df_final)

write_rds(df_final, "data/analysis.rds")

## OLS MODEL ----

form <- gf({depv} ~ {indepv} + {contr} + {fe})

# ols without robust SE
ols <- lm(form,
          data = df_final)
# robust SE with sandwich
coeftest(ols,
         vcov = vcovCL,
         type = "HC3",
         df = 40,
         cluster = ~country)
# robust SE with estimatr
ols_robust <- lm_robust(
  form,
  data = df_final,
  clusters = country,
  se_type = "stata"
)
# There's a clear difference between both.
summary(ols)
summary(ols_robust)
# No correlation between predictors
vif(ols, type = "predictor")
# Save for Paper
saveRDS(ols, "results/tables/ols_robust.rds")

# get levels for trust data for which to calculate AME
levels <- get_current_trust_levels()
# create new data for prediction (only ruth_populism and trust vary, all other at mean)
pred_data <- datagrid(ruth_populism = c(1, 0), 
                      lagged_trust_share_linear_imp_1 = levels, 
                      model = ols_robust)
# calculate AME and CIs for predicted data
meff <- marg_effects(ols_robust)

# plot interaction
meff |> 
  ggplot(aes(x = lagged_trust_share_linear_imp_1, y = estimate)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dashed") +
  geom_ribbon(aes(ymin = cilower, ymax = ciupper, alpha = ci_size), fill = "darkslategrey") +
  geom_line(aes()) +
  geom_line() +
  labs(x = "Trust in Judiciary",
       y = "AME of Populism",
       caption = "") +
  scale_x_percent(limits = c(0, 1))  +
  scale_alpha_manual(values = c(0.7, 0.5, 0.2))

ggsave("results/graphs/ols_interaction.pdf",
       device = cairo_pdf,
       width = 10,
       height = 6.2)

### OLS BASE MODELS ----

# only independent variables
form <- gf({depv} ~ ruth_populism + lagged_trust_share_linear_imp_1)
baseols <- robust_ols(form)
summary(baseols)

# plus fixed-effects
form <- gf({depv} ~ ruth_populism + lagged_trust_share_linear_imp_1 + {fe})
baseols2 <- robust_ols(form)
summary(baseols2)

# plus controls
form <- gf({depv} ~ lagged_trust_share_linear_imp_1 + ruth_populism + {contr} + {fe})
baseols3 <- robust_ols(form)
summary(baseols3)

# controls, but no fixed-effects
form <- gf({depv} ~ lagged_trust_share_linear_imp_1 + ruth_populism + {contr})
baseols4 <- robust_ols(form)
summary(baseols4)


#### COMPARISON TABLE  ----

# rename coefs
coef_names <- c(
  "ruth_populism" = "Populist",
  "lagged_trust_share_linear_imp_1" = "Trust (lagged)",
  "trust_hat" = "Trûst",
  "executive" = "Executive Power",
  "surplus" = "Surplus Seats",
  "presidential" = "Presidential System",
  "gdp_log_lag" = "GDP per capita (lagged, log)",
  "gini" = "Gini",
  "v2juncind_mean_3" = "Judicial Independence (mean of three lags)",
  "lagged_trust_share_linear_imp_1 × ruth_populism " = "Trust (lagged) x Populist",
  "trust_hat × ruth_populism" = "Trûst x Populist"
)

# add info on FE
olsrows <- data.frame("Coefficients" = "Country FE",
                      "(1)" = "No",
                      "(2)" = "Yes",
                      "(3)" = "Yes",
                      "(4)" = "No",
                      "(5)" = "Yes")
attr(olsrows, "position") <- 15

# create table
# XXX The interaction effects os getting lost right now?!
modelsummary(list(baseols, baseols2, baseols3, baseols4, ols_robust),
             coef_rename = coef_names,
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             add_rows = olsrows
             #output = "latex"
             ) |> 
  kable_styling() |> 
  kable_classic_2() ->
  mainmodels

#unsure yet which is the best way to go - I think write Lines, I'm on that
save_kable(mainmodels, file = "results/tables/my_table.tex")

test <- as.character(mainmodels)
writeLines(test, "results/tables/test")
saveRDS(mainmodels, "results/tables/test.rds")

## RANDOM EFFECTS ----

contr = c("executive",
          "surplus",
          "presidential",
          "gdp_log_lag")

form <- gf({depv} ~ {indepv} + {contr})
rols <- plm(form,
            data = df_final,
            index = c("country", "year"),
            model = "random")
summary(rols)
coeftest(rols, 
         vcov = vcovHC(rols, 
                       type="HC4", 
                       cluster="group"))

## DYNAMIC MODEL ----

indepv = c("lagged_trust_share_linear_imp_1", 
           "ruth_populism")
dynamicv = c("firstchange5")
depv = "jud_replace_cont"
contr = c("executive",
          "surplus",
          "presidential",
          "gdp_log_lag")
fe = "country"

df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe, dynamicv)), year, jud_replace) |> 
  na.omit() ->
  df_final

form <- gf({depv} ~ {indepv} + {dynamicv} + {contr} +  {fe})
dols <- robust_ols(form)
summary(dols)

dynamicv = c("firstchange5",
             "firstchange5*lagged_trust_share_linear_imp_1*ruth_populism")
form <- gf({depv} ~ {indepv} + {dynamicv} + {contr} +  {fe})
dols <- robust_ols(form)
summary(dols)

marginaleffects(dols,
                variables = "ruth_populism",
                newdata = datagrid(ruth_populism = c(1, 0), 
                                   lagged_trust_share_linear_imp_1 = levels,
                                   firstchange5 = c(0,1))) |> 
  mutate(lower = estimate - 1.96 * std.error, 
         upper = estimate + 1.96 * std.error,
         lower_90 = estimate - 1.64 * std.error, 
         upper_90 = estimate + 1.64 * std.error,
         lower_99 = estimate - 3.58 * std.error, 
         upper_99 = estimate + 3.58 * std.error) ->
  meff

meff |> 
  ggplot(aes(x = lagged_trust_share_linear_imp_1, y = estimate)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkslategrey") +
  geom_line() +
  facet_wrap(~firstchange5) +
  labs(x = "",
       y = "AME of Populism",
       caption = "") +
  theme_minimal()

df_final |> 
  ggplot(aes(x=lagged_trust_share_linear_imp_1)) +
  geom_histogram() +
  facet_wrap(~firstchange5)

## 2SLS MODEL JUDICIAL REPLACEMENT ----

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})
m_iv <- ivreg(form_iv,
              data = df_final)
summary(m_iv)

# first stage
form_fs <- gf(lagged_trust_share_linear_imp_1 ~ ruth_populism + {contr} + {instr})
form_fs_null <- gf(lagged_trust_share_linear_imp_1 ~ ruth_populism + {contr} )
firststage <- plm(form_fs,
                  data = df_final,
                  index = c("country", "year"),
                  model = "within")
summary(firststage)

firststagenull <- plm(form_fs_null,
                      data = df_final,
                      index = c("country", "year"),
                      model = "within")

# tests for first stage

# simple F-test
waldtest(firststage, firststagenull)

# Second Stage
df_final$trust_hat <- as.vector(fitted(firststage))
indepv = c("trust_hat", 
           "ruth_populism",
           "trust_hat*ruth_populism")
form_ss <- gf({depv} ~ {indepv}  + {contr})
secondstage <- plm(form_ss,
                   data = df_final,
                   index = c("country", "year"),
                   model = "within")
summary(secondstage)

# Tests
# wu-hausmann
phtest(secondstage, ols)

levels_hat <- seq(
  min(df_final$trust_hat, na.rm = TRUE),
  max(df_final$trust_hat, na.rm = TRUE),
  0.05
)

modelsummary(list(firststage, secondstage),
)

form_ss <- gf({depv} ~ {indepv}  + {contr} + {fe})
secondstage <- lm(form_ss,
                  data = df_final)

marginaleffects(secondstage,
                variables = "ruth_populism",
                newdata = datagrid(ruth_populism = c(1, 0), 
                                   trust_hat = levels_hat)) |> 
  mutate(lower = estimate - 1.96 * std.error, 
         upper = estimate + 1.96 * std.error,
         lower_90 = estimate - 1.64 * std.error, 
         upper_90 = estimate + 1.64 * std.error,
         lower_99 = estimate - 3.58 * std.error, 
         upper_99 = estimate + 3.58 * std.error) ->
  meff

meff |> 
  ggplot(aes(x = trust_hat, y = estimate)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkslategrey") +
  geom_line() +
  labs(x = "",
       y = "AME of Populism",
       caption = "") +
  theme_minimal()

# COMPARISON ----

modelsummary(list("OLS" = ols, 
                  #"Second Stage" = secondstage, 
                  "Dynamic OLS" = dols, 
                  "Random Effects OLS" = rols),
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             coef_map = c(
               "ruth_populism" = "Populist",
               "lagged_trust_share_linear_imp_1" = "Trust (lagged)", 
               "trust_hat" = "Trûst",
               "executive" = "Executive Power",
               "surplus" = "Surplus Seats",
               "presidential" = "Presidential System",
               "gdp_log_lag" = "GDP per capita (lagged, log)",
               "gini" = "Gini",
               "v2juncind_mean_3" = "Judicial Independence (mean of three lags)",
               "lagged_trust_share_linear_imp_1 × ruth_populism" = "Trust (lagged) x Populist",
               "trust_hat × ruth_populism" = "Trûst x Populist"
               ),
             output = "latex"
             ) ->
  modelcomparison
modelcomparison
saveRDS(modelcomparison, "results/tables/modelcomparison_trust.rds")

# ROBUSTNESS ----

## REVERSE ----
reverseols <- lm(dplyr::lead(trust_share_linear_imp, 3) ~ jud_replace_con,
                 data = df4)
summary(reverseols)

reverseols2 <- lm(dplyr::lead(trust_share_linear_imp, 3) ~ jud_replace_con + as.factor(country),
              data = df4)
summary(reverseols2)

form <- gf(dplyr::lead(trust_share_linear_imp, 3) ~ jud_replace_con + {contr} + {fe})
reverseols3 <- lm(form,
                 data = df4)
summary(reverseols3)

form <- gf(dplyr::lead(trust_share_linear_imp, 3) ~ jud_replace_con + {contr} + {fe})
reverseols4 <- robust_ols(form)
summary(reverseols3)

## ----

form <- gf({depv} ~ lagged_trust_share_linear_imp_1*firstchange5 + {contr} + {fe})
baseols <- lm(form,
              data = df_final)
summary(baseols)

form <- gf({depv} ~ lagged_trust_share_linear_imp_1*firstchange5 + {fe})
baseols <- lm(form,
              data = df_final)
summary(baseols)

df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), laterchange, year) |> 
  na.omit() ->
  df_final

df_final |> 
  filter(laterchange == 0) ->
  df_final_onlyfirst

form_iv <- gf({depv} ~ lagged_trust_share_linear_imp_1 + {contr} + {fe} | {contr} + {fe} + {instr})
m_iv <- ivreg(form_iv,
              data = df_final_onlyfirst)
summary(m_iv)

### PROBIT ----

depv = "jud_replace_con"
df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year) |> 
  na.omit() ->
  df_final
form <- gf({depv} ~ lagged_trust_share_linear_imp_1 + {contr} + {fe})
probit <- glm(form,
              data = df_final,
              family = binomial(link = "probit"))
summary(probit)
vif(baseols, type = "predictor")

df_final |> 
  filter(ruth_populism == 1 & jud_replace_con == 1) |> 
  select(country, year) |>  View()

### change depv ----

depv = "jud_replace_cont_mean"

df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe))) |> 
  na.omit() ->
  df_final

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})
m_ivr1<- ivreg(form_iv,
                  data = df_final)

### change instr lag  ----

depv = "jud_replace_cont"
instr = c("v2juaccnt_mean_5",
          "v2jucorrdc_mean_5",
          "v2jucorrdc_mean_5*ruth_populism",
          "v2juaccnt_mean_5*ruth_populism")

df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe))) |> 
  na.omit() ->
  df_final

form_iv  <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})
m_ivr2 <- ivreg(form_iv,
               data = df_final)

### change trust imputation lag  ----

depv = "jud_replace_cont"
indepv = c("lagged_trust_share_imp_lastv_1", 
           "ruth_populism",
           "lagged_trust_share_imp_lastv_1*ruth_populism")
contr = c("evnt_sum_lag3",
          "executive",
          "senior_length")
instr = c("v2juaccnt_mean_3",
          "v2jucorrdc_mean_3",
          "v2jucorrdc_mean_3*ruth_populism",
          "v2juaccnt_mean_3*ruth_populism")

df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe))) |> 
  na.omit() ->
  df_final

form_iv  <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})
m_ivr3 <- ivreg(form_iv,
                data = df_final)


indepv = c("lagged_trust_share_1", 
           "ruth_populism",
           "lagged_trust_share_1*ruth_populism")

df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe))) |> 
  na.omit() ->
  df_final

form_iv  <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})
m_ivr4 <- ivreg(form_iv,
                data = df_final)


modelsummary(list(m_ivtest, m_ivr1, m_ivr2, m_ivr3, m_ivr4))
                