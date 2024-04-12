# ANALYSIS TRUST

# Set-up ----

## Libraries ----
library(tidyverse)
library(stargazer)
library(patchwork)
library(ivreg)
library(glueformula)

## Graphics ----

source("src/graphics.R")

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
         lag_respect_con = lag(v2exrescon, 1),
         lagged_v2x_libdem = lag(v2x_libdem, 2),
         country = as.factor(country)) |> 
  mutate(across(starts_with("v2x"), 
                .fns = ~ . - lag(.),
                .names = "lagged_{.col}")) ->
  df4

# ANALYSIS JUDICIAL REPLACEMENT ----

## SET-UP

depv = "jud_replace_cont"
indepv = c("lagged_trust_share_linear_imp_1", 
           "ruth_populism",
           "lagged_trust_share_linear_imp_1*ruth_populism"
           )
contr = c("evnt_sum_lag3",
          "presidential",
          "senior_length")
instr = c("v2juaccnt_mean_3",
          # "v2jucorrdc_mean_3",
          # "v2jucorrdc_mean_3*ruth_populism",
          "v2juaccnt_mean_3*ruth_populism")
fe = "country"

df4 |> 
  filter(!country %in% c("Norway", "Moldova", "Ukraine", "Bosnia and Herzegovina", "Switzlerand", "Belarus")) |> 
  select(any_of(c(depv, indepv, contr, fe, instr))) |> 
  na.omit() ->
  df_final

## OLS MODEL ----

form <- gf({depv} ~ {indepv} + {contr} +  {fe})
ols <- lm(form,
          data = df_final)
summary(ols)

levels <- seq(
  min(df_final$lagged_trust_share_linear_imp_1),
  max(df_final$lagged_trust_share_linear_imp_1),
  0.05
)

ols |> 
  margins(
  variables = "ruth_populism",
    at = list(lagged_trust_share_linear_imp_1 = levels)
  ) |> 
    summary() |> 
    mutate(lower = AME - 1.96 * SE, 
           upper = AME + 1.96 * SE,
           lower_90 = AME - 1.64 * SE, 
           upper_90 = AME + 1.64 * SE,
           lower_99 = AME - 3.58 * SE, 
           upper_99 = AME + 3.58 * SE) ->
  meff

meff |> 
  ggplot(aes(x = lagged_trust_share_linear_imp_1, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  geom_line() +
  labs(x = "",
       y = "AME of Populism",
       caption = "") +
  scale_x_continuous(limits = c(0,1)) +
  theme_minimal()

df_final |> 
  ggplot(aes(x = lagged_trust_share_linear_imp_1, 
             fill = as.factor(ruth_populism),
             group = ruth_populism)) +
  geom_histogram()


## 2SLS MODEL JUDICIAL REPLACEMENT ----

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})

m_ivtest <- ivreg(form_iv,
              data = df_final)
summary(m_ivtest)


m_ivtest |> 
  margins(
    variables = "ruth_populism",
    at = list(lagged_trust_share_1 = levels)
  ) |> 
  summary() |> 
  mutate(lower = AME - 1.96 * SE, 
         upper = AME + 1.96 * SE,
         lower_90 = AME - 1.64 * SE, 
         upper_90 = AME + 1.64 * SE,
         lower_99 = AME - 3.58 * SE, 
         upper_99 = AME + 3.58 * SE) ->
  meff

meff |> 
  filter(lagged_trust_share_1 < 0.5) |> 
  ggplot(aes(x = lagged_trust_share_1, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  geom_line() +
  labs(x = "",
       y = "AME of Populism",
       caption = "") +
  scale_x_continuous(limits = c(0,1)) +
  theme_minimal()

qqPlot(m_ivtest)
outlierTest(m_ivtest)
influencePlot(m_ivtest)
avPlots(m_ivtest)

pred_data <- datagrid(lagged_trust_share_1 = predictions(m_ivtest)$lagged_trust_share_1,
         ruth_populism = unique(df_final$ruth_populism),
         model = m_ivtest)

predictions(m_ivtest, newdata = pred_data) |> 
  ggplot(aes(x = lagged_trust_share_1,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = factor(ruth_populism),
             fill = factor(ruth_populism))) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 0.5)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA) +
  labs(y = "Predicted Values for Judicial Replacement") +
  theme_minimal()

## MANUAL SET-UP ----

form_fs <- gf(lagged_trust_share_linear_imp_1 ~ ruth_populism + {contr} + {fe} + {instr})
firststage <- lm(form_fs,
              data = df_final)
summary(firststage)

df_final$fitted <- as.vector(fitted(firststage))

form_fs <- gf({depv} ~ ruth_populism + fitted + ruth_populism*fitted + {contr} + {fe})
secondstage <- lm(form_fs, df_final)
summary(secondstage)

secondstage_boot <- Boot(secondstage)

modelsummary(list(secondstage, m_iv))

levels <- seq(
  min(df_final$fitted, na.rm = TRUE),
  max(df_final$fitted, na.rm = TRUE),
  0.05
)

secondstage |> 
  margins(
    variables = "ruth_populism",
    at = list(fitted = levels)
  ) |>  View()
  summary() |> View()
  mutate(lower = AME - 1.96 * SE, 
         upper = AME + 1.96 * SE,
         lower_90 = AME - 1.64 * SE, 
         upper_90 = AME + 1.64 * SE,
         lower_99 = AME - 3.58 * SE, 
         upper_99 = AME + 3.58 * SE) ->
  meff

meff |> 
  ggplot(aes(x = fitted, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  geom_line() +
  labs(x = "",
       y = "AME of Populism",
       caption = "") +
  theme_minimal()

## WITHOUT INTERACTION ----

indepv = c("lagged_trust_share_1")
contr = c("ruth_populism", contr)
instr = c("v2juaccnt_mean_3", 
          "v2jucorrdc_mean_3")

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})

m_ivtest <- ivreg(form_iv,
                  data = df4)
summary(m_ivtest)


pred_data <- datagrid(ruth_populism = unique(df5$ruth_populism),
                      model = m_ivtest)

predictions(m_ivtest, newdata = pred_data) |> 
  ggplot(aes(x = ruth_populism,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = factor(ruth_populism),
             fill = factor(ruth_populism))) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_pointrange() 

## 2SLS WITH SPLITTING JUDICIAL REPLACEMENT ----

### Set-Up ----

indepv = c("lagged_trust_share_linear_imp_1")
instr = c("v2juaccnt_mean_3",
          "v2juncind_mean_5")

#### Splitting ----

df_pop <- df_final |>  
  filter(ruth_populism == 1) 

df_nop <- df_final |>  
  filter(ruth_populism == 0) 

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})

m_iv_pop <- ivreg(form_iv,
                  data = df_pop)
summary(m_iv_pop)

m_iv_nop <- ivreg(form_iv,
                  data = df_nop)
summary(m_iv_nop)

pred_data_pop <- datagrid(lagged_trust_share_1 = predictions(m_iv_pop)$lagged_trust_share_1,
                      model = m_iv_pop)
pred_data_nop <- datagrid(lagged_trust_share_1 = predictions(m_iv_nop)$lagged_trust_share_1,
                          model = m_iv_nop)

predictions(m_iv_pop, newdata = pred_data_pop) |> 
  ggplot(aes(x = lagged_trust_share_1,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA)+
  labs(title = "Populist")

predictions(m_iv_nop, newdata = pred_data_nop) |> 
  ggplot(aes(x = lagged_trust_share_1,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA) +
  labs(title = "Non-Populist")


#### Splitting & Random Effects ----

df4_pop <- df4 |>  
  filter(ruth_populism == 1) 

df4_nop <- df4 |>  
  filter(ruth_populism == 0) 

df_nop |> 
  mutate(id = country) ->
  df_nop

tslsform <- gf({depv} ~ {indepv}  + {contr}  | {contr}  + {instr})
library(plm)
re_nop <- plm(tslsform, 
           data = df_final,
           model = "random",
           random.method = "ht", 
           inst.method = "baltagi")
summary(re_nop)

re_pop <- plm(tslsform, 
              data = df4_pop,
              model = "random",
              random.method = "ht", 
              inst.method = "baltagi")
summary(re_pop)

pred_data_pop <- datagrid(lagged_trust_share_1 = predictions(re_pop)$lagged_trust_share_1,
                          model = re_pop)
pred_data_nop <- datagrid(lagged_trust_share_1 = predictions(re_nop)$lagged_trust_share_1,
                          model = re_nop)

predictions(m_iv_pop, newdata = pred_data_pop) |> 
  ggplot(aes(x = lagged_trust_share_1,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA)+
  labs(title = "Populist")

predictions(m_iv_nop, newdata = pred_data_nop) |> 
  ggplot(aes(x = lagged_trust_share_1,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA) +
  labs(title = "Non-Populist")

# ANALYSIS COMPLIANCE
## 2SLS Model COMPLIANCE ----

depv = "v2juhccomp"
indepv = c("lagged_trust_share_1", 
           "ruth_populism",
           "lagged_trust_share_1*ruth_populism")
instr = c(contr, 
          fe,
          "v2juaccnt_mean_3", 
          "v2jucorrdc_mean_3",
          "v2jucorrdc_mean_3*ruth_populism",
          "v2juaccnt_mean_3*ruth_populism")

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {instr})

m_ivtest <- ivreg(form_iv,
                  data = df4)
summary(m_ivtest)

pred_data <- datagrid(lagged_trust_share_1 = predictions(m_ivtest)$lagged_trust_share_1,
                      ruth_populism = unique(df4$ruth_populism),
                      model = m_ivtest)

predictions(m_ivtest, newdata = pred_data) |> 
  ggplot(aes(x = lagged_trust_share_1,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = factor(ruth_populism),
             fill = factor(ruth_populism))) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA)

### Set-Up ----

indepv = c("lagged_trust_share_1")
instr = c(contr, 
          fe,
          "v2juhcind_mean_3", 
          "v2juaccnt_mean_3", 
          "v2jucorrdc_mean_3")

#### Splitting ----

df4_pop <- df4 |>  
  filter(ruth_populism == 1) 

df4_nop <- df4 |>  
  filter(ruth_populism == 0) 

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {instr})

m_iv_pop <- ivreg(form_iv,
                  data = df4_pop)
summary(m_iv_pop)

m_iv_nop <- ivreg(form_iv,
                  data = df4_nop)
summary(m_iv_nop)

pred_data_pop <- datagrid(lagged_trust_share_1 = predictions(m_ivtest)$lagged_trust_share_1,
                          model = m_iv_pop)
pred_data_nop <- datagrid(lagged_trust_share_1 = predictions(m_ivtest)$lagged_trust_share_1,
                          model = m_iv_nop)

predictions(m_iv_pop, newdata = pred_data_pop) |> 
  ggplot(aes(x = lagged_trust_share_1,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA)+
  labs(title = "Populist")

predictions(m_iv_nop, newdata = pred_data_nop) |> 
  ggplot(aes(x = lagged_trust_share_1,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA) +
  labs(title = "Non-Populist")

# Backup stuff


## REVERSE ----

reverseols <- plm(lead(trust_share_low_linear_imp_mean_3, 3) ~ jud_replace_cont,
                  data = df)
summary(reverseols)

reverseols2 <- plm(lead(trust_share_low_linear_imp_mean_3, 3) ~ jud_replace_cont + as.factor(country),
                   data = df)
summary(reverseols2)

form <- gf(dplyr::lead(trust_share_low_linear_imp_mean_3, 3) ~ jud_replace_cont + {contr} + {fe})
reverseols3 <- plm(form,
                   data = df)
summary(reverseols3)


## 2SLS MODEL JUDICIAL REPLACEMENT ----

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})
m_iv <- ivreg(form_iv,
              data = df_final)
summary(m_iv)

# first stage
form_fs <- gf(trust_share_low_linear_imp_mean_3 ~ ruth_populism + {contr} + {instr})
form_fs_null <- gf(trust_share_low_linear_imp_mean_3 ~ ruth_populism + {contr} )
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

modelsummary(list(firststage, secondstage))

form_ss <- gf({depv} ~ {indepv}  + {contr} + {fe})
secondstage <- lm(form_ss,
                  data = df_final)

slopes(secondstage,
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
               "trust_share_low_linear_imp_mean_3" = "Trust (lagged)", 
               "trust_hat" = "Trûst",
               "executive" = "Executive Power",
               "surplus" = "Surplus Seats",
               "presidential" = "Presidential System",
               "gdp_log_lag" = "GDP per capita (lagged, log)",
               "gini" = "Gini",
               "v2juncind_mean_3" = "Judicial Independence (mean of three lags)",
               "trust_share_low_linear_imp_mean_3 × ruth_populism" = "Trust (lagged) x Populist",
               "trust_hat × ruth_populism" = "Trûst x Populist"
             ),
             output = "latex"
) ->
  modelcomparison
modelcomparison
saveRDS(modelcomparison, "results/tables/modelcomparison_trust.rds")

# ROBUSTNESS ----

## REVERSE ----
reverseols <- plm(lead(trust_share_low_linear_imp, 3) ~ jud_replace_con,
                  data = df)
summary(reverseols)

reverseols2 <- plm(lead(trust_share_low_linear_imp, 3) ~ jud_replace_con + as.factor(country),
                   data = df)
summary(reverseols2)

form <- gf(dplyr::lead(trust_share_low_linear_imp, 3) ~ jud_replace_con + {contr} + {fe})
reverseols3 <- plm(form,
                   data = df)
summary(reverseols3)


## ----

form <- gf({depv} ~ trust_share_low_linear_imp_mean_3*changelast5 + {contr} + {fe})
baseols <- lm(form,
              data = df_final)
summary(baseols)

form <- gf({depv} ~ trust_share_low_linear_imp_mean_3*changelast5 + {fe})
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

form_iv <- gf({depv} ~ trust_share_low_linear_imp_mean_3 + {contr} + {fe} | {contr} + {fe} + {instr})
m_iv <- ivreg(form_iv,
              data = df_final_onlyfirst)
summary(m_iv)

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
indepv = c("lagged_trust_share_low_imp_lastv_1", 
           "ruth_populism",
           "lagged_trust_share_low_imp_lastv_1*ruth_populism")
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


indepv = c("lagged_trust_share_low_1", 
           "ruth_populism",
           "lagged_trust_share_low_1*ruth_populism")

df4 |> 
  select(any_of(c(depv, indepv, contr, instr, fe))) |> 
  na.omit() ->
  df_final

form_iv  <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {fe} + {instr})
m_ivr4 <- ivreg(form_iv,
                data = df_final)


modelsummary(list(m_ivtest, m_ivr1, m_ivr2, m_ivr3, m_ivr4))


### PROBIT ----

depv = "jud_replace_con"
df |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year) |> 
  na.omit() ->
  df_final
form <- gf({depv} ~ trust_share_low_linear_imp_mean_3 + {contr} + {fe})
probit <- glm(form,
              data = df_final,
              family = binomial(link = "probit"))
summary(probit)
vif(baseols, type = "predictor")

df_final |> 
  filter(ruth_populism == 1 & jud_replace_con == 1) |> 
  select(country, year) |>  View()

