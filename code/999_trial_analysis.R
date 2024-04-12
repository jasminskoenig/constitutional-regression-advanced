# 9999 Trial Analysis
library(systemfit)
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
library(margins)
library(patchwork)
library(ivreg)

# Graphics ----

## Theme for Plots ----

theme_gridY <- theme_ipsum_rc(grid = "") +
  theme(
    axis.title.x = element_text(size = 20, family = "rajdhani"),
    axis.text.x = element_text(size = 20, family = "rajdhani"),
    axis.text.y = element_markdown(size = 20, family = "rajdhani"),
    axis.title.y.left = element_text(size = 20, family = "rajdhani"),
    strip.text = element_text(size = 24, family = "rajdhani"),
    plot.title = element_text(size = 21, family = "rajdhani"),
    plot.subtitle = element_text(size = 21, family = "rajdhani"),
    plot.caption = element_text(size = 28, family = "rajdhani", face = "plain", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 21, family = "rajdhani"),
    legend.title = element_text(size = 20, family = "rajdhani"),
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

# Functions ----

### Functions ----

reg_models <- function(dv, iv, moderator){
  
  # Runs base, mixed-effect and fixed-effect models to predict constitutional change
  # Returns list of regressions
  # Single regressions can be accessed like this: models$`Mixed Interaction` afterwards
  
  models <- list(
    "Base" = lm(dv ~ iv, data = df5),
    "Mixed" = lmer(dv ~ iv + (1 | country), data = df5),
    "Mixed Controls" = lmer(dv ~ iv + moderator  + v2xnp_pres + jud_ind_mean5 + evnt_sum_lag3 + executive + lag(trust_lm) + (1 | country), data = df5),    
    "Mixed Interaction" = lmer(dv ~ iv * moderator  + moderator  + jud_ind_mean5 + evnt_sum_lag3 + v2xnp_pres + executive + lag(trust_lm) +  (1 | country), data = df5),  
    "Fixed" = lm(dv ~ iv + country, data = df5),
    "Fixed Controls" = lmer(dv ~ iv + moderator  + v2xnp_pres + jud_ind_mean5 + evnt_sum_lag3 + executive + lag(trust_lm) +  country, data = df5),    
    "Fixed Interaction" = lmer(dv ~ iv * moderator  + moderator  + jud_ind_mean5 + evnt_sum_lag3 + v2xnp_pres + executive + lag(trust_lm) +  country, data = df5)
  )
  
  return(models)
  
}


reg_table <- function(modellist){
  
  # Creates a table to compare all Models estimated in reg_evnt_models
  

  #attr(rows, 'position') <- 9
  
  table <- modelsummary(
    modellist,
    fmt = 1,
    estimate  = "{estimate}{stars}",
    statistic = 'conf.int',
    coef_omit = "(Intercept|^country)",
    #add_rows = rows,
    coef_rename = c("iv1" = "Populist Government", 
                    "v2xnp_pres" = "Presidentialism Score",
                    "lagged_v2x_libdem" = "Lagged Democray (2)",
                    "coalition" = "Coalition",
                    "executive" = "Executive Power",
                    "surplus" = "Surplus Seats",
                    "evnt_sum_lag3" = "Constitutional Changes (3 years)",
                    "jud_ind_mean5" = "Judicial Independence (mean of 5 lags)",
                    "moderator" = "High Court Trust (Lag, 1)",
                    "lag(trust_lm)" = "Predicted High Court Trust (Lag, 1)",
                    "ruth_populism1xlag_trust_share" = "Populism x High Court Trust (Lag, 1)",
                    "no_govparties" = "No Gov Parties",
                    "lag_respect_con" = "Respect Constitution (Lag, 1)",
                    "mean_of_demlags" = "Mean of Lag Liberal Democracy - 5",
                    "mean_of_trustlegs" = "Mean of Lag Share Trust in High Court - ",
                    "trust_share" = "Share Trust in High Court",
                    "lag_trust_mean" = "High Court Trust (Lag, 1)"))
  
  return(table)
  
}

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
  mutate(latin = if_else(e_regiongeo %in% c(17, 18), 1, 0),
         lag_respect_con = lag(v2exrescon, 1),
         lagged_v2x_libdem = lag(v2x_libdem, 2),
         country = as.factor(country)) |> 
  mutate(across(starts_with("v2x"), 
                .fns = ~ . - lag(.),
                .names = "lagged_{.col}")) ->
  df4

# correlation of dependent variables
# 
# df4 |> 
#   select(diff_judind, evnt, v2exrescon, surplus, gov_left, singleparty_gov, gov_popul_weighted, amendment, amendmaj, v2juhcind, senior_length, gov_length, trust_mean, jud_reg) |> 
#   filter(!if_any(everything(), ~is.na(.x))) ->
#   df4
# 
# 
# df4 |> 
#   filter(!is.na(jud_reg) & !is.na(evnt) & !is.na(v2exrescon)) |> 
#   select(jud_reg, evnt, v2exrescon) ->
#   df_cor
# cor(df_cor)
# corr <- lm(judreg ~ evnt, data = df4)
# summary(corr)
# 
# f1 <- lead(evnt,1) ~ gov_popul_weighted + surplus + gov_left + jud_reg + senior_length
# f2 <- lead(v2exrescon, 1)  ~ gov_popul_weighted + singleparty_gov  + gov_left + jud_reg
# 
# # SEM ----
# 
# D <- lead(evnt,1) ~ gov_popul_weighted  + gov_left  + senior_length + surplus 
# S <- lead(v2exrescon, 1)  ~ gov_popul_weighted + singleparty_gov  + gov_left + v2juhcind + trust_mean + jud_reg
# sys <- list(D,S)
# instr <- ~ singleparty_gov + surplus + senior_length + v2juhcind + trust_mean
# truff.sys <- systemfit(sys, inst=instr, 
#                        method="2SLS", data=df4)
# summary(truff.sys)
# 
# # Step 1. Estimate reduced-form parameters
# ev <- lm(lead(evnt,1) ~  amendment + senior_length + surplus + no_govparties + as.factor(country), data = ccpc_vdem)
# res <- lm(lead(jud_replace_con, 1)  ~ , data = df4)
# summary(ev)
# summary(res)
# 
# rownos <- row.names(model.frame(ev))
# df4 |>  
#   filter(row_number() %in% rownos) ->
#   df4
# 
# # Step 2. Use the predicted value of P and plug into the right-hand side of the structural equations. 
# df4$phatev <- ev$fitted.values
# 
# rownos <- row.names(model.frame(res))
# df4 |>  
#   filter(row_number() %in% rownos) ->
#   df4
# df4$phatres <- res$fitted.values
# 
# ev.lm <- lm(lead(evnt,1) ~ phatres + gov_popul_weighted*surplus + gov_left + lag(jud_reg), data = df4)
# res.lm <- lm(lead(v2exrescon, 1)  ~ phatev + gov_popul_weighted + gov_left + singleparty_gov + lag(jud_reg), data = df4)
# 
# summary(ev.lm)
# summary(res.lm)
# 
# # sur
# 
# r1 <- lm(f1, 
#          data = df4)
# summary(r1)
# r2 <- lm(f2,
#          data = df4)
# summary(r2)
# r3 <- lm(f3,
#          data = df4)
# summary(r3)
# 
# fitsur <- systemfit(list(evntreg = f1, 
#                          respectreg = f2,
#                          independencreg = f3), 
#                     data = df4)
# summary(fitsur)

# NEW IDEA ----

# Model trust

df4 |> 
  group_by(country) |> 
  mutate(jud_corr_lag = lag(v2jucorrdc, 1),
         jud_ind_lag = lag(v2juhcind, 1),
         jud_ind_lag2 = lag(v2juhcind, 2),
         jud_ind_lag3 = lag(v2juhcind, 3),
         jud_ind_lag4 = lag(v2juhcind, 4),
         jud_ind_lag5 = lag(v2juhcind, 5),
         ruth_populism = as.factor(ruth_populism),
         lag_trust_share = lag(trust_share)) |>  
  rowwise() |> 
  mutate(jud_ind_mean5 = mean(c(jud_ind_lag, jud_ind_lag2, jud_ind_lag3, jud_ind_lag4, jud_ind_lag5)),
         jud_ind_mean2 = mean(c(jud_ind_lag, jud_ind_lag2))) |> 
  ungroup() ->
  df4

# closest to stata ----
m_iv <- ivreg(jud_replace ~ lag_trust_share + ruth_populism + lag_trust_share*ruth_populism |
                jud_ind_mean5 + ruth_populism*jud_ind_mean5 + jud_corr_lag + ruth_populism*jud_corr_lag,
              data = df4)
summary(m_iv)

# IV Interactions ----
m_iv <- ivreg(jud_replace ~ lag_trust_share + ruth_populism + lag_trust_share * ruth_populism |
                ruth_populism + jud_ind_mean5 + ruth_populism*jud_ind_mean5 + jud_corr_lag + ruth_populism*jud_corr_lag,
              data = df4)
summary(m_iv)

predictions(m_iv) |> 
  distinct(lag_trust_share, ruth_populism, .keep_all = TRUE) |> 
  ggplot(aes(x = lag_trust_share,
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

# Splitting ----

df4_pop <- df4 |>  
  filter(ruth_populism == 1) 

df4_nop <- df4 |>  
  filter(ruth_populism == 0) 

m_iv_pop <- ivreg(jud_replace ~ lag_trust_share |
                jud_ind_mean5 + jud_corr_lag,
              data = df4_pop)
summary(m_iv_pop, diagnostics = TRUE)

m_iv_nop <- ivreg(jud_replace ~ lag_trust_share |
                    jud_ind_mean5 + jud_corr_lag,
                  data = df4_nop)
summary(m_iv_nop, diagnostics = TRUE)

predictions(m_iv_pop)  |> 
  distinct(lag_trust_share, .keep_all = TRUE) |> 
  ggplot(aes(x = lag_trust_share,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA)

predictions(m_iv_nop)  |> 
  distinct(lag_trust_share, .keep_all = TRUE) |> 
  ggplot(aes(x = lag_trust_share,
             y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  geom_hline(yintercept = 0,
             color = "#C95D63",
             linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA)


new_data |> 
  filter(ruth_populism == 0) ->
  new_data_nop

test <- predict(m_iv_nop, interval="confidence", level=0.99)
test <- predict(mod2, newdata=newdf, interval="prediction", level=0.99)
predicitions_nop <- predict(m_iv_nop,
                terms = "instruments")
predicitions_nop <- predict(m_iv_nop,
                            terms = "instruments")
predicitions_pop <- predict(m_iv_pop,
                            terms = "instruments")

new_data |> 
  filter(ruth_populism == 1) ->
  new_data_pop

modelplot(m_iv_pop, coef_omit = "Intercept")
modelplot(m_iv_nop, coef_omit = "Intercept")

df5 |> 
  filter(ruth_populism == 1) ->
  df5_pop

new_data <- expand.grid(lag_trust_share = seq(min(df5$fitted_firststage, na.rm = TRUE), 
                                              max(df5$fitted_firststage, na.rm = TRUE), 
                                              length.out = 100),
                        ruth_populism = unique(df5$ruth_populism),
                        jud_ind_mean5 = seq(min(df5$jud_ind_mean5, na.rm = TRUE), 
                                            max(df5$jud_ind_mean5, na.rm = TRUE), 
                                            length.out = 100),
                        jud_corr_lag = seq(min(df5$jud_corr_lag, na.rm = TRUE), 
                                           max(df5$jud_corr_lag, na.rm = TRUE), 
                                           length.out = 100))
predict(
  m_iv_pop,
  type = "response",
  se.fit = FALSE,
  interval = "confidence",
  level = 0.95
)

ggPredict(m_iv_pop,
          se=TRUE,
          interactive=FALSE)

# MANUAL IMPLEMENTATION WITH BOOTSTRAPPING ----

bootstrap_2sls <- function(data, new_data, n_boot = 1000) {
  set.seed(123) # For reproducibility
  predictions <- replicate(n_boot, {
    # Resample data
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    
    # Fit model
    boot_model <- lm(jud_replace_con ~ ruth_populism + trust_lm + ruth_populism*trust_lm,
                    data = boot_data)
    
    # Predict values
    predict(boot_model, newdata = new_data)
    
    # with marginaleffects
    predictions(boot_model)
  })

  # Convert predictions to a data frame
  pred_df <- as.data.frame(predictions)
  
  # Calculate the mean and confidence intervals
  out <- pred_df %>% 
    t() |> 
    as.data.frame() |> 
    summarise(across(everything(), list(mean = mean, 
                                        lower95 = ~quantile(., probs = 0.05), 
                                        upper95 = ~quantile(., probs = 0.95),
                                        lower99 = ~quantile(., probs = 0.01), 
                                        upper99 = ~quantile(., probs = 0.99),
                                        lower90 = ~quantile(., probs = 0.1), 
                                        upper90 = ~quantile(., probs = 0.9)))) |> 
    pivot_longer(cols = everything(),
                 names_to = c("var", "content"),
                 names_sep = "_",
                 values_to = "value") |> 
    pivot_wider(id_cols = var,
                names_from = content,
                values_from = value) |> 
    bind_cols(new_data)
  
  return(predictions)
}

new_data <- expand.grid(trust_lm = seq(min(df5$trust_lm, na.rm = TRUE), 
                                              max(df5$trust_lm, na.rm = TRUE), 
                                              length.out = 100),
                        ruth_populism = unique(df5$ruth_populism))

newbootstrap_2sls_predictions <- function(data, predictiondata, n_boot = 1000) {
  set.seed(123)  # For reproducibility
  
  # Prepare a matrix to store bootstrap predictions
  coefficient <- predictiondata
  low <- predictiondata
  high <- predictiondata
  
  for (i in 1:n_boot) {
    
    # Resample data with replacement
    boot_data <- data[sample(nrow(df5), replace = TRUE), ]
    
    # Fit model
    boot_model <- lm(jud_replace_con ~ ruth_populism + trust_lm + ruth_populism*trust_lm,
                     data = boot_data)
    
    # Use `predictions` from `marginaleffects` to get predicted values for new_data
    preds <- predictions(boot_model, newdata = predictiondata)
    
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

bootstrap_results <- newbootstrap_2sls_predictions(df5, new_data)

# plot new bootstrap 

bootstrap_results |> 
  ggplot(aes(x = trust_lm,
             y = coefficient,
             ymin = low,
             ymax = high)) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_line(aes(color = ruth_populism)) +
  geom_ribbon(aes(fill = ruth_populism), 
              alpha = 0.2)

lmtrust <- lm(lag_trust_share ~ lag(v2jucorrdc) + jud_ind_mean5 + ruth_populism + jud_ind_mean5*ruth_populism + lag(v2jucorrdc)*ruth_populism,
   data = df4)

df4 |> 
  filter(!is.na(jud_corr_lag), !is.na(ruth_populism),!is.na(jud_ind_mean5), !is.na(lag_trust_share)) ->
  df5

# Extracting fitted values from the lmer model
fitted_values <- as.vector(fitted(lmtrust))
# Adding these fitted values to your original dataframe
df5$trust_lm <- fitted_values

secondstage <- lm(jud_replace_con ~ ruth_populism + trust_lm + ruth_populism*trust_lm,
                  data = df5)

summary(secondstage)

new_data <- expand.grid(trust_lm = seq(min(df5$trust_lm, na.rm = TRUE), 
                                              max(df5$trust_lm, na.rm = TRUE), 
                                              length.out = 100),
                        ruth_populism = unique(df5$ruth_populism))

# Run and plot old bootstrap bootstrap
bootstrap_results <- bootstrap_2sls(df5, new_data)

bootstrap_results |> 
  pivot_longer(
    cols = contains("9"),
    names_to = "ci",
    values_to = "ci_value"
  ) |>
  mutate(ci_intervall = str_extract(ci, "\\d{2}"),
         ci = str_remove(ci, "\\d{2}")) |> 
  pivot_wider(id_cols = c(mean, trust_lm, ruth_populism, ci_intervall),
              names_from = ci,
              values_from = ci_value) |> 
  filter(ruth_populism == 1) ->
  bootstrap_forplot
  

ggplot(bootstrap_forplot,
       aes(x = trust_lm, 
                     y = mean, 
                     ymin = lower, 
                     ymax = upper)) +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             color = "red") +
  geom_ribbon(aes(alpha = ci_intervall),
              fill = "darkslategrey") +
  geom_line(color = "black") +
  labs(title = "Bootstrap CI for Predicted Chane in Court Composition by Fitted Trust and Populists in Power",
       x = "First Stage Fitted Values for Trust", 
       y = "Predicted Change in Court Composition") +
  scale_alpha_manual(values = c(0.6, 0.4, 0.2)) +
  theme_minimal()

predictions(secondstage) |> 
  distinct(trust_lm, ruth_populism, .keep_all = TRUE) |> 
  ggplot(aes(x = trust_lm,
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


#-----------------

firststage <- lm(trust_share ~ ruth_populism  + v2xnp_pres + evnt_sum_lag3 + executive + lag(v2jucorrdc) + jud_ind_mean5 + as.factor(country),
           data = df4)
summary(firststage)

df4 |> 
  filter(!is.na(jud_corr_lag), !is.na(ruth_populism), !is.na(executive), !is.na(jud_ind_mean5), !is.na(trust_share), !is.na(jud_ind_mean5), !is.na(evnt_sum_lag3), !is.na(lag(v2jucorrdc)), !is.na(v2xnp_pres)) ->
  df5

# Extracting fitted values from the lmer model
fitted_values <- as.vector(fitted(firststage))
# Adding these fitted values to your original dataframe
df5$trust_lm <- fitted_values

secondstage <- lm(jud_replace_con ~ ruth_populism  + v2xnp_pres + evnt_sum_lag3 + executive + lag(v2jucorrdc) + jud_ind_mean5 + as.factor(country) + ruth_populism*trust_lm,
                 data = df5)
summary(secondstage)

## 2SLS Option A:

eq1 <- trust_share ~ ruth_populism  
eq2 <- jud_replace_con ~ ruth_populism  + ruth_populism*trust_share
inst <- ~ v2jucorrdc + jud_ind_mean5
system <- list( eq1 = eq1, eq2 = eq2 )
reg2SLS <- systemfit(system, "2SLS", inst = inst, data = df5 )

## 2SLS ----

m_iv <- ivreg(jud_replace_con ~ ruth_populism  + v2xnp_pres + jud_ind_mean5 + evnt_sum_lag3 + executive  
              | lag_trust_share  
              | lag(v2jucorrdc, 2) + jud_ind_mean5, 
              data = df4)
summary(m_iv)

library(fixest)

model_iv_fe <- feols(jud_replace_con ~ ruth_populism  + v2xnp_pres + evnt_sum_lag3 + executive + lag_trust_share*ruth_populism
                     # FE
                     | country 
                     # First Stage
                     | lag_trust_share ~ lag(v2jucorrdc, 2) + lag(jud_ind_mean5), 
                     data = df4, 
                     panel.id = ~ country)
summary(model_iv_fe, stage = 2)


library(haven)

write_dta(df5, "data/statadata.dta")


df5 |> 
  mutate(lag_trust_lm = lag(trust_lm)) ->
  df5
lmcourts <- reg_evnt_models(df5$jud_replace_con,
                            df5$ruth_populism,
                            df5$lag_trust_share)
model <- lmcourts$`Mixed Interaction`
summary(model)
reg_evnt_table(model)

levels <- seq(
  min(df5$lag_trust_share, na.rm = TRUE),
  max(df5$lag_trust_share, na.rm = TRUE),
  0.05
)

changelmer = lmer(jud_replace_con ~ ruth_populism * lag_trust_share  + lag_trust_share  + ruth_populism + jud_ind_mean5 + evnt_sum_lag3 + v2xnp_pres + executive + lag_trust_lm +  (1 | country), data = df5)  
model = lm(jud_replace_con ~ ruth_populism * lag_trust_share  + jud_ind_mean5 + evnt_sum_lag3 + v2xnp_pres + executive + lag(trust_lm) +  as.factor(country), data = df5)  
summary(model)


changelmer |> 
  margins(
    variables = "ruth_populism",
    at = list(lag_trust_share = levels)
  ) |> 
  summary() |> 
  mutate(lower = AME - 1.96 * SE, 
         upper = AME + 1.96 * SE,
         lower_90 = AME - 1.64 * SE, 
         upper_90 = AME + 1.64 * SE,
         lower_99 = AME - 3.58 * SE, 
         upper_99 = AME + 3.58 * SE) ->
  meff


df5 |> 
  ggplot(aes(x=lag_trust_share, fill = as.factor(jud_replace_con), group = as.factor(jud_replace_con))) +
  geom_histogram(alpha = 0.5) +
  scale_x_continuous(limits = c(levels[1], levels[17])) +
  scale_y_reverse() +
  scale_fill_manual(values = c("grey", "darkslategrey"),
                    labels = c("0" = "No",
                               "1" = "Yes")) +
  labs(x = "Trust (lagged)",
       y = "",
       fill = "Judges replaced or added",
       caption = "") ->
  hist
hist

theme_update(axis.text.x = element_blank())
meff |> 
  ggplot(aes(x = lag_trust_share, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  geom_line() +
  labs(x = "",
       y = "AME of Populism",
       caption = "") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    limits = c(levels[1], levels[17])
  ) ->
  plot
plot

plot + plot_spacer() + hist +
  plot_layout(ncol = 1,
              heights = c(3, -0.7, 1)) ->
  plot

plot

df5 |> 
  filter(jud_replace_con == 1) 

df5 |> 
  filter(country == "Poland" & year == 2017) |> 
  select(trust_share)

# RANDOM EFFECTS RUMBPROBIEREN ----

form_iv <- gf({depv} ~ {indepv}  + {contr} + {fe} | {contr} + {instr})

model_random <- tsls(jud_replace ~ lagged_trust_share_1 + evnt_sum_lag3 + executive + 
                       v2xcs_ccsi + regime_age + (1|country),
                     x = ~ evnt_sum_lag3 + executive + 
                       v2xcs_ccsi + regime_age + (1|country) + v2juncind_mean_3 + v2juaccnt_mean_3 + 
                       v2jucorrdc_mean_3, 
                     data = df4)

library(plm)
model_random <- plm(jud_replace ~ lagged_trust_share_1 + evnt_sum_lag3 + executive + 
                      v2xcs_ccsi + regime_age | evnt_sum_lag3 + executive + 
                      v2xcs_ccsi + regime_age + v2juncind_mean_3 + v2juaccnt_mean_3 + 
                      v2jucorrdc_mean_3, 
                    data = df4_pop,
                    model = "random")
summary(model_random)

model_random <- plm(jud_replace ~ lagged_trust_share_1 + evnt_sum_lag3 + executive + 
                      v2xcs_ccsi + regime_age | evnt_sum_lag3 + executive + 
                      v2xcs_ccsi + regime_age + v2juncind_mean_3 + v2juaccnt_mean_3 + 
                      v2jucorrdc_mean_3, 
                    data = df4_nop,
                    model = "random")
summary(model_random)

pred_data_pop <- datagrid(lagged_trust_share_1 = predictions(model_random)$lagged_trust_share_1,
                          model = model_random)
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
