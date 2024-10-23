# ANALYSIS TRUST

# Set-up ----

## Libraries ----
library(tidyverse)
library(stargazer)
library(patchwork)
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
library(estimatr)
library(glue)


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
         poptrust = ruth_populism*lagged_trust_share_low_1,
         popcorr = ruth_populism*v2jucorrdc_mean_3,
         popacc = ruth_populism*v2juaccnt_mean_3,
         changelastcon5 = if_else(jud_replace_con_sum_5 > 0, 0, 1),
         changelastcon3 = if_else(jud_replace_con_sum_3 > 0, 0, 1),
         changelast5 = if_else(jud_replace_sum_5 > 0, 1, 0),
         changelast3 = if_else(jud_replace_sum_3 > 0, 1, 0)) |> 
  group_by(country) %>% 
  mutate(across(starts_with("v2x"), 
                .fns = ~ . - lag(.),
                .names = "lagged_{.col}")) |> 
  ungroup() %>% 
  filter(!country %in% c("Norway", 
                         "Moldova", 
                         "Ukraine", 
                         "Bosnia and Herzegovina", 
                         "Switzlerand", 
                         "Belarus",
                         "Iceland",
                         "United Kingdom")) ->
  df


# ANALYSIS JUDICIAL REPLACEMENT ----

## SET-UP

model_list_interaction <- list()
model_list_nointeraction <- list()
coef_names <- create_coefnames()

set_basevars()

df |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year, v2x_cspart) |> 
  na.omit() ->
  df_final

summary(df_final)

write_rds(df_final, glue("data/analysis_{depv}.rds"))

## OLS MODEL ----

form <- gf({depv} ~ {indepv} + {interact} + {contr} + {fe})

# ols without robust SE
ols <- lm(form,
          data = df_final)
# test for heteroscedasticity
bptest(ols, studentize = FALSE)
# robust SE with sandwich
coeftest(ols,
         vcov = vcovCL,
         type = "HC1")
# No correlation between predictors
vif(ols, type = "predictor")
ols_robust <- robust_ols(form)
summary(ols_robust)
model_list_interaction$m1 <- ols_robust
assign(paste("ols_robust", depv, sep = "_"), ols_robust)


# get levels for trust data for which to calculate AME
levels <- get_current_trust_levels("trust_share_low_linear_imp_mean_3")
# create new data for prediction (only ruth_populism and trust vary, all other at mean)
pred_data <- datagrid(ruth_populism = c(1, 0), 
                      trust_share_low_linear_imp_mean_3 = levels, 
                      model = ols_robust)
# calculate AME and CIs for predicted data
meff <- marg_effects(ols_robust)

# plot interaction
plot <- create_plot(meff,
            "trust_share_low_linear_imp_mean_3")

ggsave(glue("results/graphs/ols_interaction_{depv}.pdf"),
       device = cairo_pdf,
       width = 10,
       height = 6.2)

saveRDS(plot, glue("results/graphs/ols_interaction_{depv}.RDS"))

theme_set(theme_bar)
marg_effects(ols_robust, variable = "trust_share_low_linear_imp_mean_3") %>% 
  mutate(ruth_populism = if_else(ruth_populism == 1, "Populist", "Non-Populist")) %>% 
  ggplot(aes(x = ruth_populism,
             y = estimate,
             ymin = cilower,
             ymax = ciupper,
             linewidth = fct_rev(ci_size),
             color = ruth_populism)) +
  geom_hline(yintercept = 0, 
             color = "#C95D63", 
             linetype = "dashed") +
  geom_pointrange(size = 1) +
  scale_linewidth_manual(values = c(0.5, 1.25, 2),
                         name = "CI-Level") +
  scale_color_manual(values = c(color_dark, color_colorful)) +
  guides(color = "none") +
  labs(x = NULL,
       y = "AME of Trust")


ggsave(glue("results/graphs/ols_interaction_ametrust_{depv}.pdf"),
       device = cairo_pdf,
       width = 8,
       height = 6)

### OLS BASE MODELS ----

# only independent variables
form <- gf({depv} ~ {indepv})
baseols <- robust_ols(form)
summary(baseols)

# plus fixed-effects
form <- gf({depv} ~ {indepv} + {fe})
baseols2 <- robust_ols(form)
summary(baseols2)

# plus controls
form <- gf({depv} ~ {indepv} + {contr} + {fe})
baseols3 <- robust_ols(form)
model_list_nointeraction$m1 <- baseols3
summary(baseols3)
assign(paste("baseols3", depv, sep = "_"), baseols3)

# controls, but no fixed-effects
form <- gf({depv} ~ {indepv} + {contr})
baseols4 <- robust_ols(form)
summary(baseols4)


#### COMPARISON TABLE  ----


# add info on FE
olsrows <- data.frame("Coefficients" = "Country FE",
                      "(1)" = "No",
                      "(2)" = "Yes",
                      "(3)" = "Yes",
                      "(4)" = "No",
                      "(5)" = "Yes")
attr(olsrows, "position") <- 19

# create table
modelsummary(list(baseols, baseols2, baseols3, baseols4, ols_robust),
             coef_rename = coef_names,
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             add_rows = olsrows,
             modelsummary_format_numeric_latex = "plain",
             output = "latex",
             notes = list('+p < 0.1; *p < 0.05; **p > 0.01; ***p < 0.001')
             ) ->
  mainmodels

writeLines(mainmodels, glue("results/tables/mainmodels_{depv}.tex"))

## RANDOM EFFECTS ----

form <- gf({depv} ~ {indepv} + {interact} + {contr})
rols <- plm(form,
            data = df_final,
            index = c("country", "year"),
            model = "random",
            cluster = "country")
summary(rols)
model_list_interaction$rols <- rols
assign(paste("rols", depv, sep = "_"), rols)
coeftest(rols, 
         vcov = vcovHC(rols, 
                       type="HC1"))

# without interaction

form <- gf({depv} ~ {indepv}+ {contr})
rols_ni <- plm(form,
            data = df_final,
            index = c("country", "year"),
            model = "random",
            cluster = "country")
summary(rols_ni)
model_list_nointeraction$rols <- rols_ni
assign(paste("rols_ni", depv, sep = "_"), rols_ni)


## DYNAMIC MODEL ----

### DYNAMIC TRIPLE INTERACTION ----
dynamicv = c("changelast5",
             "changelast5*trust_share_low_linear_imp_mean_3*ruth_populism")
df |> 
  select(any_of(c(depv, indepv, contr, instr, fe, dynamicv)), year, v2x_cspart) |> 
  na.omit() ->
  df_final

form <- gf({depv} ~ {indepv} + {dynamicv} + {contr} +  {fe})
dols_triple <- robust_ols(form)
summary(dols_triple)

slopes(dols_triple,
       variables = "ruth_populism",
       newdata = datagrid(ruth_populism = c(1, 0), 
                          trust_share_low_linear_imp_mean_3 = levels,
                          changelast5 = c(0,1))) |> 
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

meff %>% 
  filter(changelast5 == 0) ->
  meff
create_plot(meff, 
            "trust_share_low_linear_imp_mean_3") 

ggsave(glue("results/graphs/tripleinteraction_dynamic_{depv}.pdf"),
       device = cairo_pdf,
       width = 10,
       height = 6.2)


### DYNAMIC NO POPULIST INTERACTION ----

dynamicv = c("changelast5",
             "changelast5*trust_share_low_linear_imp_mean_3")
dynamic1 = c("changelast5")

form <- gf({depv} ~ {indepv}+ {dynamicv1} + {contr} +  {fe})
dols_main5 <- robust_ols(form)
form <- gf({depv} ~ {indepv}+ {dynamicv} + {contr} +  {fe})
dols <- robust_ols(form)
summary(dols)

slopes(dols,
       variables = "trust_share_low_linear_imp_mean_3",
       newdata = datagrid(changelast5 = c(0,1))) |> 
  mutate(lower = estimate - 1.96 * std.error, 
         upper = estimate + 1.96 * std.error,
         lower_90 = estimate - 1.64 * std.error, 
         upper_90 = estimate + 1.64 * std.error,
         lower_99 = estimate - 3.58 * std.error, 
         upper_99 = estimate + 3.58 * std.error) ->
  meff

theme_set(theme_bar)

meff |> 
  mutate(changelast5 = if_else(changelast5 == 1, "Yes", "No")) %>% 
  ggplot(aes(x = as.factor(changelast5), 
             y = estimate, 
             color = as.factor(changelast5))) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_pointrange(aes(ymin = lower_90, 
                      ymax = upper_90)) +
  geom_line() +
  scale_color_manual(values = c(color_dark, color_colorful)) +
  labs(x = "",
       y = "AME of Trust",
       caption = "",
       color = "Court Packing or Purges in the last 5 Years") 

df_final |> 
  ggplot(aes(x=trust_share_low_linear_imp_mean_3)) +
  geom_histogram() +
  facet_wrap(~changelast5)

### With first change in 3 ----

contr <- c("surplus",
            "executive",
            "presidential",
            "gdp_growth_small_mean_3",
           "changelast3",
            "regime_age",
            "coalition")
dynamicv1 = c("changelast3")
dynamicv = c("changelast3",
             "changelast3*trust_share_low_linear_imp_mean_3")

df |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year) |> 
  na.omit() ->
  df_final

form <- gf({depv} ~ {indepv}+ {dynamicv1} + {contr} +  {fe})
dols_main3 <- robust_ols(form)
form <- gf({depv} ~ {indepv}+ {dynamicv} + {contr} +  {fe})
dols3 <- robust_ols(form)
summary(dols3)

dynamicv = c("changelast3",
             "changelast3*trust_share_low_linear_imp_mean_3*ruth_populism")
form <- gf({depv} ~ {indepv} + {dynamicv} + {contr} +  {fe})
dols_triple3 <- robust_ols(form)
summary(dols_triple3)

### DYNAMIC FILTERED ----
# for some weird reason this yields a different output than the interaction effect
set_basevars()

df |> 
  filter(changelast5 == 0) %>% 
  select(any_of(c(depv, indepv, contr, instr, fe)), year) |> 
  na.omit() ->
  df_final

form <- gf({depv} ~ {indepv} + {interact}  + {contr} + {fe})

ols_robust_dyn <- robust_ols(form)
summary(ols_robust_dyn)

# get levels for trust data for which to calculate AME
levels <- get_current_trust_levels("trust_share_low_linear_imp_mean_3")
# create new data for prediction (only ruth_populism and trust vary, all other at mean)
pred_data <- datagrid(ruth_populism = c(1, 0), 
                      trust_share_low_linear_imp_mean_3 = levels, 
                      model = ols_robust_dyn)
# calculate AME and CIs for predicted data
meff <- marg_effects(ols_robust_dyn)

# plot interaction
create_plot(meff,
            "trust_share_low_linear_imp_mean_3")

ggsave(glue("results/graphs/ols_interaction_onlychangelast.pdf_{depv}"),
       device = cairo_pdf,
       width = 10,
       height = 6.2)


# add info on FE
attr(olsrows, "position") <- 27

# create table
modelsummary(list("Main Model" = ols_robust, 
                  "5 Years" = dols_main5, 
                  "& Interaction" = dols, 
                  "3 Years" = dols_main3, 
                  "& Interaction" = dols3),
             coef_rename = coef_names,
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             add_rows = olsrows,
             output = "latex",
             notes = list('+p < 0.1; *p < 0.05; **p > 0.01; ***p < 0.001'),
             modelsummary_format_numeric_latex = "plain"
) |> 
  add_header_above(c(" " = 1, "5 Years" = 3, "3 Years" = 2)) %>% 
  sub("trust_share_low_linear_imp_mean_3", "Trust$_{\\bar{t}_{-1,-2,-3}}$", ., fixed = TRUE) %>% 
  kable_styling(font_size = 9) ->
  dynamicmodels

writeLines(dynamicmodels, glue("results/tables/dynamicmodels_{depv}.tex"))

### DYNAMIC JUDICIAL INDEPENDENCE ----

#### Triple Interaction ----

dynamicv = c("judicial_independence_mean_mean_3",
             "judicial_independence_mean_mean_3*trust_share_low_linear_imp_mean_3*ruth_populism")
df |> 
  select(any_of(c(depv, indepv, contr, instr, fe, dynamicv)), year, v2x_cspart) |> 
  na.omit() ->
  df_final

form <- gf({depv} ~ {indepv} + {dynamicv} + {contr} +  {fe})
dols_triple_ji <- robust_ols(form)
summary(dols_triple_ji)


# create new data for prediction (only ruth_populism and trust vary, all other at mean)
pred_data <- datagrid(ruth_populism = c(0,1),
                      judicial_independence_mean_mean_3 = c(-2, 0, 2), 
                      trust_share_low_linear_imp_mean_3 = levels,
                      model = dols_triple_ji)
# calculate AME and CIs for predicted data
meff <- marg_effects(dols_triple_ji,
                     "ruth_populism")

df_final %>% 
  mutate(ji = case_when(
    judicial_independence_mean_mean_3 < -1 ~ "Low", 
    judicial_independence_mean_mean_3 > -1 & judicial_independence_mean_mean_3 < 1 ~ "Medium",
    judicial_independence_mean_mean_3 > 1 ~ "High",
    TRUE ~ NA
  )) %>% 
  ggplot() +
  geom_density(aes(x = trust_share_low_linear_imp_mean_3,
                     fill = ji,
                   color = ji),
               alpha = 0.7) +
  scale_y_continuous(limits = c(0,8),
                     expand = c(0,0)) +
  scale_x_percent(limits = c(0,1),
                     expand = c(0,0)) +
  scale_fill_manual(values = c(color_colorful, color_dark, color_dark_verylight),
                    labels = c("High", "Medium", "Low"),
                    name = "Judicial Independence Last Three Years ") +
  scale_color_manual(values = c(color_colorful, color_dark, color_dark_verylight),
                    labels = c("High", "Medium", "Low"),
                    name = "Judicial Independence Last Three Years ") +
  labs(y = NULL,
       x = "Trust")

ggsave("results/graphs/density_jitrust.pdf",
       width = 10,
       height = 10*0.618,
       device = cairo_pdf)

meff |> 
  filter(ci_size == 95) %>% 
  ggplot(aes(x = trust_share_low_linear_imp_mean_3, 
             y = estimate)) +
  geom_hline(yintercept = 0, 
             color = "#C95D63", 
             linetype = "dashed") +
  geom_ribbon(aes(ymin = cilower, 
                  ymax = ciupper, 
                  alpha = ci_size,
                  fill = as.factor(judicial_independence_mean_mean_3))) +
  geom_line(aes(group = as.factor(judicial_independence_mean_mean_3))) +
  labs(x = "Trust in Judiciary",
       y = "AME of Populism",
       caption = element_blank()) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-7.5, 7,5),
                     breaks = seq(-5, 7.5, by = 2.5))  +
  scale_x_percent(expand = c(0,0),
                     limits = c(0,1)) +
  scale_alpha_manual(values = c(0.7, 0.5, 0.2),
                     name = "Confidence Intervall") +
  scale_fill_manual(values = c(color_colorful, color_dark, color_dark_verylight),
                    labels = c("High", "Medium", "Low"),
                    name = "Judicial Independence Last Three Years ") +
  guides(alpha = FALSE) +
  coord_cartesian(clip = "off") 

ggsave(glue("results/graphs/dynamic_interaction_trustjipop_{depv}.pdf"),
       device = cairo_pdf,
       width = 10,
       height = 6.2)

#### Interaction ----

dynamicv = c("judicial_independence_mean_mean_3",
             "judicial_independence_mean_mean_3*trust_share_low_linear_imp_mean_3")

form <- gf({depv} ~ {indepv}+ {dynamicv} + {contr} +  {fe})
dols_ji <- robust_ols(form)
summary(dols_ji)

# get levels for trust data for which to calculate AME
levelsji <- get_current_trust_levels("judicial_independence_mean_mean_3")
# create new data for prediction (only ruth_populism and trust vary, all other at mean)
pred_data <- datagrid(judicial_independence_mean_mean_3 = levelsji, 
                      model = dols_ji)
# calculate AME and CIs for predicted data
meff <- marg_effects(dols_ji,
                     "trust_share_low_linear_imp_mean_3")

if (depv == "jud_replace_cont") {
  title <- "Court Purges and Packing"
} else if (depv == "v2jupoatck") {
  title <- "Attacks on Judiciary"
}

meff |> 
  ggplot(aes(x = judicial_independence_mean_mean_3, 
             y = estimate)) +
  geom_hline(yintercept = 0, 
             color = "#C95D63", 
             linetype = "dashed") +
  geom_ribbon(aes(ymin = cilower, 
                  ymax = ciupper, 
                  alpha = ci_size),
              fill = color_neutral) +
  geom_line(aes()) +
  geom_line() +
  labs(x = "Judicial Independence in Years Before",
       y = "AME of Trust",
       caption = element_blank(),
       title = title) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-10, 10),
                     breaks = seq(-5, 10, by = 5))  +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-3,3)) +
  scale_alpha_manual(values = c(0.7, 0.5, 0.2),
                     name = "Confidence Intervall") +
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = textsize,
                                  family = fontname,
                                  hjust = 0,
                                  margin = margin(b=30),
                                  face = "bold"),
        plot.title.position = "plot")

ggsave(glue("results/graphs/dynamic_interaction_trustji_{depv}.pdf"),
       device = cairo_pdf,
       width = 10,
       height = 6.2)

olsrows_4 <- data.frame("Coefficients" = "Country FE",
                      "(1)" = "Yes",
                      "(2)" = "Yes",
                      "(3)" = "Yes")
attr(olsrows_4, "position") <- 27

# create table
modelsummary(list("Main Model" = ols_robust, 
                  "Interaction" = dols_ji, 
                  "Triple Interaction" = dols_triple_ji),
             coef_rename = coef_names,
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             add_rows = olsrows_4,
             output = "latex",
             notes = list('+p < 0.1; *p < 0.05; **p > 0.01; ***p < 0.001'),
             modelsummary_format_numeric_latex = "plain"
) %>% 
  kable_styling(font_size = 9) %>% 
  sub("trust_share_low_linear_imp_mean_3", "Trust$_{\\bar{t}_{-1,-2,-3}}$", ., fixed = TRUE) ->
  dynamicmodels_ji

writeLines(dynamicmodels_ji, glue("results/tables/dynamicmodels_ji_{depv}.tex"))

## CHANGES IN INDEPENDENT VARIABLES  ----

set_basevars()

### MEAN OF 3 ---

indepv = c("lagged_trust_share_low_linear_imp_1",
           "ruth_populism")
interact = c(
  "lagged_trust_share_low_linear_imp_1*ruth_populism"
)

df |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year, v2x_cspart) |> 
  na.omit() ->
  df_final


# Main 
form <- gf({depv} ~ {indepv} + {interact} + {contr} + {fe})
model_list_interaction$ols_robust_mean3 <- robust_ols(form)
# without Interaction
form <- gf({depv} ~ {indepv} + {contr} + {fe})
model_list_nointeraction$ols_robust_mean3_w <- robust_ols(form)


### MEAN OF 5 ----

indepv = c("trust_share_low_linear_imp_mean_5",
           "ruth_populism"
)
interact = c("trust_share_low_linear_imp_mean_5*ruth_populism")


df |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year, v2x_cspart) |> 
  na.omit() ->
  df_final

# Main 
form <- gf({depv} ~ {indepv} + {interact} + {contr} + {fe})
model_list_interaction$ols_robust_mean5 <- robust_ols(form)
# without Interaction
form <- gf({depv} ~ {indepv} + {contr} + {fe})
model_list_nointeraction$ols_robust_mean5_w <- robust_ols(form)

### NO IMPUTATION ----

indepv = c("trust_share_low_mean_3",
           "ruth_populism"
)
interact = c("trust_share_low_mean_3*ruth_populism")

df |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year, v2x_cspart) |> 
  na.omit() ->
  df_final

# Main 
form <- gf({depv} ~ {indepv} + {interact} + {contr} + {fe})
model_list_interaction$ols_robust_noimp <- robust_ols(form)
# without Interaction
form <- gf({depv} ~ {indepv} + {contr} + {fe})
model_list_nointeraction$ols_robust_noimp_w <- robust_ols(form)

### CLOSEST IMPUTATION ----

indepv = c("trust_share_low_imp_lastv_mean_3",
           "ruth_populism")
interact = c(
           "trust_share_low_imp_lastv_mean_3*ruth_populism"
)

df |> 
  select(any_of(c(depv, indepv, contr, instr, fe)), year, v2x_cspart) |> 
  na.omit() ->
  df_final

# Main 
form <- gf({depv} ~ {indepv} + {interact} + {contr} + {fe})
model_list_interaction$ols_robust_closestimp <- robust_ols(form)
form <- gf({depv} ~ {indepv} + {contr} + {fe})
model_list_nointeraction$ols_robust_closestimp_w <- robust_ols(form)

### HIGH TRUST ----

indepv = c("trust_share_high_linear_imp_mean_3",
           "ruth_populism")
interact = c(
  "trust_share_high_linear_imp_mean_3*ruth_populism"
)

df_final <- select_vars()

# Main 
form <- gf({depv} ~ {indepv} + {interact} + {contr} + {fe})
model_list_interaction$ols_robust_high <- robust_ols(form)
form <- gf({depv} ~ {indepv} + {contr} + {fe})
model_list_nointeraction$ols_robust_high_w <- robust_ols(form)


### Tables ----
# add info on FE
modelsummary(list("Trust High" = model_list_interaction$ols_robust_high, 
                  "No Imputation" = model_list_interaction$ols_robust_noimp, 
                  "Closest Imputation" = model_list_interaction$ols_robust_closestimp, 
                  "Linear Imputation" = model_list_interaction$ols_robust_mean3, 
                  "Linear Imputation"=model_list_interaction$ols_robust_mean5
                  ),
             coef_rename = coef_names,
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             add_rows = olsrows,
             output = "latex",
             notes = list('+p < 0.1; *p < 0.05; **p > 0.01; ***p < 0.001'),
             modelsummary_format_numeric_latex = "plain"
) %>% 
  kable_styling(font_size = 10) |> 
  kable_classic_2() %>% 
  gsub("trust_share_low_linear_imp_mean_3", "Trust$_{\\bar{t}_{-1,-2,-3}}$", ., fixed = TRUE) ->
  models_indepchange

writeLines(models_indepchange, glue("results/tables/models_indepchange_{depv}.tex"))

attr(olsrows, "position") <- 23

modelsummary(list("Trust High" = model_list_nointeraction$ols_robust_high, 
                  "No Imputation" = model_list_nointeraction$ols_robust_noimp_w, 
                  "Closest Imputation" = model_list_nointeraction$ols_robust_closestimp_w, 
                  "Linear Imputation" = model_list_nointeraction$ols_robust_mean3_w, 
                  "Linear Imputation"=model_list_nointeraction$ols_robust_mean5_w),
             coef_rename = coef_names,
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             add_rows = olsrows,
             notes = list('+p < 0.1; *p < 0.05; **p > 0.01; ***p < 0.001'),
             output = "latex"
) |> 
  kable_styling(font_size = 10) |> 
  kable_classic_2() ->
  models_indepchange2

writeLines(models_indepchange2, glue("results/tables/models_indepchange_w_{depv}.tex"))

## Control Change ----

set_basevars()
contr[contr=="gdp_growth_small_mean_3"]="gdp_growth_small_mean_5"

df_final <- select_vars()
form_main <- gf({depv} ~ {indepv} + {interact} + {contr} + {fe})
form_ni <- gf({depv} ~ {indepv} + {contr} + {fe})

# ols without robust SE
model_list_interaction$ols_c11 <- robust_ols(form_main)
model_list_nointeraction$ols_c21 <- robust_ols(form_ni)

contr[contr=="gdp_growth_small_mean_5"]="gdp_growth_small_mean_3"
contr[contr=="judicial_independence_mean_mean_3"]="judicial_independence_mean_mean_5"

# ols without robust SE
model_list_interaction$ols_c12 <- robust_ols(form_main)
model_list_nointeraction$ols_c22 <- robust_ols(form_ni)

contr[contr=="judicial_independence_mean_mean_5"]="judicial_independence_min_mean_3"

# ols without robust SE
model_list_interaction$ols_c13 <- robust_ols(form_main)
model_list_nointeraction$ols_c23 <- robust_ols(form_ni)

contr[contr=="judicial_independence_min_mean_5"]="judicial_independence_max_mean_3"

# ols without robust SE
model_list_interaction$ols_c14 <- robust_ols(form_main)
model_list_nointeraction$ols_c24 <- robust_ols(form_ni)

coefs_int <- map_dfr(model_list_interaction,
        ~clean_coefs(.))

theme_set(theme_base)
coefs_int %>% 
  filter(variable != "Populism" & variable != "Trust") %>% 
  ggplot(aes(x = coef,
             fill = as.factor(sig))) +
  geom_histogram(binwidth = 0.05,
                 alpha = 0.8) +
  facet_wrap(~variable, ncol = 1) +
  labs(fill = "Signficant on 95%-Level",
       y = element_blank(),
       x = "Estimate") +
  scale_y_continuous(breaks = c(0, 5,10),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(-5, 5),
                     expand = c(0,0)) +
  scale_fill_manual(values = c(color_dark, color_colorful))

ggsave(glue("results/graphs/coefficients_{depv}.pdf"),
       device = cairo_pdf,
       width = 10,
       height = 15)

coefs <- map_dfr(model_list_nointeraction,
                 ~clean_coefs(.))

coefs %>% 
  ggplot(aes(x = coef,
             fill = as.factor(sig))) +
  geom_histogram(binwidth = 0.05,
                 alpha = 0.8) +
  facet_wrap(~variable, ncol = 1) +
  labs(fill = "Signficant on 95%-Level",
       y = element_blank(),
       x = "Estimate") +
  scale_y_continuous(breaks = c(0, 5,10),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(-1, 2),
                     expand = c(0,0)) +
  scale_fill_manual(values = c(color_dark, color_colorful))

coefs <- map_dfr(model_list_nointeraction,
                 ~clean_coefs(.))

ggsave(glue("results/graphs/coefficients_w_{depv}.pdf"),
       device = cairo_pdf,
       width = 10,
       height = 15)

