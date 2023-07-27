# Back-up Code


### Government Weighted Populism Score -> Event
# are constitutional events more likely under populist governments?
m_const_lm <- lm(lead(evnt, 1) ~ gov_popul_weighted, data = df4)
m_const_ml_reg <- m_const_lm |> 
  tbl_regression()

# controls
m_const_control <- lmer(lead(evnt, 1) ~ gov_popul_weighted + gov_left + v2xnp_pres + lagged_v2x_libdem + coalition + surplus + (1 | country), data = df4) 
m_const_control <- m_const_control |> 
  tbl_regression()
m_const_control


# Random Country Intercepts
m_const_intercept <- lmer(lead(evnt, 1) ~ gov_popul_weighted + (1 | country), data = df4) 
m_const_intercept_reg <- m_const_intercept |> 
  tbl_regression()

# Random Country Intercepts & Interaction with rile
m_const_int <- lmer(lead(evnt, 1) ~ gov_popul_weighted * gov_left + surplus + (1 | country), data = df4)
m_const_int_reg <- m_const_int |> 
  tbl_regression()
m_const_int_reg

# Random Country Intercepts & Interaction with rile
m_const_int <- lmer(lead(evnt, 1) ~ gov_popul_weighted * gov_left + v2xnp_pres + lagged_v2x_libdem + coalition + surplus + (1 | country), data = df4)
m_const_int_reg <- m_const_int |> 
  tbl_regression()
m_const_int_reg

# Random Country Intercepts & Interaction presidentialism
m_const_int_pres <- lmer(lead(evnt, 1) ~ gov_popul_weighted * v2xnp_pres + v2xnp_pres + lagged_v2x_libdem + coalition + surplus + (1 | country), data = df4)
m_const_int_pres_reg <- m_const_int |> 
  tbl_regression()
m_const_int_reg

# as fixed effect
m_const_fe <- glm(lead(evnt,1) ~ gov_popul_weighted + country, data = df4)
summary(m_const_fe)
m_const_fe_reg <- m_const_fe |> 
  tbl_regression()

# as fixed effect
m_const_fe3 <- glm(lead(evnt,1) ~ gov_popul_weighted + gov_left + surplus + country, data = df4)
summary(m_const_fe3)
m_const_fe3_reg <- m_const_fe3 |> 
  tbl_regression()


# fixed effect & interaction
m_const_int_fe <- glm(lead(evnt,1) ~ gov_popul_weighted + gov_left + surplus + lagged_v2x_libdem + no_govparties + surplus + gov_popul_weighted*gov_left + country, data = df4)
m_const_int_fe_reg <- m_const_int_fe|>  
  tbl_regression()
m_const_int_fe_reg

# fixed effect & interaction with surplus
m_const_int_fe_s <- glm(lead(evnt,1) ~ gov_popul_weighted + gov_left + surplus + lagged_v2x_libdem + coalition +  gov_popul_weighted*coalition + country, data = df4)
m_const_int_fe_s_reg <- m_const_int_fe_s |>  
  tbl_regression()
m_const_int_fe_s_reg

results <- tbl_merge(list(m_const_ml_reg, 
                          m_const_intercept_reg, 
                          m_const_int_reg, 
                          m_const_fe_reg, 
                          m_const_fe3_reg,
                          m_const_int_fe_reg, 
                          m_const_int_fe_s_reg),
                     tab_spanner = c("**Base Model**", 
                                     "**Intercept**", 
                                     "**Models**",
                                     "**FE**",
                                     "** Models**",
                                     "**+Interaction**",
                                     "**Surplus FE**"))

### Plotting Left-Wing ----
m_const_int |> 
  tidy() |> 
  filter(!str_detect(term, "^country|sd|\\(I")) |> 
  mutate(term = str_replace_all(term, c("no_govparties" = "Number of\nCoalition Partners",
                                        "surplus" = "Surplus", 
                                        "gov_popul_weighted" = "**Government Populism Score**",
                                        "gov_left" = "Left-Wing Dummy",
                                        ":" = " x<br> ",
                                        "lagged_v2x_libdem" = "Liberal Democracy<br> Lagged",
                                        "v2xnp_pres" = "Presidentialism",
                                        "coalition" = "Coalition Dummy"))) |> 
  ggplot(aes(y = term)) +
  geom_vline(xintercept = 0, color = "#C95D63", linetype = "dotted") +
  stat_halfeye(
    aes(xdist = dist_student_t(df = df.residual(m_const_int), 
                               mu = estimate, 
                               sigma = std.error)),
    fill = "darkslategrey", 
    alpha = 0.5,
    size = 0.5
  ) +
  xlim(-1, 1) +
  labs(x = "",
       y = "")

ggsave("slides/slides_cdm/images/evnt_likelihood_reg.png", width = 4, height = 3, units = "in", dev = "png")


# marginal effects of populism score based on left-wing populism


m_const_int_fe %>%
  margins(
    variables = "gov_popul_weighted",
    at = list(gov_left = c(0,1))
  ) |> 
  summary() ->
  meff

meff %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         gov_left = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plot_left

plot_left |> 
  ggplot(aes(y = gov_left)) +
  geom_vline(xintercept = 0, color = "#C95D63", linetype = "dotted") +
  #xlim(-0.5, 0.5) +
  geom_linerange(aes(x = AME, xmin = clower, xmax = cupper), alpha = 0.6, linewidth = 0.75 , color = "darkslategrey") +
  geom_linerange(aes(x = AME, xmin = clower_99, xmax = cupper_99), alpha = 0.4, linewidth = 0.5, color = "darkslategrey") +
  geom_pointrange(aes(x = AME, xmin = clower_90, xmax = cupper_90), alpha = 0.8, linewidth = 1, size= 0.3,  color = "darkslategrey") +
  labs(y = "",
       x = "Average Marginal Effect of Government Populism Score") +
  xlim(-1, 1)

ggsave("slides/slides_cdm/images/evnt_likelihood.png", width = 4, height = 3, units = "in", dev = "png")
ggsave("results/graphs/evnt_likelihood.pdf", width = 14, height = 14, units = "in", dev = cairo_pdf)
ggsave("results/graphs/evnt_likelihood.png", width = 14, height = 14, units = "in", dev = "png")


### marginal effects of left score based on populism score ---

# create sequence of 0.05 distance to calculate marginal effects
govpol_seq <- seq(
  min(df4$gov_popul_weighted, na.rm = TRUE),
  max(df4$gov_popul_weighted, na.rm = TRUE),
  0.05
)

m_const_int %>%
  margins(
    variables = "gov_left",
    at = list(gov_popul_weighted = govpol_seq)
  ) |> 
  summary() ->
  meff

meff %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE) ->
  plot_left

plot_left |> 
  ggplot(aes(x = gov_popul_weighted)) +
  geom_hline(yintercept = 0, color = "#C95D63") +
  geom_ribbon(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower, ymax = cupper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  labs(x = "Government Weighted Populism Score",
       y = "Average Marginal Effect of Left-Wing Government") +
  xlim(0,1)


### Plotting Number of Government Parties

# marginal effects of populism score based on left-wing populism

m_const_int_fe_s %>%
  margins(
    variables = "gov_popul_weighted",
    at = list(no_govparties = c(1:8))
  ) |> 
  summary() ->
  meff

meff %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE) ->
  #gov_left = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plot_left

plot_left |> 
  ggplot(aes(x = no_govparties)) +
  geom_hline(yintercept = 0, color = "#C95D63") +
  #xlim(-0.5, 0.5) +
  geom_ribbon(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.3, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower, ymax = cupper), alpha = 0.5, fill = "darkslategrey") +
  labs(y = "",
       x = "Average Marginal Effect of Government Populism Score")  +
  xlim(1,8)


### Ruth Data ----


# are constitutional events more likely under populist governments?
m_const_lm <- lm(lead(evnt, 1) ~ ruth_populism, data = df4)
summary(m_const_lm)

# Random Country Intercepts
m_const_intercept <- lmer(lead(evnt, 1) ~ ruth_populism + (1 | country), data = df4) 
summary(m_const_intercept)

# Random Country Intercepts & Interaction with rile
m_const_int <- lmer(lead(evnt, 1) ~ ruth_populism * gov_left + lag(v2x_libdem) + (1 | country), data = df4)
m_const_int |> tbl_regression()

# as fixed effect
m_const_fe <- glm(lead(evnt,1) ~ ruth_populism + country, data = df4)
summary(m_const_fe)

# as fixed effect
m_const_fe3 <- glm(lead(evnt,1) ~ ruth_populism + gov_left + country, data = df4)
summary(m_const_fe3)

# fixed effect & interaction
m_const_int_fe <- glm(lead(evnt,1) ~ ruth_populism + gov_right + ruth_populism*gov_right + country, data = df4)
m_const_int_fe |>  tbl_regression()

m_const_int |> 
  tidy() |> 
  filter(!str_detect(term, "^country|sd|\\(I")) |> 
  mutate(term = str_replace_all(term, c("surplus" = "Surplus", 
                                        "ruth_populism" = "Binary Populism Score",
                                        "gov_left" = "Left-Wing",
                                        ":" = " x\n ",
                                        "lag\\(v2x_libdem\\)" = "Liberal Democracy \n Lagged"))) |> 
  ggplot(aes(y = term)) +
  stat_halfeye(
    aes(xdist = dist_student_t(df = df.residual(m_const_int_fe), 
                               mu = estimate, 
                               sigma = std.error)),
    fill = "darkslategrey", 
    alpha = 0.5
  ) +
  xlim(-1, 1)

# marginal effects of populism score based on left-wing populism

m_const_int_fe %>%
  margins(
    variables = "ruth_populism",
    at = list(gov_right = c(0,1))
  ) |> 
  summary() ->
  meff

meff %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         # rename latin dummy for plot
         gov_right = if_else(gov_right == 0, "Left-Wing", "Right-Wing")) ->
  plot_left

plot_left |> 
  ggplot(aes(y = gov_right)) +
  geom_vline(xintercept = 0, color = "#C95D63") +
  xlim(-0.5, 0.5) +
  geom_linerange(aes(x = AME, xmin = clower_90, xmax = cupper_90), linewidth = 3) +
  geom_linerange(aes(x = AME, xmin = clower_99, xmax = cupper_99), linewidth = 0.5) +
  geom_pointrange(aes(x = AME, xmin = clower, xmax = cupper), linewidth = 1) +
  labs(y = "",
       x = "Average Marginal Effect of Government Populism Score")

### Populist PM ----


# are constitutional events more likely under populist governments?
m_const_lm <- lm(lead(evnt, 1) ~ gov_popul_prime, data = df4)
summary(m_const_lm)

# Random Country Intercepts
m_const_intercept <- lmer(lead(evnt, 1) ~ gov_popul_prime + (1 | country), data = df4) 
summary(m_const_intercept)

# Random Country Intercepts & Interaction with rile
m_const_int <- lmer(lead(evnt, 1) ~ gov_popul_prime * gov_left + (1 | country), data = df4)
m_const_int |> tbl_regression()

# as fixed effect
m_const_fe <- glm(lead(evnt,1) ~ gov_popul_prime + country, data = df4)
summary(m_const_fe)

# as fixed effect
m_const_fe3 <- glm(lead(evnt,1) ~ gov_popul_prime + gov_left + country, data = df4)
summary(m_const_fe3)

# fixed effect & interaction
m_const_int_fe <- glm(lead(evnt,1) ~ gov_popul_prime + gov_left + gov_popul_prime*gov_left + country, data = df4)
m_const_int_fe |>  tbl_regression()

m_const_int |> 
  tidy() |> 
  filter(!str_detect(term, "^country|sd|\\(I")) |> 
  ggplot(aes(y = term)) +
  stat_halfeye(
    aes(xdist = dist_student_t(df = df.residual(m_const_int_fe), 
                               mu = estimate, 
                               sigma = std.error)),
    fill = "darkslategrey", 
    alpha = 0.5
  ) 

# create sequence of 0.05 distance to calculate marginal effects
govprime_seq <- seq(
  min(df4$gov_popul_prime, na.rm = TRUE),
  max(df4$gov_popul_prime, na.rm = TRUE),
  0.05
)

# marginal effects of populism score based on left-wing populism

m_const_int_fe %>%
  margins(
    variables = "gov_popul_prime",
    at = list(gov_left = c(1,0))
  ) |> 
  summary() ->
  meff

meff %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         # rename latin dummy for plot
         gov_right = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plot_left

plot_left |> 
  ggplot(aes(y = gov_right)) +
  geom_vline(xintercept = 0, color = "#C95D63") +
  xlim(-0.5, 0.5) +
  geom_linerange(aes(x = AME, xmin = clower_90, xmax = cupper_90), linewidth = 3) +
  geom_linerange(aes(x = AME, xmin = clower_99, xmax = cupper_99), linewidth = 0.5) +
  geom_pointrange(aes(x = AME, xmin = clower, xmax = cupper), linewidth = 1) +
  labs(y = "",
       x = "Average Marginal Effect of Government Populism Score")

# marginal effects of left score based on populism score

m_const_int_fe %>%
  margins(
    variables = "gov_left",
    at = list(gov_popul_prime = govprime_seq)
  ) |> 
  summary() ->
  meff

meff %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE) ->
  plot_left

plot_left |> 
  ggplot(aes(x = gov_popul_prime)) +
  geom_hline(yintercept = 0, color = "#C95D63") +
  geom_ribbon(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower, ymax = cupper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  labs(x = "Government Weighted Populism Score",
       y = "Average Marginal Effect of Left-Wing Government") +
  xlim(0,1)


### V-Party Score ----

### Liberal Democracy ----
m1 <- lmer(lead(v2x_libdem, 1) ~ gov_popul_weighted + (1 | country) + (1 | year), data = df4)
summary(m1)

m3 <- lmer(lead(v2x_libdem, 1) ~ evnt + (1 | country) + (1 | year), data = df4)
summary(m3)

m3 <- lmer(lead(v2x_libdem, 1) ~ gov_left + (1 | country) + (1 | year), data = df4)
summary(m3)

m4 <- lmer(lead(v2x_libdem, 1) ~ evnt * gov_popul_weighted * gov_left + lagged_v2x_libdem + surplus + coalition +  (1 | country), data = df4)
summary(m4)
m4 |>  tbl_regression()

m4_lag <- lmer(lead(v2x_libdem, 1) ~ evnt * gov_popul_weighted * lagged_v2x_libdem + gov_left + surplus + coalition +  (1 | country), data = df4)
m4_lag |>  tbl_regression()

m5 <- lmer(lead(v2x_libdem, 1) ~ evnt * gov_popul_weighted * gov_left + (1 | country) + (1 | year), data = df4)


m_cl <- lm_robust(lead(v2x_libdem, 1)  ~ gov_left * gov_popul_weighted * lag(v2x_libdem)  + gov_left + lag(v2x_libdem),
                  clusters = country,
                  fixed_effects = ~ country,
                  data = df4)
summary(m_cl)

### Participation ----

m1latcs <- lmer(lead(v2x_partip, 1) ~ gov_popul_weighted + (1 | country) + (1 | year), data = df4)
summary(m1latcs)

m3latcs <- lmer(lead(v2x_partip, 1) ~ evnt + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

m3latcs <- lmer(lead(v2x_partip, 1) ~ evnt * gov_popul_weighted * gov_left + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

m4latcs <- lmer(lead(v2x_partip, 1) ~ evnt * gov_popul_weighted * gov_left + lagged_v2x_partip + surplus + coalition +  (1 | country), data = df4)
summary(m4latcs)

### Egalitarian Model  ----

m1latcs <- lmer(lead(v2x_egaldem, 1) ~ gov_popul_weighted + (1 | country) + (1 | year), data = df4)
summary(m1latcs)

m3latcs <- lmer(lead(v2x_egaldem, 1) ~ evnt + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

m3latcs <- lmer(lead(v2x_egaldem, 1) ~ evnt * gov_popul_weighted * gov_left + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

m4eg <- lmer(lead(v2x_egaldem, 1) ~ evnt * gov_popul_weighted * gov_left + lagged_v2x_egaldem + surplus + coalition +  (1 | country), data = df4)
summary(m4latcs)

#### Regression Output----

# create sequence of 0.05 distance to calculate marginal effects
govpol_seq <- seq(
  min(df4$gov_popul_weighted, na.rm = TRUE),
  max(df4$gov_popul_weighted, na.rm = TRUE),
  0.05
)

### Plotting for Liberal Democracy ----

# marginal effects for liberal democracy based on interaction model

m4 %>%
  margins(
    variables = "evnt",
    at = list(gov_popul_weighted = govpol_seq, gov_left = c(0, 1))
  ) %>%
  summary() ->
  meff

# calculate lower and upper confidence interval

meff %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         # rename latin dummy for plot
         gov_left = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plotdf_ld

## Final Plot for Liberal Democracy 

ggplot(plotdf_ld, aes(x = gov_popul_weighted, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower, ymax = cupper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  geom_line() +
  facet_grid(~gov_left) +
  labs(x = "Government Populism Score",
       y = "",
       caption = "Average Marginal Effect of Constitutional Change Conditioned By Ideology & Government Populism Score.") +
  scale_x_continuous(
    breaks = c(0, 0.35, 0.5, 0.75, 1),
    limits = c(0, 1)
  ) +
  ylim(-0.1, 0.1) 

ggsave("slides/slides_cdm/images/libdem_interaction.png", width = 8, height = 3.5,  units = "in", dev = "png")
ggsave("results/graphs/liberaldem_interaction.pdf", width = 14, height = 6, units = "in", dev = cairo_pdf)
ggsave("results/graphs/liberaldem_interaction.png", width = 14, height = 6, units = "in", dev = "png")

### Plotting for Participation ----

m4latcs %>%
  margins(
    variables = "evnt",
    at = list(gov_popul_weighted = govpol_seq, gov_left = c(0, 1))
  ) %>%
  summary() ->
  meff_cs

# calculate confidence intervals 


meff_cs %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         # rename dummy for plots
         gov_left = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plotdf_cs

ggplot(plotdf_cs, aes(x = gov_popul_weighted, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower, ymax = cupper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  geom_line() +
  facet_grid(~gov_left) +
  labs(x = "Government Populism Score",
       y = "",
       caption = "Average Marginal Effect of Constitutional Change Conditional on Ideology & Weighted Populism Score.") +
  scale_x_continuous(
    breaks = c(0, 0.35, 0.5, 0.75, 1),
    limits = c(0, 1)
  ) +
  ylim(-0.1, 0.1) 

ggsave("slides/slides_cdm/images/participation_interaction.png", width = 8, height = 3.5,  units = "in", dev = "png")
ggsave("results/graphs/participationdem_interaction.pdf", width = 14, height = 6, units = "in", dev = cairo_pdf)
ggsave("results/graphs/participation_interaction.png", width = 14, height = 6, units = "in", dev = "png")

### Plotting for Egalitarian ----

m4eg %>%
  margins(
    variables = "evnt",
    at = list(gov_popul_weighted = govpol_seq, gov_left = c(0, 1))
  ) %>%
  summary() ->
  meff_eg

# calculate confidence intervals 


meff_eg %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         # rename dummy for plots
         gov_left = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plotdf_eg

ggplot(plotdf_eg, aes(x = gov_popul_weighted, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_ribbon(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.4, fill = "darkslategrey") +
  geom_ribbon(aes(ymin = clower, ymax = cupper), alpha = 0.3, fill = "darkslategrey") +
  geom_line(aes(y = AME)) +
  geom_line() +
  facet_grid(~gov_left) +
  labs(caption = "Average Marginal Effect of Constitutional Change Conditioned By Ideology & Government Populism Score.",
       x = "Government Populism Score",
       y = "") +
  scale_x_continuous(
    breaks = c(0, 0.35, 0.5, 0.75, 1),
    limits = c(0, 1)
  ) +
  ylim(-0.1, 0.1) 

ggsave("slides/slides_cdm/images/egaldem_interaction.png", width = 8, height = 3.5, units = "in", dev = "png")



### Combined Final Plots for Paper ----

patchworked <- plot_cs + plot_ld + plot_layout(ncol = 1)

patchworked

ggsave("results/graphs/interaction.pdf", width = 8, height = 8, units = "in", dev = cairo_pdf)
ggsave("results/graphs/interaction.png", width = 8, height = 8, units = "in", dev = "png")

### RUTH POPULISM - DEMOCRATIC QUALITY ---- 


### Liberal Democracy ----
m1 <- lmer(lead(v2x_libdem, 1) ~ ruth_populism + (1 | country) + (1 | year), data = df4)
summary(m1)

m3 <- lmer(lead(v2x_libdem, 1) ~ evnt + (1 | country) + (1 | year), data = df4)
summary(m3)

m3 <- lmer(lead(v2x_libdem, 1) ~ gov_left + (1 | country) + (1 | year), data = df4)
summary(m3)

m4 <- lmer(lead(v2x_libdem, 1) ~ evnt * ruth_populism * gov_left + surplus + coalition +  (1 | country), data = df4)
summary(m4)

m4 |> 
  tbl_regression()

m5 <- lmer(lead(v2x_libdem, 1) ~ evnt * ruth_populism * gov_left + (1 | country) + (1 | year), data = df4)


m_cl <- lm_robust(lead(v2x_libdem, 1)  ~ gov_left * ruth_populism * lag(v2x_libdem)  + gov_left + lag(v2x_libdem),
                  clusters = country,
                  fixed_effects = ~ country,
                  data = df4)
summary(m_cl)

### Participation ----

m1latcs <- lmer(lead(v2x_partip, 1) ~ ruth_populism + (1 | country) + (1 | year), data = df4)
summary(m1latcs)

m3latcs <- lmer(lead(v2x_partip, 1) ~ evnt + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

m3latcs <- lmer(lead(v2x_partip, 1) ~ evnt * ruth_populism * gov_left + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

m4latcs <- lmer(lead(v2x_partip, 1) ~ evnt * ruth_populism * gov_left + surplus + coalition +  (1 | country), data = df4)
summary(m4latcs)

### Egalitarian Model  ----

m1latcs <- lmer(lead(v2x_egaldem, 1) ~ ruth_populism + (1 | country) + (1 | year), data = df4)
summary(m1latcs)

m3latcs <- lmer(lead(v2x_egaldem, 1) ~ evnt + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

m3latcs <- lmer(lead(v2x_egaldem, 1) ~ evnt * ruth_populism * gov_left + (1 | country) + (1 | year), data = df4)
summary(m3latcs)

m4eg <- lmer(lead(v2x_egaldem, 1) ~ evnt * ruth_populism * gov_left + surplus + coalition +  (1 | country), data = df4)
summary(m4latcs)

#### Regression Output----

### Plotting for Liberal Democracy ----

# marginal effects for liberal democracy based on interaction model

m4 %>%
  margins(
    variables = "evnt",
    at = list(ruth_populism = c(0,1), gov_left = c(0, 1))
  ) %>%
  summary() ->
  meff

# calculate lower and upper confidence interval

meff %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         # rename latin dummy for plot
         gov_left = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plotdf_ld

## Final Plot for Liberal Democracy 

ggplot(plotdf_ld, aes(x = ruth_populism, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_linerange(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, color = "darkslategrey") +
  geom_linerange(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.4, color = "darkslategrey") +
  geom_linerange(aes(ymin = clower, ymax = cupper), alpha = 0.3, color = "darkslategrey") +
  geom_point(aes(y = AME)) +
  facet_grid(~gov_left) +
  labs(x = "Government Populism Score",
       y = "",
       caption = "Average Marginal Effect of Constitutional Change Conditioned By Ideology & Government Populism Score.") +
  scale_x_continuous(
    breaks = c(0,1),
    limits = c(0, 1)
  ) +
  ylim(-0.1, 0.1) 

ggsave("slides/slides_cdm/images/libdem_interaction_ruth.png", width = 8, height = 3.5,  units = "in", dev = "png")
ggsave("results/graphs/liberaldem_interaction_ruth.pdf", width = 14, height = 6, units = "in", dev = cairo_pdf)
ggsave("results/graphs/liberaldem_interaction_ruth.png", width = 14, height = 6, units = "in", dev = "png")

### Plotting for Participation ----

m4latcs %>%
  margins(
    variables = "evnt",
    at = list(ruth_populism = c(0,1), gov_left = c(0, 1))
  ) %>%
  summary() ->
  meff_cs

# calculate confidence intervals 


meff_cs %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         # rename dummy for plots
         gov_left = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plotdf_cs

ggplot(plotdf_cs, aes(x = ruth_populism, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_linerange(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, color = "darkslategrey") +
  geom_linerange(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.4, color = "darkslategrey") +
  geom_linerange(aes(ymin = clower, ymax = cupper), alpha = 0.3, color = "darkslategrey") +
  geom_point(aes(y = AME)) +
  facet_grid(~gov_left) +
  labs(x = "Government Populism Score",
       y = "",
       caption = "Average Marginal Effect of Constitutional Change Conditional on Ideology & Weighted Populism Score.") +
  scale_x_continuous(
    breaks = c(0, 0.35, 0.5, 0.75, 1),
    limits = c(0, 1)
  ) +
  ylim(-0.1, 0.1) 

ggsave("slides/slides_cdm/images/participation_interaction_ruth.png", width = 8, height = 3.5,  units = "in", dev = "png")
ggsave("results/graphs/participationdem_interaction_ruth.pdf", width = 14, height = 6, units = "in", dev = cairo_pdf)
ggsave("results/graphs/participation_interaction_ruth.png", width = 14, height = 6, units = "in", dev = "png")

### Plotting for Egalitarian ----

m4eg %>%
  margins(
    variables = "evnt",
    at = list(ruth_populism = c(0,1), gov_left = c(0, 1))
  ) %>%
  summary() ->
  meff_eg

# calculate confidence intervals 


meff_eg %>%
  mutate(clower = AME - 1.96 * SE, 
         cupper = AME + 1.96 * SE,
         clower_90 = AME - 1.64 * SE, 
         cupper_90 = AME + 1.64 * SE,
         clower_99 = AME - 3.58 * SE, 
         cupper_99 = AME + 3.58 * SE,
         # rename dummy for plots
         gov_left = if_else(gov_left == 0, "Right-Wing", "Left-Wing")) ->
  plotdf_eg

ggplot(plotdf_eg, aes(x = ruth_populism, y = AME)) +
  geom_hline(yintercept = 0, color = "#C95D63", linetype = "dotted") +
  geom_linerange(aes(ymin = clower_90, ymax = cupper_90), alpha = 0.8, color = "darkslategrey") +
  geom_linerange(aes(ymin = clower_99, ymax = cupper_99), alpha = 0.4, color = "darkslategrey") +
  geom_linerange(aes(ymin = clower, ymax = cupper), alpha = 0.3, color = "darkslategrey") +
  geom_point(aes(y = AME)) +
  facet_grid(~gov_left) +
  labs(caption = "Average Marginal Effect of Constitutional Change Conditioned By Ideology & Government Populism Score.",
       x = "Government Populism Score",
       y = "") +
  scale_x_continuous(
    breaks = c(0, 0.35, 0.5, 0.75, 1),
    limits = c(0, 1)
  ) +
  ylim(-0.1, 0.1) 

ggsave("slides/slides_cdm/images/egaldem_interaction.png", width = 8, height = 3.5, units = "in", dev = "png")


