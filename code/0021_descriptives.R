library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(ggrepel)
library(gt)

# Set Up ----

## Party Data ----

partydata <- readRDS("data/party_populism.rds")

## Government Data ----

governmentdata <- readRDS("data/ccpc_vdem_eu_la.rds")
analysisdata <- readRDS("data/analysis_jud_replace_cont.rds") 
analysisdata1 <- readRDS("data/analysis_jud_replace_cont.rds")
analysisdata2 <- readRDS("data/analysis_v2jupoatck.rds") 

analysisdata |>
  mutate(id = paste0(country, year)) ->
  analysisdata

governmentdata |> 
  mutate(id = paste0(country, year),
         latin = as.factor(if_else(e_regiongeo %in% c(17, 18), 1, 0))) |> 
  mutate(analysis = if_else(
    id %in% analysisdata$id, 1, 0
  )) ->
  governmentdata

## Theme ----

source("../../src/graphics.R")

# Scatterplot----

analysisdata %>% 
  arrange(country) %>% 
  group_by(country) %>% 
  mutate(group_id = cur_group_id()) %>% 
  ungroup() ->
  analysisdata

analysisdata %>% 
  filter(group_id < 21) %>% 
  mutate(trust = if_else(trust_share_low_linear_imp_mean_3 < 0.33, "low", if_else(trust_share_low_linear_imp_mean_3 < 0.66, "medium", "high"))) %>% 
  ggplot(aes(x = trust_share_low_linear_imp_mean_3,
             y = jud_replace_cont)) +
  geom_smooth(method="lm") +
  geom_point(aes(color = as.factor(ruth_populism), shape = as.factor(ruth_populism))) +
  facet_wrap(~country, ncol = 5) +
  ylim(-2, 4) +
  scale_x_continuous(limits = c(0, 1),
                     expand = c(0,0),
                     breaks = c("0" = 0,"0.5"=0.5,"1"=1)) +
  scale_y_continuous(
                     expand = c(0,0),
                     breaks = seq(-2,4, by = 2)) +
  scale_color_manual(values = c(color_dark, color_colorful),
                    name = NULL) +
  labs(shape = NULL,
       y = "Court Purges and Packing",
       x = "Trust in Judiciary")

ggsave("results/graphs/country_descriptives.pdf",
       device = cairo_pdf,
       width = 29,
       height = 22.5,
       unit = "cm")

analysisdata %>% 
  arrange(country) %>% 
  group_by(country) %>% 
  mutate(group_id = cur_group_id()) %>% 
  filter(group_id > 20) %>% 
  mutate(trust = if_else(trust_share_low_linear_imp_mean_3 < 0.33, "low", if_else(trust_share_low_linear_imp_mean_3 < 0.66, "medium", "high"))) %>% 
  ggplot(aes(x = trust_share_low_linear_imp_mean_3,
             y = jud_replace_cont)) +
  geom_smooth(method="lm") +
  geom_point(aes(color = as.factor(ruth_populism), shape = as.factor(ruth_populism))) +
  facet_wrap(~country, ncol = 5) +
  ylim(-2, 4) +
  scale_x_continuous(limits = c(0, 1),
                     expand = c(0,0),
                     breaks = c("0" = 0,"0.5"=0.5,"1"=1)) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(-2,4, by = 2)) +
  scale_color_manual(values = c(color_dark, color_colorful),
                     name = NULL) +
  labs(shape = NULL,
       y = "Court Purges and Packing",
       x = "Trust in Judiciary")

ggsave("results/graphs/country_descriptives2.pdf",
       device = cairo_pdf,
       width = 29,
       height = 22.5,
       unit = "cm")


# Histogram Jud Replacement ----

textsize <- textsize-6
source("../../src/graphics.R")

governmentdata %>% 
  select(ruth_populism, jud_replace_cont, v2jupoatck) %>% 
  pivot_longer(cols = c(jud_replace_cont, v2jupoatck),
               names_to = "variable",
               values_to = "value") %>%  
  filter(!is.na(ruth_populism)) %>% 
  mutate(variable = if_else(variable == "jud_replace_cont", 
                            "Court Purges and Packing",
                            "Attacks on Judiciary")) %>% 
  ggplot(aes(x = value,
             fill = ruth_populism,
             group = ruth_populism,
             color = ruth_populism)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~ variable) +
  scale_x_continuous(limits = c(-5, 5),
                     expand = c(0,0),
                     breaks = c("-5\nLow" = -5,
                                "0" = 0,
                                "5\nHigh" = 5)) +
  scale_y_continuous(limits = c(0, 0.9),
                     expand = c(0,0),
                     breaks = seq(0.2,0.8, by = 0.2)) +
  scale_fill_manual(values = c(color_dark, color_colorful),
                    name = NULL) +
  scale_color_manual(values = c(color_dark, color_colorful),
                    name = NULL) +
  labs(y = NULL,
       x = NULL) +
  coord_cartesian(clip = "off")

ggsave("results/graphs/independentvar_histogram.pdf",
       device = cairo_pdf,
       width = 18,
       height = 10,
       unit = "cm")

governmentdata %>% 
  select(ruth_populism, trust_share_low_linear_imp_mean_3) %>% 
  filter(!is.na(ruth_populism)) %>% 
  ggplot(aes(x = trust_share_low_linear_imp_mean_3,
             fill = ruth_populism,
             group = ruth_populism,
             color = ruth_populism)) +
  geom_density(alpha = 0.7) +
  scale_x_percent(limits = c(0, 1),
                     expand = c(0,0),
                     breaks = c("0" = 0,
                                "1" = 1,
                                "5" = 5)) +
  scale_y_continuous(limits = c(0, 4),
                     expand = c(0,0),
                     breaks = seq(1,4, by = 1)) +
  scale_fill_manual(values = c(color_dark, color_colorful),
                    name = NULL) +
  scale_color_manual(values = c(color_dark, color_colorful),
                     name = NULL) +
  labs(y = NULL,
       x = NULL) +
  coord_cartesian(clip = "off")

ggsave("results/graphs/trust_histogram.pdf",
       device = cairo_pdf,
       width = 12,
       height = 12*0.618,
       unit = "cm")


textsize <- textsize+6
source("../../src/graphics.R")

ggsave("results/graphs/trust_histogram.pdf",
       device = cairo_pdf,
       width = 12,
       height = 12*0.618,
       unit = "cm")

governmentdata %>% 
  group_by(year, jud_replace_con) %>% 
  reframe(n = n()) %>% 
  filter(jud_replace_con == 1) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_line()


governmentdata %>% 
  group_by(year) %>% 
  reframe(mean = mean(jud_replace_cont)) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_line() +
  scale_x_continuous(limits = c(1990, 2020),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0.5, 1),
                     expand = c(0,0),
                     breaks = seq(0.6,1, by = 0.1))

governmentdata %>% 
  group_by(year) %>% 
  mutate(mean_world = mean(v2jupoatck)) %>% 
  group_by(year, latin, mean_world) %>% 
  reframe(mean = mean(v2jupoatck)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = mean_world), color = color_dark_light) +
  geom_line(aes(x = year, 
                y = mean,
                color = as.factor(latin),
                group = as.factor(latin))) +
  scale_x_continuous(limits = c(1990, 2020),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.5, 1.5),
                     expand = c(0,0),
                     breaks = seq(0,1.5, by = 0.5)) +
  scale_color_manual(values = c(color_dark, color_colorful))

# Independent Judiciary & Populism

governmentdata %>% 
  filter(judicial_independence_mean_mean_3 > 1.8 & ruth_populism == "Populist") %>% 
  View()

  

# Validty of V-Party Populism Score & Ruth-Lovell/Grahn ----

## Correlation ----

governmentdata |>  
  filter(!is.na(gov_popul_weighted) & !is.na(ruth_populism)) |> 
  mutate(matches = if_else(gov_popul_prime > 0.5 & ruth_populism == 0 |
           gov_popul_prime < 0.5 & ruth_populism == 1, "FALSE", "TRUE")) ->
  gov_filtered
  
validate_populism <- glm(ruth_populism ~ gov_popul_prime, data = gov_filtered)
summary(validate_populism)

pred <- predict(validate_populism, type = "response")
pred_df <- data.frame(gov_popul_prime = gov_filtered$gov_popul_weighted, pred)

ggplot(gov_filtered, aes(x = gov_popul_prime, y = ruth_populism)) +
  geom_point(aes(color = matches,
                 shape = matches),
             size = 8) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = FALSE,
              color = "darkslategrey") +
  scale_color_manual(values = c("#689295", "#263f3f")) +
  labs(color = "Matches Ruth-Lovell/Grahn",
       shape = "Matches Ruth-Lovell/Grahn",
       y = "Binary Populism Score",
       x = "Continuous Populism Score",
       caption = "\nLogistic Regression Predicting Binary Populism Score from Weighted Populism Score. Correlation = 0.74.")

ggsave("results/graphs/liberaldem_interaction.pdf", width = 45, height = 6, units = "in", dev = cairo_pdf)
ggsave("slides/slides_cdm/images/populism_validity.png", width = 24, height = 14, units = "in", dev = "png")


gov_filtered |> 
  filter(gov_popul_weighted > 0.5 & ruth_populism == 0 |
           gov_popul_weighted < 0.5 & ruth_populism == 1) |> 
  select(gov_popul_weighted, ruth_populism, country, year)

# Histogram V-Party in Governments ----

theme_update(panel.grid.major.y = element_line(color = "lightgrey",
                                        linewidth = 0.3,
                                        linetype = 1))

governmentdata |> 
  mutate(latin = as.factor(latin)) |> 
  ggplot(aes(x = gov_popul_weighted, 
             fill = latin,
             group = latin)) +
  geom_histogram(color = "white",
                 linewidth = 0.3) +
  scale_fill_manual(values = c(color_colorful, color_dark),
                    labels = c("Europe", "Latin America")) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1)) +
  labs(y = "Observations",
       x = "Weighted Populism Score per Government",
       fill = element_blank())

ggsave("results/graphs/histogram_populistgov.pdf",
       device = cairo_pdf,
       width = 8,
       height = 5)

## Cases Constitutional Change under Populist Government ----

# But which populist government

governmentdata |> 
  filter(evnt == 1) |> 
  filter(ruth_populism == 1 | gov_popul_weighted > 0.5) |> 
  select(country, year, president, ruth_populism, gov_popul_weighted) |> 
  gt(
    groupname_col = "country",
    rowname_col = "year"
  ) 

governmentdata |> 
  mutate(gov_popul_weighted = if_else(gov_popul_weighted > 0.5, 1, 0)) |> 
  group_by(ruth_populism, gov_popul_weighted) |> 
  count() 

governmentdata |> 
  filter(is.na(ruth_populism) & !is.na(gov_popul_weighted)) |> 
  group_by(country) |>  
  count() |> 
  filter(n > 4) |> 
  gt()

governmentdata |> 
  filter(evnt == 1 & gov_popul_weighted > 0.5) |> 
  count()

# Validty of left - right econ ----

governmentdata |> 
  filter(country == "Poland") |> 
  ggplot() +
  geom_point(aes(x = year, y = gov_rile_weighted, color = as.factor(gov_left)))

governmentdata |> 
  mutate(dummy = as.factor(if_else(gov_rile_weighted > -8 & gov_rile_weighted < 8, 1, 0))) |> 
  ggplot() +
  geom_histogram(aes(gov_rile_weighted, fill = dummy, group = dummy), binwidth = 2)

# Histogram

governmentdata |> 
  filter(!is.na(evnt)) |> 
  mutate(evnt = if_else(evnt == 1, "Yes", "No")) |> 
  ggplot() +
  geom_histogram(aes(gov_popul_weighted, fill = as.factor(evnt), group = as.factor(evnt)),
                 binwidth = 0.01) +
  labs(x = "Weighted Populism Score",
       y = "",
       fill = "Constitutional Change",
       caption = "\nHistogram of Continuous Weighted Government Score") +
  scale_fill_manual(values = c("#689295", "#263f3f"))

ggsave("slides/slides_cdm/images/weightedscore.png", width = 16, height = 10, units = "in", dev = "png")


governmentdata |> 
  filter(!is.na(evnt)) |> 
  mutate(evnt = if_else(evnt == 1, "Yes", "No")) |> 
  ggplot() +
  geom_histogram(aes(ruth_populism, fill = as.factor(evnt), group = as.factor(evnt)),
                 binwidth = 0.01) +
  labs(x = "Weighted Populism Score",
       y = "",
       fill = "Constitutional Change",
       caption = "\nHistogram of Continuous Weighted Government Score") +
  scale_fill_manual(values = c("#689295", "#263f3f"))

ggsave("slides/slides_cdm/images/ruthscore.png", width = 16, height = 10, units = "in", dev = "png")

# Trust Data ----

governmentdata |> 
  filter(id %in% analysisdata$id) %>% 
  arrange(country) %>% 
  group_by(country) %>% 
  mutate(group_id = cur_group_id()) %>% 
  mutate(Imputed = as.factor(if_else(
    is.na(trust_share_high) & !is.na(trust_share_high_linear_imp), "Imputed", "Original"
  ))) ->
  governmentdata_ordered

governmentdata_ordered %>% 
  filter(group_id < 21) %>%
  ggplot(aes(x = year,
             y = trust_share_high_linear_imp, 
             color = Imputed, 
             group = country))+
  geom_point(size = 1) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ country, ncol = 5) +
  scale_x_continuous(limits = c(1990,2020),
                     expand = c(0,0),
                     breaks = c("'90"=1990,
                                "'05"=2005,
                                "'20"=2020)) +
  scale_y_percent(limits = c(0,1),
                  expand = c(0,0),
                  breaks = c(0,0.5,1)) +
  scale_color_manual(values = c(color_colorful_light, color_dark)) +
  labs(x = "Trust",
       y = NULL,
       color = NULL)

ggsave("results/graphs/country_descriptives_trust.pdf",
       device = cairo_pdf,
       width = 29,
       height = 22.5,
       unit = "cm")

governmentdata_ordered %>% 
  filter(group_id > 20) %>%
  ggplot(aes(x = year,
             y = trust_share_high_linear_imp, 
             color = Imputed, 
             group = country))+
  geom_point(size = 1) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ country, ncol = 5) +
  scale_x_continuous(limits = c(1990,2020),
                     expand = c(0,0),
                     breaks = c("'90"=1990,
                                "'05"=2005,
                                "'20"=2020)) +
  scale_y_percent(limits = c(0,1),
                  expand = c(0,0),
                  breaks = c(0,0.5,1)) +
  scale_color_manual(values = c(color_colorful_light, color_dark)) +
  labs(x = "Trust",
       y = NULL)

ggsave("results/graphs/country_descriptives_trust2.pdf",
       device = cairo_pdf,
       width = 29,
       height = 22.5,
       unit = "cm")

governmentdata |> 
  mutate(imputed = as.factor(if_else(
    is.na(trust_share_high) & !is.na(trust_share_high_linear_imp), 1, 0
  ))) |> 
  ggplot(aes(x = year,
             y = trust_share_high_linear_imp, 
             color = analysis, 
             group = country))+
  geom_point(size = 0.4) +
  geom_line() +
  facet_wrap(~ country) +
  theme_minimal()

governmentdata |> 
  filter(id %in% analysisdata$id) |> 
  mutate(imputed = as.factor(if_else(
    is.na(trust_share_high) & !is.na(trust_share_high_linear_imp), 1, 0
  ))) |> 
  filter(imputed == 1) %>% 
  select(country, 
         year, 
         trust_share_high_imp_lastv, 
         trust_share_high_linear_imp,
         trust_share_high_NAyears,
         trust_share_high_NAchange,
         jud_replace) |> 
  mutate(across(c(trust_share_high_linear_imp, 
                  trust_share_high_imp_lastv,
                  trust_share_high_NAchange), 
                ~ round(., 2)),
         trust_share_high_NAyears = trust_share_high_NAyears -1) ->
  imputed_years
  
imputed_years %>% 
  filter(jud_replace == 1) %>% 
  select(-jud_replace) %>%
  mutate(country = ifelse(duplicated(country), "", country)) |> 
  gt() %>% 
  fmt_markdown(columns = everything()) %>% 
  cols_label(
    country = html("<b>Country</b>"),
    year = html("<b>Year</b>"),
    trust_share_high_imp_lastv = html("<b>Closest<br>Imputation</b>"),
    trust_share_high_linear_imp = html("<b>Linear<br>Imputation</b>"),
    trust_share_high_NAyears = html("<b>Missing<br>Years</b>"),
    trust_share_high_NAchange = html("<b>Change Between<br>Observations</b>")
  ) %>% 
  opt_table_lines("none") %>% 
  tab_options(
    table.font.size = "80%"
  ) %>% 
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_borders(sides = "top", color = "black", weight = px(1.5)),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(rows = 15)
  ) %>% 
  tab_style(
    style = cell_borders(sides = c("top"), color = "black", weight = px(0.1)),
    locations = cells_body(rows = c(1, 4, 6, 8, 10, 11, 14, 15))
  ) %>% 
  opt_table_font(font = google_font(fontname))  ->
  table_imputation

gtsave(table_imputation, "results/tables/imputed.png")
  

# General Decrease in Judicial Independence ----

governmentdata |> 
  group_by(country) |> 
  mutate(regressed = if_else(lag(v2juhcind) > v2juhcind, 1, 0)) |> 
  ungroup() |> 
  group_by(year) |> 
  summarize(n = sum(regressed, na.rm = TRUE)) |> 
  ggplot(aes(x= year, 
             y = n)) +
  geom_point() +
  geom_line()

df_final |> 
  mutate(populism = if_else(ruth_populism == 1, "Pop", "Nopop")) |> 
  ggplot(aes(x = lagged_trust_share_linear_imp_1)) +
  geom_histogram() +
  facet_grid(populism ~ firstchange5)

df_final |> 
  filter(ruth_populism == 1)

library(ggstatsplot)
ggbetweenstats(
  data = df_final,
  x = ruth_populism,
  y = lagged_trust_share_linear_imp_1
)
ggbetweenstats(
  data = df_final,
  x = ruth_populism,
  y = jud_replace_cont
)
ggbetweenstats(
  data = df_final,
  x = jud_replace,
  y = lagged_trust_share_linear_imp_1
)
df_final |>  mutate(firstchange5 = as.factor(firstchange5)) -> df_final
ggbetweenstats(
  data = df_final |> filter(),
  x = firstchange5,
  y = lagged_trust_share_linear_imp_1
)

# Tables ----

governmentdata |> 
  filter(analysis == 1) |>
  mutate(year = as.numeric(as.character(year))) |> 
  group_by(country) |> 
  dplyr::summarize(N = n(),
                   Start = min(year, na.rm = TRUE),
                   End = max(year),
                   .groups = "drop") ->
  countries

governmentdata |> 
  mutate(year = as.character(year)) |> 
  filter(is.na(trust_share) & !is.na(trust_share_linear_imp)) |> 
  group_by(country) |> 
  select(country, year) |> 
  dplyr::summarize(Imputed = paste(year, collapse = ", ")) ->
  imputed

countries |> 
  left_join(imputed, by = "country") ->
  countries

saveRDS(countries, "results/tables/country_overview.rds")

# CHANGES UNDER POPULISTS CONTENTS ----

ccpc_populism |> 
  mutate()