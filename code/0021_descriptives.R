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
analysisdata <- readRDS("data/analysis.rds") 

analysisdata |>
  mutate(id = paste0(country, year)) ->
  analysisdata

governmentdata |> 
  mutate(id = paste0(country, year)) |> 
  mutate(analysis = if_else(
    id %in% analysisdata$id, 1, 0
  )) ->
  governmentdata

## Theme ----

source("src/graphics.R")

# Validity of Policy Indice ----

partydata |> 
  filter(!is.na(gal) & !is.na(galtan)) |> 
  ggplot(aes(x = gal, y = galtan)) +
  geom_smooth(method = "lm") +
  geom_point()


partydata |> 
  mutate(gal_low = if_else(gal > 0, 1, 0),
         galtan_low = if_else(galtan > 5, 1, 0),
         category = as.factor(if_else(gal_low == galtan_low, 1, 0))) |> 
  filter(!is.na(gal) & !is.na(galtan)) |> 
  filter(e_regiongeo %in% c(17:18)) |>
  filter(gal < 50) |> 
  ggplot(aes(x = gal, y = galtan)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = category)) +
  geom_label_repel(aes(label = country_name))

validate <- lm(gal ~ galtan, data = partydata)
summary(validate)
cor.test(partydata$gal, partydata$galtan,  method = "pearson", use = "complete.obs")

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
  mutate(imputed = as.factor(if_else(
    is.na(trust_share) & !is.na(trust_share_linear_imp), 1, 0
  ))) |> 
  ggplot(aes(x = year,
             y = trust_share_linear_imp, 
             color = imputed, 
             shape = factor(analysis),
             group = country))+
  geom_point(size = 0.4) +
  geom_line() +
  facet_wrap(~ country) +
  theme_minimal()

governmentdata |> 
  mutate(imputed = as.factor(if_else(
    is.na(trust_share) & !is.na(trust_share_linear_imp), 1, 0
  ))) |> 
  ggplot(aes(x = year,
             y = trust_share_linear_imp, 
             color = analysis, 
             group = country))+
  geom_point(size = 0.4) +
  geom_line() +
  facet_wrap(~ country) +
  theme_minimal()

governmentdata |> 
  filter(jud_replace == 1) |> 
  select(country, 
         year, 
         trust_share, 
         trust_share_imp_lastv, 
         trust_share_linear_imp,
         trust_share_NAyears,
         trust_share_NAchange) |> 
  gt()

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