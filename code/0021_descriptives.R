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

## Theme ----

theme_gridY <- theme_ipsum_rc(grid = "Y") +
  theme(
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y.left = element_text(size = 18),
    strip.text = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = grid::unit(c(1, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1)
  )

theme_set(theme_gridY)


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
  filter(!is.na(gov_popul_weighted) & !is.na(ruth_populism)) ->
  gov_filtered
  
validate_populism <- glm(ruth_populism ~ gov_popul_prime, data = gov_filtered)
summary(validate_populism)

pred <- predict(validate_populism, type = "response")
pred_df <- data.frame(gov_popul_prime = gov_filtered$gov_popul_weighted, pred)

ggplot(gov_filtered, aes(x = gov_popul_prime, y = ruth_populism)) +
  geom_point(aes(color = gov_popul_prime > 0.5 & ruth_populism == 0 |
                   gov_popul_prime < 0.5 & ruth_populism == 1)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = FALSE,
              color = "darkslategrey") +
  scale_color_viridis_d(labels = c("Correct", "False")) +
  labs(color = "Matches Ruth-Lovell/Grahn")

gov_filtered |> 
  filter(gov_popul_weighted > 0.5 & ruth_populism == 0 |
           gov_popul_weighted < 0.5 & ruth_populism == 1) |> 
  select(gov_popul_weighted, ruth_populism, country, year)

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
