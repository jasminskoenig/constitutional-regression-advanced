#### ANALYSIS DISSERTATION

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
    country != "Suriname",
    country != "Venezuela"
  ) |> 
  arrange(country, year) |> 
  group_by(country) |> 
  mutate(latin = if_else(e_regiongeo %in% c(17:18), 1, 0),
         year = as.factor(year),
         lead_libdem = dplyr::lead(v2x_libdem, 1)) |> 
  ungroup() |> 
  mutate(ruth_populism_lr = relevel(factor(ruth_populism_lr), ref = "Non-Populist")) ->
  df4

# to plm data 

plmdata <- pdata.frame(
  df4,
  index = c(
    "country",
    "year"
  )
)

# democracytypes 

demtypes <- create_names()
democracytypes <- demtypes$long

# leads 

leads <- c(1:4)
combinations_democracytypes_leads <- cross2(democracytypes, leads)

## LEFT-WING & RUTH MAIN MODEL----

models_ruth <- map(democracytypes, ~reg_main(plmdata[[.]],
                                             1,
                                             plmdata$ruth_populism_lr))

coef_names <- c(
  "populismscoreLeft-wing Populist" = "Left-wing Populist",
  "populismscoreRight-wing Populist" = "Right-wing Populist",
  "evnt" = "Constitutional Change",
  "moderator" = "Lagged Democracy Score",
  "surplus" = "Surplus Seats"
)

table_ruthmodels <- create_regressiontable(models_ruth,
                                           add_row = TRUE,
                                           row_position = 17,
                                           row_data = rows,
                                           latex = TRUE
)

writeLines(table_ruthmodels, "results/tables/constitutionalchange_ruth.tex")

## COMPLETE MODELS ----

complete_models_ruth <- map(combinations, 
                            ~reg_dem_ruth(plmdata[[.x[[1]]]], 
                                          .x[[2]], 
                                          plmdata$ruth_populism_lr))

## JACKKNIFE COUNTRIES ----
reg_dem_jackknife_ruth("v2x_libdem", 1, "ruth_populism_lr")

jackknife_plots_ruth <- map(democracytypes, ~ plot_jackknife_ruth(.,
                                                                  1,
                                                                  "ruth_populism_lr"))

jackknife_plots_ruth[[1]] + jackknife_plots_ruth[[2]] + jackknife_plots_ruth[[3]] + 
  plot_layout(guides = "collect") ->
  jackknife_appendix

ggsave("results/graphs/jackknife_plots.pdf",
       device = cairo_pdf,
       width = 10,
       height = 7)
