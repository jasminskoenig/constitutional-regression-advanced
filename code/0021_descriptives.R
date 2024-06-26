library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(ggrepel)
library(gt)
library(geomtextpath)
library(ggdist)
library(colorspace)
library(scales)

# Set Up ----

## Party Data ----

partydata <- readRDS("data/party_populism.rds")

## Government Data ----

governmentdata_world <- readRDS("data/ccpc_vdem.rds")
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

# Custom label function for dates
short_year_format_date <- function(x) {
  years <- format(x, "%Y")
  paste0("'", substr(years, 3, 4))
}

shorten_year <- function(years) {
  # Convert numeric years to string, extract the last two digits, and prepend an apostrophe
  sapply(years, function(year) paste0("'", substr(as.character(year), 3, 4)))
}


## Theme ----

source("../../dissertation/src/graphics.R")

# Scatterplot----

analysisdata %>% 
  arrange(country) %>% 
  group_by(country) %>% 
  mutate(group_id = cur_group_id()) %>% 
  ungroup() ->
  analysisdata

analysisdata %>% 
  filter(group_id < 21) %>% 
  mutate(trust = if_else(trust_share_low_linear_imp_mean_3 < 0.33, "low", if_else(trust_share_low_linear_imp_mean_3 < 0.66, "medium", "high")),
         ruth_populism = if_else(ruth_populism == 1, "Populist", "Non-Populist")) %>% 
  ggplot(aes(x = trust_share_low_linear_imp_mean_3,
             y = jud_replace_cont)) +
  geom_smooth(method="lm") +
  geom_point(aes(color = as.factor(ruth_populism), 
                 shape = as.factor(ruth_populism)),
             size = 0.8) +
  facet_wrap(~country, ncol = 5) +
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
       width = 32,
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
  geom_point(aes(color = as.factor(ruth_populism), 
                 shape = as.factor(ruth_populism)),
             size = 0.8) +
  facet_wrap(~country, ncol = 5) +
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
       width = 32,
       height = 22.5,
       unit = "cm")

analysisdata2 %>% 
  arrange(country) %>% 
  group_by(country) %>% 
  mutate(group_id = cur_group_id()) %>% 
  ungroup() ->
  analysisdata2

analysisdata2 %>% 
  filter(group_id < 21) %>% 
  mutate(trust = if_else(trust_share_low_linear_imp_mean_3 < 0.33, "low", if_else(trust_share_low_linear_imp_mean_3 < 0.66, "medium", "high")),
         ruth_populism = if_else(ruth_populism == 1, "Populist", "Non-Populist")) %>% 
  ggplot(aes(x = trust_share_low_linear_imp_mean_3,
             y = v2jupoatck)) +
  geom_smooth(method="lm") +
  geom_point(aes(color = as.factor(ruth_populism), 
                 shape = as.factor(ruth_populism)),
             size = 0.8) +
  facet_wrap(~country, ncol = 5) +
  scale_x_continuous(limits = c(0, 1),
                     expand = c(0,0),
                     breaks = c("0" = 0,"0.5"=0.5,"1"=1)) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(-3,3, by = 3)) +
  scale_color_manual(values = c(color_dark, color_colorful),
                     name = NULL) +
  coord_cartesian(ylim=c(-4, 3)) +
  labs(shape = NULL,
       y = "Attacks on Judiciary",
       x = "Trust in Judiciary")

ggsave("results/graphs/country_descriptives_attacks.pdf",
       device = cairo_pdf,
       width = 32,
       height = 22.5,
       unit = "cm")

analysisdata2 %>% 
  arrange(country) %>% 
  group_by(country) %>% 
  mutate(group_id = cur_group_id()) %>% 
  filter(group_id > 20) %>%
  ggplot(aes(x = trust_share_low_linear_imp_mean_3,
             y = v2jupoatck)) +
  geom_smooth(method="lm") +
  geom_point(aes(color = as.factor(ruth_populism), 
                 shape = as.factor(ruth_populism)),
             size = 0.8) +
  facet_wrap(~country, ncol = 5) +
  scale_x_continuous(limits = c(0, 1),
                     expand = c(0,0),
                     breaks = c("0" = 0,"0.5"=0.5,"1"=1)) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(-3,3, by = 3)) +
  scale_color_manual(values = c(color_dark, color_colorful),
                     name = NULL) +
  coord_cartesian(ylim=c(-4, 3)) +
  labs(shape = NULL,
       y = "Attacks on the Judiciary",
       x = "Trust in Judiciary")

ggsave("results/graphs/country_descriptives2_attacks.pdf",
       device = cairo_pdf,
       width = 32,
       height = 22.5,
       unit = "cm")


# Histogram Jud Replacement ----

textsize <- textsize-6
source("../../dissertation/src/graphics.R")

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

textsize <- textsize-2
source("../../dissertation/src/graphics.R")

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
       x = "Trust in Judiciary") +
  coord_cartesian(clip = "off")

ggsave("results/graphs/trust_histogram.pdf",
       device = cairo_pdf,
       width = 12,
       height = 8,
       unit = "cm")


textsize <- textsize+6
source("../../dissertation/src/graphics.R")

## JUDICIARY AROUND THE WORLD ----

governmentdata_world %>% 
  group_by(year, jud_replace, region) %>% 
  reframe(n = n()) %>% 
  arrange(year, region, desc(jud_replace)) %>% 
  group_by(region, year) %>% 
  slice(1) %>%  
  mutate(n = if_else(jud_replace == 0, 0, n),
         hjust = if_else(region %in% c("Europe", "Oceania"),
                         0.665,
                         0.5)) %>% 
  ggplot(aes(x = year, 
             y = n, 
             color = region, 
             group = region, 
             label = region,
             linetype = region,
             hjust = hjust),
         linewidth = 2) +
  geom_textline(show.legend = FALSE) +
  labs(x = NULL,
       y = NULL,
       color = "Continent",
       linetype = "Continent") +
  scale_y_continuous(limits = c(0,40),
                     expand = c(0,0),
                     breaks = seq(10,80, by = 10)) +
  scale_x_continuous(limits= c(1990, 2022),
                     expand = c(0,0),
                     breaks = seq(1990, 2022, by = 8)) +
  scale_color_manual(values = c(color_dark, color_colorful, color_blue, color_yellow, color_brown)) +
  guides(col = guide_legend(ncol = 2))

ggsave("results/graphs/judges_replacement.pdf",
       width = 17,
       height = 17*0.618,
       unit = "cm",
       device = cairo_pdf)

governmentdata_world %>% 
  mutate(region = if_else(e_regiongeo == 16, "North America", if_else(region == "North & Latinamerica", "Latinamerica", region))) %>% 
  filter(!is.na(jud_replace)) %>% 
  group_by(year, jud_replace, region) %>% 
  reframe(n = n()) %>% 
  group_by(year, region) %>% 
  mutate(n_reg = sum(n),
         share = n/n_reg) %>%
  mutate(jud_replace = if_else(jud_replace == 0, "No", "Yes")) %>% 
  mutate(share = if_else(region == "Oceania" & year == 2000 & jud_replace == "No", share  + 1, if_else(region == "Oceania" & year == 2015 & jud_replace == "No", share + 0.5, share))) %>% 
  ggplot(aes(x = year,
             y = share,
             fill = as.factor(jud_replace),
             group = as.factor(jud_replace),
             alpha = as.factor(jud_replace),
             color = as.factor(jud_replace))) +
  geom_area() +
  facet_wrap(~ region, axes = "all_x") +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1990, 2022, by = 8),
                     labels = shorten_year(seq(1990, 2022, by = 8)),
                     limits = c(1990, 2022),
                     name = NULL) +
  scale_y_percent(expand = c(0,0),
                     breaks = seq(0.25,1, by = 0.25),
                  name = NULL) +
  scale_fill_manual(values = c(color_dark_light, color_colorful_light), name = "Court Purges or Packing") +
  scale_color_manual(values = c("white", color_colorful_light), name = "Court Purges or Packing") +
  scale_alpha_manual(values = c(0.3, 0.8), name = "Court Purges or Packing") +
  coord_cartesian(ylim = c(0,1)) 

ggsave("results/graphs/judicial_purges.pdf",
       width = 29,
       height = 21,
       units = "cm",
       device = cairo_pdf)

governmentdata %>% 
  group_by(year) %>% 
  reframe(mean = mean(jud_replace_cont)) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_line() +
  scale_x_continuous(limits = c(1990, 2020),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0.5, 1),
                     expand = c(0,0),
                     breaks = seq(0.6,1, by = 0.00001))

governmentdata_world %>% 
  mutate(region = if_else(e_regiongeo == 16, "North America", if_else(region == "North & Latinamerica", "Latinamerica", region)),
         emphasis = if_else(region == "Europe", 0.8, if_else(region=="North America", 1, 0.5)),
         hjust = if_else(region == "Europe", 0.7, if_else(region == "Oceania", 0.25, 0.35))) %>% 
  group_by(year, region, emphasis, hjust) %>% 
  reframe(mean = mean(v2jupoatck, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(x = year, 
                y = mean,
                color = as.factor(emphasis),
                group = as.factor(region)),
            show.legend = FALSE) +
  facet_wrap(~region, axes = "all_x") +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(1990, 2022),
                     expand = c(0,0),
                     breaks = c("'90" = 1990,
                                "'06" = 2006,
                                "'22" = 2022)) +
  labs(y = "← Fewer Attacks                                                                                                                                More Attacks →") +
  scale_color_manual(values = c(color_dark, color_colorful, color_neutral),
                     name = NULL) +
  coord_cartesian(clip = "off",
                  ylim = c(-3, 4)) +
  scale_color_manual(values = c(color_neutral, color_colorful_light, color_colorful)) +
  coord_cartesian(clip = "off") +
  theme(axis.title.y.left = element_text(size = textsize/2,
                                         family = "Arial"))

ggsave("results/graphs/attacks_judiciary.pdf",
       width = 29,
       height = 21,
       unit = "cm",
       device = cairo_pdf)

governmentdata_world %>% 
  mutate(diff = v2jupoatck - dplyr::lag(v2jupoatck)) %>% 
  filter(year > 2010 & e_regiongeo %in% c(1:4, 16:18)) %>%  
  arrange(diff) %>%  
  select(diff, v2jupoatck, country, year, ruth_populism) %>% 
  filter(country != "Guyana") %>% 
  slice(1:10) %>% 
  pull(country) ->
  countries_jud_decrease
  
governmentdata_world %>% 
  filter(country %in% countries_jud_decrease) %>% 
  mutate(ruth_populism = if_else(country == "United States of America" & year %in% c(2017:2021), "Populist", ruth_populism)) %>% 
  ggplot() +
  geom_line(aes(x = year, 
                y = v2jupoatck,
                color = ruth_populism,
                group = country)) +
  labs(x = NULL,
       y = NULL) +
  facet_wrap(~country, ncol = 5, axes = "all_x") +
  scale_x_continuous(limits = c(1990, 2022),
                     expand = c(0,0),
                     breaks = c("'90" = 1990,
                                "'06" = 2006,
                                "'22" = 2022)) +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(-2,4, by = 2)) +
  labs(y = "← Fewer Attacks                                                                                                                                More Attacks →") +
  scale_color_manual(values = c(color_dark, color_colorful, color_neutral),
                     name = NULL) +
  coord_cartesian(clip = "off",
                  ylim = c(-3, 4)) +
  theme(axis.title.y.left = element_text(size = textsize/2,
                                             family = "Arial"))

ggsave("results/graphs/attacks_judiciary_cases.pdf",
       width = 29,
       height = 21,
       unit = "cm",
       device = cairo_pdf)

## POPULISM AROUND THE WORLD

governmentdata %>% 
  group_by(region, ruth_populism, year) %>% 
  reframe(n = n()) %>% 
  filter(ruth_populism == "Populist") %>% 
  ggplot(aes(x = year, y = n, group = region, color = region)) +
  geom_line()

governmentdata %>% 
  filter(region == "Europe") %>% 
  group_by(ruth_populism) %>% 
  mutate(year = ymd(year, truncated = 2L)) %>% 
  filter(!all(is.na(ruth_populism))) %>% 
  ggplot(aes(x = year,
             y = fct_rev(country))) +
  geom_tile(aes(fill = ruth_populism),
            color = "white",
            height = 1) +
  scale_x_date(labels = short_year_format_date, 
               date_breaks = "2 years",
               limits = c(ymd("1994-12-31"), ymd("2022-12-31")),
               expand = c(0,0)) +
  labs(x = NULL,
       y = NULL,
       fill = "Government in Office...",
       title = "Europe") +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = c(color_dark_light, color_colorful_light)) +
  theme(panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        legend.title.position = "top",
        legend.title = element_text(size = textsize*0.88),
        plot.title = element_text(size = textsize,
                                  family = fontname,
                                  hjust = 0.091,
                                  face = "bold"),
        plot.title.position = "plot") ->
  europe

governmentdata %>% 
  filter(region == "North & Latinamerica") %>% 
  group_by(ruth_populism) %>% 
  filter(!all(is.na(ruth_populism))) %>% 
  mutate(year = ymd(year, truncated = 2L)) %>% 
  ggplot(aes(x = year,
             y = fct_rev(country))) +
  geom_tile(aes(fill = ruth_populism),
            color = "white",
            height = 1) +
  scale_x_date(labels = short_year_format_date, 
               date_breaks = "2 years",
               limits = c(ymd("1994-12-31"), ymd("2022-12-31")),
               expand = c(0,0)) +
  labs(x = NULL,
       y = NULL,
       fill = "Government in Office...",
       title = "Latin America") +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = c(color_dark_light, color_colorful_light)) +
  theme(panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        legend.title.position = "top",
        legend.title = element_text(size = textsize*0.88),
        plot.title = element_text(size = textsize,
                                  family = fontname,
                                  hjust = 0.0015,
                                  face = "bold"),
        plot.title.position = "plot") ->
  latinamerica

europe + latinamerica + patchwork::plot_layout(heights = c(5,3),
                                               guides = "collect")

ggsave("results/graphs/populistsinoffice.pdf",
       height = 29*1.5,
       width = 21*1.5,
       units = "cm",
       device = cairo_pdf)

## RELATIONSHIP POPULISM & JUDICIARY

theme_set(theme_bar)

governmentdata %>% 
  filter(!is.na(ruth_populism)) %>% 
  mutate(region = if_else(e_regiongeo == 16, "North America", if_else(region == "North & Latinamerica", "Latinamerica", region)),
         emphasis = if_else(region == "Europe", 0.8, if_else(region=="North America", 1, 0.5)),
         hjust = if_else(region == "Europe", 0.7, if_else(region == "Oceania", 0.25, 0.35))) %>% 
  ggplot(aes(x = as.factor(ruth_populism),
             y = v2jupoatck,
             color = as.factor(ruth_populism),
             fill = as.factor(ruth_populism))) +
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.32, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA,
    alpha = 0.5
  ) + 
  geom_boxplot(
    width = .15, 
    ## remove outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## add some transparency
    alpha = .1,
    shape = 95,
    size = 6,
    transformation = PositionIdentity
  ) +
  geom_text(data = governmentdata %>% filter(country == "Brazil" & year == 2023),
             aes(label = paste(country, year, sep = " "),
                 y = v2jupoatck - 0.8,
                 x = 2.15),
            family = fontname,
            color = color_neutral) + 
  geom_curve(data = governmentdata %>% filter(country == "Brazil" & year == 2023),
             aes(y = v2jupoatck - 0.00001,
                 x = 1.81,
                 yend = v2jupoatck - 0.8,
                 xend = 2),
             color = color_neutral,
             linewidth = 0.7,
             curvature = 0.4,
             ncp = 3000, 
             alpha = 0.7) +
  geom_text(data = governmentdata %>% filter(country == "Romania" & year == 2018),
            aes(label = paste(country, year, sep = " "),
                y = v2jupoatck + 0.8,
                x = 1.15),
            family = fontname,
            color = color_neutral) + 
  geom_curve(data = governmentdata %>% filter(country == "Romania" & year == 2018),
             aes(y = v2jupoatck + 0.00001,
                 x = 0.81,
                 yend = v2jupoatck + 0.8,
                 xend = 1),
             color = color_neutral,
             linewidth = 0.7,
             curvature = -0.4,
             ncp = 3000,
             alpha = 0.7) +
  annotate(geom = "text",
           x = 0.536,
           y = -2.9,
           label = "Fewer Attacks",
           family = fontname,
           color = color_neutral) +
  annotate(geom = "text",
           x = 0.523,
           y = 5.1,
           label = "More Attacks",
           color = color_neutral) +
  labs(x = NULL,
       y = "Government attacks on judiciary") +
  scale_color_manual(values = c(color_dark, color_colorful)) +
  scale_fill_manual(values = c(color_dark, color_colorful)) +
  scale_y_continuous(breaks = c(-3, 0, 3, 5),
                     limits = c(-3, 5.1)) +
  theme(legend.position="none")

ggsave("results/graphs/populism_attacks.pdf",
       width = 17,
       height = 17*0.8,
       unit = "cm",
       device = cairo_pdf)

# Independent Judiciary & Populism

governmentdata %>% 
  filter(judicial_independence_mean_mean_3 > 1.8 & ruth_populism == "Populist") %>% 
  View()

# CONST CHANGE PAPER ----

textsize + 10
source("../../dissertation/src/graphics.R")

governmentdata |> 
  mutate(latin = as.factor(latin)) |> 
  ggplot(aes(x = gov_popul_weighted)) +
  geom_curve(aes(x = 0.17, y = 164, xend = 0.25, yend = 190),
             curvature = -0.3,
             color = color_neutral,
             linewidth = 0.001,
             ncp = 500) +
  annotate(geom = "text",
           x = 0.28,
           y = 190,
           label = "Complete Dataset",
           color = color_neutral,
           family = fontname,
           hjust = 0,
           size = 7) +
  geom_curve(aes(x = 0.637, y = 32, xend = 0.67, yend = 55),
             curvature = 0.3,
             color = color_neutral,
             linewidth = 0.001,
             ncp = 500) +
  annotate(geom = "text",
           x = 0.67,
           y = 58,
           label = "Share of Continent",
           color = color_neutral,
           family = fontname,
           hjust = 0.3,
           size = 7,
           vjust = 0) +
  geom_histogram(color = "white",
                 linewidth = 0.3,
                 alpha = 0.2) +
  geom_histogram(data = governmentdata %>%  filter(latin == 1),
                 color = "white",
                 linewidth = 0.3,
                 alpha = 0.8,
                 aes(fill = latin),
                 show.legend = FALSE) +
  scale_fill_manual(values = c(color_colorful),
                    labels = c("Latin America")) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = c("0" = 0,
                                "0.25" = 0.25,
                                "0.5" = 0.5,
                                "0.75" = 0.75,
                                "1" = 1)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,250),
                     breaks = seq(50,250, by = 50)) +
  labs(y = "Observations",
       x = "Weighted Populism Score per Government",
       fill = element_blank(),
       title = "Latin America") +
  theme(plot.title = element_text(family = fontname,
                                  size = textsize*1.5,
                                  hjust = 0,
                                  vjust = 3)) ->
  LA

governmentdata |> 
  mutate(latin = as.factor(latin)) |> 
  ggplot(aes(x = gov_popul_weighted)) +
  geom_histogram(color = "white",
                 linewidth = 0.3,
                 alpha = 0.2) +
  geom_histogram(data = governmentdata %>%  filter(latin == 0),
                 color = "white",
                 linewidth = 0.3,
                 alpha = 0.8,
                 aes(fill = latin),
                 show.legend = FALSE) +
  scale_fill_manual(values = c(color_dark),
                    labels = c("Europe")) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = c("0" = 0,
                                "0.25" = 0.25,
                                "0.5" = 0.5,
                                "0.75" = 0.75,
                       "1" = 1)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,250),
                     breaks = seq(50,250, by = 50)) +
  labs(y = "Observations",
       x = "Weighted Populism Score per Government",
       fill = element_blank(),
       title = "Europe") +
  theme(plot.title = element_text(family = fontname,
                                  size = textsize*1.5,
                                  hjust = 0,
                                  vjust = 3)) ->
  EU

EU + LA +
  patchwork::plot_layout(axis_titles = "collect",
              axes = "collect",
              guides = "collect")

ggsave("results/graphs/histogram_populistgov.pdf",
       device = cairo_pdf,
       width = 14,
       height = 7)

## Change of Rights ----

textsize <- textsize -10
source("../../dissertation/src/graphics.R")

rights_labs <- c("Rights of Executive", "Rights of Judiciary", "Political Rights", "Social Rights")
names(rights_labs) <- c("diff_executive", "diff_judiciary", "diff_rights_political", "diff_rights_social")

theme_set(theme_bar)

rect_data <- tibble(
  ymin = c(-10,-5, -0.2, 0.2,5,10, 15),
  ymax = c(-5, -0.2, 0.2, 5,10,15, 20),
  alpha = c(0.2, 0.2, 1, 0.2, 0.2, 0.2, 0.2)
)

colors <- c(color_colorful, 
color_colorful_light, 
color_dark_verylight, 
color_dark_light, 
color_dark)
colors <- adjust_transparency(colors, alpha = 0.2)

yintercepts <- tibble(yintercept = c (-5, 0, 5, 10),
                      label = c("-5", "0", "5", "10"))

governmentdata %>% 
  dplyr::select(country, gov_popul_weighted, year, e_regiongeo, diff_rights_ind, diff_rights_social, diff_rights_political, diff_executive, diff_judiciary, v2x_libdem) %>% 
  filter(gov_popul_weighted > 0.5) %>% 
  dplyr::select(-diff_rights_ind) %>%
  filter(year > 1990) %>% 
  dplyr::select(-v2x_libdem) %>% 
  pivot_longer(cols = contains("diff"), values_to = "n", names_to = "rights") %>% 
  mutate(col = case_when(
    n > 0 & rights == "diff_executive" ~ color_colorful_light,
    n < 0 & rights == "diff_executive" ~ color_dark_light,
    n > 0 & rights != "diff_executive" ~ color_dark_light,
    TRUE ~ color_colorful_light
    )) %>% 
  filter(n != 0) %>% 
  mutate(continent = case_when(
    e_regiongeo %in% c(17:29) ~ "Latinamerika",
    e_regiongeo %in% c(1:4) ~ "Europe",
    TRUE ~ NA),
    case = paste(country, year, sep = "\n")) %>% 
  ggplot() +
  geom_hline(data = yintercepts,
             aes(yintercept = yintercept),
             color = "lightgray", 
             linewidth = 0.7) +
  geom_point(x = 1, y = -10, color = "lightgray", size = 0.8) +
  # geom_rect(data = rect_data,
  #           aes(ymin = ymin,
  #               ymax = ymax,
  #               xmin = -Inf,
  #               xmax = Inf,
  #               fill = ymax),
  #           color = NA) +
  geom_hline(yintercept = 0, color = "black") +
  geom_col(aes(x = case, 
               y = n,
               group = col,
               fill = col), 
                   color = NA,
                   position = "dodge", # Adjust the density of the pattern
                   alpha = 0.7
  ) +
  facet_wrap(~ rights,
             labeller = labeller(rights = rights_labs),
             ncol = 2) +
  coord_curvedpolar(clip = "off") +
  scale_y_continuous(limits = c(-10, 13),
                     breaks = c(-15, -10, -5, 0, 5, 10)) +
  scale_fill_identity() +
  theme_void()+
  theme( 
    axis.text.x = element_text(
    family = fontname,
    size = textsize*0.88
  ),
  panel.spacing = unit(2, "lines"),
  strip.text = element_text(
    family = fontname,
    size = textsize + 3,
    margin = margin(b = 10),
    face = "bold"
  )) ->
  plot

ggplot(governmentdata, aes(country, diff_rights_executive)) +
  geom_point(aes(x = "Venezuela",
                 y = -10),
             color = "lightgrey") +
  geom_texthline(data = yintercepts,
                 aes(yintercept = yintercept,
                     label = label),
             color = "lightgray", 
             linewidth = 0.7,
             size = 3, 
             family = fontname) +
  geom_text(data = yintercepts,
            aes(x = "Venezuela",
                y = yintercept, 
                label = label),
            color = "black",
            size = 3,
            family = fontname) +
  annotate(geom = "text",
       x = "Venezuela",
       y = -10, 
       label = -10,#
       color = "black",
       vjust = 1.1,
       size = 3,
       family = fontname) +
  coord_polar(clip = "off") +
  theme_void() -> 
  legend

ggplot(governmentdata, aes(country, diff_rights_executive)) +
  annotate(geom = "richtext",
           x = "Venezuela",
           y = -10, 
           label = "<b>Number of rights</b> that were added or removed <br>from the constitution.<br>In case of political, social or right for the judiciary,<br>increases in rights are <span style='color:#7D8C78'>**democratic improvements**</span>.<br>If the executive gains more rights, it is a <span style='color:#D09D6A'><b>democratic<br>decline</b></span>.",
           color = "black",
           fill = NA, 
           label.color = NA,
           size = 3,
           hjust = 0,
           family = fontname) +
  scale_x_discrete(expand = c(-20,-10, 0, -0)) +
  theme_void() +
  theme(plot.margin = margin(c(0,0,0,0))) ->
  text

design <- "AAAAAAA
           BBBBBBB
           #CCDDDD"

plot + patchwork::plot_spacer()+ legend + text  + patchwork::plot_layout(ncol = 1,
                                       heights = c(5, 0.1, 1.5, 0.01),
                                       design = design)

ggsave("results/graphs/rights_change.pdf",
       width = 16,
       height = 20,
       units = "cm",
       device = cairo_pdf)

+# Validty of V-Party Populism Score & Ruth-Lovell/Grahn ----

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
    style = cell_borders(sides = c("top"), color = "black", weight = px(0.00001)),
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