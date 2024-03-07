library(showtext)
library(ggthemes)
library(tidyverse)
library(hrbrthemes)
library(ggtext)

# fonts ----
font_add_google(family = "quicksand", name = "Quicksand")
showtext_auto()

fontname <- "quicksand"

if(!exists("textsize")) {
  textsize <- 22
}

# colors ----

color_dark = "#2A3C24"
color_dark_light = "#7D8C78"
color_dark_verylight = "#b2b9af"
color_colorful = "#C16200"
color_colorful_light = "#D09D6A"
color_colorful_verylight = "#FFD8B1"
color_darkgrey = "#373737"

# party colors ----

color_fpoe <- "#184e77"
color_oevp <-"#5bc0be"
color_spoe <- "#C65441"
color_neos <- "#D89C98"
color_greens <- "#588157"


# general theme

theme_base <- theme_ipsum_rc(grid = "X") +
  theme(
    axis.text = element_text(size = textsize,
                             margin = margin(t = -5),
                             family = fontname),
    axis.text.y.left = element_text(size = textsize,
                                    family = fontname),
    axis.title.y = element_text(size = textsize,
                                family = fontname),
    axis.title.x = element_text(size = textsize,
                                family = fontname),
    axis.text.x = element_text(size = textsize/100*88,
                               vjust = 0,
                               family = fontname),
    axis.ticks.x = element_line(linewidth = 0.5),
    axis.ticks.length = unit(0.4, "cm"),
    axis.line.x = element_line(linewidth = 0.5),
    strip.text = element_text(size = textsize,
                              family = fontname),
    legend.position = "bottom",
    legend.box = "vertical",  
    legend.box.just = "center",
    legend.justification = "center",
    legend.spacing = grid::unit(c(2, 2, 2, 30), "mm"),
    legend.text = element_text(size = textsize,
                               family = fontname),
    legend.title = element_text(size = textsize,
                                family = fontname),
    plot.margin = grid::unit(c(5, 10, 5, 5), "mm"),
    plot.title = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(family = fontname))

theme_set(theme_base)

# theme regression ----

# set-up ----
theme_regression <- theme_ipsum_rc(grid = "") +
  theme(
    axis.title.x = element_text(size = 30, family = fontname),
    axis.text.x = element_text(size = 30, family = fontname),
    axis.text.y = element_markdown(size = 30, family = fontname),
    axis.title.y.left = element_text(size = 30, family = fontname),
    strip.text = element_text(size = 34, family = fontname),
    panel.grid.major.y = element_line(size = 0.5, color = "lightgrey"),
    plot.title = element_text(size = 31, family = fontname),
    plot.subtitle = element_text(size = 31, family = fontname),
    plot.caption = element_text(size = 28, family = fontname, face = "plain", hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 31, family = fontname),
    legend.title = element_text(size = 30, family = fontname),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = grid::unit(c(0, 0.5, 0.5, 0), "mm"),
    panel.border = element_rect(colour = "darkgrey", fill = NA, linewidth = 1),
    text = element_text(family = fontname)
  )
