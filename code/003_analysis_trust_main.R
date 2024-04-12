## MAIN ANALYSIS FILE

depv <- "jud_replace_cont"

source("code/0031_analysistrust2.R")

depv <- "v2jupoatck"

source("code/0031_analysistrust2.R")

olsrows <- data.frame("Coefficients" = "Country FE",
                      "(1)" = "Yes",
                      "(2)" = "Yes",
                      "(3)" = "Yes",
                      "(4)" = "Yes")
attr(olsrows, "position") <- 21

modelsummary(list("No Interaction" = baseols3_jud_replace_cont, 
                  "Interaction" = ols_robust_jud_replace_cont, 
                  "No Interaction" = baseols3_v2jupoatck, 
                  "Interaction" = ols_robust_v2jupoatck),
             coef_rename = coef_names,
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             add_rows = olsrows,
             output = "latex",
             modelsummary_format_numeric_latex = "plain"
) |> 
  add_header_above(c(" " = 1, "Court Purges & Packing" = 2, "Attacks on Judiciary" = 2)) %>% 
  kable_styling() |> 
  kable_classic_2() ->
  mainmodels

writeLines(mainmodels, glue("results/tables/mainmodels.tex"))

plot1 <- readRDS("results/graphs/ols_interaction_jud_replace_cont.RDS") +
  theme(axis.title.x = element_blank())
plot2 <- readRDS("results/graphs/ols_interaction_v2jupoatck.RDS") +
  theme(axis.text.y.left = element_blank(),
        axis.title.y = element_blank())

plot1 + plot_spacer() + plot2 + plot_layout(guides = 'collect',
                                            widths = c(1,0.015,1))

ggsave("results/graphs/ols_interaction.pdf",
       device = cairo_pdf,
       width = 10,
       height = 5)

## RANDOM EFFECTS ----

olsrows <- data.frame("Coefficients" = "Clustered SE",
                      "(1)" = "Yes",
                      "(2)" = "Yes",
                      "(3)" = "Yes",
                      "(4)" = "Yes")

attr(olsrows, "position") <- 21

modelsummary(list("No Interaction" = rols_jud_replace_cont, 
                  "Interaction" =rols_ni_jud_replace_cont, 
                  "No Interaction" = rols_v2jupoatck, 
                  "Interaction" =rols_ni_v2jupoatck),
             coef_rename = coef_names,
             estimate  = "{estimate}{stars}",
             statistic = c("conf.int"),
             coef_omit = c("Intercept|country"),
             add_rows = olsrows,
             output = "latex",
             modelsummary_format_numeric_latex = "plain"
) |> 
  add_header_above(c(" " = 1, "Court Purges & Packing" = 2, "Attacks on Judiciary" = 2)) %>% 
  kable_styling() |> 
  kable_classic_2() ->
  randommodels

writeLines(randommodels, glue("results/tables/randommodels.tex"))

