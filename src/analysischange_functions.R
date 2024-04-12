library(ggrepel)
library(glueformula)

# DISSERTATION VERSION ----

reg_main <- function(democracyscore, 
                     leadyears, 
                     populismscore, 
                     moderator = FALSE){
  
  #' Runs a Country-FE Regression on the plmdata
  #' Takes df columns from plmdata as input for democracyscore (dep. var.)
  #' Takes df columns from plmdata as input for populismscore and moderator
  #' Takes integer for leadyears
  
  interaction  <-  paste("lead(democracyscore, leadyears) ~ populismscore * evnt")
   
  if (is.double(moderator)){
    interaction <-  paste(interaction, "* moderator")
  } 
  
  form <- paste(interaction, "+ surplus")
 
  form_final <- as.formula(form)
  
  mainmodel <- plm(form_final,
      model = "within",
      se = "HC1",
      data = plmdata)
  
  return(mainmodel)
  
}

reg_dem <- function(democracyscore, leadyears, populismscore, moderator){
  
  #doesn't work yet
  suppressWarnings('Setting class(x) to multiple strings ("pseries", "Interval", ...); result will no longer be an S4 object')
  
  models = list(
    # mixed model predictor only populism
    "Populism" =  plm(lead(democracyscore, 1) ~ populismscore,
                      model = "within",
                      se = "HC1",
                      data = plmdata),
    
    # mixed model predictor only event
    "Event" =  plm(lead(democracyscore, 1) ~ evnt,
                      model = "within",
                      se = "HC1",
                      data = plmdata),
    
    # mixed model predictor only moderator
    "Moderator" =  plm(lead(democracyscore, 1) ~ moderator,
                   model = "within",
                   se = "HC1",
                   data = plmdata),
    # Controls
    "Controls" = plm(lead(democracyscore, 1) ~ populismscore + evnt + moderator + surplus,
                                model = "within",
                                se = "HC1",
                                data = plmdata),
    
    # mixed model with three-way interaction and controls
    "Interaction and Controls" = reg_main(democracyscore, leadyears, populismscore, moderator)
  )
  
  # Save models as Latex Table
  
  rows <- data.frame(
    "Coefficients" = "Country FE",
    "(1)" = "Yes",
    "(2)" = "Yes",
    "(3)" = "Yes",
    "(4)" = "Yes",
    "(5)" = "Yes"
  )
  
  table <- create_regressiontable(models,
                                             add_row = TRUE,
                                             row_position = 17,
                                             row_data = rows,
                                             latex = TRUE
  )
  
  inputname <- deparse(substitute(democracyscore))
  name <- str_remove(inputname, ".*\\$")
  
  tablename <- paste0("results/tables/constitutionalchange_", name, ".tex")
  
  writeLines(table, tablename)
  
  # Return models for further analyses
  
  return(models)

  
}

calc_ame <- function(model) {
  
  
  levels <- seq(
    min(model$model$populismscore, na.rm = TRUE),
    max(model$model$populismscore, na.rm = TRUE),
    0.05
  )
  
  pred_data <- datagrid(evnt = c(0,1), 
                        populismscore = levels, 
                        moderator = c(0,1),
                        model = model)
  
  slopes(model,
                  variables = "evnt",
                  newdata = pred_data
  ) |> 
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
  
  return(meff)
}

create_names <- function() {

  
  tibble(short = c("mld", 
                   "mpa", 
                   "mpa", 
                   #"mdd", 
                   "med",
                   "mcs"),
         long = c("v2x_libdem", 
                  "v2x_polyarchy",
                   "v2x_partip", 
                   #"v2x_delibdem", 
                   "v2x_egaldem",
                  "v2x_cspart"),
         name =  c("Liberal Democracy",
                   "Electoral Democracy",
                   "Participation",
                   #"Deliberative Democracy",
                   "Egalitarian Democracy",
                   "Civil Society"),
         order = c(1,2,3,4,5)) ->
    lookup
  
  return(lookup)
}

get_name <- function(model, from = "model", whitespace = FALSE){
  
  lookup <- create_names()
  
  if (from == "model") {
    lookup |> 
      filter(short == model) ->
      demtype
  } else if (from == "var") {
    lookup |> 
      filter(long == model) ->
      demtype
  }
  
  if (whitespace == TRUE) {
    demtype |>
      mutate(name = case_when(
        str_detect(name, "Egalitarian") ~ paste0("<span style='color: white'>ss</span>", name),
        str_detect(name, "Liberal") ~ paste0("<span style='color: white'>sss</span>", name),
        TRUE ~ name
      )) ->
      demtype
  }
  
  demtype |> 
    pull(name) ->
    name
  
  return(name)
}


plot_ame <- function(predictedata, 
                     model, 
                     add_histogram = FALSE, 
                     adapt_tag = FALSE){

  
  modelname <- deparse(substitute(model))
  
  name <- get_name(modelname, whitespace = TRUE)
  
  predictedata |> 
    mutate(moderator = if_else(moderator == 0, 
                               "Right-Wing", 
                               "Left-Wing")) |> 
    ggplot(aes(x = populismscore,
               y = estimate)) +
    geom_hline(yintercept = 0, 
               color = "#C95D63", 
               linetype = "dashed") +
    geom_ribbon(aes(ymin = cilower, 
                    ymax = ciupper, 
                    alpha = ci_size, 
                    fill = moderator)) +
    geom_line() +
    labs(x = "Government Populism Score",
         y = "AME of Constitutional Change",
         fill = element_blank(),
         tag = name,
         alpha = "CI-Level") +
    facet_wrap(~ moderator) +
    scale_x_continuous(limits = c(0,1),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(-0.12, 0.12)) +
    scale_alpha_manual(values = c(0.7, 0.5, 0.2),
                       labels = c("90%", "95%", "99%")) +
    scale_fill_manual(values = c(color_dark, color_colorful)) +
    theme(strip.text = element_blank(),
          plot.margin = margin(r = 25, l =10, b = 10),
          plot.tag = element_markdown(angle = -90,
                                  size = textsize,
                                  face = "bold",
                                  lineheight = 0.7,
                                  hjust = 0.2,
                                  margin = margin(l = 20)),
          plot.tag.position = "right"
          ) ->
    main
  main
    
  if (add_histogram){
    model$model |> 
      as.data.frame() |> 
      mutate(moderator = if_else(moderator == 0, 
                                 "Right-Wing", 
                                 "Left-Wing")) |> 
      ggplot(aes(x = populismscore)) + 
      geom_histogram(bins = 100) +
      scale_x_continuous(limits = c(0, 1))  +
      scale_y_continuous(expand = c(0,0)) +
      facet_wrap(~ moderator) +
      theme_void() +
      theme(strip.text = element_text(size = textsize + 10,
                                      family = fontname,
                                      margin = margin(b = 10),
                                      hjust = 0),
            plot.margin = margin(r = 50)) ->
      histo
    
    histo + main + plot_layout(ncol = 1,
                               heights = c(1, 4)) ->
      main
  }
    
    main
    return(main)
  
} 

create_regressiontable <-  function(modellist, 
                                    add_row = FALSE,
                                    row_position = NULL,
                                    row_data = NULL,
                                    latex = FALSE){
  
  #' Creates a Latex Regression Table 
  #' Takes a list of regression model as input
  #' 
  #' Optionally a row can be added to the model with add_row
  #' row_data needs to be a dataframe
  #' row_position takes integer and declares row in which additional row should be displayed
  
  if (add_row) {
    attr(row_data, "position") <- row_position
  } 
  
  if (latex) {
    type <- "latex"
  } else {
    type <- "default"
  }
  
  names <- create_names() |> 
    mutate(name = str_squish(str_remove(name, "Democracy")))
  
  if (length(modellist) == length(names$name)){
    names |> 
      arrange(order) ->
      names
    
    names(modellist) <- names$name
  }
   
  modelsummary(modellist,
               estimate  = "{estimate}{stars}",
               statistic = c("conf.int"),
               coef_omit = c("Intercept"),
               coef_rename = coef_names,
               add_rows = row_data,
               output = type
  ) |> 
    kable_styling() |> 
    kable_classic_2() ->
    table
  
  return(table)
}

reg_dem_jackknife <- function(democracyscore, leadyears, populismscore, moderator) {
  
  jack.reg <- plmdata$country |> 
    unique()
  
  jackedreg <- data.frame(country = character(), 
                          coefjack = numeric(),
                          stringsAsFactors = FALSE)
  
  for (i in jack.reg) {
    
    print(i)
    
    df_temp <- df4[df4$country != i, ]
    
    plm_temp <- pdata.frame(
      df_temp, 
      index = c(
        "country",
        "year"
      )
    )
    
    # create formula
    form <- as.formula(sprintf("lead(%s, %d) ~ %s * evnt * %s", democracyscore, leadyears, populismscore, moderator))
    
    # run regression
    model = plm(form,
                model = "within",
                se = "HC1",
                data = plm_temp)
    
    # get coefficient of triple interaction
    coefficients <- coef(model)
    index <- grep(paste0(populismscore, ":evnt:", moderator), names(coefficients))
    coefficient_value <- coefficients[index]

    newobs <- data.frame(country = i, 
                         coefjack = coefficient_value)

    jackedreg <- rbind(jackedreg, newobs)
    
  }
  
  return(jackedreg)
}

reg_dem_jackknifelead <- function(democracyscore, leadyears, populismscore, moderator) {
  
  jackedreg <- data.frame(lead = character(), 
                          coefjack = numeric(), 
                          ci_low = numeric(),
                          ci_high = numeric(),
                          stringsAsFactors = FALSE)
  
  for (i in c(1,2,3,4)) {
    
    # create formula
    form <- as.formula(sprintf("lead(%s, %d) ~ %s * evnt * %s", democracyscore, i, populismscore, moderator))
    
    # run regression
    model = plm(form,
                model = "within",
                se = "HC1",
                data = plmdata)

    # get coefficient and ci of triple interaction
    coefficients <- coef(model)
    cis <- confint(model)
    name_interaction <- paste0(populismscore, ":evnt:", moderator)
    index <- grep(name_interaction, names(coefficients))
    coefficient_value <- coefficients[index]
    ci_value <- cis |> 
      as.data.frame() |> 
      rownames_to_column("name") |> 
      filter(name %in% name_interaction)
    
    # save new observation
    newobs <- data.frame(country = i, 
                         coefjack = coefficient_value,
                         ci_low = ci_value[2],
                         ci_high = ci_value[3])
    
    # bind to dataframe
    jackedreg <- rbind(jackedreg, newobs)
    
  }
  
  lookup <- create_names()
  
  jackedreg |> 
    mutate(democracytype = democracyscore) |> 
    left_join(lookup |> select(name, long),
              by = join_by("democracytype" == "long"))->
    jackedreg
  
  return(jackedreg)
}

find_outlier <- function(x) {
  
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  
}

plot_jackknife <- function(democracyscore, 
                           leadyears, 
                           populismscore, 
                           moderator){
  
  name <- get_name(democracyscore, 
           from = "var",
           whitespace = FALSE)
  
  coefs <- reg_dem_jackknife(democracyscore, 
                             leadyears, 
                             populismscore, 
                             moderator)
  
  coefs |> 
    mutate(label = ifelse(find_outlier(coefjack), country, NA),
           Outlier = if_else(!is.na(label), "Yes", "No")) ->
    coefs
  
  coefs |> 
    ggplot(aes(x = coefjack)) +
    geom_text_repel(aes(
      x = coefjack,
      y = 1,
              label = label,
      nudge_y = 10,
              box.padding = 1.5,
              segment.curvature = -0.1,
              segment.ncp = 3,
              segment.angle = 20)) +
    labs(y = "N",
         x = "Coefficient of Triple-Interaction",
         title = name) +
    geom_histogram(aes(fill = Outlier,
                       group = Outlier)) +
    scale_x_continuous(limits = c(0.04, 0.12)) +
    scale_y_continuous(limits = c(0, 20), 
                       expand = c(0,0)) +
    scale_fill_manual(values = c(color_dark, color_colorful)) ->
    plot
  
  return(plot)
  
}

create_regressiontable_leads <- function(var){
  
  leads <- c(1, 2, 3, 4)
  
  models <- map(leads, ~reg_main(plmdata[[var]],
                                 .,
                                 plmdata$gov_popul_weighted,
                                 plmdata$gov_left))
  
  rows <- rows |> 
    select(-last_col())
  
  tables <- create_regressiontable(models,
                                   add_row = TRUE,
                                   row_position = 17,
                                   row_data = rows,
                                   latex = TRUE
  )
  
  
  return(tables)
}

create_regressiontable_lags <- function(){
  
  models <- map(democracytypes, ~reg_main(plmdata[[.]],
                                 1,
                                 plmdata$gov_popul_weighted,
                                 plmdata$gov_left))
  
  return(models)
  
}
