library(ggrepel)
library(glueformula)

# DISSERTATION VERSION ----

options(modelsummary_format_numeric_latex = "plain")

## CONSTITUTIONAL CHANGE ----

get_varname_la <- function(variable) {
  nameshort <- str_remove(variable, ".*\\$")
  
  return(plmdata_la[[nameshort]])
}

reg_main_change <- function(leadyears, 
                            populismscore, 
                            moderator = FALSE,
                            dataset = plmdata){
  
  #' Runs a Country-FE Regression on the plmdata
  #' Takes df columns from plmdata as input for democracyscore (dep. var.)
  #' Takes df columns from plmdata as input for populismscore and moderator
  #' Takes integer for leadyears
  
  interaction  <-  paste("lead(evnt, leadyears) ~ populismscore * moderator")
  
  form <- paste(interaction, "+ surplus_size + coalition")
  
  form_final <- as.formula(form)
  
  mainmodel <- plm(form_final,
                   model = "within",
                   se = "HC1",
                   data = dataset)
  
  return(mainmodel)
  
}

reg_change <- function(leadyears, populismscore, moderator, dv = plmdata$evnt){

  popname <- deparse(substitute(populismscore))
  popname <- str_remove(popname, ".*\\$")
  populismscore_la <- plmdata_la[[popname]]
  modname <- deparse(substitute(moderator))
  modname <- str_remove(modname, ".*\\$")
  moderator_la <- plmdata_la[[modname]]
  dvname <- deparse(substitute(dv))
  dvname <- str_remove(dvname, ".*\\$")

  #doesn't work yet
  suppressWarnings('Setting class(x) to multiple strings ("pseries", "Interval", ...); result will no longer be an S4 object')
  models = list(
    # mixed model predictor only populism
    "Base" =  plm(lead(dv, 1) ~ populismscore + moderator,
                      model = "within",
                      se = "HC1",
                      data = plmdata),
    
    # Controls
    "Controls" = plm(lead(dv, 1) ~ populismscore  + moderator + surplus_size + coalition,
                     model = "within",
                     se = "HC1",
                     data = plmdata),
    
    # mixed model with three-way interaction and controls
    "Interaction and Controls" = reg_main_change(leadyears, populismscore, moderator),
    
    # surplus_size
    "Surplus" = plm(lead(dv, 1) ~ populismscore  + surplus_size*populismscore + surplus_size + moderator + coalition,
                   model = "within",
                   se = "HC1",
                   data = plmdata),
    
    # mixed model with three-way interaction and controls
    "Latin America" = reg_main_change(leadyears, populismscore_la, moderator_la, dataset = plmdata_la),
    
    # GAL-TAN
    "GALTAN1" = plm(lead(dv, 1) ~ populismscore  + gov_galtan_weighted,
                   model = "within",
                   se = "HC1",
                   data = plmdata),
    
    # GAL-TAN
    "GALTAN2" = plm(lead(dv, 1) ~ populismscore  + gov_galtan_weighted + surplus_size + coalition,
                   model = "within",
                   se = "HC1",
                   data = plmdata),
    
    # GAL-TAN
    "GALTAN3" = plm(lead(dv, 1) ~ populismscore  + gov_galtan_weighted*populismscore + surplus_size + coalition,
                   model = "within",
                   se = "HC1",
                   data = plmdata)
  )
  
  # Save models as Latex Table
  
  rows <- data.frame(
    "Coefficients" = "Country FE",
    "(1)" = "Yes",
    "(2)" = "Yes",
    "(3)" = "Yes",
    "(4)" = "Yes"
  )
  
  modellist <- models[1:4]
  table <- create_regressiontable(modellist,
                                  add_row = TRUE,
                                  row_position = 15,
                                  row_data = rows,
                                  latex = TRUE
  )
  
  tablename <- paste0("results/tables/constitutionalchangelikelihood_", dvname, ".tex")
  
  writeLines(table, tablename)
  
  rows <- data.frame(
    "Coefficients" = "Country FE",
    "(1)" = "Yes",
    "(2)" = "Yes",
    "(3)" = "Yes",
    "(4)" = "Yes"
  )
  
  modellist <- models[5:8]
  table <- create_regressiontable(modellist,
                                  add_row = TRUE,
                                  row_position = 17,
                                  row_data = rows,
                                  latex = TRUE
  )
  
  tablename <- paste0("results/tables/constitutionalchangelikelihood_EU_", dvname, ".tex")
  
  writeLines(table, tablename)
  
  # Return models for further analyses
  
  return(models)
  
}


reg_change_jackknife <- function(populismscore, moderator) {
  
  jack.reg <- plmdata$country |> 
    unique()
  leadyears <- c(1:4)
  
  jackedreg <- data.frame(rowname = character(), 
                          Estimate = numeric(),
                          Error = numeric(),
                          t = numeric(),
                          p = numeric(),
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
    
    for (n in leadyears){
      # create formula
      form <- as.formula(sprintf("lead(evnt, %d) ~ %s * %s + surplus_size + coalition", leadyears, populismscore, moderator))
      
      # run regression
      model = plm(form,
                  model = "within",
                  se = "HC1",
                  data = plm_temp)
      
      # get coefficient of triple interaction
      index <- paste0(populismscore, ":", moderator)
      newobs <- coef(summary(model)) %>% 
        as.data.frame %>% 
        rownames_to_column() %>% 
        filter(rowname == index) %>% 
        rename("Error" = 3,
               "t" = 4,
               "p" = 5)
      
      jackedreg <- rbind(jackedreg, newobs)
    }
    
  }
  
  return(jackedreg)
}


## DEMOCRATIC QUALITY ----

reg_main <- function(democracyscore, 
                     leadyears, 
                     populismscore, 
                     moderator = FALSE,
                     dataset = plmdata){
  
  #' Runs a Country-FE Regression on the plmdata
  #' Takes df columns from plmdata as input for democracyscore (dep. var.)
  #' Takes df columns from plmdata as input for populismscore and moderator
  #' Takes integer for leadyears
  
  interaction  <-  paste("lead(democracyscore, leadyears) ~ populismscore * evnt")
   
  if (is.double(moderator)){
    interaction <-  paste(interaction, "* moderator")
  } 
  
  form <- paste(interaction, "+ surplus_size + coalition")
 
  form_final <- as.formula(form)
  
  mainmodel <- plm(form_final,
      model = "within",
      se = "HC1",
      data = dataset)
  
  return(mainmodel)
  
}

reg_dem <- function(democracyscore, leadyears, populismscore, moderator){
  
  #doesn't work yet
  suppressWarnings('Setting class(x) to multiple strings ("pseries", "Interval", ...); result will no longer be an S4 object')
  
  demname <- deparse(substitute(democracyscore))
  democracyscore_la <- get_varname_la(demname)
  popname <- deparse(substitute(populismscore))
  populismscore_la <- get_varname_la(popname)
  modname <- deparse(substitute(moderator))
  moderator_la <- get_varname_la(modname)
  
  
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
    "Controls" = plm(lead(democracyscore, 1) ~ populismscore + evnt + moderator + surplus_size + coalition,
                                model = "within",
                                se = "HC1",
                                data = plmdata),
    
    # Controls
    "Interaction" = plm(lead(democracyscore, 1) ~ populismscore + evnt + moderator + surplus_size + coalition + populismscore*evnt,
                     model = "within",
                     se = "HC1",
                     data = plmdata),
    
    # mixed model with three-way interaction and controls
    "Triple-Interaction" = reg_main(democracyscore, leadyears, populismscore, moderator),
    
    # Controls
    "Surplus" = plm(lead(democracyscore, 1) ~ populismscore + evnt + moderator + surplus_size*populismscore*evnt +  surplus_size + coalition,
                     model = "within",
                     se = "HC1",
                     data = plmdata),
    
    # mixed model with three-way interaction and controls
    "Latinamerica" = reg_main(democracyscore_la, leadyears, populismscore_la, moderator_la, dataset = plmdata_la),
    
    # GAL-TAN
    "GALTAN" = plm(lead(democracyscore, 1) ~ populismscore + evnt + gov_galtan_weighted*populismscore*evnt + surplus_size + coalition,
                     model = "within",
                     se = "HC1",
                     data = plmdata)
  )
  
  # Save models as Latex Table
  
  rows <- data.frame(
    "Coefficients" = "Country FE",
    "(1)" = "Yes",
    "(2)" = "Yes",
    "(3)" = "Yes",
    "(4)" = "Yes",
    "(5)" = "Yes",
    "(6)" = "Yes"
  )
  
  
  table <- create_regressiontable(models[1:6],
                                             add_row = TRUE,
                                             row_position = 19,
                                             row_data = rows,
                                             latex = TRUE
  )
  
  inputname <- deparse(substitute(democracyscore))
  name <- str_remove(inputname, ".*\\$")
  
  tablename <- paste0("results/tables/constitutionalchange_", name, ".tex")
  
  writeLines(table, tablename)
  
  rows <- data.frame(
    "Coefficients" = "Country FE",
    "(1)" = "Yes",
    "(2)" = "Yes",
    "(3)" = "Yes"
  )
  
  table <- create_regressiontable(models[7:9],
                                  add_row = TRUE,
                                  row_position = 33,
                                  row_data = rows,
                                  latex = TRUE
  )
  
  tablename <- paste0("results/tables/constitutionalchange_alternative_", name, ".tex")
  
  writeLines(table, tablename)
  
  # Return models for further analyses
  
  return(models)

  
}

calc_ame <- function(model) {
  
  
  modelname <- deparse(substitute(model))
  
  if (str_detect(modelname, "EU")) {
    model$model$moderator <- model$model$gov_galtan_weighted
  }
  
  levels <- seq(
    min(model$model$populismscore, na.rm = TRUE),
    max(model$model$populismscore, na.rm = TRUE),
    0.05
  )
  
  min_lr <- min(model$model$moderator, na.rm = TRUE)
  max_lr <- max(model$model$moderator, na.rm = TRUE)
  diff_lr <- max_lr - min_lr 
  middle_lr <- min_lr + diff_lr/2
  min_lr <- min_lr + diff_lr/10
  max_lr <- max_lr - diff_lr/10
  levels_lr <- c(middle_lr, min_lr, max_lr)
  
  mean_surplus_size <- mean(model$model$surplus_size, na.rm = TRUE)
  
  if (str_detect(modelname, "EU")) {
    pred_data <- datagrid(evnt = c(0,1), 
                          populismscore = levels, 
                          gov_galtan_weighted = levels_lr,
                          surplus_size = mean_surplus_size,
                          coalition = 1,
                          model = model)
  } else {
    pred_data <- datagrid(evnt = c(0,1), 
                          populismscore = levels, 
                          moderator = levels_lr,
                          surplus_size = mean_surplus_size,
                          coalition = 1,
                          model = model)
  }
  
  slopes(model,
         variables = "evnt",
         newdata = pred_data
         ) %>%  
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
  
  if (str_detect(modelname, "EU")) {
    meff %>% 
      mutate(moderator = gov_galtan_weighted) ->
      meff
  }
  
  return(meff)
}

create_names <- function() {

  
  tibble(short = c("mld", 
                   "mpo", 
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
         easy =  c("Liberal",
                   "Electoral",
                   "Participation",
                   #"Deliberative",
                   "Egalitarian",
                   "Civil"),
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
                     name = FALSE,
                     moderatorname = "lr",
                     add_histogram = FALSE, 
                     adapt_tag = FALSE){
  

  
  if (name == FALSE) {
    modelname <- deparse(substitute(model))
    name <- get_name(modelname, whitespace = TRUE)
  }
  
  if (moderatorname == "democracylevel") {
    predictedata %>% 
      mutate(moderator = case_when(
        moderator == min(moderator) ~ "Low",
        moderator == max(moderator) ~ "High",
        TRUE ~ "Medium"
      ),
      moderator = fct_relevel(moderator, c("Low", "Medium", "High"))) ->
      predictedata
  } else {
    predictedata %>% 
      mutate(moderator = case_when(
        moderator == min(moderator) ~ "Left",
        moderator == max(moderator) ~ "Right",
        TRUE ~ "Centre"
      ),
      moderator = fct_relevel(moderator, "Left")) ->
      predictedata
    
  }
  
  
  predictedata |> 
    filter(ci_size == 95) %>% 
    ggplot(aes(x = populismscore,
               y = estimate,
               color = moderator,
               fill = moderator, 
               group = moderator)) +
    geom_hline(yintercept = 0, 
               color = "#C95D63", 
               linetype = "dashed") +
    geom_ribbon(aes(ymin = cilower,
                    ymax = ciupper),
                alpha = 0.2,
                linewidth = 0.2) +
    geom_line() +
    labs(x = "Government Populism Score",
         y = "AME of Constitutional Change",
         tag = name) +
    facet_wrap(~ moderator) +
    scale_x_continuous(limits = c(0,1),
                       expand = c(0,0),
                       breaks = c("0" = 0,
                                  "0.25" = 0.25,
                                  "0.5" = 0.5,
                                  "0.75" = 0.75,
                                  "1" = 1)) +
    scale_y_continuous(limits = c(-0.15, 0.15),
                       expand = c(0,0),
                       breaks = seq(-0.10, 0.15, by = 0.05)) +
    # scale_alpha_manual(values = c(0.7, 0.5, 0.2),
    #                    labels = c("90%", "95%", "99%")) +
    scale_fill_manual(values = c(color_dark, color_colorful, color_neutral)) +
    scale_color_manual(values = c(color_dark, color_colorful, color_neutral)) +
    theme(plot.margin = margin(r = 25, l =10, b = 10),
          plot.tag = element_markdown(angle = -90,
                                  size = textsize,
                                  face = "bold",
                                  lineheight = 0.7,
                                  hjust = 0.5,
                                  margin = margin(l = 20)),
          plot.tag.position = "right",
          strip.text = element_text(hjust = 0.5),
          legend.position = "none"
          ) ->
    main
  main
    
  # if (add_histogram){
  #   model$model |>
  #     as.data.frame() |>
  #     mutate(moderator = if_else(moderator == 0,
  #                                "Right-Wing",
  #                                "Left-Wing")) |>
  #     ggplot(aes(x = populismscore)) +
  #     geom_histogram(bins = 100) +
  #     scale_x_continuous(limits = c(0, 1))  +
  #     scale_y_continuous(expand = c(0,0)) +
  #     facet_wrap(~ moderator) +
  #     theme_void() +
  #     theme(strip.text = element_text(size = textsize + 10,
  #                                     family = fontname,
  #                                     margin = margin(b = 10),
  #                                     hjust = 0),
  #           plot.margin = margin(r = 50)) ->
  #     histo
  # 
  #   histo + main + plot_layout(ncol = 1,
  #                              heights = c(1, 4)) ->
  #     main
  # }
    
    main
    return(main)
  
} 

get_EU_plot <- function(modelEU, name) {
  
  margeffEU <- calc_ame(modelEU)
  plotEU <- plot_ame(margeffEU,
                     modelEU,
                     name = name,
                     add_histogram = FALSE
  ) +
    scale_y_continuous(limits = c(-0.1, 0.16),
                       expand = c(0,0),
                       breaks = seq(-0.05, 0.15, by = 0.05)) 
  
  return(plotEU)
}

remove_strip <- function(plot) {

    plot +
    theme(strip.text = element_blank()) ->
    plot
  
  return(plot)
}

remove_strip_multiple <- function(plotlist){
  
  n <- length(plotlist)
  plotlist[2:n] <- map(plotlist[2:n], remove_strip)
    
  return(plotlist)
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
               output = type,
               notes = list('+p < 0.1; *p < 0.05; **p > 0.01; ***p < 0.001'),
               modelsummary_format_numeric_latex = "plain"
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

reg_dem_jackknife_ruth <- function(democracyscore, leadyears, populismscore, moderator) {
  
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
    form <- as.formula(sprintf("lead(%s, %d) ~ %s * evnt + coalition + surplus_size", democracyscore, leadyears, populismscore))
    
    # run regression
    model = plm(form,
                model = "within",
                se = "HC1",
                data = plm_temp)
    
    # get coefficient of triple interaction
    coefficients <- coef(model)
    index <- grep(paste0(populismscore, ":evnt"), names(coefficients))
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
  lower_bound <- quantile(x, .05)  # 5th percentile
  upper_bound <- quantile(x, .95)  # 95th percentile
  return(x < lower_bound | x > upper_bound)
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
    geom_histogram(aes(fill = Outlier,
                       group = Outlier), 
                   alpha = 0.7,
                   fill = color_neutral) +
    geom_text_repel(aes(
      x = coefjack,
      y = 1,
      label = label,
      nudge_y = 10,
      box.padding = 5,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20),
      size = 4) +
    scale_x_continuous(limits = c(-0.06, 0),
                       expand = c(0,0),
                       breaks = seq(-0.06, 0, by = 0.03)) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,35)) +
    labs(y = NULL,
         x = "Coefficient of Triple-Interaction",
         title = name) +
    scale_fill_manual(values = c(color_dark_light, color_colorful_light)) ->
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
                                   row_position = 19,
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
