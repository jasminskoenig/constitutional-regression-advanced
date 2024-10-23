reg_dem_jackknife_ruth <- function(democracyscore, leadyears, populismscore) {
  
  jack.reg <- plmdata$country |> 
    unique()
  
  jackedreg <- data.frame(country = character(), 
                          coefjackLeft = numeric(),
                          coefjackRight = numeric(),
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
    form <- as.formula(sprintf("lead(%s, %d) ~ %s * evnt + coalition + surplus", democracyscore, leadyears, populismscore))
    
    # run regression
    model = plm(form,
                model = "within",
                se = "HC1",
                data = plm_temp)
    
    # get coefficient of triple interaction
    coefficients <- coef(model)
    indexLeft <- grep(paste0(populismscore, "Left-wing Populist:evnt"), names(coefficients))
    indexRight <- grep(paste0(populismscore, "Right-wing Populist:evnt"), names(coefficients))
    coefficient_valueLeft <- coefficients[indexLeft]
    coefficient_valueRight <- coefficients[indexRight]
    
    
    newobs <- data.frame(country = i, 
                         coefjackLeft = coefficient_valueLeft,
                         coefjackRight = coefficient_valueRight)
    
    jackedreg <- rbind(jackedreg, newobs)
    
  }
  
  return(jackedreg)
}


find_outlier <- function(x) {
  
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  
}

plot_jackknife_ruth <- function(democracyscore, 
                                leadyears, 
                                populismscore){
  
  name <- get_name(democracyscore, 
                   from = "var",
                   whitespace = FALSE)
  
  coefs <- reg_dem_jackknife_ruth(democracyscore, 
                             leadyears, 
                             populismscore)
  
  
  coefs |> 
    mutate(coefLabelLEFT = ifelse(find_outlier(coefjackLeft), country, NA),
           coefLabelRIGHT = ifelse(find_outlier(coefjackLeft), country, NA),
           coefOutlierLEFT = if_else(!is.na(coefLabelLEFT), "Yes", "No"),
           coefOutlierRIGHT = if_else(!is.na(coefLabelRIGHT), "Yes", "No")) ->
    coefs
  
  coefs %>% 
    pivot_longer(cols = starts_with("coefjack"),
                 names_to = "LR",
                 values_to = "coef") %>% 
    mutate(outlier = if_else(str_detect(LR, "Left"), coefOutlierLEFT, coefOutlierRIGHT),
           label = if_else(str_detect(LR, "Left"), coefLabelLEFT, coefLabelRIGHT)) %>% 
    mutate(LR = if_else(LR == "coefjackLeft", "Left-Wing Populism", "Right-Wing Populism"))  %>% 
    group_by(LR) %>% 
    mutate(label = if_else(min(coef) == coef, country, if_else(max(coef) == coef, country, NA))) ->
    coefs
  
  coefs |> 
    ggplot(aes(x = coef)) +
    geom_text_repel(aes(
      x = coef,
      y = 1,
      label = label,
      nudge_y = 10,
      box.padding = 1.5,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20)) +
    facet_wrap(~ LR) +
    labs(y = NULL,
         x = "Coefficient of Interaction",
         title = name) +
    geom_histogram() +
    # scale_x_continuous(limits = c(-0.05, 0.0)) +
    scale_y_continuous(limits = c(0, 50),
                       expand = c(0,0)) +
    scale_fill_manual(values = c(color_dark, color_colorful)) ->
    plot
  
  return(plot)
  
}
