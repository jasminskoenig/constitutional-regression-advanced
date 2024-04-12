# Blog ----

## Government Weighted Populism Score Models -----

### Liberal Democracy by Weighted Populism & Left-Wing ----

models_libdem_latin = reg_dem(df4$v2x_libdem, 1, df4$gov_popul_weighted, df4$latin)

model_libdem_latin <- models_libdem_latin$`Interaction`

levels_popul <- reg_mod_levels(model_libdem_latin, df4$gov_popul_weighted)
levels_govleft <- reg_mod_levels(model_libdem_latin, df4$latin)

effects_libdem <- reg_effects_multiinteraction(model_libdem_latin, 
                                               levels_libdem, levels_govleft, 
                                               df4$gov_popul_weighted, df4$latin, 
                                               "Europe", "Latin America")

reg_plot(effects_libdem)

reg_dem_jackknife("v2x_libdem", 1, "gov_popul_weighted", "gov_left")

### Participation Democracy by Weighted Populism & Left-Wing ----

models_partip = reg_dem(df4$v2x_partip, 1, df4$gov_popul_weighted, df4$latin)

model_partip <- models_partip$`Interaction`

levels_partip <- reg_mod_levels(model_partip, df4$gov_popul_weighted)
levels_partip <- reg_mod_levels(model_partip, df4$latin)

effects_partip <- reg_effects_multiinteraction(model_partip, 
                                               levels_partip, levels_govleft, 
                                               df4$gov_popul_weighted, df4$latin, 
                                               "Right", "Left")

reg_plot(effects_partip)
