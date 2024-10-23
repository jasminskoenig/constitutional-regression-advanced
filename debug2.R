common_names <- intersect(names(dfold), names(df))

ccpc_vdem_eu_la_test |> 
  mutate(id = paste0(country, year)) ->
  ccpc_vdem_eu_la_test

df4_old |> 
  select(-date, -id) ->
  df4_old

all.equal(ccpc_vdem_eu_la_old, ccpc_vdem_eu_la_test)

ccpc_vdem_eu_la_old |> 
  filter(is.na(gov_popul_weighted))

all.equal(select(df4_new, common_names), df4)
