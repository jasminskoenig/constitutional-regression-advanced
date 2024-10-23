countries_old |>  pull(country) -> country_list

df |> 
  select(common_names, gov_left) ->
  df

df |> 
  left_join(dfold, by = c("country", "year")) ->
  df_combined

df_combined |> 
  mutate(latin.x = as.numeric(latin.x),
         evnt.x = as.numeric(evnt.x),
         latin.y = as.numeric(latin.y),
         evnt.y = as.numeric(evnt.y)) |> 
  pivot_longer(cols = ends_with("x") | ends_with(".y"),
               names_to = c("var", "df"),
               names_sep = "\\.",
               values_to = "value") |> 
  pivot_wider(names_from = df,
              values_from = value) |> 
  filter(x != y) |>  View()
  
df_combined |>
  filter(!is.na(v2x_polyarchy.x) | !is.na(v2x_polyarchy.y)) |> 
  select(contains("v2x_polyarchy")) ->
  libdem

cor(libdem)

df4 |> 
  select(country, year, v2x_polyarchy, v2x_partip, v2x_libdem, v2x_delibdem, v2x_egaldem, gov_left, v2x_cspart, evnt, latin, gov_popul_weighted) ->
  df
df4_old |> 
  select(country, year, v2x_polyarchy, v2x_libdem, v2x_delibdem, v2x_egaldem, v2x_cspart, evnt, latin, gov_popul_weighted) ->
  dfold

all.equal(df, dfold)
