library(tidyverse)
library(manifestoR)
library(vdemdata)

# VParty ----

# Import Data and filter to timeframe
vparty %>% 
  filter(year>1989) -> 
  vparty2

# Choose relevant variables
vparty2 %>% 
  select(v2paenname,v2paid,country_name,year,
         country_id,e_regiongeo,v2xpa_antiplural,v2xpa_popul,
         v2paseatshare,v2patotalseat,v2pavote,v2pagovsup,
         ep_type_populism,ep_type_populist_values, 
         ep_v8_popul_rhetoric,ep_v9_popul_saliency, v2pariglef, pf_party_id,
         ep_galtan) -> 
  vparty2

# partyfacts ----

# To improve the matching between datasets, partyfacts is imported

file_name <- "data/partyfacts-mapping.csv"
if( ! file_name %in% list.files("")) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)

partyfacts <- partyfacts_raw |> 
  filter(!is.na(partyfacts_id))

partyfacts |> 
  select(partyfacts_id, dataset_party_id, country, dataset_key, name_english) ->
  partyfacts_lookup

# match with popuList ----

# Populist Import
popuList <- readxl::read_xlsx("data/popuList.xlsx")

# save populis partyfacts_id which is later used to match with vparty

partyfacts_lookup |> 
  filter(dataset_key == "parlgov") |> 
  mutate(dataset_party_id = as.integer(dataset_party_id)) |> 
  distinct(partyfacts_id, .keep_all = TRUE) ->
  partyfacts_parlgov

popuList |> 
  # match with partyfacts_id
  left_join(partyfacts_parlgov, by = c("parlgov_id" = "dataset_party_id")) |> 
  mutate(partyfacts_id = if_else(
    is.na(partyfacts_id.x), partyfacts_id.y, partyfacts_id.x
  )) |> 
  select(-partyfacts_id.x, partyfacts_id.y) |>
  # change start and end of populist "area" of each party to interval
  mutate(populist_start = if_else(populist_start < 1990, 1990, populist_start),
         populist_start = if_else(populist_start == 2100, 2019, populist_start),
         populist_end = if_else(populist_end == 2100, 2020, populist_end),
         populist_interval = interval(ymd(populist_start, truncated = 2L),
                                      ymd(populist_end, truncated = 2L))) ->
  popuList

# select columns for matching with vparty
popuList_short <- popuList |> 
  select(partyfacts_id, party_name_english, populist_interval, country_name)

# keep each party only once 
popuList_shorter <- popuList_short |> 
  select(partyfacts_id, populist_interval) |> 
  distinct(partyfacts_id, .keep_all = TRUE)

# popuList included countries

popuList |> 
  group_by(country_name) |> 
  distinct(country_name) |> 
  mutate(dataset = "PopuList") ->
  popuList_countries

# match with vparty
vparty2 |> 
  # match by party & country name
  left_join(popuList_short,
            by = c("v2paenname" = "party_name_english",
                   "country_name" = "country_name")) |>
  select(-partyfacts_id) |> 
  # match on partyfacts id base if names didn't match
  left_join(popuList_shorter, 
            by = c("pf_party_id" = "partyfacts_id"),
            na_matches = "never") |>  
  # choose the interval that's not NA (based on two matching ways)
  mutate(populist_interval = if_else(
    !is.na(populist_interval.x), populist_interval.x, populist_interval.y)
  ) |> 
  # only keep the final column
  select(-populist_interval.x, -populist_interval.y) |> 
  # now check for each year of party in vparty dataset whether it is included in pop interval
  mutate(rooduijn = if_else(
    # if there is no interval for the party check whether country is in popuList 
    # that would mean its not populist (but maybe eurosceptic and included because of that)
    is.na(populist_interval), if_else(
      country_name %in% popuList$country_name, 0, NA # 0 if country included, NA if it isnt
    ), if_else(
      ymd(year, truncated = 2L) %within% populist_interval, 1, 0)
    ),
    # is the party a senior party in government and coded as poulist?
    rooduijn_senior = case_when(
      rooduijn == 1 & v2pagovsup == 0 ~ 1,
      is.na(rooduijn) ~ NA,
      TRUE ~ 0
      )) ->
  vparty2

# Manifesto data ----
mp_setapikey(".secrets/manifesto_apikey.txt")

manifesto <- mp_maindataset() 

manifesto |> 
  distinct(countryname, edate) |> 
  mutate(year = year(edate)) ->
  manifesto_coverage

vparty2 |> 
  distinct(country_name, year) |> 
  left_join(manifesto_coverage, by = join_by(year, country_name == countryname)) 

partyfacts_lookup |> 
  filter(dataset_key == "manifesto") |> 
  mutate(dataset_party_id = as.integer(dataset_party_id)) |> 
  distinct(partyfacts_id, .keep_all = TRUE) ->
  partyfacts_manifesto

manifesto |> 
  mutate(left = per103 + per105 + per106 + per107 + per403 + per404 + per406 + per412 + per413 + per504 + per506 + per701,
         right = per104 + per201 + per305 + per401 + per402 + per407 + per414 + per505 + per601 + per603,
         rile = right - left,
         rile_kim = (right -left) / (right + left),
         rile_lowe = log(right/left),
         constitutionalism = per204 - per203,
         econ_left = per413 + per409 + per403 + per404 + per412 + per504 + per506 + per503 + per701,
         econ_right = per401 + per414 + per505 + per507 + per402 + per702,
         econ_rile = econ_right - econ_left,
         econ_rile_kim = (econ_right - econ_left) / (econ_right + econ_left),
         econ_rile_lowe = log(econ_right/econ_left),
         gal_left = per201 + per604 + per607 + per705 + per103 + per105 + per106 + per107 + per602,
         gal_right = per602 + per109 + per104 + per608 + per603 + per605 + per410 + per411,
         gal = gal_right - gal_left,
         gal_kim = (gal_right - gal_left) / (gal_right - gal_left),
         gal_lowe = log(gal_right - gal_left)) ->
  manifesto_positions

manifesto_positions |> 
  select(countryname, edate, party, partyname, rile, id_perm, contains("rile"), contains("gal")) |> 
  left_join(partyfacts_manifesto, by = c("party" = "dataset_party_id")) |> 
  mutate(year = year(edate)) |> 
  # in 8 countries there were two election in one year - i always choose the first one
  group_by(party, country, year) |> 
  slice(1) |> 
  ungroup() ->
  manifesto_ids

vparty2 |> 
  left_join(manifesto_ids |>  select(partyfacts_id, year, rile, contains("rile"), contains("gal")), 
            by = c("pf_party_id" = "partyfacts_id", "year" = "year"),
            na_matches = "never") |> 
  left_join(manifesto_ids |>  select(countryname, partyname, year, rile, contains("rile"), contains("gal")), 
            by = c("country_name" = "countryname", "year" = "year", "v2paenname" = "partyname"),
            na_matches = "never") |>  
  mutate(rile = if_else(!is.na(rile.x), rile.x, if_else(!is.na(rile.y), rile.y, NA)),
         econ_rile = if_else(!is.na(econ_rile.x), econ_rile.x, if_else(!is.na(econ_rile.y), econ_rile.y, NA)),
         gal = if_else(!is.na(gal.x), gal.x, if_else(!is.na(gal.y), gal.y, NA))) |>  
  select(-rile.x, -rile.y) ->
  vparty2

# 64 countries with scores from manifesto
vparty2 |> 
  filter(!is.na(gal)) |> 
  distinct(country_name) 

# CHES ----

# CHES is needed to validate lef-rig & gal-tan scores

ches_la <- readRDS("data/ches.rds")

ches_la |> 
  ungroup() |> 
  select(country_en, party_en, party_id, lrecon, galtan, lrgen) |> 
  # only one year available for latin america
  mutate(party_id = as.numeric(party_id)) ->
  ches_la_small

ches_eu <- read_csv("data/ches_eu.csv") 

ches_eu|> 
  select(year, party_id, lrecon, galtan, lrgen) ->
  ches_eu_small

partyfacts_lookup |> 
  filter(dataset_key == "ches") |> 
  mutate(dataset_party_id = as.integer(dataset_party_id)) |> 
  distinct(partyfacts_id, .keep_all = TRUE) ->
  partyfacts_ches

ches_eu_small |> 
  left_join(partyfacts_ches |> select(partyfacts_id, 
                                     dataset_party_id), 
            by = c("party_id" = "dataset_party_id")) ->
  ches_ids

vparty2 |> 
  left_join(ches_ids |>  select(-party_id), 
            by = c("pf_party_id" = "partyfacts_id", "year" = "year"),
            na_matches = "never") |> 
  left_join(ches_la_small, 
           by = c("country_name" = "country_en", "v2paenname" = "party_en"),
           na_matches = "never")  |>  
  mutate(galtan = if_else(!is.na(galtan.x), galtan.x, if_else(!is.na(galtan.y), galtan.y, NA)),
         lrgen = if_else(!is.na(lrgen.x), lrgen.x, if_else(!is.na(lrgen.y), lrgen.y, NA))) |>  
  select(-ends_with(".x"), -ends_with(".y")) ->
  vparty2

saveRDS(vparty2, "data/party_populism.rds")


# Coding the V-Party Dataset ----

# calculate populism score of government

# rename due to older naming in code
vparty2 -> 
  df

# party in government
df$gov_party <- ifelse(df$v2pagovsup %in% c(0, 1, 2), 1, 0)

# exclude parties that only support government, but are not respresented
df$gov_party_con <- ifelse(df$v2pagovsup %in% c(0, 1), 1, 0)

# prime minister
df$primemin_party <- ifelse(df$v2pagovsup == 0, 1, 0)

df %>% 
  filter(gov_party == 1) ->
  vparty_governments

# government composition

gov_stab <- function(x){
  
  if (x == "senior") {
    vparty_governments %>%
      filter(v2pagovsup == 0) ->
      filtered
  } else if (x == "junior"){
    vparty_governments %>%
      filter(v2pagovsup == 1) ->
      filtered
  } else {
    vparty_governments %>%
      filter(v2pagovsup <= 2) ->
      filtered
  }
    
      filtered |> 
      select(country_name, year, party_id, v2paenname) %>%
      group_by(country_name, year) %>%
      summarise(
        id = list(party_id),
        name = list(v2paenname),
        .groups = "drop"
      ) %>% 
      group_by(country_name) |> 
      mutate(lag_name = lag(name)) |> 
      ungroup() |> 
      mutate(gov_stability = case_when(
        sapply(lag_name, is.null) ~ NA,
        TRUE ~ map2_lgl(name, lag_name, ~all(.x %in% .y)))
      ) |> 
      group_by(country_name) |> 
      filter(gov_stability == FALSE | is.na(gov_stability)) |> 
      mutate(start = ifelse(is.na(gov_stability), NA, year),
             end = lead(year), ) |> 
      select(country_name, start, end) |> 
      group_by(country_name) |> 
      mutate(start = if_else(row_number() == 1, 1990, start),
             end = if_else(row_number() == n(), 2022, end),
             interval = interval(ymd(start, truncated = 2L), ymd(end, truncated = 2L))) ->
      stability
  
  return(stability)
}

senior_stability <- gov_stab("senior")
junior_stability <- gov_stab("junior")
gov_stability <- gov_stab("both")

# some cases are returned duplicated now XXX
# calculate government information
vparty_governments %>% 
  group_by(country_name,year) %>%
  # seat share of government
  mutate(gov_seatshare = sum(v2paseatshare), 
         # calculate weight of government parties
         weight = v2paseatshare/gov_seatshare,
         # code a dummy whether party is populist according to V-Party
         vparty_populist = if_else(v2xpa_popul > 0.5, 1, 0),
         # code a dummy whether senior is populist
         vparty_populist_senior = if_else(v2xpa_popul > 0.5 & v2pagovsup == 0, 1, 0),
         # code a dummy whether junior is populist
         vparty_populist_junior = if_else(v2xpa_popul > 0.5 & v2pagovsup == 1, 1, 0),
         # code whether any government party is populist according to popuList
         rooduijn_government = if_else(sum(rooduijn) > 0, "Populist", "Non-Populist"),
         # code whether senior party is populist accoring to popuList
         rooduijn_government_senior = if_else(sum(rooduijn_senior) > 0, "Populist", "Non-Populist")) |>  
  group_by(country_name, year, gov_seatshare, e_regiongeo, rooduijn_government, rooduijn_government_senior) %>% 
  # calulcate mean of populism score and weighted populism score per year of country (= per government)
  summarise(gov_popul_mean = mean(v2xpa_popul),
            # calculate weighted pop score per party and then add as weighted pop score per government
            gov_popul_weighted = sum(v2xpa_popul*weight),
            # calulcate weighted galtan vparty score
            gov_galtan_weighted = sum(ep_galtan*weight),
            # calculate weighted left - right econ
            gov_ideol_mean = mean(v2pariglef),
            # calculate weighted ideology
            gov_ideol_weighted = sum(v2pariglef*weight),
            # calculate weighted rile score
            gov_rile_weighted = sum(rile*weight),
            # calculate weighted gal score
            gov_gal_weighted = sum(gal*weight),
            # calculate weighted weighted econ rile score
            gov_econ_rile_weighted = sum(econ_rile*weight),
            # calculate number of government parties
            no_govparties = n(),
            # calculate number of populist vparty gov parties
            no_pop_vparty = sum(v2xpa_popul > 0.5),
            no_pop_vparty_sen = sum(vparty_populist_senior),
            no_pop_vparty_jun = sum(vparty_populist_junior),
            # calculate number of populist popuList gov parties
            no_pop_rooduijn = sum(rooduijn == 1),
            .groups = "drop") |> 
  # dummies for economic left-right
  mutate(econ_right = if_else(gov_ideol_weighted > 0.5, 1, 0),
         econ_left = if_else(gov_ideol_weighted < 0.5, 1, 0),
         coalition = if_else(no_govparties > 1, 1, 0),
         # dummy for solo government
         solo = if_else(no_govparties == 1, 1, 0),
         vparty_populist_senior = if_else(no_pop_vparty_sen > 0, 1, 0),
         vparty_populist_junior = if_else(no_pop_vparty_jun > 0, 1, 0),
         vparty_populist = if_else(no_pop_vparty > 0, 1, 0)) |> 
  select(-starts_with("no_pop_vparty"))-> 
  vparty_governments_populism

vparty_governments_populism |> 
  left_join(senior_stability,
          by = join_by(country_name == country_name, year >= start, year < end)) |> 
  left_join(junior_stability,
          by = join_by(country_name == country_name, year >= start, year < end)) |> 
  left_join(gov_stability,
            by = join_by(country_name == country_name, year >= start, year < end)) |> 
  select(-starts_with("start"), -starts_with("end")) |> 
  rename("interval_sen" = "interval.x",
         "interval_jun" = "interval.y",
         "interval_gov" = "interval") ->
  vparty_governments_populism

# populism score of party of president in vparty
df %>% 
  filter(primemin_party == 1) |>  
  group_by(country_name, year, rooduijn) %>% 
  mutate(gov_popul_prime = v2xpa_popul) %>%
  # Only very few countries have heads of governments from two parties (conflict resolution)
  # for these we calculate the mean
  summarize(gov_popul_prime = mean(gov_popul_prime),
            .groups = "drop") ->
  vparty_prime

# join together as new dataframe that includes information on populism in government
partydat <- left_join(vparty_governments_populism,
                      vparty_prime,
                      by=c("country_name","year")) 
glimpse(partydat)


saveRDS(partydat, "data/government_populism.rds")

