library(tidyverse)
library(lubridate)
library(vdemdata)
library(stringi)
library(readtext)
library(readxl)
library(WDI)
library(beepr)

# FUNCTIONS

source("src/cleaning_functions.R")

# ADD VDEM AND CCPC DATA TO PARTYDATA

# Data Import

partydat <- readRDS("data/government_populism.rds")

# VDEM ----
vdem %>% 
  filter(year > 1989) -> 
  vdem2


vdem2 %>% 
  dplyr::select(country_name, year, country_id, iso3 = country_text_id, e_regiongeo, v2x_polyarchy, v2x_partip, v2xdd_i_pl,
         v2x_libdem, v2x_delibdem, v2x_egaldem, v2x_liberal, v2xcl_rol, v2x_cspart, v2xcs_ccsi,
         v2x_jucon, v2jureform_ord, v2jupurge_ord, v2jupurge, v2jupack, v2jupoatck_ord, v2jupack_ord,
         v2juaccnt, v2jucorrdc, v2juhcind, v2juncind, v2juhccomp,
         v2jucomp, v2jureview, v2x_regime, v2xnp_pres, v2reginfo,
         v2exrescon, v2jucomp, v2juhccomp, v2juhcind, v2x_jucon,
         v3eldirepr, v3elagepr) |> 
  mutate(country_name = if_else(country_name == "Czechia", "Czech Republic", country_name)) ->
  vdem2

vdem2 |> 
  mutate(jud_pack = if_else(v2jupack_ord < 2.5, 1, 0),
         jud_pack_con = if_else(v2jupack_ord < 1.5, 1, 0),
         jud_atck = if_else(v2jupoatck_ord < 3.5, 1, 0),
         jud_atck_con = if_else(v2jupoatck_ord < 2.5, 1, 0),
         jud_purg = if_else(v2jupurge_ord < 2.5, 1, 0),
         jud_purg_con = if_else(v2jupurge_ord < 1.5, 1, 0),
         jud_ref = if_else(v2jureform_ord < 0.5, 1, 0),
         # this doesnt work yet
         rev_remov = if_else(v2jureview == 0 & lag(v2jureview) == 1, 1, 0),
         jud_reg = if_else(jud_pack + jud_atck + jud_purg + jud_ref > 0, 1, 0),
         jud_replace = if_else(jud_pack + jud_purg > 0, 1, 0),
         jud_replace_con = if_else(jud_pack_con + jud_purg_con > 0, 1, 0),
         jud_replace_cont = if_else(v2jupurge < v2jupack, v2jupurge, v2jupack),
         jud_replace_cont_mean = (v2jupurge + v2jupack)/2,
         jud_replace_cont = jud_replace_cont*-1,
         jud_replace_cont_mean = jud_replace_cont_mean*-1,
         regime_time = str_extract(v2reginfo, "\\(\\d{2}\\/\\d{2}\\/\\d{4}"),
         regime_start = as.numeric(str_extract(regime_time, "\\d{4}")),
         regime_age = year - regime_start + 1,
         latin = if_else(e_regiongeo %in% c(17,18), 1, 0)) |> 
  select(-regime_time) ->
  vdem2

joined_vdata <- left_join(vdem2,
                          partydat,
                          by=c("country_name", "year", "e_regiongeo")) 

glimpse(joined_vdata)


# Add Presidentialism ----

joined_vdata |> 
  distinct(country_name) -> 
  country_list

executive <- read_excel("data/executive.xlsx")

executive |> 
  filter(hos == "Executive") |> 
  mutate(country = str_trim(country)) ->
  presidential

presidential |> 
  mutate(included = if_else(country %in% joined_vdata$country_name, 1, 0)) -> 
  presidential

joined_vdata |> 
  mutate(presidential = if_else(country_name %in% presidential$country, 1, 0)) ->
  joined_vdata

# add ruth data, as this isn't based on party but president

# Ruth 2022 ----

ruth <- read_excel("data/ruth_2022.xlsx")

ruth |> 
  select(country_name, year, leader_name, extremism, populism) |> 
  filter(populism == "Populist") ->
  ruth_handcoding

write_csv(ruth_handcoding, "data/ruth_handcoding.csv")

ruth_add <- read_excel("data/ruth_handcoding_lr.xlsx") 

ruth |>  
  left_join(ruth_add, 
          by = c("country_name",
                 "year")) |> 
  mutate(populism_lr = case_when(
    is.na(lr) ~ "Non-Populist",
    lr == "Unclear" ~ NA,
    TRUE ~ paste0(lr, "-wing Populist"))) ->
  ruth


ruth_smaller <- ruth |> 
  select("country" = "country_name",
         "president" = "leader_name",
         "ruth_populism" = "populism",
         "ruth_extremism" = "extremism",
         "ruth_populism_lr" = "populism_lr",
         year) 

joined_vdata |> 
  left_join(ruth_smaller, by = c("country_name" = "country",
                                 "year" = "year")) ->
  joined_vdata

ruth_smaller |> 
  group_by(country) |> 
  distinct(country) |> 
  mutate(dataset = "Ruth-Lovell & Grahn") ->
  ruth_countries

# Add World Bank Data ----

vdem2 |> 
  distinct(iso3) |> 
  filter(!iso3 %in% c("YMD", "DDR", "PSG", "SML", "ZZB")) ->
  isocountry_vdem

isocountry <- read.csv("data/isocountry.csv")

isocountry_vdem |> 
  left_join(isocountry |>  select(alpha.2, alpha.3),
            by = join_by("iso3" == "alpha.3")) |> 
  filter(!is.na(alpha.2)) |> 
  pull(alpha.2) ->
  isocountry_list

wbd = WDI(indicator = c('gdp' = 'NY.GDP.MKTP.CD',
                       'gdp_growth' = 'NY.GDP.PCAP.KD.ZG'),
          country = isocountry_list,
          start = 1994, end=2022)

wbd |> 
  mutate(
    gdp_log = log(gdp),
    gdp_growth_small = gdp_growth / 100
  ) |> 
  rename("country_name" = "country") ->
  wbd_clean

joined_vdata |> 
  left_join(wbd_clean, 
            by = c("country_name", "year")) ->
  joined_vdata

# Add Barometer Data on Trust in Judiciary ----

eurobarometer <- readRDS("C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Dissertation/barometer/data/eurobarometer_complete.rds")
latinobarometer <- readRDS("C:/Users/Jasmin/OneDrive - Universität Hamburg/Desktop/Dissertation/barometer/data/latinobarometer_original.rds")

eurobarometer |> 
  group_by(country, year) |> 
  mutate(n = n(), 
         mean = mean(trust_judiciary, na.rm = TRUE)) |> 
  group_by(country, year, trust_judiciary, mean) |> 
  reframe(share = n()/n) |> 
  distinct() ->
  eurobarometer_shares

latinobarometer |> 
  filter(country != "Spain") |> 
  group_by(country, year) |> 
  mutate(n = n(), 
         mean = mean(trust_judiciary, na.rm = TRUE)) |> 
  group_by(country, year, trust_judiciary, mean) |> 
  reframe(share = n()/n) |> 
  distinct() ->
  latinobarometer_shares

barometer <- rbind(
  latinobarometer_shares, 
  eurobarometer_shares
  ) |> 
  mutate(year = as.double(year))

barometer |> 
  filter(trust_judiciary == 1) |> 
  select(-trust_judiciary) |> 
  rename("trust_share" = "share",
         "trust_mean" = "mean") ->
  barometer_forbind

barometer_forbind |> 
  group_by(country) |> 
  mutate(diff = if_else(
    year - lag(year) > 1, trust_share - lag(trust_share), NA
  )) 

joined_vdata |> 
  left_join(barometer_forbind, by = join_by(country_name == country,
                                            year == year)) ->
  joined_vdata

# Fill VParty ----

# fill in vparty columns & barometer for the additional observations in vdem years
joined_vdata %>%
  mutate(trust_share_imp_lastv = trust_share,
         trust_mean_imp_lastv = trust_mean) |> 
  group_by(country_name) %>% 
  fill(gov_popul_mean,
       gov_popul_weighted,
       gov_seatshare,
       e_regiongeo,
       no_govparties,
       no_pop_rooduijn,
       gov_popul_prime,
       rooduijn_government,
       rooduijn_government_senior,
       gov_ideol_weighted,
       gov_gal_weighted,
       gov_rile_weighted,
       gov_econ_rile_weighted,
       coalition,
       vparty_populist_senior,
       vparty_populist_junior,
       vparty_populist,
       solo,
       interval_sen,
       interval_jun,
       interval_gov,
       gov_galtan_weighted, 
       trust_mean_imp_lastv,
       trust_share_imp_lastv) %>% 
  ungroup() |> 
  mutate(surplus = if_else(gov_seatshare - 50 < 0, 0, 1),
         gov_rile_left = as.factor(if_else(gov_rile_weighted < -8 , 1, 0)),
         gov_rile_left = as.factor(if_else(gov_rile_weighted > 8 , 1, 0)),
         gov_galtan_left = case_when(
           gov_galtan_weighted < 0.4 ~ "low",
           gov_galtan_weighted < 0.7 ~ "medium",
           gov_galtan_weighted < 1 ~ "high",
           TRUE ~ NA
         )) -> 
  joined_vdata

# define whether government is left wing or right wing

joined_vdata |> 
  mutate(gov_left = if_else(gov_ideol_weighted < 0, 1, if_else(is.na(gov_ideol_weighted), NA, 0)),
         gov_right = if_else(gov_ideol_weighted > 0, 1, if_else(is.na(gov_ideol_weighted), NA, 0)),
         gov_center = if_else(gov_ideol_weighted == 0, 1, if_else(is.na(gov_ideol_weighted), NA, 0))) |> 
  group_by(country_name, interval_sen) |> 
  mutate(senior_length = row_number()) |> 
  group_by(country_name, interval_gov) |> 
  mutate(gov_length = row_number()) |>
  ungroup() ->
  joined_vdata


# make table showing whether countries are included in datasets - needed for blog

joined_vdata %>%
  dplyr::select(country_name, rooduijn_government, rooduijn_government_senior, ruth_populism, president, gov_popul_prime, gov_popul_mean, gov_popul_weighted) %>% 
  group_by(country_name) %>%
  summarize_all(~ sum(!is.na(.x))) ->
  country_datasets

write_csv(country_datasets, "data/country_datasets.csv")
  
# was there a single party government
joined_vdata$singleparty_gov <- ifelse(
  joined_vdata$no_govparties == 1, 1, 0
)

# clean names
joined_vdata %>%
  rename(country = country_name) -> 
  joined_vdata

joined_vdata |> 
  pull(country) |> 
  unique() ->
  country_list

# CCPC ----

# This code was executed based on the original csv provided by ccp: 
# https://comparativeconstitutionsproject.org/download-data/
# Due to its size, we can not upload it in the repository - 
# please download this datset before running code

# DOWNLOAD FIRST (LINK ABOVE)
ccpc <- read.csv("data/ccpcnc_v3_small.csv")

ccpc |> 
  mutate(country = case_when(
    country == "Slovak Republic" ~ "Slovakia",
    TRUE ~ country
  )) -> 
  ccpc

ccpc |> 
  filter(country %in% country_list)->
  ccpc

## Count Rights Changes ----

# number of rights in constitution
# import names of rights in dataset

# use the codebook of ccp to create a list of rights in constitutions
rightsindex <- readtext("codebooks/rightsindex_ccp.pdf") |> 
  select(-doc_id) %>%
  mutate(text = str_remove_all(text, "ID\\b")) %>%
  mutate(text = str_extract_all(text, "[:upper:]{2,}\\d{0,}")) %>%
  unlist(text) %>%
  str_to_lower() %>%
  stringi::stri_replace_all_regex(c("equal\\B","artspec", "debtrght", "health", "indrght", "intprop\\B", "socsec1", "socsec(?=\\d)"), c("equalgr_", "artspec_1", "debtors", "healthr", "indpolgr_", "intprop_", "socsec", "finsup_"), vectorize_all = FALSE)%>%
  str_remove("appeal|envref|intprop$|solissuf") %>%
  stri_remove_empty()

# only keep ccp colums that refer to rights
ccpc %>%
  select(one_of(rightsindex), rghtapp) %>%
  select(-arms) %>% 
  mutate(across(.cols = everything(), ~ifelse(.==1, 1, 0))) ->
  ccpc_rights

# sum of all rights in constitution
ccpc_rights %>% 
  mutate(rights_sum = rowSums(across(everything()))) %>% 
  pull(rights_sum) ->
  ccpc$rights_sum

# add rights from an area to an index 
ccpc_rights %>%
  # criminal rights
  mutate(rights_crim = jury, vicright, excrim, prerel, habcorp, wolaw, rghtapp, corppun, dueproc, falseimp, fairtri, speedtri, presinoc, trilang, juvenile, doubjep, miranda, couns, debtors,
  # rule of law index
  rights_ruleolaw = rowSums(select(., "citren", 
                                          contains("equal"), 
                                          "infoacc", 
                                          "libel", 
                                          "freerel", "seprel", # religion
                                          "exprop", # enteignung möglich
                                          "remuner", "socsec", "standliv", contains("finsup"), "shelter", "healthr", # faire entlohnung, soziale sicherheit und lebensstandard, finanzielle unterstützung bestimmtr gruppen, recht auf wohnen, gesundheitsversorgung
                                          "jointrde", "strike", "occupate", "safework", "childwrk", #gewerkschaft und streik, eigene berufswahl, sichere arbeitsumgebung, verbot v kinderarbeit
                                          "testate", "transfer", "inherit", contains("intprop"), "proprght", # property and inheritance
                                          "busines", "freecomp", # establish busines, free market
                                          "conright", # consumer rights
                                          "scifree", "acfree",  # wissenschaftsfreiheit
                                          "achighed", # bildung
                                          "marriage", "fndfam", "matequal", "childpro", 2, # family
                                          "selfdet", # selfdetermination
                                          "life", 
                                          "slave", "torture", "cruelty",  
                                          "privacy",
                                          "freemove", 
                                          "opinion", "express", "petition", "censor", "press", "assem", "assoc",  # politische freiheit
                                          contains("intrght"), # international human rights declarations
                                          "devlpers", # persönliche freiheit
                                          "nomil", # verweigerung militär
                                          "asylum",
                                          "artspec_1")),
         # political rights
         rights_political = rowSums(select(., "infoacc", "scifree", "acfree", "opinion", "express", "petition", "censor", "press", "assem", "assoc", contains("intrght"), "asylum","jointrde", "strike")),
         # economic rights
         rights_econ = rowSums(select(., "busines", "freecomp", "conright", "testate", "transfer", "inherit", contains("intprop"), "proprght")),
         # individual rights
         rights_ind = freerel, citren, selfdet, life, privacy, freemove, devlpers,
         # social rights
         rights_social = rowSums(select(., "remuner", "socsec", "standliv", contains("finsup"), "shelter", "healthr", "safework", "childwrk", "achighed"))) %>% 
  select(contains("rights_")) ->
  ccpc_rights_sums

ccpc %>%
  cbind(ccpc_rights_sums) ->
  ccpc

ccpc %>%
  # remove all 99s and 0
  mutate(across(starts_with("con"), ~ str_remove_all(., "\\D")),
         hosterm = str_remove_all(hosterm, "\\D"),
         # 99 NA, 0 not specified, 100 constitutional monarchies
         hosterm = if_else(hosterm %in% c("99", "0", "100", ""), NA, hosterm),
         # 99 NA, 0 not specified
         conterm = if_else(conterm %in% c("99", "0", ""), NA, conterm),
         # 90 non-constitutional law, 96 other, 98 not specified, 99 NA
         hosterml = if_else(hosterml %in% c("90", "96", "98", "99", ""), NA, hosterml)) %>% 
  group_by(country) %>% 
  # numeric for calculations of rights
  mutate(hosterm = as.numeric(hosterm),
         across(starts_with("con"), ~ as.numeric(.)),
         across(starts_with("jud"), ~as.numeric(.)),
         across(starts_with("rights_"), 
                .fns = ~ . - lag(.),
                .names = "diff_{.col}"),
         across(c("conterm", "judind", "hosterm"),
                .fns = ~ . - lag(.),
                .names = "diff_{.col}"),
         # decree power head of state
         decree = ifelse(hosdec == 1 | hogdec == 1, 1, 0),
         # declare state of emergency by head of state
         emergency = ifelse(emdecl < 5, 1, 0),
         # removal of legislature by anyone but legislature themselved
         removal_leg = ifelse(legdiss < 5, 1, 0),
         # does any institution have veto power against the legislature
         veto = ifelse(legapp < 5, 1, 0),
         # does the head of state or government have the standing to challenge the constitutionality of legislation?
         review = ifelse(challeg_1 == 1 | challeg_2 == 1| challeg_3 == 1, 1, 0),
         # who can propose an amendment to the constitution?
         amendment = ifelse(amndprop_1 == 1 | amndprop_2 == 1| amndprop_3 == 1, 1, 0),
         # how large does the majority need to be for an amendment
         amendmaj = case_when(
           amndamaj == 2 ~ 1,
           amndapct < 5 ~ amndapct + 1,
           TRUE  ~ NA
         ),
         # power of the executive
         executive = decree + emergency + removal_leg + veto + review + amendment,
         # head of state, head of government or government decide on supreme court justices
         supnom = if_else(supnom_1 + supnom_2 + supnom_3 > 0, 1, 0), 
         # number of other players that need to approve new judges
         supap = supap_4 + supap_5 + supap_6 + supap_7,
         # number of institutions included in the selection of judges
         selection = ifelse(connom_1 + connom_2 + connom_3 + connom_4 + connom_5 + connom_7 + conap_1 + conap_2 + conap_3 + conap_4 + conap_5 + conap_7 > 1 | connom_6 + conap_6 > 0, 1, 0),
         # only one term for judges allowed
         appointment = ifelse(conlim == 1, 1, 0),
         # number of institutions that can propose the dismissal of judges
         removal = ifelse(jrempro_1 + jrempro_2 + jrempro_3 + jrempro_4 + jrempro_5 + jrempro_7 + jrempro_9 + jremap_1 + jremap_2 + jremap_3 + jremap_4 + jremap_5 + jremap_7 > 1 | jrempro_8 > 0 | jremap_6 + jrempro_6 > 0 | jrem == 2, 1, 0),
         # are judges protected against random dismissal (general dissatisfactin 1, incapacitation 5, other 96)
         removal_reason = ifelse(jremcon_1 != 1 & jremcon_5 != 1 & jremcon_96 != 1, 1, 0),
         # are judicial salaries protected from government interference
         salary = ifelse(judsal == 1, 1, 0),
         # rights of the judiciary
         judiciary = judind + selection + appointment + appointment + removal + removal_reason + salary,
         across(c("judiciary", "executive"),
                .fns = ~ . - lag(.),
                .names = "diff_{.col}"),
         judiciary_change = if_else(judiciary != lag(judiciary), 1, 0),
         # power change of executive
         diff_executive = executive - lag(executive),
         # increase in terms allowance for head of state
         term_change = case_when(
           hosterml != 1 & lag(hosterml) == 1 ~ 1,
           lag(hosterml) == 2 & hosterml != 3 & hosterml != 1 & hosterml != 2 ~ 1,
           lag(hosterml) == 3 & hosterml != 1 & hosterml != 3  ~ 1,
           lag(hosterml) == 4 & hosterml == 5 & hosterml != 4 ~ 1,
           is.na(hosterml) | is.na(lag(hosterml)) ~ NA_real_,
           TRUE ~ 0)) %>%
  ungroup() ->
  ccpc

ccpc_populism <- ccpc |> 
  left_join(joined_vdata |>  
              select(country, year, gov_popul_weighted, e_regiongeo),
            by=c("country","year")) 

all_zero_check <- function(x) {
  all(x = 0)
}

ccpc_populism |> 
  filter(e_regiongeo %in% c(1:4, 17:18)) |> 
  mutate(across(30:1233, ~ if_else(. == lag(.), 0, 1))) |> 
  filter(
    gov_popul_weighted > 0.5 
##    | lead(gov_popul_weighted) > 0.5
    ) ->
  ccpc_populism_changes

ccpc_populism_changes |> 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) 


# only relevant columns 
ccpc %>%
  as_tibble() %>%
  select(
    country, year, syst, evnt, evnttype, contains("rights_"), starts_with("diff_"), supnom, supap, selection, executive, amendment, amendmaj, judiciary_change,
  ) ->
ccpc

glimpse(ccpc)

ccpc_vdem <- left_join(joined_vdata,
                       ccpc,
                       by=c("country","year")) |> 
  distinct(country, year, .keep_all = TRUE)

# ADDITIONAL MUTATIONS ----

## IMPUTATIONS ----

ccpc_vdem |> 
  distinct(country) |> 
  pull() ->
  all_countries

# function for imputation returns dataset with additional columns for multiple imputations
ccpc_vdem <- map_dfr(all_countries,  ~get_linear_imputation(.x, "trust_share"))
ccpc_vdem <- map_dfr(all_countries,  ~get_linear_imputation(.x, "gdp"))
ccpc_vdem <- map_dfr(all_countries,  ~get_linear_imputation(.x, "gdp_growth_small"))

ccpc_vdem |> 
  mutate(gdp_linearimp_log = log(gdp_linear_imp)) ->
  ccpc_vdem

## MEAN LAGS ----

# For some variables we need the sum of the last years
# such as constitutional changes or judicial replacements

evnt_sum_lag10 <- calculate_lagsummary("evnt", 10, func = "sum", by_government = TRUE) 
evnt_sum_lag5 <- calculate_lagsummary("evnt", 5, func = "sum", by_government = TRUE) 
evnt_sum_lag3 <- calculate_lagsummary("evnt", 3, func = "sum", by_government = TRUE) 
jud_replace_con_lag3 <- calculate_lagsummary("jud_replace_con", 5, func = "sum", by_government = FALSE)
jud_replace_con_lag5 <- calculate_lagsummary("jud_replace_con", 3, func = "sum", by_government = FALSE)
jud_replace_lag3 <- calculate_lagsummary("jud_replace", 5, func = "sum", by_government = FALSE)
jud_replace_lag5 <- calculate_lagsummary("jud_replace", 3, func = "sum", by_government = FALSE)

# For others we need the mean
trust_mean_5 <- calculate_lagsummary("trust_share", 5, func = "mean", by_government = FALSE) 
trust_mean_3 <- calculate_lagsummary("trust_share", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))
trustlinearimp_mean_5 <- calculate_lagsummary("trust_share_linear_imp", 5, func = "mean", by_government = FALSE) 
trustlinearimp_mean_3 <- calculate_lagsummary("trust_share_linear_imp", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))
trustlastvimp_mean_5 <- calculate_lagsummary("trust_share_imp_lastv", 5, func = "mean", by_government = FALSE) 
trustlastvimp_mean_3 <- calculate_lagsummary("trust_share_imp_lastv", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))
judacc_mean_5 <- calculate_lagsummary("v2juaccnt", 5, func = "mean", by_government = FALSE) 
judacc_mean_3 <- calculate_lagsummary("v2juaccnt", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))
judind_mean_5 <- calculate_lagsummary("v2juncind", 5, func = "mean", by_government = FALSE) 
judind_mean_3 <- calculate_lagsummary("v2juncind", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))
judhc_mean_5 <- calculate_lagsummary("v2juhcind", 5, func = "mean", by_government = FALSE) 
judhc_mean_3 <- calculate_lagsummary("v2juhcind", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))
judcorr_mean_5 <- calculate_lagsummary("v2jucorrdc", 5, func = "mean", by_government = FALSE) 
judcorr_mean_3 <- calculate_lagsummary("v2jucorrdc", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))
gdp_growth_mean_5 <- calculate_lagsummary("gdp_growth_small", 5, func = "mean", by_government = FALSE) 
gdp_growth_mean_3 <- calculate_lagsummary("gdp_growth_small", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))
gdp_log_linear_imp_mean_5 <- calculate_lagsummary("gdp_linearimp_log", 5, func = "mean", by_government = FALSE) 
gdp_log_linear_imp_mean_3 <- calculate_lagsummary("gdp_linearimp_log", 3, func = "mean", by_government = FALSE) |> 
  select(-starts_with("lagged"))



purrr::reduce(list(ccpc_vdem,
                   trust_mean_3, 
                   trust_mean_5, 
                   trustlastvimp_mean_3,
                   trustlastvimp_mean_5,
                   trustlinearimp_mean_5,
                   trustlinearimp_mean_3,
                   judacc_mean_5, 
                   judacc_mean_3,
                   judind_mean_5,
                   judind_mean_3,
                   judhc_mean_5,
                   judhc_mean_3,
                   judcorr_mean_5,
                   judcorr_mean_3,
                   gdp_growth_mean_5,
                   gdp_growth_mean_3,
                   gdp_log_linear_imp_mean_5,
                   gdp_log_linear_imp_mean_3,
                   jud_replace_lag5,
                   jud_replace_lag3,
                   jud_replace_con_lag3,
                   jud_replace_con_lag5,
                   evnt_sum_lag3,
                   evnt_sum_lag5,
                   evnt_sum_lag10
                   ), 
              dplyr::left_join, 
              by = c("country", "year")) ->
  ccpc_vdem

beep()
# Define whether court compositions have been changed before for each year

ccpc_vdem |> 
  group_by(country) |> 
  mutate(year = as.numeric(as.character(year))) |> 
  filter(jud_replace == 1) |> 
  summarize(start = min(year),
            .groups = "drop") ->
  firstchange

ccpc_vdem |> 
  group_by(country) |> 
  summarize(everreplaced = sum(jud_replace),
            .groups = "drop") ->
  firstchange2

firstchange |> 
  full_join(firstchange2, by = "country") ->
  firstchange

ccpc_vdem |> 
  mutate(year = as.numeric(as.character(year))) |> 
  left_join(firstchange, by = "country") |> 
  mutate(laterchange = case_when(
    everreplaced > 0 & year > start ~ 1,
    everreplaced > 0 & year <= start ~ 0,
    everreplaced == 0 ~ 0,
    TRUE ~ NA
    )) |> 
  select(-start, -everreplaced) ->
  ccpc_vdem

# Save Data ----

## Special Cases ----

# Bolsonaro's party is coded as antipluralist but not populist
ccpc_vdem %>% 
  filter(country == "Brazil") 

## Save ----

saveRDS(ccpc_vdem, "data/ccpc_vdem.rds")

## Save Data Europe and Latin America ----

ccpc_vdem |> 
  filter(e_regiongeo %in% c(1:4, 17:18)) |> 
  arrange(country, year) |> 
  filter(country != "German Democratic Republic" & country != "Suriname") |> 
  mutate(date = ymd(year, truncated = 2L)) ->
  ccpc_vdem_eu_la

saveRDS(ccpc_vdem_eu_la, "data/ccpc_vdem_eu_la.rds")

beep()
