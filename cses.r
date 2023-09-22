########################################################
# Cambodian socieeconomic census
# 20th. September 2023
# Yuzuru Utsunomiya, Ph. D.
########################################################
# ---- load library ----
library(tidyverse)
library(haven)
library(labelled)
library(sf)

# ----- read.data -----

# family_members <- 
#   haven::read_dta("hhmembers.dta")

# ----- read.data -----
# object including address information
# We need to combine the address data and household size data using psu ()
# to distinguish household individually (hhid)
# # 2004
# hoge <- haven::read_dta("x02lcses2004.dta")
# hogehoge <- 
#   hoge %>% 
#   dplyr::mutate(across(where(is.labelled)), labelled::unlabelled(.))
# write_excel_csv(hogehoge, "hoge.csv")
# 
# # 2009
# hoge <- haven::read_dta("weighthouseholds_area.dta")
# hoge

# NOTE
# 1. We need to focus on the data in 2014, 2019, and 2021 due to
# inconnectivity.
# 2. To unlabel labeled data, we use labelled() package.
# In detail, refer to the following page.
# https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html
# 
# 2014
# 
psulisting_2014 <- haven::read_dta("./cses_fishery/psulisting_2014.dta")
# object including primary sampling unit (PSU) id numbers and household id (hhid)
households_2014 <- haven::read_dta("./cses_fishery/households_2014.dta")
# add geographical information (address) to hhid data
# Later, we will add geometry object to the data
# For integration, to the data in 2014, we will set Tboung Khmum province
# while spliting Kampong Cham province as in 2021 and 2019.
target_2014 <- 
  psulisting_2014 %>% 
  dplyr::left_join(
    households_2014, 
    by = "psu"
  ) %>% 
  # select necessary variables
  dplyr::select(
    psu, hhid, pname, dname, cname, vname
  ) %>% 
  # Transform the character data into factor
  dplyr::mutate(
    dplyr::across(where(is.character), factor)
  ) %>% 
  # replace variables' names for convenience
  data.table::setnames(
    c("psu", "hhid", "province_name", "district_name", "commune_name", "village_name")
  ) %>% 
  # add year, month, and date for convenience
  dplyr::mutate(year_month_date = lubridate::ymd("2014-01-01")) %>% 
  # Gather the province name that are not common among data in 2014, 2019, and 2021
  dplyr::mutate(
    province_name = factor(
      stringr::str_replace_all(
        province_name, 
        "Oddar Meanchey",
        "Otdar Meanchey"
        )
      )
  ) %>% 
  # integrate province's name to one in 2021 using districts' names
  # Regarding the districts' names, refer to the following page.
  # http://www.statoids.com/ukh.html
  dplyr::mutate(
    province_name = dplyr::case_when(
      district_name == "Dambae" |
      district_name == "Krouch Chhmar" | 
      district_name == "Memot" |
      district_name == "Ou Reang Ov" |
      district_name == "Ponhea Kraek" | 
      district_name == "Tboung Khmum" | 
      district_name == "Krong Suong"  ~ "Tboung Khmum",
      TRUE ~ province_name
      )
  ) %>% 
  dplyr::mutate(
    dplyr::across(where(is.character), factor)
  )
# fish_cultivation_1 <- haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2014_1.dta")
# fish_cultivation_2 <- haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2014_2.dta")
# fish_cultivation_3 <- haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2014_3.dta")
# 2019
psulisting_2019 <- haven::read_dta("./cses_fishery/areainformationCSES2019.dta") %>% labelled::unlabelled()
target_2019 <- 
  psulisting_2019 %>% 
  dplyr::select(
    psu, province_name, district_name, commune_name, village_name
  ) %>% 
  dplyr::mutate(
    dplyr::across(where(is.character), factor)
  ) %>% 
  dplyr::mutate(year_month_date = lubridate::ymd("2019-01-01"))
readr::write_excel_csv(target_2019, "target_2019.csv")
# 
# 2021
target_2021 <- 
  haven::read_dta("Areainformation_2021.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(
    psu, province_name, district_name, commune_name, village_name
  ) %>% 
  dplyr::mutate(
    dplyr::across(where(is.character), factor)
  ) %>% 
  dplyr::mutate(year_month_date = lubridate::ymd("2021-01-01"))
readr::write_excel_csv(target_2021, "target_2021.csv")
# 
# ----- check.difference -----
# To integrate name of provinces, districts, communes, and villages, we need to where to correct. 
# Using logical function, we check name of the administrative boundaries to correct.
# In concluding, we merely need to correct provinces' names. 
# Other names of the districts, communes, and villages are not available to correct
# due to sampling procedure.
# # province -> OK
# setdiff(target_2014$province_name, target_2019$province_name)
# setdiff(target_2014$province_name, target_2021$province_name)
# setdiff(target_2019$province_name, target_2014$province_name)
# setdiff(target_2019$province_name, target_2021$province_name)
# setdiff(target_2021$province_name, target_2014$province_name)
# setdiff(target_2021$province_name, target_2019$province_name)
# district -> NG
# district_differences <- 
#   levels(
#     factor(
#       c(
#         setdiff(target_2014$district_name, target_2019$district_name),
#         setdiff(target_2014$district_name, target_2021$district_name),
#         setdiff(target_2019$district_name, target_2014$district_name),
#         setdiff(target_2019$district_name, target_2021$district_name),
#         setdiff(target_2021$district_name, target_2014$district_name),
#         setdiff(target_2021$district_name, target_2019$district_name)
#         )
#     )
#   )
# commune -> NG
# commune_differences <- 
#   levels(
#     factor(
#       c(
#         setdiff(target_2014$commune_name, target_2019$commune_name),
#         setdiff(target_2014$commune_name, target_2021$commune_name),
#         setdiff(target_2019$commune_name, target_2014$commune_name),
#         setdiff(target_2019$commune_name, target_2021$commune_name),
#         setdiff(target_2021$commune_name, target_2014$commune_name),
#         setdiff(target_2021$commune_name, target_2019$commune_name)
#       )
#     )
#   )
# village -> NG
# village_differences <- 
#   levels(
#     factor(
#       c(
#         setdiff(target_2014$village_name, target_2019$village_name),
#         setdiff(target_2014$village_name, target_2021$village_name),
#         setdiff(target_2019$village_name, target_2014$village_name),
#         setdiff(target_2019$village_name, target_2021$village_name),
#         setdiff(target_2021$village_name, target_2014$village_name),
#         setdiff(target_2021$village_name, target_2019$village_name)
#       )
#     )
#   )
# ----- read.fishery.data -----
# 2014
fish_2014_01 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2014_1.dta") %>% 
  data.table::setnames(c("hhid","pond_number","pond_owning","area","market_value","monthly_rent")) %>% 
  dplyr::mutate(pond_owning = factor(pond_owning)) %>% 
  dplyr::left_join(target_2014, by = "hhid") %>% 
  dplyr::select(hhid, province_name, year_month_date, pond_number, pond_owning, area, market_value, monthly_rent)
fish_2014_02 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2014_2.dta")
fish_2014_03 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2014_3.dta")
# 2019
fish_2019_01 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2019_1.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(psu, hhid, q05f1c01, q05f1c02, q05f1c03, q05f1c04, q05f1c05) %>% 
  data.table::setnames(c("psu", "hhid","pond_number","pond_owning","area","market_value","monthly_rent")) %>% 
  dplyr::mutate(pond_owning = factor(pond_owning)) %>% 
  dplyr::left_join(target_2019, by = "psu") %>%
  dplyr::select(hhid, province_name, year_month_date, pond_number, pond_owning, area, market_value, monthly_rent)
fish_2019_02 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2019_2.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(hhid, q05fq1, q05fq2, q05fq3, q05f2_1, q05f2_2, q05f2_3, q05f2_4, q05f2_5, q05f2_6, q05f2_7, q05f2_8, q05f2_9, q05f2_10, q05f2_11, q05f2_12, q05f2_13)
fish_2019_03 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2019_3.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(hhid, q05f3_1, q05f3_2, q05f3_3, q05f3_4, q05f3_5, q05f3_6, q05f3_7, q05f3_8)
# 2021
fish_2021_01 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2021_1.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(psu, hhid, q05f1c01, q05f1c02, q05f1c03, q05f1c04, q05f1c05) %>% 
  dplyr::left_join(target_2021, by = "psu") %>%
  dplyr::select(-district_name, -commune_name, -village_name) %>% 
  data.table::setnames(c("psu", "hhid", "pond_number","pond_owning","area","market_value","monthly_rent","province_name", "year_month_date")) %>% 
  dplyr::select(hhid, province_name, year_month_date, pond_number, pond_owning, area, market_value, monthly_rent) %>% 
  dplyr::mutate(pond_owning = factor(pond_owning))
fish_2021_02 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2021_2.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(hhid, q05fq1, q05fq2, q05fq3, q05f2_1, q05f2_2, q05f2_3, q05f2_4, q05f2_5, q05f2_6, q05f2_7, q05f2_8, q05f2_9, q05f2_10, q05f2_11, q05f2_12, q05f2_13) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(.x, 0)))
fish_2021_03 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2021_3.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(hhid, q05f3_1, q05f3_2, q05f3_3, q05f3_4, q05f3_5, q05f3_6, q05f3_7, q05f3_8) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(.x, 0)))
# 
# combine altogether
# 1. experience of aquaculture
fish_01 <- 
  fish_2014_01 %>% 
  dplyr::bind_rows(fish_2019_01) %>% 
  dplyr::bind_rows(fish_2021_01) %>% 
  dplyr::mutate(
    pond_owning = dplyr::case_when(
      pond_owning == "1" ~ "own", 
      pond_owning == "Own" ~ "own", 
      pond_owning == "2" ~ "own_but_rent_out", 
      pond_owning == "3" ~ "rented_in_from_others", 
      pond_owning == "Rented in from others" ~ "rented_in_from_others", 
      pond_owning == "4" ~ "free_use", 
      pond_owning == "Free use of pond" ~ "free_use", 
      pond_owning == "5" ~ "other", 
      pond_owning == "Other (specify)" ~ "other",
      TRUE ~ "hoge"
    )
  ) %>% 
  dplyr::mutate(
    pond_owning = factor(pond_owning),
    hhid = factor(hhid)
    ) %>% 
  na.omit()











































# ----- combine.shapefiles -----

khm_shp <- 
  sf::st_read("./KHM_adm/KHM_adm1.shp")

khm_province_en <- 
  c(
    "Banteay Meanchey", "Battambang", "Kampot", "Kampong Cham", "Kampong Chhnang", "Kampong Speu", "Kampong Thom",
    "Kandal", "Koh Kong", "Kep", "Kratie", "Pailin", "Preah Sihanouk", "Mondul Kiri", "Otdar Meanchey", 
    "Phnom Penh", "Pursat", "Preah Vihear", "Prey Veng", "Ratanak Kiri", "Siemreap",
    "Stung Treng", "Svay Rieng", "Takeo", "Tboung Khmum"
  )
  
khm_shp_province <- 
  khm_shp %>% 
  bind_cols(
    ., 
    province_name =khm_province_en
    )

target_2014_shp <- 
  target_2014 %>% 
  dplyr::left_join(
    khm_shp_province,
    by = ("province_name")
  ) %>% 
  dplyr::select(psu, hhid, province_name, year_month_date, geometry)


