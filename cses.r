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
library(khroma)
library(ggrepel)
library(ggsn)
library(ggspatial)


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
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2014_2.dta") %>% 
  dplyr::left_join(target_2014, by = "hhid") %>% 
  dplyr::select(hhid, province_name, year_month_date, q05f2c01, q05f2c03) %>% 
  data.table::setnames(c("hhid","province_name","year_month_date","item","amount")) %>% 
  dplyr::mutate(item = factor(item))
fish_2014_03 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2014_3.dta") %>% 
  dplyr::left_join(target_2014, by = "hhid") %>% 
  dplyr::select(hhid, province_name, year_month_date, q05f3c01, q05f3c03) %>% 
  data.table::setnames(c("hhid","province_name","year_month_date","item","amount")) %>% 
  dplyr::mutate(item = factor(item))
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
  dplyr::select(psu, hhid, q05f2_1, q05f2_2, q05f2_3, q05f2_4, q05f2_5, q05f2_6, q05f2_7, q05f2_8, q05f2_9, q05f2_10, q05f2_11, q05f2_12, q05f2_13) %>% 
  data.table::setnames(c("psu", "hhid","i1","i2","i3","i4","i5","i6","i7","i8","i9","i10","i11","i12","i13")) %>% 
  tidyr::pivot_longer(
    cols = dplyr::starts_with("i"),
    names_to = "item",
    values_to = "amount"
  ) %>% 
  dplyr::mutate(item = stringr::str_sub(item, start = 2)) %>% 
  dplyr::left_join(target_2019, by = "psu") %>% 
  dplyr::select(hhid, province_name, year_month_date, item, amount)
fish_2019_03 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2019_3.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(psu, hhid, q05f3_1, q05f3_2, q05f3_3, q05f3_4, q05f3_5, q05f3_6, q05f3_7, q05f3_8) %>% 
  data.table::setnames(c("psu", "hhid","i1","i2","i3","i4","i5","i6","i7","i8")) %>% 
  tidyr::pivot_longer(
    cols = dplyr::starts_with("i"),
    names_to = "item",
    values_to = "amount"
  ) %>% 
  dplyr::mutate(item = stringr::str_sub(item, start = 2)) %>% 
  dplyr::left_join(target_2019, by = "psu") %>% 
  dplyr::select(hhid, province_name, year_month_date, item, amount)

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
  dplyr::select(psu, hhid, q05f2_1, q05f2_2, q05f2_3, q05f2_4, q05f2_5, q05f2_6, q05f2_7, q05f2_8, q05f2_9, q05f2_10, q05f2_11, q05f2_12, q05f2_13) %>% 
  data.table::setnames(c("psu", "hhid","i1","i2","i3","i4","i5","i6","i7","i8","i9","i10","i11","i12","i13")) %>% 
  tidyr::pivot_longer(
    cols = dplyr::starts_with("i"),
    names_to = "item",
    values_to = "amount"
  ) %>% 
  dplyr::mutate(item = stringr::str_sub(item, start = 2)) %>% 
  dplyr::left_join(target_2021, by = "psu") %>% 
  dplyr::select(hhid, province_name, year_month_date, item, amount) %>% 
  dplyr::mutate(across(everything(), ~ replace_na(.x, 0)))
fish_2021_03 <- 
  haven::read_dta("./cses_fishery/S05F_HHFishCultivation_2021_3.dta") %>% 
  labelled::unlabelled() %>% 
  dplyr::select(psu, hhid, q05f3_1, q05f3_2, q05f3_3, q05f3_4, q05f3_5, q05f3_6, q05f3_7, q05f3_8) %>% 
  data.table::setnames(c("psu", "hhid","i1","i2","i3","i4","i5","i6","i7","i8")) %>% 
  tidyr::pivot_longer(
    cols = dplyr::starts_with("i"),
    names_to = "item",
    values_to = "amount"
  ) %>% 
  dplyr::mutate(item = stringr::str_sub(item, start = 2)) %>% 
  dplyr::left_join(target_2021, by = "psu") %>% 
  dplyr::select(hhid, province_name, year_month_date, item, amount) %>% 
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
  na.omit() %>% 
  dplyr::mutate(
    area_value = market_value / area,
    area_rent = monthly_rent / area
  )

readr::write_rds(fish_01, "fish_01.rds")
# 2. expense for fishery
fish_02 <- 
  fish_2014_02 %>% 
  dplyr::bind_rows(fish_2019_02) %>% 
  dplyr::bind_rows(fish_2021_02) %>% 
  dplyr::mutate(
    dplyr::across(where(is.character), factor)
  ) %>% 
  dplyr::mutate(
    item = dplyr::case_when(
      item == "1" ~ "Breeding stock for raising fish/shrimp etc.",
      item == "2" ~ "Feed for raising fish/shrimp etc.",
      item == "3" ~ "Hired labour (cash plus Kind)",
      item == "4" ~ "Ice",
      item == "5" ~ "Repair and maintenance of nets and traps etc.",
      item == "6" ~ "Repair and maintenance of boat",
      item == "7" ~ "Boat fuel",
      item == "8" ~ "Boat rent (cash)",
      item == "9" ~ "Cash rent for tank, if leased in",
      item == "10" ~ "Transportation of fish/shrimp/crab etc. to market",
      item == "11" ~ "Services (technical assistance) received",
      item == "12" ~ "Other (specify)",
      item == "13" ~ "Total",
      TRUE ~ "hoge"
    )
  )
readr::write_rds(fish_02, "fish_02.rds")
# 3. Reward
fish_03 <- 
  fish_2014_03 %>% 
  dplyr::bind_rows(fish_2019_03) %>% 
  dplyr::bind_rows(fish_2021_03) %>% 
  dplyr::mutate(
    dplyr::across(where(is.character), factor)
  ) %>% 
  dplyr::mutate(
    item = dplyr::case_when(
      item == "1" ~ "Proceeds from sale of fish, shrimp, crab etc. raised or captured",
      item == "2" ~ "Value of fish, shrimp, crab etc. consumed in household",
      item == "3" ~ "Value of fish, shrimp, crab etc. given away as gift, charity, barter, etc.",
      item == "4" ~ "Value of fish, shrimp used for drying (dried fish/shrimp, smoked fish etc.)",
      item == "5" ~ "Value of fish, shrimp used for preparation of fish/shrimp sauce",
      item == "6" ~ "Value of fish, shrimp used for animal feed",
      item == "7" ~ "Value of fish, shrimp used for other (specify)",
      item == "8" ~ "Total",
      TRUE ~ "hoge"
    )
  )
readr::write_rds(fish_03, "fish_03.rds")
# 
# ----- map.data.fish.01 -----
# 
khm_shp <- 
  sf::st_read("./KHM_adm/KHM_adm1.shp")
# 
khm_province_en <- 
  c(
    "Banteay Meanchey", "Battambang", "Kampot", "Kampong Cham", "Kampong Chhnang", "Kampong Speu", "Kampong Thom",
    "Kandal", "Koh Kong", "Kep", "Kratie", "Pailin", "Preah Sihanouk", "Mondul Kiri", "Otdar Meanchey", 
    "Phnom Penh", "Pursat", "Preah Vihear", "Prey Veng", "Ratanak Kiri", "Siemreap",
    "Stung Treng", "Svay Rieng", "Takeo", "Tboung Khmum"
  )
# 
khm_shp_province <- 
  khm_shp %>% 
  bind_cols(
    ., 
    province_name =khm_province_en
  )
# 

# draw chropleth maps
# 1. aquaculture
fish_01_map <- 
  fish_01 %>% 
  dplyr::mutate(year = factor(stringr::str_sub(.$year_month_date, start = 1, end = 4))) %>%
  tidyr::pivot_longer(
    cols = c(-hhid, -province_name, -year_month_date, -year, -pond_number, -pond_owning),
    names_to = "item",
    values_to = "number"
  ) %>% 
  dplyr::select(hhid, province_name, year, item, number) %>% 
  dplyr::mutate(item = factor(item)) %>% 
  dplyr::group_by(year, item, province_name) %>% 
        dplyr::summarise(
          N. = n(),
          Sum = sum(number),
          Mean = mean(number),
        ) %>%
  ungroup() %>% 
  tidyr::complete(
    province_name,year, item
    ) %>% 
  dplyr::left_join(
    khm_shp_province,
    by = ("province_name")
  ) %>% 
  dplyr::select(province_name, year, item, N., Sum, Mean, geometry) %>% 
  group_by(year, item) %>% 
  nest() %>% 
  dplyr::mutate(
    chropleth_map = purrr::map(
      data,
      ~
        ggplot2::ggplot(
          data = ., 
          aes(
            geometry = geometry
          )
        ) +
        geom_sf(
          aes(
            fill = scale(Sum)
          ),
          linewidth = 0.5
        ) +
        viridis::scale_fill_viridis(
          option = "mako", 
          direction = -1, 
          na.value = "white"
        ) +
        labs(
          title = paste(item, year, sep = " "), 
          fill = "", 
          x = "Longitude", 
          y = "Latitude"
        ) +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(15, "mm"),
          legend.key.height = unit(2, "mm")
        )
    )
  )
# save figures
pdf("fish_01_map.pdf")
fish_01_map$chropleth_map
dev.off()
# 
# 2. expenses
fish_02_map <- 
  fish_02 %>% 
  dplyr::mutate(year = factor(stringr::str_sub(.$year_month_date, start = 1, end = 4))) %>%
  dplyr::select(province_name, year, item, amount) %>% 
  dplyr::mutate(item = factor(item)) %>% 
  dplyr::group_by(year, item, province_name) %>% 
  dplyr::summarise(
    N. = n(),
    Sum = sum(amount),
    Mean = mean(amount),
  ) %>%
  ungroup() %>% 
  tidyr::complete(
    province_name,year, item
  ) %>% 
  dplyr::left_join(
    khm_shp_province,
    by = ("province_name")
  ) %>% 
  dplyr::select(province_name, year, item, N., Sum, Mean, geometry) %>% 
  group_by(year, item) %>% 
  nest() %>% 
  dplyr::mutate(
    chropleth_map = purrr::map(
      data,
      ~
        ggplot2::ggplot(
          data = ., 
          aes(
            geometry = geometry
          )
        ) +
        geom_sf(
          aes(
            fill = scale(Sum)
          ),
          linewidth = 0.5
        ) +
        viridis::scale_fill_viridis(
          option = "mako", 
          direction = -1, 
          na.value = "white"
        ) +
        labs(
          title = paste(item, year, sep = " "), 
          fill = "", 
          x = "Longitude", 
          y = "Latitude"
        ) +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(15, "mm"),
          legend.key.height = unit(2, "mm")
        )
    )
  )
# save figures
pdf("fish_02_map.pdf")
fish_02_map$chropleth_map
dev.off()
# 
# 3. revenue
fish_03_map <- 
  fish_03 %>% 
  dplyr::mutate(year = factor(stringr::str_sub(.$year_month_date, start = 1, end = 4))) %>%
  dplyr::select(province_name, year, item, amount) %>% 
  dplyr::mutate(item = factor(item)) %>% 
  dplyr::group_by(year, item, province_name) %>% 
  dplyr::summarise(
    N. = n(),
    Sum = sum(amount),
    Mean = mean(amount),
  ) %>%
  ungroup() %>% 
  tidyr::complete(
    province_name,year, item
  ) %>% 
  dplyr::left_join(
    khm_shp_province,
    by = ("province_name")
  ) %>% 
  dplyr::select(province_name, year, item, N., Sum, Mean, geometry) %>% 
  group_by(year, item) %>% 
  nest() %>% 
  dplyr::mutate(
    chropleth_map = purrr::map(
      data,
      ~
        ggplot2::ggplot(
          data = ., 
          aes(
            geometry = geometry
          )
        ) +
        geom_sf(
          aes(
            fill = scale(Sum)
          ),
          linewidth = 0.5
        ) +
        viridis::scale_fill_viridis(
          option = "mako", 
          direction = -1, 
          na.value = "white"
        ) +
        labs(
          title = paste(item, year, sep = " "), 
          fill = "", 
          x = "Longitude", 
          y = "Latitude"
        ) +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(15, "mm"),
          legend.key.height = unit(2, "mm")
        )
    )
  )
# save figures
pdf("fish_03_map.pdf")
fish_03_map$chropleth_map
dev.off()
# 
# ----- cambodia.map -----
# NOTE
# Before use, download the data from GADM
# (https://gadm.org/)
# To make the layers, we use osmdata() library
# The library has functions to check features and tags.
# Before loading data for the layer, set them while checking them.
# In detail of the features and tags, refer to the following page.
# https://wiki.openstreetmap.org/wiki/Map_features
# features
osmdata::available_features()
# tags
osmdata::available_tags("natural")
# 
# administrative boundaries (province)
khm_shp_province_name <- 
  khm_shp_province %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) 
# administrative boundaries (country)
khm_shp_country <- 
  sf::st_read("./KHM_adm/KHM_adm0.shp")
# inland water
# In detail of status (permanent / intermittent), 
# refer to contents of .dbf file.
# permanent water such as the Mekong river
khm_shp_permanent_water <- 
  sf::st_read("./KHM_adm/KHM_water_areas_dcw.shp"
  ) %>% 
  dplyr::filter(F_CODE_DES == "Inland Water")
# intermittent water fluctuating by season
khm_shp_intermittent_water <- 
  sf::st_read("./KHM_adm/KHM_water_areas_dcw.shp"
  ) %>% 
  dplyr::filter(F_CODE_DES != "Inland Water")
# surrounding countries to cover unnecessary features
khm_shp_surroundings <- 
  sf::st_read("./KHM_adm/THA_adm0.shp") %>% 
  sf::st_union(sf::st_read("./KHM_adm/LAO_adm0.shp")) %>% 
  sf::st_union(sf::st_read("./KHM_adm/VNM_adm0.shp"))


# obtain featured object's data
# road
road <- 
  osmdata::opq(
    bbox = (
      sf::st_transform(
        khm_shp_country, 
        4326
        ) %>% 
        sf::st_bbox(.)
      )
    ) %>%
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "motorway", 
      "motorway_junction",
      "motorway_link",
      "primary", 
      "primary_link",
      "secondary", 
      "secondary_link",
      "tertiary",
      "tertiary_link",
      "trunk",
      "trunk_link"
    )
  ) %>%
  osmdata::osmdata_sf()



# overlay the area and line above
khm_map_adm_water <- 
  # intermittent water
  khm_shp_intermittent_water %>% 
  ggplot() +
  xlim(102,108) +
  ylim(10, 15) +
  geom_sf(
    fill = "lightblue", 
    colour = NA,
    alpha = 0.5
    ) +
  # permanent water
  geom_sf(
    data = khm_shp_permanent_water,
    inherit.aes = FALSE,
    fill = "blue", colour = NA,
    size = 0.4,
    alpha = 0.5
  ) +
  # road from osm
  geom_sf(
    data = road$osm_lines,
    inherit.aes = FALSE,
    color = "orange",
    size = 0.2,
    alpha = 1.0
  ) +
  # province boudnaries
  geom_sf(
    data = khm_shp_province_name,
    linewidth = 0.5,
    colour = "black",
    fill = NA
  ) +
  geom_sf(
    data = khm_shp_surroundings,
    colour = NA,
    linewidth = 1.0,
    fill = "white"
  ) +
  # country boundaries
  geom_sf(
    data = khm_shp_country,
    colour = "black",
    linewidth = 1.0,
    fill = NA
  ) +
  # provinces' name
  ggrepel::geom_text_repel(
    data = khm_shp_province_name,
    aes(
      center_x, 
      center_y,
      label =  stringr::str_wrap(province_name, 6)
    ),
    min.segment.length = unit(200, "mm"),
    size = 4,
    direction = "both",
    max.overlaps = Inf
    ) +
  # scalebar
  ggspatial::annotation_scale(
    location = "br",
    pad_x = unit(15, "mm")
  ) +
  # north arrow
  ggspatial::annotation_north_arrow(
    location = "br", 
    pad_x = unit(0.8, "in"), 
    pad_y = unit(0.8, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  ) +
  labs(
    x = "Longitude", 
    y = "Latitude"
    ) + 
  theme_classic()
# save the map
ggsave(
  "khm_map_adm_water.pdf",
  plot = khm_map_adm_water,
  device = cairo_pdf(),
  height = 250,
  width =250,
  units = "mm"
)

