# food custom survey in Cambodia
# 10th. February 2021
# Yuzuru Utsunomiya

### ---- read.library ----
# read library
library(tidyverse)
library(patchwork)
library(GGally)
library(ggmosaic)
library(khroma)
library(sf)
library(tableone)
library(osmdata)
library(brms)

### ---- load.data ----
# load the data
food_custom_fy2020 <- 
  readxl::read_excel(
    path = "Cambodia_fy2020.xlsx",
    sheet = "food_custom_fy2020",
    col_names = TRUE
    )

### ---- table.1 ----
# Table 1 Demographic status
# code:
# age: 017
# gender: gender
# marial status: 018  
# family structure: 19
# occupation: 20
# income fluctuation: 21
# revenue: 22
# district: district 
# commune: commune
# village: village
# 
# make a special dataset for the table 1
data_table01 <- 
  food_custom_fy2020 %>% 
  # pick up necessary part
  dplyr::select(district:q022) %>% 
  dplyr::select(-province, -q012, -q013, -q014, -q015) %>%
  # change variables' names
  data.table::setnames(
    c(
      "district",
      "gender",
      "age",
      "marital_status",
      "family_structure",
      "occupation",
      "revenue_status",
      "revenue_fluctuation"
    )
  ) %>% 
  # change data type
  dplyr::mutate(
    district = factor(district),
    gender = factor(gender),
    marital_status = factor(marital_status),
    family_structure = factor(family_structure),
    occupation = factor(occupation),
    revenue_status = factor(revenue_status),
    revenue_fluctuation = factor(revenue_fluctuation)
    ) %>% 
  # replace reply codes to names
  # to display the name. Otherwise, only the code number will be.
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "1" ~ "female",
      gender == "2" ~ "male",
      gender == "3"  ~ "prefer not to say",
      TRUE  ~ "NA"
    ),
    marital_status = dplyr::case_when(
      marital_status == 1 ~ "Single",
      marital_status == 2 ~ "Married with kids",
      marital_status == 3 ~ "Married without kids",
      marital_status == 4 ~ "Married with grandkids",
      marital_status == 5 ~ "Others",
      TRUE  ~ "NA"
    ),
    family_structure = dplyr::case_when(
      family_structure == 1 ~ "Single",
      family_structure == 2 ~ "Couple",
      family_structure == 3 ~ "Family",
      family_structure == 4 ~ "living together with 3 generations",
      family_structure == 5 ~ "Others",
      TRUE  ~ "NA"
    ),
    occupation = dplyr::case_when(
      occupation == 1 ~ "Mainly agriculture",
      occupation == 2 ~ "Biased toward agriculture more than fishewry",
      occupation == 3 ~ "Biased toward fishery more than agriculture",
      occupation == 4 ~ "Mainly fishery",
      occupation == 5 ~ "Others",
      TRUE  ~ "NA"
    ),
    revenue_status = dplyr::case_when(
      revenue_status == 1 ~ "Stable",
      revenue_status == 2 ~ "Fluctuated a bit",
      revenue_status == 3 ~ "Large fluctuation",
      revenue_status == 4 ~ "Extreme fluctuation",
      revenue_status == 5 ~ "Unknown",
      TRUE  ~ "NA"
    ),
    revenue_fluctuation = dplyr::case_when(
      revenue_fluctuation == 1 ~ "Large decline",
      revenue_fluctuation == 2 ~ "Small decline",
      revenue_fluctuation == 3 ~ "Small increase",
      revenue_fluctuation == 4 ~ "Large increase",
      revenue_fluctuation == 5 ~ "Unknown",
      TRUE  ~ "NA"
    )
  )
#
# set values for tableone::CreateTableOne
# list of target variables' name
listvar <- colnames(data_table01)
#
listcat <- c("district")
#
# make the table 1
table01 <- 
  tableone::CreateTableOne(
    vars = listvar,
    strata = "gender",
    data = data_table01,
    factorVars = listcat
  )
# print the table
table01
# check the table in detail
summary(table01$CatTable)
# write .csv file to confirm
table01_extract <- 
table01 %>% 
  print(
    nonnormal = c("marial_status", "family_structure", "occupation", "revenue_status", "revenue_fluctuation"), 
    exact = c("district", "gender", "age"), 
    quote = FALSE, noSpaces = TRUE, printToggle = FALSE
  )
write.csv(
  x = table01_extract,
  file = "table01_extract.csv"
)
#
#
#
##
### END of section --- ###
##
#

### ---- map.kratie ---- 
# set spatial data up
# load administrative boundaries
# NOTE
# Before use, download the data from GADM
# (https://gadm.org/)
adm_01 <- readRDS("gadm36_KHM_1_sf.rds")
adm_02 <- readRDS("gadm36_KHM_2_sf.rds")
adm_03 <- readRDS("gadm36_KHM_3_sf.rds")
khm <- readRDS("gadm36_KHM_0_sf.rds")
#
# select Kratieh province by object
# NOTE
# An English-translated name of Kracheh, "Kracheh", is currently not available.
# Instead, use Khmer name
# Province-level object
adm_01_kratie <- 
  adm_01 %>% 
  dplyr::filter(
    NAME_1 == "Krâchéh"
  )
# district-level data
adm_02_kratie <- 
  adm_02 %>% 
  dplyr::filter(
    NAME_1 == "Krâchéh"
  ) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
    )
# commune-level data
adm_03_kratie <- 
  adm_03 %>% 
  dplyr::filter(
    NAME_1 == "Krâchéh"
  )
# # Unite the 
# adm_01_kratie_union <- 
#   adm_01_kratie %>% 
#   sf::st_union() 
# Omit the Kratieh province
# We use this object to emphasize the Kratieh province by drawing
# the administrative boundary with bold line.
adm_01_ex_kratie <- 
  adm_01 %>% 
  dplyr::filter(
    NAME_1 != "Krâchéh"
  )
# Select target communes
# We use this object to paint the communes by different color
# and add communes' ID.
adm_03_kratie_target <- 
  adm_03_kratie %>% 
  dplyr::filter(
    # specify target communes from original survey results
    NAME_3 %in% levels(
      factor(
        food_custom_fy2020$commune
        )
      )
    ) %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2],
    number = c(1:length(centroid))
  )
#
# make some layers such as street and waterway
# To make the layers, we use osmdata() library
# The library has functions to check features and tags.
# Before loading data for the layer, set them while checking them.
# In detail of the features and tags, refer to the following page.
# https://wiki.openstreetmap.org/wiki/Map_features
# features
osmdata::available_features()
# tags
osmdata::available_tags("natural")
osmdata::available_tags("water")
tags_waterway <- available_tags("waterway")
# 
# obtain features' data using osmdata()
# street
# "Street" refers to small roads excluding motorway and major road.
streets <- 
  adm_01_kratie_union %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "footway", 
      "residential", 
      "service", 
      "track",
      "residential", 
      "living_street",
      "service",
      "unclassified"
    )
  ) %>%
  osmdata::osmdata_sf()
streets
# road
road <- 
  adm_01_kratie_union %>%
  osmdata::opq() %>%
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
road
# river
# NOTE
# The feature, "river", only returns center of rivers. To obtain
# width / shape of the river use "natural" instead as below.
river <- 
  adm_01_kratie_union %>%
  opq()%>%
  add_osm_feature(
    key = "waterway", 
    value = tags_waterway
  ) %>%
  osmdata_sf()
river
# riverbank (= riverbed)
# In detail of difference between river and riberbank, refer to the following page.
# https://wiki.openstreetmap.org/wiki/Rivers
riverbank <- 
  adm_01_kratie_union %>%
  opq()%>%
  add_osm_feature(
    key = "natural", 
    value = c(
      "water"
    )
  ) %>%
  osmdata_sf()
riverbank
# canal
canal <- 
  adm_01_kratie_union %>%
  opq()%>%
  add_osm_feature(
    key = "waterway", 
    value = c(
      "canal"
    )
  ) %>%
  osmdata_sf()
canal
#
# Draw a multi-layered map
adm_01_kratie_union_multilayer_map <- 
  ggplot() +
  geom_sf() +
  # commune-level administrative boundaries of Kratie province
  geom_sf(
    data = adm_03_kratie_target,
    inherit.aes = FALSE,
    color = "black",
    fill = "grey50",
    size = 0.1,
    linetype = "dotted",
    alpha = 0.4
  ) +
  # street
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "grey50",
    size = 0.2,
    alpha = 1.0
  ) +
  # road
  geom_sf(
    data = road$osm_lines,
    inherit.aes = FALSE,
    color = "orange",
    size = 0.4,
    alpha = 1.0
  ) +
  # river
  geom_sf(
    data = river$osm_lines,
    inherit.aes = FALSE,
    color = "steelblue",
    size = 0.2,
    alpha = 1.0
  ) +
  # riberbed
  geom_sf(
    data = riverbank$osm_multipolygons,
    inherit.aes = FALSE,
    color = "steelblue",
    fill = "steelblue",
    size = 0.2,
    alpha = 1.0
  ) +
  # canal
  geom_sf(
    data = canal$osm_lines,
    inherit.aes = FALSE,
    color = "steelblue",
    size = 0.2,
    alpha = 1.0
  ) +
  # commune-level administrative boundaries of Kratie province
  geom_sf(
    data = adm_03_kratie,
    inherit.aes = FALSE,
    color = "black",
    fill = NA,
    size = 0.5,
    linetype = "dotted",
    alpha = 1.0
  ) +
  # district-level administrative boundaries of Kratie province
  geom_sf(
    data = adm_02_kratie,
    inherit.aes = FALSE,
    color = "black",
    fill = NA,
    size = 0.8,
    linetype = "dashed",
    alpha = 1.0
  ) +
  # province-level administrative boundaries
  geom_sf(
    data = adm_01_kratie,
    inherit.aes = FALSE,
    fill = NA,
    color = "black",
    size = 0.8,
    alpha = 1.0
  ) +
  # paint provinces other than Kratie in white
  geom_sf(
    data = adm_01_ex_kratie,
    inherit.aes = FALSE,
    color = "black",
    fill = "white",
    size = 0.5,
    alpha = 1.5
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    caption = "\U00a9 OpenStreetMap contributors"
  ) +
  geom_text(
    data = adm_02_kratie,
    aes(
      x = center_x,
      y = center_y,
      label = NAME_2,
      # adjust font size when necessary
      size = 2
    ),
    show.legend = FALSE,
    family = "Times"
  )+
  geom_text(
    data = adm_03_kratie_target,
    aes(
      x = center_x,
      y = center_y,
      label = number,
      # adjust font size when necessary
      size = 2
    ),
    show.legend = FALSE,
    check_overlap = FALSE,
    family = "sans"
  )+
  # fix boundary box
  coord_sf(xlim = c(105.6, 106.8),
           ylim = c(12.0, 13.4),
           expand = TRUE
           ) +
  theme_classic() +
  theme(
    plot.background = element_rect(fill = NA)
  )
adm_01_kratie_union_multilayer_map
# save the map
ggsave(
  "adm_01_kratie_union_multilayer_map.pdf",
  plot = adm_01_kratie_union_multilayer_map,
  # width = 700,
  # height = 700,
  # units = "mm",
  device = cairo_pdf # important!!
)
# add some components to the map
adm_01_kratie_union_multilayer_map_02 <- 
  adm_01_kratie_union_multilayer_map +
  # # provinces' name
  # annotate(
  #   geom = "label",
  #   x = mrd_centroid$center_x,
  #   y = mrd_centroid$center_y,
  #   label = mrd_centroid$VARNAME_1,
  #   size = 3,
  #   family = "Times",
  #   fill = "white"
  # ) +
  # scalebar
  ggsn::scalebar(
    x.min = 105.6,
    x.max = 106.0,
    y.min =12.0,
    y.max =12.1,
    dist = 20, 
    dist_unit = "km",
    st.dist = 0.3, 
    st.size = 3, 
    height= 0.3, 
    transform = TRUE
  ) +
  # north arrow
  ggsn::north(
    x.min = 105.6,
    x.max = 105.7,
    y.min =12.05,
    y.max = 12.15,
    symbol = 8,
    scale = 1
  )
adm_01_kratie_union_multilayer_map_02
# save the map
ggsave(
  "adm_01_kratie_union_multilayer_map_02.pdf", 
  plot = adm_01_kratie_union_multilayer_map_02
)
#
#
#
##
### END of section --- ###
##
#
### ---- q.food.custom ----
# Make data tables for specific purposes
# 
# make a contingency table by fish dish and yes/no
# (whether to eat or not)
yes_no_table <- 
  fisheat_custom %>% 
  dplyr::select(
    Chopped_small_fish, 
    Sliced_large_fish, 
    Puffer_fish,
    yes_no_fish
  ) %>% 
  ftable(
    ., 
    row.vars = c("Chopped_small_fish", "Sliced_large_fish"), 
    col.vars = "Puffer_fish"
  ) %>% 
  data.frame()
# 
# reshape the data form
# the loaded raw data is not available to analyse.
# we need to separate the data into some parts and
# bind them together.
# 
fisheat_custom_yes_no <- 
  food_custom_fy2020 %>% 
  # pick up necessary part
  dplyr::select(q100, q200, q300) %>% 
  data.table::setnames(
    c(
      "Chopped_small_fish",
      "Sliced_large_fish",
      "Puffer_fish"
      ) 
    ) %>% 
  # change data type
  dplyr::mutate(
    Chopped_small_fish = factor(Chopped_small_fish),
    Sliced_large_fish = factor(Sliced_large_fish),
    Puffer_fish = factor(Puffer_fish)
    ) %>% 
  dplyr::mutate_all(
    ~ 
      ifelse(
        .== "1", "No", "Yes"
        )
    ) 
#
# No in detail
# part 2. Separate multiple choice answers
# set option of answers
# This time, we provided 7 options for receipient.
q_list <- c("1","2","3","4","5","6","7")
fisheat_custom_no_detail <- 
  food_custom_fy2020 %>% 
  # select the detailed question for not eating fishes
  # q131: Why do not you eat chopped small fish?
  # q231: Why do not you eat sliced large fish?
  # q331: Why do not you eat pufferfish?
  select(q131, q231, q331) %>% 
  # 
  dplyr::transmute(
    # chopped small fish
    q1311 = +(str_detect(q131, q_list[1])),
    q1312 = +(str_detect(q131, q_list[2])),
    q1313 = +(str_detect(q131, q_list[3])),
    q1314 = +(str_detect(q131, q_list[4])),
    q1315 = +(str_detect(q131, q_list[5])),
    q1316 = +(str_detect(q131, q_list[6])),
    q1317 = +(str_detect(q131, q_list[7])),
    # sliced large fish
    q2311 = +(str_detect(q231, q_list[1])),
    q2312 = +(str_detect(q231, q_list[2])),
    q2313 = +(str_detect(q231, q_list[3])),
    q2314 = +(str_detect(q231, q_list[4])),
    q2315 = +(str_detect(q231, q_list[5])),
    q2316 = +(str_detect(q231, q_list[6])),
    q2317 = +(str_detect(q231, q_list[7])),
    # pufferfish
    q3311 = +(str_detect(q331, q_list[1])),
    q3312 = +(str_detect(q331, q_list[2])),
    q3313 = +(str_detect(q331, q_list[3])),
    q3314 = +(str_detect(q331, q_list[4])),
    q3315 = +(str_detect(q331, q_list[5])),
    q3316 = +(str_detect(q331, q_list[6])),
    q3317 = +(str_detect(q331, q_list[7]))
  )
# part 3. on risks eating the fishes
fisheat_custom_no_risks <- 
  food_custom_fy2020 %>% 
  select(q132, q232, q332) 
# part 4. Yes in detail
fisheat_custom_yes_detail <- 
  food_custom_fy2020 %>% 
  # select the detailed question for not eating fishes
  # q131: Why do not you eat chopped small fish?
  # q231: Why do not you eat sliced large fish?
  # q331: Why do not you eat pufferfish?
  dplyr::select(q111:q118, q211:q218, q311:q318) 
# combine the 5 data altogether
fisheat_custom <- 
  # combine the data
  dplyr::bind_cols(
    data_table01,
    fisheat_custom_yes_no,    # part 1
    fisheat_custom_no_detail, # part 2
    fisheat_custom_no_risks,  # part 3
    fisheat_custom_yes_detail # part 4
    ) %>% 
  # add 8 eating patterns
  dplyr::mutate(
    yes_no_fish = dplyr::case_when(
      (Chopped_small_fish == "Yes" & Sliced_large_fish == "Yes" & Puffer_fish == "Yes")  ~ "1",
      (Chopped_small_fish == "No" & Sliced_large_fish == "Yes" & Puffer_fish == "Yes")  ~ "2",
      (Chopped_small_fish == "Yes" & Sliced_large_fish == "No" & Puffer_fish == "Yes")  ~ "3",
      (Chopped_small_fish == "No" & Sliced_large_fish == "No" & Puffer_fish == "Yes")  ~ "4",
      (Chopped_small_fish == "Yes" & Sliced_large_fish == "Yes" & Puffer_fish == "No")  ~ "5",
      (Chopped_small_fish == "No" & Sliced_large_fish == "Yes" & Puffer_fish == "No")  ~ "6",
      (Chopped_small_fish == "Yes" & Sliced_large_fish == "No" & Puffer_fish == "No")  ~ "7",
      (Chopped_small_fish == "No" & Sliced_large_fish == "No" & Puffer_fish == "No")  ~ "8",
      TRUE  ~ "NA"
        )
    ) 
# 
# subset of those who do NOT eat fish dish
fisheat_custom_no <- 
  fisheat_custom %>% 
  dplyr::filter(yes_no_fish %in% c("5","6","7","8")) %>% 
  dplyr::select(district:q3317, yes_no_fish) %>% 
  na.omit()
# 
# reshape the subset of those who do NOT eat fish dish
# More tidy data, more convenient to analyse.
# The raw data contains the same questions for different fish at different columns.
# 
# read common columns' names
column_names_no <- 
  c(
    "district", 
    "gender", 
    "age", 
    "marital_status", 
    "family_structure", 
    "occupation", 
    "revenue_status", 
    "revenue_fluctuation",
    "a11", # multiple choice option 1. "Harmful to health" 
    "a12", # multiple choice option 2. "Not tasty" 
    "a13", # multiple choice option 3. "Allergy to fish"
    "a14", # multiple choice option 4. "Religious regulation" 
    "a15", # multiple choice option 5. "Unable to bite" 
    "a16", # multiple choice option 6. "Experience" 
    "a17", # multiple choice option 7. "Others" 
    "yes_no_fish"
    )
#
# separate the "fisheat_custom_no" by fish to rebind later
# BEWARE!!
# Confirm range of the answers!!
# Otherwise, malfunctions might occur. 
# chopped small fish
fisheat_custom_no_01 <-
  fisheat_custom_no %>%
  # range check!!
  dplyr::select(-Chopped_small_fish, -Sliced_large_fish, -Puffer_fish, -c(q2311:q3317)) %>%
  data.table::setnames(
    column_names_no
  ) %>%
  dplyr::mutate(type = "Chopped_small_fish")
# sliced large fish
fisheat_custom_no_02 <-
  fisheat_custom_no %>%
  # range check!
  dplyr::select(-Chopped_small_fish, -Sliced_large_fish, -Puffer_fish, -c(q1311:q1317,q3311:q3317)) %>%
  data.table::setnames(
    column_names_no
  ) %>%
  dplyr::mutate(type = "Sliced_large_fish")
# puffer fish
fisheat_custom_no_03 <-
  fisheat_custom_no %>%
  # range check!
  dplyr::select(-Chopped_small_fish, -Sliced_large_fish, -Puffer_fish, -c(q1311:q2317)) %>%
  data.table::setnames(
    column_names_no
  ) %>%
  dplyr::mutate(type = "Puffer_fish")
# rebind the data and replace the codes into words
# Oh, SUTOKUIN!! 
fisheat_custom_nono <-
  # bind the 3 component above
  dplyr::bind_rows(
    fisheat_custom_no_01, 
    fisheat_custom_no_02, 
    fisheat_custom_no_03
    ) %>%
  tidyr::pivot_longer(
    cols = c(a11:a17),
    names_to = "question",
    values_to = "answer"
  ) %>%
  dplyr::mutate(
    question = dplyr::case_when(
      question == "a11" ~ "Harmful to health",
      question == "a12" ~ "Not tasty",
      question == "a13" ~ "Allergy to fish",
      question == "a14" ~ "Religious regulation",
      question == "a15" ~ "Unable to bite",
      question == "a16" ~ "Experience",
      question == "a17" ~ "Others",
      TRUE ~ "NA"
    ),
    answer = dplyr::case_when(
      answer == "1" ~ "Yes",
      answer == "0" ~ "No",
      TRUE ~ "NA"
      )
  ) %>%
  # transform character data into factor
  dplyr::mutate_if(
    is.character,
    list(~ factor(.))
  )
#
# compute the N of choosing persons by reason 
# Reason:
# 1. "Harmful to health"
# 2. "Not tasty"
# 3. "Allergy to fish"
# 4. "Religious regulation"
# 5. "Unable to bite"
# 6. "Experience"
# 7. "Others"
# assign the reason into the object
reason <- c("Harmful to health","Not tasty","Allergy to fish","Religious regulation","Unable to bite","Experience","Others")
# compute the number
fisheat_custom_no_sum <- 
  fisheat_custom_no %>% 
  select(q1311:q3317) %>% 
  summarise_each(dplyr::funs(sum)) %>% 
  data.frame() 
# separate the results by fish
# chopped small fish
fisheat_custom_no_sum_01 <- 
  fisheat_custom_no_sum %>% 
  select(q1311:q1317) %>% 
  data.table::setnames(no_reason)
# sliced large fish
fisheat_custom_no_sum_02 <- 
  fisheat_custom_no_sum %>% 
  select(q2311:q2317) %>% 
  data.table::setnames(no_reason)
# puffer fish
fisheat_custom_no_sum_03 <- 
  fisheat_custom_no_sum %>% 
  select(q3311:q3317) %>% 
  data.table::setnames(no_reason) 
# 
# make a contingency table by fish dish type and reason 
# set fish dish type
type <- c("Chopped small fish","Sliced large fish","Puffer fish")
# bind the separated data above
fisheat_custom_no_sumsum <- 
  dplyr::bind_rows(
    fisheat_custom_no_sum_01,
    fisheat_custom_no_sum_02,
    fisheat_custom_no_sum_03
    ) 
# old-fashioned!!
rownames(fisheat_custom_no_sumsum) <- type
# make the table
fisheat_custom_no_sumsum_table <- 
fisheat_custom_no_sumsum %>% 
  as_tibble() %>% 
  bind_cols(type = type) %>% 
  pivot_longer(
    cols = -type,
    names_to = "question",
    values_to = "Freq"
  )
#


# Frequency to eat the fish dish by fish dish type
# Yes in detail
# select answers regarding the frequency
fisheat_custom_yes <-
  fisheat_custom %>%
  dplyr::filter(
    yes_no_fish %in% c("1","2","3","4")
    ) %>%
  # select related part
  # Sometimes demographic information part interrupts fine view.
  dplyr::select(
    district:Puffer_fish,
    q111:yes_no_fish
    ) %>%
  # replace "999", the code for NA, into NA
  dplyr::mutate_at(
    vars(q111:q318),
    list(
      ~
        ifelse(
          . == 999, # if an observation is equal to 999,
          NA,       # we will replace the observation into NA.
          .         # If not, we leave it still.
          )
      )
  ) %>% 
  dplyr::select(q111:q116, q211:q216, q311:q316) %>%
  dplyr::mutate_all(list(~ replace(., is.na(.), 0))) %>% 
  summarise_each(dplyr::funs(sum))



occasion <- 
  c(
    "daily_meal_annual",
    "daily_meal_weekly_dry",
    "daily_meal_weekly_rainy",
    "daily_banquet_annual",
    "daily_banquet_weekly_dry",
    "daily_banquet_weekly_rainy"
  )

fisheat_custom_yes_c <- fisheat_custom_yes %>% dplyr::select(q111:q116) %>% data.table::setnames(occasion)
fisheat_custom_yes_s <- fisheat_custom_yes %>% dplyr::select(q211:q216) %>% data.table::setnames(occasion)
fisheat_custom_yes_p <- fisheat_custom_yes %>% dplyr::select(q311:q316) %>% data.table::setnames(occasion)

fisheat_custom_yes_csp <- 
  dplyr::bind_rows(fisheat_custom_yes_c, fisheat_custom_yes_s, fisheat_custom_yes_p) %>% 
  dplyr::bind_cols(type = c("Chopped small fish","Sliced large fish","Puffer fish")) %>% 
  pivot_longer(
    cols = -type,
    names_to = "occasion",
    values_to = "Freq"
  )

# separate the data by pattern of fish dish eat
# Yes 1/4
# N = 1
# Chopped small fish: Yes
# Sliced large fish: Yes
# Puffer fish: Yes
fisheat_custom_yes_01 <-
  fisheat_custom %>%
  dplyr::mutate_at(
    vars(q111:q318),
    list(
      ~
        ifelse(
          . == 999, # if an observation is equal to 999,
          NA,       # we will replace the observation into NA.
          .         # If not, we leave it still.
        )
    )
  ) %>% 
  dplyr::filter(yes_no_fish == "1") %>%
  dplyr::select(q111:q116, q211:q216, q311:q316) %>%
  dplyr::mutate_all(list(~ replace(., is.na(.), 0))) %>%
  summarise_each(dplyr::funs(median))
# Yes 2/4
# N = 3
# Chopped small fish: No
# Sliced large fish: Yes
# Puffer fish: Yes
fisheat_custom_yes_02 <-
  fisheat_custom %>%
  dplyr::mutate_at(
    vars(q111:q318),
    list(
      ~
        ifelse(
          . == 999, # if an observation is equal to 999,
          NA,       # we will replace the observation into NA.
          .         # If not, we leave it still.
        )
    )
  ) %>% 
  dplyr::filter(yes_no_fish == "2") %>%
  dplyr::select(q211:q216, q311:q316) %>%
  dplyr::mutate_all(list(~ replace(., is.na(.), 0)))%>%
  summarise_each(dplyr::funs(median))
# Yes 3/4
# N = 19
# Chopped small fish: Yes
# Sliced large fish: No
# Puffer fish: Yes
fisheat_custom_yes_03 <-
  fisheat_custom %>%
  dplyr::mutate_at(
    vars(q111:q318),
    list(
      ~
        ifelse(
          . == 999, # if an observation is equal to 999,
          NA,       # we will replace the observation into NA.
          .         # If not, we leave it still.
        )
    )
  ) %>% 
  dplyr::filter(yes_no_fish == "3") %>%
  dplyr::select(q111:q116, q311:q316) %>%
  dplyr::mutate_all(list(~ replace(., is.na(.), 0)))%>%
  summarise_each(dplyr::funs(median))
# Yes 4/4
# N = 15
# Chopped small fish: No
# Sliced large fish: No
# Puffer fish: Yes
fisheat_custom_yes_04 <-
  fisheat_custom %>%
  dplyr::mutate_at(
    vars(q111:q318),
    list(
      ~
        ifelse(
          . == 999, # if an observation is equal to 999,
          NA,       # we will replace the observation into NA.
          .         # If not, we leave it still.
        )
    )
  ) %>% 
  dplyr::filter(yes_no_fish == "4") %>%
  dplyr::select(q311:q316) %>%
  dplyr::mutate_all(list(~ replace(., is.na(.), 0)))%>%
  summarise_each(dplyr::funs(median))
#
#
#
##
### END of section --- ###
##
#
#
### ---- analysis ----
#
#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# IMPORTANT PART
#
# 1. Whether the fish dish type and reason not to eat are independent  
# test independence
chisq.test(fisheat_custom_no_sumsum )
#
# 2. Do the reason and fish dish type affect frequency not to eat the fish dish eat?
# log linear model
fisheat_custom_no_sumsum_table_lm <- 
  summary(lm(log(Freq+0.5) ~ question + type , data = fisheat_custom_no_sumsum_table))
# Bayesian inference
fisheat_custom_no_sumsum_table_bayes <- 
  brms::brm(formula = log(Freq+0.5) ~ question + type, 
            family = "gaussian",
            chains = 4,
            iter = 500,
            thin = 1,
            data = hogetable
  )
fisheat_custom_no_sumsum_table_bayes
plot(fisheat_custom_no_sumsum_table_bayes)
#
# 3. Do the fish dish type affects the frequency to eat the fish dish eat?
# log linear model
yes_no_table_lm <- 
  summary(lm(log(Freq+0.5) ~ Chopped_small_fish + Sliced_large_fish + Puffer_fish, data = yes_no_table))
# Bayesian inference
yes_no_table_bayes <- 
  brms::brm(formula = log(Freq+0.5) ~ Chopped_small_fish + Sliced_large_fish + Puffer_fish, 
            family = "gaussian",
            chains = 4,
            iter = 500,
            thin = 1,
            data = yes_no_table
  )
yes_no_table_bayes
plot(yes_no_table_bayes)
# 
# 4. How often / when do they eat the fish dish by pattern?
fisheat_custom_yes_01
fisheat_custom_yes_02
fisheat_custom_yes_03
fisheat_custom_yes_04

#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
#
##
### END of section --- ###
##
#
