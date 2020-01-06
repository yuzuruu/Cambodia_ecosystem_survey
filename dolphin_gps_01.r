# Dolphin boat and ecosystem service at Kampi, Kratie, Cambodia
# original: 1st. January 2020
# revised: 
# by Yuzuru Utsunomiya
#

# ---- read.library ----
library(classInt)
library(DCluster)
library(dichromat) # color brewer following universal color design
library(geosphere)
library(ggmap)
library(ggsn)
library(ggrepel)
library(ggspatial)
library(gplots)
library(gpclib)
library(grid)
library(lubridate)
library(maptools)
library(MASS)
# library(OpenStreetMap)
library(raster)
library(RColorBrewer)
library(readxl)
library(rgdal)
library(sf)
library(spsurvey)
library(sp)
library(spdep)
library(tidyverse) #
library(viridis)
library(viridisLite)
# library(tidybayes)
# stan and itssettings
# library(brms)
# library(rstan)
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
# theme_set(theme_get() + theme(aspect.ratio = 3/4))
# library(shinystan)




readr::read_csv("../../southeastasiastudy/Cambodia/gps_log_file_dolphin/11/2019-03-10_07-48-25.csv")

list.gps.log <- 
  base::list.files("../../southeastasiastudy/Cambodia/gps_log_file_dolphin/",
                   pattern = "csv$",
                   recursive = TRUE,
                   include.dirs = TRUE
                   ) %>% 
  paste("../../southeastasiastudy/Cambodia/gps_log_file_dolphin/",.,
        sep = ""
        )

select.fun <- function(x){
  dplyr::select(data = x,
    INDEX, 
    TIME, 
    LATITUDE, 
    LONGITUDE
  )
  }

hoge <- 
  list.gps.log %>% 
  map(readr::read_csv) %>% 
  
  
  
  
  mutate(selected.data = map(select.fun(.)))

head(hoge)

# ---- read.data ----
# read data
dolphin.gps <- 
  readr::read_csv("2019-12-18_08-02-02.csv") %>% 
  dplyr::select(INDEX, TIME, LATITUDE, LONGITUDE) %>% 
  dplyr::rename(id = INDEX,
                time = TIME,
                lat = LATITUDE,
                lon = LONGITUDE
                ) %>% 
  mutate(time = lubridate::parse_date_time2(.$time,
                                          orders = "%Y/%m/%d %H:%M:%S",
                                          tz = "UTC"
                                          )
  ) %>% 
  mutate(time = with_tz(.$time, "Asia/Ho_Chi_Minh")
  )

# filter the data in accordance with following conditions.
# set a target interval
date.start <- ymd_hms("2019-12-18 07:30:00", tz = "Asia/Ho_Chi_Minh")
date.end <- ymd_hms("2019-12-18 16:30:00", tz = "Asia/Ho_Chi_Minh")
target_interval <- interval(
  date.start,
  date.end
)
# implement filster
dolphin.gps.01 <- 
  dolphin.gps %>% 
  dplyr::filter(time %within% target_interval)

# compute moving speed
# NOTE
# We should revise the code below to avoid
# using for loop.
speed <- NULL
for(i in 2:nrow(dolphin.gps.01)){
  speed.individual <- geosphere::distGeo(c(dolphin.gps.01$lon[i-1], dolphin.gps.01$lat[i-1]),
                             c(dolphin.gps.01$lon[i], dolphin.gps.01$lat[i])
  )
  speed <- c(speed, speed.individual)
}
speed <- as.data.frame(c(0, speed))

# combine gps data and speed data and
# transform unit of the speed into km/h
dolphin.gps.01 <- 
  dolphin.gps.01 %>% 
  bind_cols(speed) %>%  # bind the two data set togegher
  mutate(speed = .$`c(0, speed)`) %>%  # rename the speed data
  select(id, time, lat, lon, speed) %>% 
  mutate(speed = .$speed*12*60/1000)

# line plot (speed)
dolphin.gps.line.01 <- 
  dolphin.gps.01 %>%   
  ggplot2::ggplot(aes(x = time, y = speed)) +
  geom_line() +
  labs(x = "Time (07.30-16.30, Freq.=5.0sec.)", y = "Speed (km/h)") + 
  theme_classic()
#
# --- END --- ###

# ---- route.map ----
# read Google API key
# Without the API, we cannot use get_map() function below
source("map.key.r")
# center of satellite imagery map
# Set the number while referring to satellite map
lat.center.dolphin <- c(12.608)
lon.center.dolphin <- c(106.015)
# Obtain satellite imagery
dolphin.sat.01 <- 
  get_map(location = c(lon = lon.center.dolphin,
                       lat = lat.center.dolphin
  ), 
  maptype = "satellite",
  zoom = 15
  ) %>% 
  ggmap()
# add points onto the imagery
dolphin.sat.gps.01 <- 
  dolphin.sat.01 + 
  geom_point(data = dolphin.gps.01, 
             aes(x = lon, 
                 y = lat,
                 colour = speed
             ),
             
  ) +
  labs(x = "Longitude",
       y = "Latitude"
  ) +
  scale_color_viridis(option = "plasma",
                      begin = 1, 
                      end = 0
                      ) +
  theme_classic()

# # save the figure
# # Comment out when not in use
# ggsave("dolphin.sat.gps.01.pdf",
#        plot = dolphin.sat.gps.01
# )
#
## --- END --- ###




