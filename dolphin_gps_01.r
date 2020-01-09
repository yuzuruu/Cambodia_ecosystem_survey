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


# NOTE
# 1. To read multiple .csv files while using the map2_df() function, refer to the following website
# https://stackoverflow.com/questions/42028710/add-new-variable-to-list-of-data-frames-with-purrr-and-mutate-from-dplyr
# 2. Form of the data is data.frame including all log data. Separate as necessary when in use.

# ---- obtain.list ----
# make a list of target files
list.gps.log <- 
  base::list.files("../../southeastasiastudy/Cambodia/gps_log_file_dolphin/",
                   pattern = "csv$",
                   recursive = TRUE,
                   include.dirs = TRUE
  ) %>% 
  paste("../../southeastasiastudy/Cambodia/gps_log_file_dolphin/",
        .,
        sep = ""
  )
# make a list of gps loggers' name
name.gps.logger <- 
  paste("gps.",
        stringr::str_sub(list.gps.log,
                         start = 56,
                         end = 57
                         ), 
                         sep=""
                         )
# 
## --- END --- ###

# ---- read.data ----
# read the target data
dolphin.gps <- 
  list.gps.log %>% 
  purrr::map(readr::read_csv) %>%  # read data
  map2_df(name.gps.logger,         # add logger's name as a variable
          ~ mutate(.x, 
                   gps.logger.name = .y
                   )
          ) %>% 
  dplyr::select(INDEX, 
                TIME, 
                LATITUDE, 
                LONGITUDE, 
                gps.logger.name
                ) %>% 
  dplyr::rename(id = INDEX,
                time = TIME,
                lat = LATITUDE,
                lon = LONGITUDE
  ) %>% 
  mutate(time = lubridate::parse_date_time2(.$time,
                                            orders = "%Y/%m/%d %H:%M:%S",
                                            tz = "utc"
                                            )
  )
# 
## --- END --- ###

# ---- set.period.function ----
# set period
date.start <- ymd_hms("2019-12-19 07:30:00")
date.end <- ymd_hms("2019-12-19 16:30:00")
target_interval <- interval(
  date.start,
  date.end
)
# function 
lon.lat.02.fun <- 
  function(x){
    data_frame(
      lon.02 = c(x$lon[-1],0),
      lat.02 = c(x$lat[-1],0)
    )
  }
# function computing speed
distGeo.fun <- 
  function(x,y){
    tibble::enframe(distGeo(cbind(x$lon, x$lat),
                            cbind(y$lon.02, y$lat.02)
                            )*12*60/1000,
                    name = "id.speed",
                    value = "speed"
    )
  }
# function to omit unnecessary points
omit.last.fun <- 
  function(x){
    x[-nrow(x),]
  }
# 
## --- END --- ###

# ---- add.speed ----
dolphin.gps.dec.19 <- 
  dolphin.gps %>% 
  dplyr::filter(time %within% target_interval) %>% 
  group_by(gps.logger.name) %>% 
  nest() %>% 
  mutate(lon.lat.02 = map(data, 
                          lon.lat.02.fun
                          )
         ) %>% 
  mutate(speed = map2(data,
                      lon.lat.02,
                      distGeo.fun
                      )
  ) %>% 
  mutate(data = map(data, 
                    omit.last.fun
                    ),
         lon.lat.02 = map(lon.lat.02, 
                          omit.last.fun
                          ),
         speed = map(speed, 
                     omit.last.fun
                     ) 
         ) %>% 
  unnest(cols = c(data, lon.lat.02, speed))
# 
## --- END --- ###

# just for confirmation
# write.csv(dolphin.gps.dec.19, "hoge.csv")

# ---- subset.gps.data ----
dolphin.gps.dec.19.sub  <- 
  dolphin.gps.dec.19 %>%
  dplyr::filter(gps.logger.name %in% c("gps.15","gps.13") & speed < 35) 
# 
## --- END --- ###


# ---- speed.line ----
dolphin.gps.dec.19.sub %>% 
  ggplot(aes(x = time, 
             y = speed, 
             colour = gps.logger.name
             )
         ) +
  geom_line() +
  facet_wrap(~gps.logger.name)
# 
## --- END --- ###

# ---- map.key ----
# read Google API key
# Without the API, we cannot use get_map() function below
source("map.key.r")

# ---- route.map ----
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
  geom_point(data = dolphin.gps.dec.19.sub, 
             aes(x = lon, 
                 y = lat,
                 colour = speed
             ),
             
  ) +
  labs(x = "Longitude",
       y = "Latitude",
       color = "Speed (km/h)"
  ) +
  scale_color_viridis(option = "plasma",
                      begin = 1, 
                      end = 0
  ) +
  facet_wrap(~ gps.logger.name) +
  theme_classic()

dolphin.sat.gps.01

# # save the figure
# # Comment out when not in use
# ggsave("dolphin.sat.gps.01.pdf",
#        plot = dolphin.sat.gps.01
# )
# 
## --- END --- ###




