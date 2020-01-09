# Working status and ecosystem service at Kampi, Kratie, Cambodia
# original: 9th. January 2020
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
#
# --- END --- ###



# ---- read.data ----
living.status.original <-  
  readxl::read_excel("../../southeastasiastudy/Cambodia/cambodia_fy2019.xlsx",
                     sheet = "results.dec.2019",
                     range = "A4:Cx33"
                     )

selected.valuables <- c("id", "date", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "10", "13", "15.01", "15.02", "15.03", "15.04", "15.05", "15.06", "15.07", "15.08", "15.09", "15.10", "15.11", "15.12", "16.01", "16.02", "16.03", "16.04", "16.05", "16.06", "16.07", "16.08", "16.09", "16.10", "16.11", "16.12", "19", "20", "21", "22", "23.01", "23.02", "23.03", "23.04", "23.05", "23.06", "23.07", "23.08", "23.09", "23.10", "23.11", "23.12", "24.01", "24.02", "24.03", "24.04", "24.05", "24.06", "24.07", "24.08", "24.09", "24.10", "24.11", "24.12", "boat", "fish")

living.status <- 
  living.status.original %>% 
  dplyr::select_at(selected.valuables) %>% 
  tidyr::gather(key = question, 
                value = answer, -id, -date, -boat, -fish) %>% 
  dplyr::mutate(id = factor(id),
                date = dmy(date),
                boat = factor(boat),
                fish = factor(fish),
                question = factor(question)
  )
#
# --- END --- ###

# ---- filter.number ----
# filtering number
# 15: cost for fishing
# 16: sales from fishing
# 23: cost for dolphin boat
# 24: sales for dolphin boat
#
# --- END --- ###

# ---- data.fish.cost ----
living.status.boat.fish.cost <- 
  living.status %>% 
  dplyr::filter(boat == "1") %>%   # person engaging in dolphin boat (If not, 0)
  dplyr::filter(grepl("15.",       # cost for fishing
                      question
                      )
                ) %>%  
  dplyr::mutate(month = 
                  as.numeric(
                    str_sub(question, 
                            start = 4, 
                            end = 5
                            )
                    ),
                answer = as.numeric(answer)
                ) %>% 
  dplyr::mutate(month = 
                  as.Date(paste0("2019-",month(month), "-", "01"))) %>% 
  dplyr::mutate(month = as.POSIXct(month))

#
# --- END --- ###

# ---- line.cost ----
line.boat.fish.cost <- 
  living.status.boat.fish.cost %>% 
  ggplot(aes(x = month, y = answer, colour = id)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_datetime(date_labels = "'%m-%Y") +
  scale_color_viridis(option = "viridis", 
                      discrete = TRUE
                      ) +
  annotate("rect",
           xmin = as.POSIXct("2019-06-01"),
           xmax = as.POSIXct("2019-11-01"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.1
           ) +
  labs(x = "Month (CY2019)", 
       y = "Monthly cost for fishing (Unit: USD)",
       colour = "ID of dolphin boat captain"
       ) +
  guides(colour = guide_legend(ncol = 3)) +
  theme_classic() +
  theme(
    legend.position = c(0.15, 0.8)
  )
#
# --- END --- ###

