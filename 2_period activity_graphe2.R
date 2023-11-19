

#----------- Module 09 -- Multiple animals ------------X

# Load packages ----
library(tidyverse)
library(mvtnorm)
library(amt)
library(purrr)
library(lubridate)
library(sf)
library(terra)
library(glmmTMB)
library(broom)

# 1. Load data ----

# GPS data 
dagami_42 = read.csv('data/X42.csv',sep=';', dec = ",") %>% 
  mutate( id = "42")
dagami_43 =read.csv('data/X43.csv',sep=';', dec = ",") %>% 
  mutate( id = "43")
dagami_44 =read.csv('data/X44.csv',sep=';', dec = ",") %>% 
  mutate( id = "44")
dagami_45 =read.csv('data/X45.csv',sep=';', dec = ",") %>% 
  mutate( id = "45")
dagami_46 =read.csv('data/X46.csv',sep=';', dec = ",") %>% 
  mutate( id = "46")
dagami_47 =read.csv('data/X47.csv',sep=';', dec = ",") %>% 
  mutate( id = "47")
dagami_48 =read.csv('data/X48.csv',sep=';', dec = ",") %>% 
  mutate( id = "48")
dagami_50 =read.csv('data/X50.csv',sep=';', dec = ",") %>% 
  mutate( id = "50")
dagami_52 =read.csv('data/X52.csv',sep=';', dec = ",") %>% 
  mutate( id = "52")
dagami_53 =read.csv('data/X53.csv',sep=';', dec = ",") %>% 
  mutate( id = "53")

dagami = rbind(dagami_42, 
               dagami_43, 
               dagami_44, 
               dagami_45, 
               dagami_46, 
               dagami_47, 
               dagami_48, 
               dagami_50, 
               dagami_52,
               dagami_53) %>% 
  subset(., Date != "10/05/2023") # journee des tests de predeploiement avec Argos


colony <- tibble(
  Longitude = -52.158637, 
  Latitude  = 4.644658 )%>% 
  st_as_sf(.,coords = c("Longitude","Latitude"), crs = 4326) %>% 
  st_transform(., crs = 2971)

colony_radius = 100 # units: meters

dagami  = dagami %>% 
  mutate(ts = dmy_hms(paste(dagami$Date, dagami$Time))) %>% 
  mutate(ts = ts - hms("03:00:00")) %>% 
  rename(x_ = Longitude, y_ = Latitude) %>% 
  subset(., CRC == "OK") %>% 
  dplyr::select(id, x_, y_, ts, Fix) %>% 
  st_as_sf(.,coords = c("x_","y_"), crs = 4326) %>% 
  st_transform(., crs = 2971) %>% #
  mutate(x_ = st_coordinates(.)[, "X"],
         y_ = st_coordinates(.)[, "Y"]) %>%
  st_drop_geometry()

# iSSF --------------------------------------------------------------------


trk <- dagami %>% 
  make_track(x_, y_, ts,
             id = id, Fix = Fix,
             crs = 2971) %>% 
  tracked_from_to(from = as.POSIXct("2023-05-19 20:00"),
                  to = as.POSIXct("2024-10-01 08:00"))


dat1 <- trk %>% nest(data = -id)
dat2 <- dat1 %>% 
  mutate(dat.resample = map(data, ~ track_resample(., rate = minutes(60), 
                                                   tolerance = minutes(10))))%>% 
  mutate(ssf = map(dat.resample, ~ .x %>% steps_by_burst())) %>% 
  dplyr::select(ssf, id) %>% unnest(cols = ssf) %>% 
  filter(sl_ > 0) %>% 
  na.omit(., cols=c("x2_","y2_")) %>% 
  st_as_sf(.,coords = c("x2_","y2_"), crs = 2971) %>% 
  mutate(dist_colony = st_distance(., colony) %>% 
           as.numeric())%>% 
  mutate(x1_ = st_coordinates(.)[, "X"],
         y1_ = st_coordinates(.)[, "Y"]) %>% 
  st_drop_geometry() %>% 
  subset(dist_colony > 0) %>%
  subset(dist_colony < Inf) %>%
  mutate(presence_colony = dist_colony < colony_radius)



trk = dat2 %>% 
  na.omit()

trk$sl_cat = trk$sl_ %>% 
  cut(.,
      breaks=c(0, 15, 200, 1000, 10000000),
      labels=c('Noise', 'Short','Medium', 'long')) %>% 
  as.factor()

levels(trk$sl_cat) = c('0-15', '15-200','200-1000', '1000+')

library(gcookbook) # Load gcookbook for the cabbage_exp data set

trk  = trk %>% rename(Category = sl_cat)

p = ggplot(trk, aes(x = hour(t1_), y = length(Category), fill = Category)) +
  geom_col(position = "fill") +
  ylab("Step length category (meter)") +
  xlab("Hour") +
  scale_fill_brewer(palette="Greys", direction=-1)


p 
