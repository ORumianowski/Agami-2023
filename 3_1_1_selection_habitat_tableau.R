

#----------- Module 09 -- Multiple animals ------------X


rm(list = ls())

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
library(readxl)

# 1. Load data ----

bordereau <- read_excel("data/bordereau.xlsx")

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

colony_radius = 1000 # units: meters

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


# Habitat data 

habitat = st_read("habitat/Habitats_forestiers_2015_clean.shp") 

habitat = habitat %>% 
  dplyr::select(grid_code) %>% 
  mutate(grid_code = as.character(grid_code)) %>% 
  mutate(grid_code = fct_collapse(grid_code,
                                   Mangroves = c("41120"),
                                   Marais_interieur = c("0"),
                                   Forets_inondables = c("41110", "41111","41210"), 
                                   Forets_terre_ferme = c("41610"))) %>% 
  subset(., grid_code == "Marais_interieur" |
           grid_code == "Forets_inondables")

cycle_hire_osm_projected = st_transform(habitat, "EPSG:2971")
raster_template = rast(ext(cycle_hire_osm_projected), resolution = 100,
                       crs = st_crs(cycle_hire_osm_projected)$wkt)

hab = rasterize(cycle_hire_osm_projected, raster_template, field = "grid_code",
                fun = "max")


#writeRaster(hab, filename=file.path("hab_rasterize.tif"), overwrite=TRUE)
#rast("hab_rasterize.tif")

names(hab) = c("sol_onf")


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
  mutate(ssf = map(dat.resample, ~ .x %>% steps_by_burst() %>% 
                     random_steps(n_control = 50) %>% 
                     extract_covariates(hab$sol_onf))) %>% 
  select(ssf, id) %>% unnest(cols = ssf) %>% 
  mutate(log_sl_ = log(sl_), sin_ta_ = sin(ta_),
         step_id1_ = paste(id, step_id_, sep = "."))%>% 
  filter(sl_ > 0) %>% 
  mutate(log_sl_ = log(sl_), sin_ta_ = sin(ta_)) %>% 
  na.omit(., cols=c("x2_","y2_")) %>% 
  st_as_sf(.,coords = c("x2_","y2_"), crs = 2971) %>% 
  mutate(dist_colony = st_distance(., colony) %>% 
           as.numeric())%>% 
  mutate(x1_ = st_coordinates(.)[, "X"],
         y1_ = st_coordinates(.)[, "Y"]) %>% 
  st_drop_geometry() %>% 
  subset(dist_colony > colony_radius) %>%
  subset(dist_colony < Inf) %>%
  mutate(log_sl_ = log(sl_), sin_ta_ = sin(ta_),
         step_id1_ = paste(id, step_id_, sep = ".")) 

trk = dat2 %>% 
  na.omit()

trk$day_night = trk$t1_ %>% 
  hour() %>% 
  cut(.,
      breaks=c(-0.1, 6, 19, 24),
      labels=c('Night', 'Day','Night'))

trk_day = trk %>% 
  subset(., day_night == "Day")

trk_night = trk %>% 
  subset(., day_night == "Night")

trk = trk_night




# iSSF for each individuals -----------------------------------------------

trk <- within(trk, sol_onf <- relevel(sol_onf, ref = "Marais_interieur"))


df = tibble()

for (cind in unique(trk$id)){
  
  m = trk %>% 
    subset(id == cind) %>% 
    fit_issf(., case_ ~ sol_onf  +  strata(step_id_), model = TRUE) 
  
  n = m$model %>% summary() 
  p = n$coefficients %>% 
    data.frame()%>% 
    mutate(id = cind)
  q = cbind(p, hab_)
  
  df = rbind(df, q)
}



df2 = cbind(df[,6], df[, 1:5] %>% round(digits = 2))

colnames(df2) = c("ID", "coef", "exp(coef)", "se(coef)", "z", "P-value")

df2
