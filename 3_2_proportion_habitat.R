

#----------- Module 03 -- Home Range ------------X


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
library(tidyverse)
library(raster)    
library(here)    
library(prismatic)
library(ggmap)


# 1. Load data ----

# Habitat data 




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
  subset(., Date != "10/05/2023") # pre deployment test day

colony <- tibble(
  Longitude = -52.158637, 
  Latitude  = 4.644658 )%>% 
  st_as_sf(.,coords = c("Longitude","Latitude"), crs = 4326) %>% 
  st_transform(., crs = 2971)

rayon_colony = 1000 # unit: m

dagami  = dagami %>% 
  mutate(ts = dmy_hms(paste(dagami$Date, dagami$Time))) %>% 
  mutate(ts = ts - hms("03:00:00")) %>% 
  rename(x_ = Longitude, y_ = Latitude) %>% 
  subset(., CRC == "OK") %>% 
  dplyr::select(id, x_, y_, ts, Fix) %>% 
  st_as_sf(.,coords = c("x_","y_"), crs = 4326) %>% 
  st_transform(., crs = 2971) %>% 
  mutate(dist_colony = st_distance(., colony) %>% as.numeric())%>% 
  mutate(x_ = st_coordinates(.)[, "X"],
         y_ = st_coordinates(.)[, "Y"]) 



onf = st_read("ocs_onf_2015/V3/onfv3.shp") %>% 
  dplyr::select(NIVEAU3_15) %>% 
  mutate(NIVEAU3_15 = fct_collapse(NIVEAU3_15,
                                   Mangroves = c("318"),
                                   Marais_interieur = c("411"),
                                   Forets_inondables = c("317"), 
                                   Forets_terrestres = c("3151", 
                                                         "3152", "3161",
                                                         "3162", "3154"))) %>% 
  st_transform(., crs = st_crs(dagami))



dagami_at_colony = dagami %>% subset(., dist_colony < rayon_colony)
dagami_hors_colony = dagami %>% subset(., dist_colony > rayon_colony)


Mangroves_zone = subset(onf, NIVEAU3_15 == "Mangroves")
Marais_interieur_zone = subset(onf, NIVEAU3_15 == "Marais_interieur")
Forets_inondables_zone = subset(onf, NIVEAU3_15 == "Forets_inondables")
Forets_terrestres_zone = subset(onf, NIVEAU3_15 == "Forets_terrestres")


df = tibble(
  Colony_prop = nrow(dagami_at_colony) / nrow(dagami) , 
  Mangroves_prop = (st_within(dagami_hors_colony$geometry, st_make_valid(Mangroves_zone$geometry), sparse = FALSE) %>%
    sum())/nrow(dagami),
  Marais_interieur_prop = (st_within(dagami_hors_colony$geometry, st_make_valid(Marais_interieur_zone$geometry), sparse = FALSE)%>% 
    sum())/nrow(dagami),
  Forets_inondables_prop = (st_within(dagami_hors_colony$geometry, st_make_valid(Forets_inondables_zone$geometry), sparse = FALSE)%>% 
    sum())/nrow(dagami),
  Forets_terrestres_prop = (st_within(dagami_hors_colony$geometry, st_make_valid(Forets_terrestres_zone$geometry), sparse = FALSE)%>%
    sum())/nrow(dagami)
)

df

