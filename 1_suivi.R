
# Load packages ----
library(tidyverse)
library(mvtnorm)
library(amt)
library(purrr)
library(lubridate)
library(sf)
library(terra)
library(broom)
library(here)    
library(prismatic)
library(readxl)


# 1. Load data ----

# data agami
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

# coordonnees de la colonie
colony <- tibble(
  Longitude = -52.158637, 
  Latitude  = 4.644658 )%>% 
  st_as_sf(.,coords = c("Longitude","Latitude"), crs = 4326) %>% 
  st_transform(., crs = 2971)

# rayon estime de la colonie
rayon_colony = 1000 # unit: meter

# Données de bordereau

bordereau <- read_excel("data/bordereau.xlsx")

# formatage
dagami  = dagami %>% 
  mutate(ts = dmy_hms(paste(dagami$Date, dagami$Time))) %>% 
  mutate(ts = ts - hms("03:00:00")) %>% 
  rename(x_ = Longitude, y_ = Latitude) %>% 
  subset(., CRC == "OK") %>% 
  dplyr::select(id, x_, y_, ts, Fix) 

dagami  = dagami %>% 
  st_as_sf(.,coords = c("x_","y_"), crs = 4326) %>% 
  st_transform(., crs = 2971) %>% 
  mutate(dist_colony = st_distance(., colony))%>% 
  mutate(x_ = st_coordinates(.)[, "X"],
         y_ = st_coordinates(.)[, "Y"]) %>%
  st_drop_geometry() %>% 
  mutate(dist_colony = as.numeric(dist_colony))



# Filtres -----------------------------------------------------------------

dagami = dagami 


# SUIVI -------------------------------------------------------------------


suivi = dagami %>% 
  nest(data = -c(id)) %>% 
  mutate(min = map(data, ~ .x %>% pull(ts) %>% min())) %>% 
  mutate(max = map(data, ~ .x %>% pull(ts) %>% max())) %>% 
  unnest(cols = c(min, max)) %>% 
  mutate(duration = as.numeric(max - min) %>% round(., digits = 2)) %>% 
  dplyr::select(id, duration) %>% 
  mutate(fixes = table(dagami$id) %>% as.integer())

suivi2 = merge(suivi, bordereau, by = c("id")) %>% 
  rename(date_equip = Date) %>% 
  dplyr::select(id, SEXE, duration, fixes, date_equip) %>% 
  mutate(date_emession = "2023-05-20")


colnames(suivi2) = c("Individual", "Sex", "Monitoring Duration (day)",  "Number of Locations",
                     "Date of Equipment", "Monitoring Start Date ")

# plot --------------------------------------------------------------------


library(kableExtra)

kbl(suivi2) %>%
  kable_paper("striped", full_width = F) 



#   -----------------------------------------------------------------------



trk <- dagami %>% 
  make_track(x_, y_, ts,
             id = id, Fix = Fix,
             crs = 2971) %>% 
  tracked_from_to(from = as.POSIXct("2023-05-19 20:00"),
                  to = as.POSIXct("2024-10-01 08:00"))


dat1 <- trk %>% nest(data = -id)

dat2 <- dat1 %>% 
  mutate(dat.resample = map(data, ~ track_resample(., rate = minutes(60), tolerance = minutes(10)))) %>% 
  mutate(ssf = map(dat.resample, ~ .x %>% steps_by_burst())) 

dat.ssf2 <- dat2 %>% dplyr::select(ssf, id) %>% unnest(cols = ssf) %>% 
  mutate(log_sl_ = log(sl_), sin_ta_ = sin(ta_))



# nombre de step valides
nrow(dat.ssf2)
