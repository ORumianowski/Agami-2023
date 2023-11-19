

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
library(track2KBA)


# 1. Load data ----

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
  dplyr::select(id, x_, y_, ts, Fix) 


dagami  = dagami %>%
  st_as_sf(.,coords = c("x_","y_"), crs = 4326) %>% 
  st_transform(., crs = 2971) %>% 
  mutate(dist_colony = st_distance(., colony))%>% 
  mutate(x_ = st_coordinates(.)[, "X"],
         y_ = st_coordinates(.)[, "Y"]) %>%
  st_drop_geometry() %>% 
  mutate(dist_colony = as.numeric(dist_colony))

dagami_breed = dagami# %>% 
 # subset(., id == 42 | id == 45 |id == 46  |id == 52) %>% 
 #subset(dist_colony < 5000) 




#   -----------------------------------------------------------------------


trk <- dagami_breed %>% 
  make_track(x_, y_, ts,
             id = id,
             crs = 2971) %>% 
  tracked_from_to(from = as.POSIXct("2023-05-19 20:00"),
                  to = as.POSIXct("2024-10-01 08:00"))

# nest --------------------------------------------------------------------


dat2 <- trk |> nest(data = -c(id)) |> 
  mutate(
    data = map(data, ~ track_resample(.x, rate = hours(1), tolerance = minutes(5))), 
    n = map_int(data, nrow)) |> 
  mutate(
    hr.akde= map(data, hr_akde),
    hr.kde = map(data, hr_kde)
  )


dat3 <- dat2 |> 
  pivot_longer(hr.kde, names_to = "estimator", values_to = "hr") %>% 
  mutate(hr_kde = map_dbl(hr, ~ round(as.numeric(hr_area(.x)$area)/1000000, digits = 1))) %>% 
  dplyr::select(hr_kde, n)

dat4  = dat2 %>% 
  pivot_longer(hr.akde, names_to = "estimator", values_to = "hr") %>% 
  mutate(hr_akde_inf = map_dbl(hr, ~ round(as.numeric(hr_area(.x)$area[1])/1000000, digits = 1)),
         hr_akde_est = map_dbl(hr, ~ round(as.numeric(hr_area(.x)$area[2])/1000000, digits = 1)),
         hr_akde_sup = map_dbl(hr, ~ round(as.numeric(hr_area(.x)$area[3])/1000000, digits = 1)))%>% 
  dplyr::select(id, hr_akde_inf, hr_akde_est, hr_akde_sup) %>% 
  mutate(hr_akde_inf = hr_akde_inf,
         hr_akde_sup = hr_akde_sup)

res = cbind(dat4, dat3) 

res

ggplot()+
  geom_point(data = res,
             aes(x=id, y=hr_kde, color = 'red'))+#, group=ID, color=ID))+
  scale_color_manual(values=c('#999999'))+
  geom_pointrange(data = res,
                  aes(x=id,y=hr_akde_est, ymin=hr_akde_inf, ymax=hr_akde_sup))
  

