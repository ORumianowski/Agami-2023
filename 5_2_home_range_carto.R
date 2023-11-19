

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
library(ggsn)
library(readxl)


# 1. Load data ----


bordereau <- read_excel("data/bordereau.xlsx")

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

colony_radius = 1000 # unit: m

dagami  = dagami %>% 
  mutate(ts = dmy_hms(paste(dagami$Date, dagami$Time))) %>% 
  mutate(ts = ts - hms("03:00:00")) %>% 
  rename(x_ = Longitude, y_ = Latitude) %>% 
  subset(., CRC == "OK") %>% 
  dplyr::select(id, x_, y_, ts, Fix) %>% 
  st_as_sf(.,coords = c("x_","y_"), crs = 4326) %>% 
  st_transform(., crs = 2971) %>% 
  mutate(dist_colony = st_distance(., colony))%>% 
  mutate(x_ = st_coordinates(.)[, "X"],
         y_ = st_coordinates(.)[, "Y"]) %>%
  st_drop_geometry() 

dagami2  = dagami %>% 
  mutate(dist_colony = as.numeric(dist_colony)) 


#   -----------------------------------------------------------------------


trk <- dagami2 %>% 
  make_track(x_, y_, ts,
             id = id, Fix = Fix,
             dist_colony = dist_colony,
             crs = 2971) %>% 
  tracked_from_to(from = as.POSIXct("2023-05-19 20:00"),
                  to = as.POSIXct("2024-10-01 08:00"))



# fond satellite ----------------------------------------------------------

df = read.csv('data/sat_colony_all.csv',sep=',', dec = ".") 

x_max = 428000
x_min = 357000

y_max = 530000
y_min = 468000


df_zoom = df %>% 
  subset(., x < x_max)%>% 
  subset(., x > x_min)%>% 
  subset(., y < y_max)%>% 
  subset(., y > y_min)

# production carte --------------------------------------------------------

# KDE ---------------------------------------------------------------------

m <- trk %>% nest(data = -id) %>% 
  mutate(kdes = map(data,  function(x) {hr_isopleths(hr_kde(x, levels = c(0.95)))})) %>% 
  dplyr::select(kdes, id)



iso_all = 
  rbind(
      subset(m, id == 42)$kdes[[1]] %>% mutate(id = 42),
      subset(m, id == 43)$kdes[[1]] %>% mutate(id = 43),
      subset(m, id == 44)$kdes[[1]] %>% mutate(id = 44),
      subset(m, id == 45)$kdes[[1]] %>% mutate(id = 45),
      subset(m, id == 46)$kdes[[1]] %>% mutate(id = 46),
      subset(m, id == 47)$kdes[[1]] %>% mutate(id = 47),
      subset(m, id == 48)$kdes[[1]] %>% mutate(id = 48),
      subset(m, id == 50)$kdes[[1]] %>% mutate(id = 50),
      subset(m, id == 52)$kdes[[1]] %>% mutate(id = 52),
      subset(m, id == 53)$kdes[[1]] %>% mutate(id = 53)) %>% 
  mutate(id = as.factor(id)) 



iso_all = merge(iso_all, bordereau, by = c("id")) %>% 
  dplyr::select(id, level, what, geometry, SEXE) %>% 
  rename(Individual = id)



ggplot() +
  geom_raster(data = df_zoom, aes(x = x, y =y),
              fill = rgb(r = df_zoom$Red,
                         g = df_zoom$Green,
                         b = df_zoom$Blue,
                         maxColorValue = 255)) +
  scale_fill_identity() +
  geom_sf(data = iso_all, aes(color = Individual),
          fill = NA, linewidth = 1, alpha = 0.9) +
  scale_color_brewer(name = "Individual", palette = "Spectral") +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  scalebar(iso_all, dist = 10, dist_unit = "km",
           transform = FALSE, model = "WGS84", st.size = 3.5) +
  north(iso_all)+
  geom_sf(data = colony,
          color = "yellow", size = 5, alpha = 1, shape = 18)

