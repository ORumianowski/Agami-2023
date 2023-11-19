
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

# Habitat data 

onf = st_read("ocs_onf_2015/oc_sol_2015.shp") %>% 
  dplyr::select(NIVEAU3_15) %>% 
  mutate(NIVEAU3_15 = fct_collapse(NIVEAU3_15,
                                   Mangroves = c("318"),
                                   Marais_interieur = c("411"),
                                   Forets_inondables = c("317"), 
                                   Forets_terrestres = c("3151", "3152", "3161", "3162", "3154"))) %>% 
  subset(., NIVEAU3_15 == "Marais_interieur" | 
           NIVEAU3_15 == "Forets_inondables" )

cycle_hire_osm = onf
cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:2971")
raster_template = rast(ext(cycle_hire_osm_projected), resolution = 100,
                       crs = st_crs(cycle_hire_osm_projected)$wkt)

hab = rasterize(cycle_hire_osm_projected, raster_template, field = "NIVEAU3_15",
                fun = "max")

names(hab) = c("sol_onf")

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

#   -----------------------------------------------------------------------

trk <- dagami %>% 
  make_track(x_, y_, ts,
             id = id, Fix = Fix,
             crs = 2971) %>% 
  tracked_from_to(from = as.POSIXct("2023-05-19 20:00"),
                  to = as.POSIXct("2024-10-01 08:00"))


dat1 <- trk %>% nest(data = -id)

dat2 <- dat1 %>% 
  mutate(dat.resample = map(data, ~ track_resample(., rate = minutes(60), tolerance = minutes(10))))

dat.ssf <- dat2 %>% 
  mutate(ssf = map(dat.resample, ~ .x %>% steps_by_burst())) 

dat.ssf2 <- dat.ssf %>% dplyr::select(ssf, id) %>% unnest(cols = ssf) %>% 
  mutate(log_sl_ = log(sl_), sin_ta_ = sin(ta_))

n_sl = c()
mean_sl = c()
sd_sl = c()
lower_sl = c()
upper_sl = c()


for (h in 0:23){
  
  sl_h = dat.ssf2 %>% subset(.,hour(t1_) == h) %>% pull(sl_) 
  
  n_sl_h = length(sl_h)
  mean_sl_h = mean(sl_h)
  sd_sl_h = sd(sl_h)
  lower_sl_h = t.test(sl_h)["conf.int"][[1]][1]
  upper_sl_h = t.test(sl_h)["conf.int"][[1]][2]
    
    n_sl = c(n_sl, n_sl_h)
    mean_sl = c(mean_sl, mean_sl_h)
    sd_sl = c(sd_sl, sd_sl_h)
    lower_sl = c(lower_sl, lower_sl_h)
    upper_sl = c(upper_sl, upper_sl_h)}


df = tibble(time = 0:23) %>% 
  mutate(N = n_sl,
         Mean = mean_sl,
         SD = sd_sl,
         Lower95 = lower_sl,
         Upper95 = upper_sl
         )


p = ggplot() + 
  geom_rect(data = df, aes(xmin = 0, xmax = 0, ymin = -Inf, ymax = Inf))+
  geom_rect(aes(xmin = 18.5, xmax = 23, ymin = -Inf, ymax = Inf, alpha = 0.1))+
  geom_rect(aes(xmin = 0, xmax = 5.5, ymin = -Inf, ymax = Inf, alpha = 0.1))+
  geom_pointrange(data = df, aes(time, Mean, ymin = Lower95, ymax = Upper95))+
  geom_vline(xintercept = c(5.5, 18.5), linetype="dashed", 
             linewidth=0.8)+
  xlab("Time (hour)") + ylab("Mean step length (meter)") +
  xlim(0, 23)+
  theme(legend.position='none', text = element_text(size = 20))  
p



# GLMM --------------------------------------------------------------------


dat.ssf2$day_night = dat.ssf2$t1_ %>% 
  hour() %>% 
  cut(.,
      breaks=c(0, 6.25, 18.75, 24),
      labels=c('Night', 'Day','Night'))

mod <- lm(sl_ ~ day_night, data = dat.ssf2)
res = mod %>% summary() %>% broom::tidy()


kableExtra::kbl(res) %>%
  kableExtra::kable_paper("striped", full_width = F) 
