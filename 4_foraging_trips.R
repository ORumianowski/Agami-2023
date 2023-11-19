
#-------- Foraging trips Agamia agami ------------X


# Load packages ----
library(tidyverse)
library(amt)
library(lubridate)
library(sf)
library(track2KBA)

# sur csv: suppression de des noms de colonnes vides

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
  subset(., Date != "10/05/2023")

dagami$ts = paste(dagami$Date, dagami$Time) %>%
  dmy_hms()

dagami$ts = dagami$ts - hms("03:00:00")

dagami2 = dagami %>% 
  subset(., CRC == "OK") %>% 
  rename(x_ = Longitude,
         y_ = Latitude)

dataGroup <- formatFields(
  dataGroup = dagami2, 
  fieldID   = "id", 
  fieldDateTime = "ts",
  fieldLon  = "x_", 
  fieldLat  = "y_"
)


colony <- dataGroup %>% 
  summarise(
    Longitude = -52.158637, 
    Latitude  = 4.644658
  )


trips <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = 1,      # kilometers
  returnBuff = 3,
  duration   = 0.5,      # hours
  rmNonTrip  = TRUE
)


trips <- subset(trips, trips$Returns == "Yes" )

sumTrips <- tripSummary(trips = trips, colony = colony)

res = sumTrips %>% 
  dplyr::select(ID, duration, max_dist, n_locs) %>% 
  mutate(duration = duration%>% round(digits = 2))%>% 
  mutate(max_dist = max_dist%>% round(digits = 2)) %>% 
  mutate(n_locs = n_locs %>% floor() %>% as.character()) 


res = rbind(res,
            tibble( ID = "Median",
                    duration = median(res$duration),
                    max_dist = median(res$max_dist), 
                    n_locs = ""))


colnames(res) = c("ID", "Duration (hour)", "Dist.Max (km)", "N.Locs")



library(kableExtra)

kbl(res) %>%
  kable_paper("striped", full_width = F)  %>%
  row_spec(nrow(res), bold = T) 

