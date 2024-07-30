### Data preparation CTD (small) ###
rm(list=ls())
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)

#### Import and wrangling of TOB files ----

# Save working directory for later
wd <- getwd()
getwd()

### Data preparation CTD ###
source("./R/get_TOB.R")                   # Funktion zum Einlesen der cleanen TOB Dateien laden
setwd("./data-raw/AL614_CTD_ascii")

files <- dir()                            # Vektor mit allen Dateiennamen wie sie im Ordner abgespeichert sind

it <- length(files)                       # Anzahl der Iterationen ist Anzeil der Dateien
file_list <- vector("list", length = it)  # In file_list werden die tibbles als Liste abgespeichert, hier noch eine leere Liste

for(i in 1:it) {
  file_list[[i]] <- get_TOB(files[i])     # get TOB Funktion für die i. Datei, Listen immer mit [[]]
}

ctd_tbl <- bind_rows(file_list)           # tibbles die in file_list gelistet sind werden zusammengefügt

ctd_tbl <- rename(ctd_tbl, Cruise = cruise,
                  Station = station, 
                  Haul = hol,
                  Date = date,
                  Time = datim,
                  Longitude = long,
                  Latitude = lat,
                  Press = press, 
                  Temp = temp, 
                  Cond = cond, 
                  SALIN = salin, 
                  SIGMA = sigma, 
                  AO2 = "ao2%", 
                  DO_mg = domg, 
                  ChlA = chla, 
                  pH = ph)

coordinates <- ctd_tbl %>%
  group_by(Station) %>%
  summarise(Longitude = first(Longitude),
            Latitude = first(Latitude), .groups = 'drop') %>%
  distinct()                                                          # erste koordinaten zum einfügen in beiden Datansätzen speichern

ctd_clean <- ctd_tbl %>% 
  mutate(.by = c(Cruise, Station), Press = round(Press * 2) / 2) %>%  # tiefenwerte auf nächstes 0.5 runden
  group_by(Cruise, Station, Press) %>%                                # innerhalb dieser Grupierung alle werte außer Date & Time (wird jeweils der erste Wert genommen) mitteln
  summarise( 
    across(-c(Date, Time, Longitude, Latitude), mean, na.mr = TRUE),  # calculate mean 
    Date = first(Date), # take first date
    Time = first(Time), # take first time
    .groups = "drop") %>%
  distinct(Cruise, Station, Press, .keep_all = TRUE) %>% #alle Duplikate aus dem Datensatz löschen
  mutate(Type = "CTD") # erfassungsform hinzufügen

ctd_clean <- ctd_clean %>%                                            # copy coordinates to second dataset
  left_join(coordinates, by = "Station")
  
ctd_clean <- select(ctd_clean, Cruise, Station, Haul, Type, Date, Time, Longitude, Latitude, Press, Temp, Cond, SALIN, SIGMA, AO2, DO_mg, ChlA, pH) # sort

setwd(wd)

### Data preparation WS ###
source("./R/get_WS_data.R")
source("./R/get_WS_data_adapt.R")

setwd("./data-raw/AL614_CTD_WS")

files <- dir()                                  # Vektor mit allen Dateiennamen wie sie im Ordner abgespeichert sind

it <- length(files)                             # Anzahl der Iterationen ist Anzeil der Dateien
file_list <- vector("list", length = it)        # in file_list werden die tibbles als Liste abgespeichert, hier noch eine leere Liste

for(i in c(2:7, 9:it)) {
  file_list[[i]] <- get_WS_data(files[i])       # get WS Funktion für die i. Datei, Listen immer mi [[]]
}

for(i in c(1,8)) {
  file_list[[i]] <- get_WS_data_adapt(files[i]) # get WS Funktion für die i. Datei, Listen immer mi [[]]
}

ws_tbl <- bind_rows(file_list)                  # tibbles die in file_list gelistet sind werden zusammengefügt

ws_tbl <- rename(ws_tbl, Cruise = cruise,
                  Station = station, 
                  Haul = hol,
                  Date = date,
                  Time = time,
                  Press = pressure_dbar, 
                  Temp = temperature_c, 
                  Cond = conductivity_mscm, 
                  SALIN = salinity_psu,
                  AO2 = oxygen_as, 
                  DO_mg = oxygen_mgl, 
                  ChlA = chlorophyll_agl)

ws_clean <- ws_tbl %>% 
  mutate(.by = c(Cruise, Station), Press = round(Press * 2) / 2) %>%  # tiefenwerte auf nächstes 0.5 runden
  group_by(Cruise, Station, Press) %>%                                # innerhalb dieser Gruppierung alle werte außer Date & Time (wird jeweils der erste Wert genommen) mitteln
  summarise( 
    across(-c(Date, Time), mean, na.mr = TRUE),
    Date = first(Date),
    Time = first(Time), .groups = "drop") %>%
  distinct(Cruise, Station, Press, .keep_all = TRUE) %>%              # alle Duplikate aus dem Datensatz löschen
  mutate(Type = "WS")                                                 # erfassungs vrom hinzufügen

ws_clean <- ws_clean %>%                                              # copy coordinates to second dataset
  left_join(coordinates, by = "Station")

ws_clean <- select(ws_clean, Cruise, Station, Haul, Type, Date, Time, Longitude, Latitude, Press, Temp, Cond, SALIN, AO2, DO_mg, ChlA) # sort

setwd(wd)

### Einzelspeichern ###
save(ctd_clean, file = "./data/CTD.Rdata")
#write.csv(ctd_clean, file = "./data/CTD.csv", row.names = FALSE)
save(ws_clean, file = "./data/WS.Rdata")
#write.csv(ws_clean, file = "./data/WS.csv", row.names = FALSE)

### Zusammenführen & Speichern###
data_full <- bind_rows(ctd_clean, ws_clean)

save(data_full, file = "./data/Full.Rdata")
#write.csv(data_full, file = "./data/Full.csv")

### Data preparation Nutrients ###
chemical <- read.csv2("./data-raw/chemical.csv")

chemical <- chemical %>%                                                      # copy coordinates to third dataset
  left_join(coordinates, by = "Station")

chemical <- rename(chemical, Press = Depth_m)

save(chemical, file = "./data/Chemical.Rdata")
