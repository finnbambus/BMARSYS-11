# Load Packages
library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(gstat)
library(tidyterra)
library(ggplot2)
library(ggspatial)
library(gridExtra)
library(grid)

# 1. Load Data
load("./data/full.Rdata")

wbcountries_sf <- st_read("./data/vector_data.gpkg", layer = "wbcountries")
helcom_areas_sf <- st_read("./data/vector_data.gpkg", layer = "helcom_areas")

## 1.2 Calculate mean for each station for each value

mean_station <- data_full %>%
  filter(Press >= 0 & Press <= 5) %>%
  summarise(Longitude = first(Longitude),
            Latitude = first(Latitude),
            Temp = mean(Temp),
            SALIN = mean(SALIN),
            AO2 = mean(AO2),
            ChlA = mean(ChlA),
            pH = mean(pH, na.rm = TRUE), .by = Station)

mean_station <- na.omit(mean_station)

mean_station <- st_as_sf(
  x = mean_station,
  coords = c ("Longitude", "Latitude"),
  crs = 4326)

## 1.3 Interpolation map function
 interpolate_and_plot_nn <- function(data, variable, variable_name, unit, gradient) {

grid <- rast(helcom_areas_sf,
             nrows  = 500, ncols = 500)

grid_sf <- st_as_sf(as.data.frame(crds(grid)),
                    coords = c("x", "y"),
                    crs = st_crs(data))

mod_nn <- gstat::gstat(
  formula = variable ~ 1, 
  locations = station_sf, 
  nmax = 5,
  set = list(idp = 0))

resp_nn <- predict(mod_nn, grid_sf)

resp_nn$x <- st_coordinates(resp_nn)[,1]
resp_nn$y <- st_coordinates(resp_nn)[,2]
resp_nn$pred <- resp_nn$var1.pred

pred_nn <- terra::rasterize(resp_nn, grid, field = "pred", fun = "mean")

ggplot() +
  geom_spatraster(data = pred_nn, aes(fill = mean)) +
  scale_fill_viridis_c(direction = 1, na.value = NA, name = unit, option = gradient) +
  geom_sf(data = wbcountries_sf) +
  geom_sf(data = data) +
  labs(title = paste("Interpolated", variable_name)) +
  ggspatial::annotation_north_arrow(
    location = "tl", 
    style = ggspatial::north_arrow_minimal) +
  ggspatial::annotation_scale(
    location = "bl") +
  coord_sf(
    xlim = c(10, 14.2),
    ylim = c(54, 55.2))
 }
 
 interpolate_and_plot_idw <- function(data, variable, variable_name, unit, gradient) {
   
   # Create the grid
   grid <- rast(helcom_areas_sf, nrows = 500, ncols = 500)
   grid_sf <- st_as_sf(as.data.frame(crds(grid)), coords = c("x", "y"), crs = st_crs(data))
   
   # IDW interpolation
   formula <- as.formula(paste(variable, "~ 1"))
   mod_idw <- gstat::gstat(
     formula = formula, 
     locations = data, 
     set = list(idp = 2))
   
   resp_idw <- predict(mod_idw, grid_sf)
   resp_idw$x <- st_coordinates(resp_idw)[,1]
   resp_idw$y <- st_coordinates(resp_idw)[,2]
   resp_idw$pred <- resp_idw$var1.pred
   
   pred_idw <- terra::rasterize(resp_idw, grid, field = "pred", fun = "mean")
   
   # Plotting
   ggplot() +
     geom_spatraster(data = pred_idw, aes(fill = mean)) +
     scale_fill_viridis_c(direction = 1, na.value = NA, name = unit, option = gradient) +
     geom_sf(data = wbcountries_sf) +
     geom_sf(data = data) +
     labs(title = paste("Interpolated", variable_name)) +
     ggspatial::annotation_north_arrow(location = "tl", style = ggspatial::north_arrow_minimal) +
     ggspatial::annotation_scale(location = "bl") +
     coord_sf(xlim = c(10, 14.2), ylim = c(54, 55.2))
 }

## 1.4 Plot each Variable
plot_temp <- interpolate_and_plot_idw(mean_station, "Temp", "Temperature", "°C", "viridis")
plot_salin <- interpolate_and_plot_idw(mean_station, "SALIN", "Salinity", "Salinity", "viridis")
plot_pH <- interpolate_and_plot_idw(mean_station, "pH", "Acidity", "pH", "viridis")

grid.arrange(plot_salin, plot_temp, plot_pH, nrow = 3)

# 2. Load chemical data
load("./data/Chemical.Rdata")

chemical <- chemical %>% 
  mutate(.by = c(Cruise, Station), Press = round(Press * 2) / 2)

chemical <- chemical[!is.na(chemical$Longitude),]

chemical <- st_as_sf(
  x = chemical,
  coords = c ("Longitude", "Latitude"),
  crs = 4326)

nutrients <- chemical %>%
  filter(Chl.a.max == "TRUE")

nutrients <- na.omit(nutrients)

nutrients <- st_as_sf(
  x = nutrients,
  coords = c ("Longitude", "Latitude"),
  crs = 4326)

## 2.2 Plot each variable
plot_nitrate <- interpolate_and_plot_idw(nutrients, "Nitrate", "Nitrate", "µmol/L", "magma")
plot_nitrate
plot_nitrite <- interpolate_and_plot_idw(nutrients, "Nitrite", "Nitrite", "µmol/L", "magma")
plot_nitrite
plot_phosphate <- interpolate_and_plot_idw(nutrients, "Phosphate", "Phosphate", "µmol/L", "magma")
plot_phosphate
plot_silicate <- interpolate_and_plot_idw(nutrients, "Silicate", "Silicate", "µmol/L", "magma")
plot_silicate

grid.arrange(plot_nitrate, plot_nitrite, plot_phosphate, plot_silicate, nrow = 2)

# 3. Compare O2 and Chl-a
## 3.1 filter chlorophyll data
chloro <- data_full %>%
  semi_join(nutrients, by = c("Station", "Press"))

chloro <- select(chloro, Cruise, Station, Haul, Type, Press, ChlA, Longitude, Latitude)

chloro <- st_as_sf(
  x = chloro,
  coords = c ("Longitude", "Latitude"),
  crs = 4326)

##3.2 filter oxygen data
oxygen <- data_full %>%
  semi_join(chemical, by = c("Station", "Press"))

oxygen <- select(oxygen, Cruise, Station, Haul, Type, Press, DO_mg, Longitude, Latitude)
  
oxygen <- st_as_sf(
  x = oxygen,
  coords = c ("Longitude", "Latitude"),
  crs = 4326)

## 3.3 Plot each chlorophyll variant
plot_chloro_chem <- interpolate_and_plot_idw(nutrients, "ChlA", "Chlorophyll Chem.", "mg/L", "rocket")
plot_chloro_chem
plot_chloro_ctd <- interpolate_and_plot_idw(chloro[chloro$Type == "CTD", ], "ChlA", "Chlorophyll CTD", "mg/L", "rocket")
plot_chloro_ctd
plot_chloro_ws <- interpolate_and_plot_idw(chloro[chloro$Type == "WS", ], "ChlA", "Chlorophyll WS", "mg/L", "rocket")
plot_chloro_ws

grid.arrange(plot_chloro_chem, plot_chloro_ctd, plot_chloro_ws, nrow = 3)

## 3.4 Plot each oxygen variant
plot_oxygen_chem <- interpolate_and_plot_idw(chemical, "O2_mg.L", "Oxygen Chem.", "mg/L", "rocket")
plot_oxygen_chem
plot_oxygen_ctd <- interpolate_and_plot_idw(oxygen[oxygen$Type == "CTD", ], "DO_mg", "Oxygen CTD", "mg/L", "rocket")
plot_oxygen_ctd
plot_oxygen_ws <- interpolate_and_plot_idw(oxygen[oxygen$Type == "WS", ], "DO_mg", "Oxygen WS", "mg/L", "rocket")
plot_oxygen_ws

grid.arrange(plot_oxygen_chem, plot_oxygen_ctd, plot_oxygen_ws, nrow = 3)
