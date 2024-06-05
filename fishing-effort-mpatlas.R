################################################################################

# Contribution of marine protected areas to abate industrial fishing effort within and nearby their boundaries
# Millenne Ohanna S. M. S. Barreto, Juliette Jacquemont and Joachim Claudet

################################################################################

# Loading packages

library(tidyverse)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(sp)
library(data.table)
library(ggplot2)
library(gridExtra)
library(ggspatial)
library(ggExtra)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)

################################################################################

#### 1. Processing MPAs ####

MPAs_shp <- st_read(dsn = "./Data/WDPA/March_24_zoneassessments_full.gdb",
                    layer = "zoneassessment_export_geom_view")
MPAs_shp <- subset(MPAs_shp, MPAs_shp$implemented_date >= "2013-01-01 01:00:00" & MPAs_shp$implemented_date <= "2019-10-01 02:00:00")
MPAs_shp <- MPAs_shp[MPAs_shp$establishment_stage %in% c("actively managed", "implemented"), ]
MPAs_shp$AREA_KM2 <- st_area(MPAs_shp) / 10^6
MPAs_shp$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_shp$AREA_KM2))
MPAs_shp$AREA_KM2[c(1, 29)] <- 108685.3
MPAs_shp$AREA_KM2[c(113, 115)] <- 1531744
MPAs_shp$AREA_KM2[c(114, 116)] <- 18688.55
#st_write(MPAs_shp, "./Data/WDPA/MPAs_guide.shp", append = FALSE)

MPAs_buffer <- st_buffer(MPAs_shp, dist = 10000)
MPAs_buffer <- st_difference(MPAs_buffer, MPAs_shp)
which(!st_is_valid(MPAs_buffer))
MPAs_buffer$AREA_KM2 <- st_area(MPAs_buffer) / 10^6
MPAs_buffer$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_buffer$AREA_KM2))
#MPAs_buffer$AREA_KM2[c(1, 29)] <- 3744.573 # For 10km
#MPAs_buffer$AREA_KM2[c(113, 115)] <- 9305.796 # For 10km
#MPAs_buffer$AREA_KM2[c(114, 116)] <- 191274 # For 10km
#MPAs_buffer$AREA_KM2[c(1, 29)] <- 6697.71 # For 20km
#MPAs_buffer$AREA_KM2[c(113, 115)] <- 17023.14 # For 20km
#MPAs_buffer$AREA_KM2[c(114, 116)] <- 241286.5 # For 20km
#MPAs_buffer$AREA_KM2[c(1, 29)] <- 144605.739 # For 30km
#MPAs_buffer$AREA_KM2[c(113, 115)] <- 1871159 # For 30km
#MPAs_buffer$AREA_KM2[c(114, 116)] <- 30975.26 # For 30km
#MPAs_buffer$AREA_KM2[c(1, 29)] <- 15652.46 # For 50km
#MPAs_buffer$AREA_KM2[c(113, 115)] <- 481024.3 # For 50km
#MPAs_buffer$AREA_KM2[c(114, 116)] <- 10829.4 # For 50km
#st_write(MPAs_buffer, "./Data/WDPA/MPAs_guide_buffer_10km.shp", append = F)

##### 1.1. Importing files #####

MPAs_shp <- vect("./Data/WDPA/MPAs_guide.shp")
MPAs_shp <- st_as_sf(MPAs_shp)
MPAs_shp <- MPAs_shp[-c(7,60,64,68),]
st_is_valid(MPAs_shp)

MPAs_buffer <- vect("./Data/WDPA/MPAs_guide_buffer_10km.shp")
MPAs_buffer <- st_as_sf(MPAs_buffer)
MPAs_buffer <- MPAs_buffer[-c(7,60,64,68),]
st_is_valid(MPAs_buffer)

##### 1.2. Plotting world map and MPAs #####

#MPAs_shp$protection[MPAs_shp$protection %in% c("light", "minimal")] <- "light_minimal"

# Loading world and oceans' data
land <- ne_countries(scale = "medium", returnclass = "sf")

ocean <- ne_download(scale = 110, category = "physical", type = "ocean", returnclass = "sf")

# Creating shapefiles for zoom
zoom_mex <- data.frame(x = c(-125, -85, -80, -118, -125), y = c(25, 25, 7, 7, 25))
zoom_mex <- st_sf(geometry = st_sfc(st_polygon(list(cbind(zoom_mex$x, zoom_mex$y)))), crs = 4326)

zoom_atl <- data.frame(x = c(-46, -13, -18, -62, -46), y = c(-46, -46, -64, -64, -46))
zoom_atl <- st_sf(geometry = st_sfc(st_polygon(list(cbind(zoom_atl$x, zoom_atl$y)))), crs = 4326)

zoom_ind <- data.frame(x = c(26, 82, 104, 33, 26), y = c(-32, -32, -55, -55, -32))
zoom_ind <- st_sf(geometry = st_sfc(st_polygon(list(cbind(zoom_ind$x, zoom_ind$y)))), crs = 4326)

zoom_aus <- data.frame(x = c(110, 159, 177, 117, 110), y = c(-9, -9, -48, -48, -9))
zoom_aus <- st_sf(geometry = st_sfc(st_polygon(list(cbind(zoom_aus$x, zoom_aus$y)))), crs = 4326)

# Plotting MPAs' map
ggplot() +
  geom_sf(data = land, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_shp, aes(fill = protection, size = 0.2)) +
  scale_fill_manual(values = c("full" = "#0868ac", "high" = "#56b0c8",
                             "light_minimal" = "#a5dcbf", "incompatible" = "#f0f9e8"),
                    name = "Level of protection",
                    labels = c("Full", "High", "Light & Minimal", "Incompatible")) +
  
  # Adding squares for zoom
  geom_sf(data = zoom_mex, fill = "transparent", color = "black") +
  geom_sf(data = zoom_atl, fill = "transparent", color = "black") +
  geom_sf(data = zoom_ind, fill = "transparent", color = "black") +
  geom_sf(data = zoom_aus, fill = "transparent", color = "black") +
  
  coord_sf(crs = "+proj=moll") +
  theme_void() +
  theme(legend.position = "none")

# Plotting Antarctica's view
ggplot() +
  geom_sf(data = land, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_shp, aes(fill = protection, size = 0.2)) +
  scale_fill_manual(values = c("full" = "#0868ac", "high" = "#56b0c8",
                               "light_minimal" = "#a5dcbf", "incompatible" = "#f0f9e8"),
                    name = "Level of protection",
                    labels = c("Full", "High", "Light & Minimal", "Incompatible")) +
  coord_sf(crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +datum=WGS84 +units=m +no_defs", 
           xlim = c(-2500000, 2500000), ylim = c(-2700000, 2200000)) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Mexico
map_mex <- st_crop(land, xmin = -85, xmax = -118, ymin = 7, ymax = 25)

ggplot() +
  geom_sf(data = map_mex, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_shp, aes(fill = protection, size = 0.2)) +
  scale_fill_manual(values = c("full" = "#0868ac", "high" = "#56b0c8",
                               "light_minimal" = "#a5dcbf", "incompatible" = "#f0f9e8")) +
  coord_sf(xlim = c(-85, -118), ylim = c(7, 25)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in the Atlantic
map_atl <- st_crop(land, xmin = -46, xmax = -19, ymin = -64, ymax = -50)

ggplot() +
  geom_sf(data = map_atl, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_shp, aes(fill = protection, size = 0.2)) +
  scale_fill_manual(values = c("full" = "#0868ac", "high" = "#56b0c8",
                               "light_minimal" = "#a5dcbf", "incompatible" = "#f0f9e8")) +
  coord_sf(xlim = c(-46, -19), ylim = c(-64, -50)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in the Indian
map_ind <- st_crop(land, xmin = 30, xmax = 85, ymin = -55, ymax = -34)

ggplot() +
  geom_sf(data = map_ind, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_shp, aes(fill = protection, size = 0.2)) +
  scale_fill_manual(values = c("full" = "#0868ac", "high" = "#56b0c8",
                               "light_minimal" = "#a5dcbf", "incompatible" = "#f0f9e8")) +
  coord_sf(xlim = c(30, 85), ylim = c(-55, -34)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Australia
map_aus <- st_crop(land, xmin = 110, xmax = 160, ymin = -48, ymax = -9)

ggplot() +
  geom_sf(data = map_aus, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_shp, aes(fill = protection, size = 0.2)) +
  scale_fill_manual(values = c("full" = "#0868ac", "high" = "#56b0c8",
                               "light_minimal" = "#a5dcbf", "incompatible" = "#f0f9e8")) +
  coord_sf(xlim = c(110, 160), ylim = c(-48, -9)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

##### 1.3. Histograms #####

# Histogram of protection level
#MPAs_shp$protection[MPAs_shp$protection %in% c("light", "minimal")] <- "light_minimal"
#MPAs_shp$protection <- factor(MPAs_shp$protection, levels = c("full", "high", "light_minimal", "incompatible"))
hist_protection <- ggplot() +
  geom_bar(data = MPAs_shp, aes(x = protection, fill = protection)) +
  scale_fill_manual(values=c("full"="#0868ac","high"="#56b0c8",
                             "light_minimal"="#a5dcbf","incompatible"="#f0f9e8")) +
  scale_x_discrete(labels=c("full" = "Full", "high" = "High", 
                            "light_minimal" = "Light &\nMinimal", "incompatible" = "Incompatible")) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  #theme(panel.grid.major = element_line(color = "#D3D3D3")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#D3D3D3"),
        axis.line = element_line(color = "#D3D3D3"),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 65)) +
  theme(legend.position = "none")
print(hist_protection)

# Histogram of size distribution
#MPAs_shp$protection[MPAs_shp$protection %in% c("light", "minimal")] <- "light_minimal"
#MPAs_shp$protection <- factor(MPAs_shp$protection, levels = rev(c("full", "high", "light_minimal", "incompatible")))
hist_size <- ggplot(MPAs_shp, aes(x = AREA_KM2, fill = protection)) +
  geom_histogram(position = "stack") +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = function(x) ifelse(x == 0, "0", paste(format(x / 1000, scientific = FALSE), ".10³", sep = ""))) +
  scale_fill_manual(values=c("full"="#0868ac","high"="#56b0c8",
                             "light_minimal"="#a5dcbf","incompatible"="#f0f9e8")) +
  theme_minimal() +
  #theme(panel.grid.major = element_line(color = "#D3D3D3")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#D3D3D3"),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 75)) +
  theme(legend.position = "none")
print(hist_size)

# Histogram of implementation year
#MPAs_shp$protection[MPAs_shp$protection %in% c("light", "minimal")] <- "light_minimal"
#MPAs_shp$protection <- factor(MPAs_shp$protection, levels = rev(c("full", "high", "light_minimal", "incompatible")))
hist_year <- ggplot(data = MPAs_shp, aes(x = as.numeric(substr(MPAs_shp$implemente, 1,4)), fill = protection)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values=c("full"="#0868ac","high"="#56b0c8",
                             "light_minimal"="#a5dcbf","incompatible"="#f0f9e8")) +
  labs(x = NULL, y = NULL, fill = "Level of protection") +
  theme_minimal() +
  #theme(panel.grid.major = element_line(color = "#D3D3D3")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#D3D3D3"),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 60)) +
  theme(legend.position = "none")
print(hist_year)

grid.arrange(hist_protection, hist_size, ncol = 2, heights = c(25, 5))
grid.arrange(hist_protection, hist_year, ncol = 2, heights = c(25, 5))


################################################################################

#### 2. Processing fishing effort data per year ####

##### 2.1. Importing files #####

r_fishing_2012 <- rast("./Data/GFW/r_fishing_2012.tif")
r_fishing_2013 <- rast("./Data/GFW/r_fishing_2013.tif")
r_fishing_2014 <- rast("./Data/GFW/r_fishing_2014.tif")
r_fishing_2015 <- rast("./Data/GFW/r_fishing_2015.tif")
r_fishing_2016 <- rast("./Data/GFW/r_fishing_2016.tif")
r_fishing_2017 <- rast("./Data/GFW/r_fishing_2017.tif")
r_fishing_2018 <- rast("./Data/GFW/r_fishing_2018.tif")
r_fishing_2019 <- rast("./Data/GFW/r_fishing_2019.tif")
r_fishing_2020 <- rast("./Data/GFW/r_fishing_2020.tif")

##### 2.2. For 2020 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2020, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2020) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2020.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2020, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                         MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2020) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2020.csv")

##### 2.3. For 2019 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2019, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2019) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2019.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2019, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                            by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2019) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2019.csv")

##### 2.4. For 2018 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2018, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2018) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2018.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2018, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                            by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2018) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2018.csv")

##### 2.5. For 2017 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2017, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2017) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2017.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2017, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                            by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2017) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2017.csv")

##### 2.6. For 2016 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2016, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2016) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2016.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2016, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                            by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2016) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2016.csv")

##### 2.7. For 2015 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2015, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2015) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2015.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2015, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                            by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2015) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2015.csv")

##### 2.8. For 2014 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2014, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2014) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2014.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2014, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                            by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2014) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2014.csv")

##### 2.9. For 2013 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2013, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2013) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2013.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2013, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                            by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2013) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2013.csv")

##### 2.10. For 2012 #####

## Joining with mpas
fishing_mpa <- extract(r_fishing_2012, vect(MPAs_shp), fun = sum, na.rm = T)
fishing_mpa <- data.frame(OBJECTID = MPAs_shp$OBJECTID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_shp %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                         by = "OBJECTID") %>%
  mutate(TYPE = "MPA", YEAR = 2012) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_mpa,"./Data/Results/MPA_guide/summarized_fishing_mpa_2012.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2012, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(OBJECTID = MPAs_buffer$OBJECTID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(OBJECTID, wdpa_id, wdpa_pid, name, country, protection, implemente, AREA_KM2), 
                            by = "OBJECTID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2012) %>%
  select(TYPE, YEAR, protection, OBJECTID, wdpa_id, wdpa_pid, name, country, implemente, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 2 & row_number() != 30 & row_number() != 112 &
           row_number() != 114 & row_number() != 116 & row_number() != 118) %>%
#  filter(row_number() != 2 & row_number() != 31 & row_number() != 116 &
#           row_number() != 118 & row_number() != 120 & row_number() != 122) %>%
  rename(PROTECTION = protection, WDPA_ID = wdpa_id, WDPA_PID = wdpa_pid, NAME = name, COUNTRY = country, IMPLEMENTATION = implemente)

write_csv(fishing_buffer,"./Data/Results/MPA_guide/summarized_fishing_buffer_2012.csv")

################################################################################

##### 3. Analysis ####

files <-  list.files(path="./Data/Results/MPA_guide",pattern="*.csv")
myfiles <-  lapply(paste("./Data/Results/MPA_guide/",files,sep=""), read_csv)
total_fishing_pressure <- rbindlist(myfiles)
total_fishing_pressure$IMPLEMENTATION <- as.numeric(substr(total_fishing_pressure$IMPLEMENTATION, 1,4))
rm(files, myfiles)

# Creating the dataset with total fishing effort values for every year within and outside MPAs
fishing_pressure <- function(data) {
  
  fishing_pressure_before <- data %>%
    filter(YEAR < IMPLEMENTATION) %>%
    group_by(TYPE, PROTECTION, OBJECTID, WDPA_ID, WDPA_PID, NAME, COUNTRY, IMPLEMENTATION, AREA_KM2) %>%
    summarize(fishing_pressure_before = mean(fishing_pressure, na.rm = TRUE))
  
  fishing_pressure_after <- data %>%  
    filter(YEAR > IMPLEMENTATION) %>%
    group_by(TYPE, OBJECTID) %>%
    summarize(fishing_pressure_after = mean(fishing_pressure, na.rm = TRUE))
  
  result <- left_join(fishing_pressure_before, fishing_pressure_after, by = c("TYPE", "OBJECTID"))
  
  return(result)
}

fishing_pressure <- fishing_pressure(total_fishing_pressure)

# Normalizing fishing effort values per total MPA or buffer area
fishing_pressure <- fishing_pressure %>% 
  mutate(
    fishing_pressure_before_norm = fishing_pressure_before / AREA_KM2,
    fishing_pressure_after_norm = fishing_pressure_after / AREA_KM2,
    fishing_pressure_difference = fishing_pressure_after_norm - fishing_pressure_before_norm
  )
print(fishing_pressure)

write_csv(fishing_pressure,"./Data/Results/MPA_guide/Summarized/fishing_pressure_guide_10km.csv")
fishing_pressure <- read.csv("./Data/Results/MPA_guide/Summarized/fishing_pressure_guide_10km.csv")

##### 3.1. Log ratios - before/after inside ####
fishing.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  slice(-5, -14, -23, -24, -50) %>%
  mutate(PROTECTION = ifelse(PROTECTION %in% c("light", "minimal"), "light_minimal", PROTECTION),
         fishing_pressure_after_norm=case_when(
           fishing_pressure_after_norm==0 ~ 0.001,
           TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(
           fishing_pressure_before_norm==0 ~ 0.001,
           TRUE~fishing_pressure_before_norm),
         log.ratio=log(fishing_pressure_after_norm/fishing_pressure_before_norm),
         PROTECTION=factor(PROTECTION,levels=rev(c("full","high","light_minimal","incompatible")))) %>%
  arrange(PROTECTION)


fishing.MPAs.summary <- fishing.MPAs %>%
  group_by(PROTECTION) %>%
  summarize(mean.log=mean(log.ratio),
            CI.log=sd(log.ratio)/sqrt(n())*1.96,
            mean.initial=mean(fishing_pressure_before_norm),
            pvalue=wilcox.test(log.ratio,mu=0,paired=FALSE)$p.value,
            sample=n(),
            lowCI=mean.log-CI.log,
            highCI=mean.log+CI.log)%>%
  mutate(PROTECTION=factor(PROTECTION,levels=rev(c("full","high","light_minimal","incompatible"))),
         significant=case_when(lowCI*highCI>0~"significant",
                               TRUE~"not significant"),
         fill=case_when(lowCI*highCI>0~"black",
                        TRUE~"white"))

## plots
# change in log ratios before/after
ggplot() +
  geom_point(data=fishing.MPAs, aes(x=PROTECTION, y=log.ratio),color="lightgrey",
             size=0.6)+
  geom_pointrange(data=fishing.MPAs.summary, 
                  aes(x=PROTECTION, y=mean.log, ymin=lowCI, 
                      ymax=highCI),size= 0.6,shape=21,fill=fishing.MPAs.summary$fill,
                  stroke=0.5)+
  coord_flip()+
  theme_classic()+
  geom_segment(aes(y=0,yend=0,x=0,xend=4.5), lty=2, size=0.2)+
  xlab("Level of protection") + ylab("Change in fishing effort (lnRR)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.text = element_text(size = 12))+
  geom_text(data=fishing.MPAs.summary, aes(label=sample,y=mean.log,x=(seq(1,4))-0.2), size=3)+
  labs(fill="significant") +
  scale_x_discrete(labels=c("full" = "Full", 
                            "high" = "High", 
                            "light_minimal" = "Light &\nMinimal", 
                            "incompatible" = "Incompatible"))

# scatter plot: change in fishing pressure x initial fishing pressure
ggplot() +
  geom_point(data=fishing.MPAs,aes(x=log.ratio,
                                       y=sqrt(sqrt(fishing_pressure_before_norm)),
                                       color=PROTECTION),
             size=1,alpha=0.7)+
  geom_point(data=fishing.MPAs.summary,aes(x=mean.log,
                                               y=sqrt(sqrt(mean.initial)),
                                               fill = PROTECTION),
             size=3,shape=21,stroke=0.5)+
  #,position=position_jitter(height = 2,width=1))+
  #geom_jitter(width = 0.2, height = 0) +
  theme_classic() +
  scale_fill_manual(name="Level of protection", values=c("full"="#0868ac","high"="#56b0c8",
                                                         "light_minimal"="#a5dcbf","incompatible"="#f0f9e8"),
                    labels=c("full"= "Full","high"="High",
                             "light_minimal"="Light & Minimal","incompatible"="Incompatible")) +
  scale_color_manual(name="Level of protection", values=c("full"="#0868ac","high"="#56b0c8",
                                                          "light_minimal"="#a5dcbf","incompatible"="#f0f9e8"),
                     labels=c("full"= "Full","high"="High",
                              "light_minimal"="Light & Minimal","incompatible"="Incompatible")) +
  guides(fill=guide_legend(reverse=TRUE),color=guide_legend(reverse=TRUE)) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12))+
  geom_vline(xintercept = 0, lty = 2, size = 0.2) +
  xlab("Change in fishing effort (lnRR)") + 
  ylab("Initial fishing pressure (hour km-2 year-1)")


##### 3.2. Log ratios - before/after outside ####
fishing.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference) & OBJECTID %in% fishing.MPAs$OBJECTID) %>%
  mutate(PROTECTION = ifelse(PROTECTION %in% c("light", "minimal"), "light_minimal", PROTECTION),
         fishing_pressure_after_norm=case_when(
           fishing_pressure_after_norm==0 ~ 0.001,
           TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(
           fishing_pressure_before_norm==0 ~ 0.001,
           TRUE~fishing_pressure_before_norm),
         log.ratio=log(fishing_pressure_after_norm/fishing_pressure_before_norm),
         PROTECTION=factor(PROTECTION,levels=rev(c("full","high","light_minimal","incompatible")))) %>%
  arrange(PROTECTION)


fishing.buffer.summary <- fishing.buffer %>%
  group_by(PROTECTION) %>%
  summarize(mean.log=mean(log.ratio),
            CI.log=sd(log.ratio)/sqrt(n())*1.96,
            mean.initial=mean(fishing_pressure_before_norm),
            pvalue=wilcox.test(log.ratio,mu=0,paired=FALSE)$p.value,
            sample=n(),
            lowCI=mean.log-CI.log,
            highCI=mean.log+CI.log)%>%
  mutate(PROTECTION=factor(PROTECTION,levels=rev(c("full","high","light_minimal","incompatible"))),
         significant=case_when(lowCI*highCI>0~"significant",
                               TRUE~"not significant"),
         fill=case_when(lowCI*highCI>0~"black",
                        TRUE~"white"))

## plots
# change in log ratios before/after
ggplot() +
  geom_point(data=fishing.buffer, aes(x=PROTECTION, y=log.ratio),color="lightgrey",
             size=0.6)+
  geom_pointrange(data=fishing.buffer.summary, 
                  aes(x=PROTECTION, y=mean.log, ymin=lowCI, 
                      ymax=highCI),size= 0.6,shape=21,fill=fishing.buffer.summary$fill,
                  stroke=0.5)+
  coord_flip()+
  theme_classic()+
  geom_segment(aes(y=0,yend=0,x=0,xend=4.5), lty=2, size=0.2)+
  xlab("Level of protection") + ylab("Change in fishing effort (lnRR)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 12))+
  geom_text(data=fishing.buffer.summary, aes(label=sample,y=mean.log,x=(seq(1,4))-0.2), size=3)+
  labs(fill="significant") +
  scale_x_discrete(labels=c("full" = "Full", 
                            "high" = "High", 
                            "light_minimal" = "Light &\nMinimal", 
                            "incompatible" = "Incompatible"))

# scatter plot: change in fishing pressure x initial fishing pressure
ggplot() +
  geom_point(data=fishing.buffer,aes(x=log.ratio,
                                         y=sqrt(sqrt(fishing_pressure_before_norm)),
                                         color=PROTECTION),
             size=1,alpha=0.7)+
  geom_point(data=fishing.buffer.summary,aes(x=mean.log,
                                                 y=sqrt(sqrt(mean.initial)),
                                                 fill = PROTECTION),
             size=3,shape=21,stroke=0.5)+
  #,position=position_jitter(height = 2,width=1))+
  #geom_jitter(width = 0.2, height = 0) +
  theme_classic() +
  scale_fill_manual(name="Level of protection", values=c("full"="#0868ac","high"="#56b0c8",
                                                         "light_minimal"="#a5dcbf","incompatible"="#f0f9e8"),
                    labels=c("full"= "Full","high"="High",
                             "light_minimal"="Light & Minimal","incompatible"="Incompatible")) +
  scale_color_manual(name="Level of protection", values=c("full"="#0868ac","high"="#56b0c8",
                                                          "light_minimal"="#a5dcbf","incompatible"="#f0f9e8"),
                     labels=c("full"= "Full","high"="High",
                              "light_minimal"="Light & Minimal","incompatible"="Incompatible")) +
  guides(fill=guide_legend(reverse=TRUE),color=guide_legend(reverse=TRUE)) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12))+
  geom_vline(xintercept = 0, lty = 2, size = 0.2) +
  xlab("Change in fishing effort (lnRR)") + 
  ylab("Initial fishing pressure (hour km-2 year-1)")


##### 3.3. Log ratios - inside/outside after ####
fishing.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  mutate(PROTECTION = ifelse(PROTECTION %in% c("light", "minimal"), "light_minimal", PROTECTION),
         fishing_pressure_after_norm=case_when(
           fishing_pressure_after_norm==0 ~ 0.001,
           TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(
           fishing_pressure_before_norm==0 ~ 0.001,
           TRUE~fishing_pressure_before_norm))

fishing.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference)) %>%
  mutate(PROTECTION = ifelse(PROTECTION %in% c("light", "minimal"), "light_minimal", PROTECTION),
         fishing_pressure_after_norm=case_when(
           fishing_pressure_after_norm==0 ~ 0.001,
           TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(
           fishing_pressure_before_norm==0 ~ 0.001,
           TRUE~fishing_pressure_before_norm)) 

fishing.log <-  fishing.MPAs %>%
  select(OBJECTID, fishing_pressure_before_norm,fishing_pressure_after_norm,PROTECTION) %>%
  left_join(fishing.buffer %>% 
              select(OBJECTID, fishing_pressure_before_norm,fishing_pressure_after_norm),
            by= "OBJECTID",suffix=c(".in",".out")) %>%
  mutate(log.ratio = log(fishing_pressure_after_norm.in/fishing_pressure_after_norm.out),
         PROTECTION=factor(PROTECTION,levels=rev(c("full","high","light_minimal","incompatible")))) %>%
  arrange(PROTECTION)%>%
  filter(!is.na(log.ratio))


fishing.log.summary <- fishing.log %>%
  group_by(PROTECTION) %>%
  summarize(mean.log=mean(log.ratio),
            CI.log=sd(log.ratio)/sqrt(n())*1.96,
            mean.initial=mean(fishing_pressure_before_norm.in),
            pvalue=wilcox.test(log.ratio,mu=0,paired=FALSE)$p.value,
            sample=n(),
            lowCI=mean.log-CI.log,
            highCI=mean.log+CI.log)%>%
  mutate(PROTECTION=factor(PROTECTION,levels=rev(c("full","high","light_minimal","incompatible"))),
         significant=case_when(lowCI*highCI>0~"significant",
                               TRUE~"not significant"),
         fill=case_when(lowCI*highCI>0~"black",
                        TRUE~"white"))

## plot
# change in log ratios inside/outside after
ggplot() +
  geom_point(data=fishing.log, aes(x=PROTECTION, y=log.ratio),color="lightgrey",
             size=0.6)+
  geom_pointrange(data=fishing.log.summary, 
                  aes(x=PROTECTION, y=mean.log, ymin=lowCI, 
                      ymax=highCI),size= 0.6,shape=21,fill=fishing.log.summary$fill,
                  stroke=0.5)+
  coord_flip()+
  theme_classic()+
  geom_segment(aes(y=0,yend=0,x=0,xend=4.5), lty=2, size=0.2)+
  xlab("IUCN category") + ylab("change in fishing effort (logRR)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(axis.text = element_text(size = 12))+
  geom_text(data=fishing.log.summary, aes(label=sample,y=mean.log,x=(seq(1,4))-0.2), size=3)+
  labs(fill="significant") +
  scale_x_discrete(labels=c("full" = "Full", 
                            "high" = "High", 
                            "light_minimal" = "Light &\nMinimal", 
                            "incompatible" = "Incompatible"))


##### 3.4. Log ratios - BACI #####
fishing.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  mutate(PROTECTION = ifelse(PROTECTION %in% c("light", "minimal"), "light_minimal", PROTECTION),
         fishing_pressure_after_norm=case_when(
           fishing_pressure_after_norm==0 ~ 0.001,
           TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(
           fishing_pressure_before_norm==0 ~ 0.001,
           TRUE~fishing_pressure_before_norm))

fishing.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference)) %>%
  mutate(PROTECTION = ifelse(PROTECTION %in% c("light", "minimal"), "light_minimal", PROTECTION),
         fishing_pressure_after_norm=case_when(
           fishing_pressure_after_norm==0 ~ 0.001,
           TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(
           fishing_pressure_before_norm==0 ~ 0.001,
           TRUE~fishing_pressure_before_norm))  

fishing.baci <-  fishing.MPAs %>%
  select(OBJECTID, fishing_pressure_before_norm,fishing_pressure_after_norm,PROTECTION) %>%
  left_join(fishing.buffer %>% 
              select(OBJECTID, fishing_pressure_before_norm,fishing_pressure_after_norm),
            by= "OBJECTID",suffix=c(".in",".out")) %>%
  mutate(log.ratio = log((fishing_pressure_after_norm.in/fishing_pressure_before_norm.in) /
                           (fishing_pressure_after_norm.out/fishing_pressure_before_norm.out)),
         PROTECTION=factor(PROTECTION,levels=rev(c("full","high","light_minimal","incompatible")))) %>%
  arrange(PROTECTION)%>%
  filter(!is.na(log.ratio))

fishing.baci.summary <- fishing.baci %>%
  group_by(PROTECTION) %>%
  summarize(mean.baci=mean(log.ratio),
            CI.baci = sd(log.ratio)/sqrt(n())*1.96,
            mean.initial=mean(fishing_pressure_before_norm.in),
            pvalue=wilcox.test(mean.baci,mu=0,paired=FALSE)$p.value,
            sample=n(),
            lowCI=mean.baci-CI.baci,
            highCI=mean.baci+CI.baci)%>%
  mutate(PROTECTION=factor(PROTECTION,levels=rev(c("full","high","light_minimal","incompatible"))),
         significant=case_when(lowCI*highCI>0~"significant",
                               TRUE~"not significant"),
         fill=case_when(lowCI*highCI>0~"black",
                        TRUE~"white"))%>%
  arrange(PROTECTION)

## plot
ggplot() +
  geom_point(data=fishing.baci, aes(x=PROTECTION, y=log.ratio),color="lightgrey",
             size=0.6)+
  geom_pointrange(data=fishing.baci.summary, 
                  aes(x=PROTECTION, y=mean.baci, ymin=lowCI, 
                      ymax=highCI),size= 0.6,shape=21,fill=fishing.baci.summary$fill,
                  stroke=0.5)+
  coord_flip()+
  theme_classic()+
  geom_segment(aes(y=0,yend=0,x=0,xend=4.5), lty=2, size=0.2)+
  xlab("IUCN category") + ylab("change in fishing effort (logRR)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.text = element_text(size = 12))+
  geom_text(data=fishing.baci.summary, aes(label=sample,y=mean.baci,x=(seq(1,4))-0.2), size=3)+
  labs(fill="significant") +
  scale_x_discrete(labels=c("full" = "Full", 
                            "high" = "High", 
                            "light_minimal" = "Light &\nMinimal", 
                            "incompatible" = "Incompatible"))

# scatter plot: change in fishing pressure x initial fishing pressure
ggplot() +
  geom_point(data=fishing.baci,aes(x=log.ratio,
                                   y=sqrt(sqrt(fishing_pressure_before_norm.in)),
                                   color=PROTECTION),
             size=1,alpha=0.7)+
  geom_point(data=fishing.baci.summary,aes(x=mean.baci,
                                           y=sqrt(sqrt(mean.initial)),
                                           fill = PROTECTION),
             size=3,shape=21,stroke=0.5)+
  #,position=position_jitter(height = 2,width=1))+
  #geom_jitter(width = 0.2, height = 0) +
  theme_classic() +
  scale_fill_manual(name="Level of protection", values=c("full"="#0868ac","high"="#56b0c8",
                                                         "light_minimal"="#a5dcbf","incompatible"="#f0f9e8"),
                    labels=c("full"= "Full","high"="High",
                             "light_minimal"="Light & Minimal","incompatible"="Incompatible")) +
  scale_color_manual(name="Level of protection", values=c("full"="#0868ac","high"="#56b0c8",
                                                          "light_minimal"="#a5dcbf","incompatible"="#f0f9e8"),
                     labels=c("full"= "Full","high"="High",
                              "light_minimal"="Light & Minimal","incompatible"="Incompatible")) +
  guides(fill=guide_legend(reverse=TRUE),color=guide_legend(reverse=TRUE)) +
  theme(legend.position = "none") +
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.text = element_text(size = 12))+
  geom_vline(xintercept = 0, lty = 2, size = 0.2) +
  xlab("Change in fishing effort (lnRR)") + 
  ylab("Initial fishing pressure (hour km-2 year-1)")


################################################################################


