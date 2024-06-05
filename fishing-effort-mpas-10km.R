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
library(rstatix)
library(ggplot2)
library(gridExtra)
library(ggspatial)
library(ggExtra)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)

################################################################################

#### 1. Processing MPAs ####

##### 1.1. Exploring data #####

## Importing data
mpas <- read.csv("./Data/WDPA/WDPA_WDOECM_Jan2024_Public_marine.csv", header = T, sep = ",")

## Filtering by status
proposed <- mpas %>%
  filter(STATUS == "Proposed")

inscribed <- mpas %>%
  filter(STATUS == "Inscribed")

adopted <- mpas %>%
  filter(STATUS == "Adopted")

designated <- mpas %>%
  filter(STATUS == "Designated")

established <- mpas %>%
  filter(STATUS == "Established")

not_reported <- mpas %>%
  filter(STATUS == "Not Reported")


## Filtering by date
proposed_filtered <- proposed %>%
  filter(STATUS_YR >= 2013 & STATUS_YR <= 2019)

inscribed_filtered <- inscribed %>%
  filter(STATUS_YR >= 2013 & STATUS_YR <= 2019)

adopted_filtered <- adopted %>%
  filter(STATUS_YR >= 2013 & STATUS_YR <= 2019)

designated_filtered <- designated %>%
  filter(STATUS_YR >= 2013 & STATUS_YR <= 2019)

established_filtered <- established %>%
  filter(STATUS_YR >= 2013 & STATUS_YR <= 2019)

not_reported_filtered <- not_reported %>%
  filter(STATUS_YR >= 2013 & STATUS_YR <= 2019)


## Filtering by IUCN protection levels
designated_filtered_Ia <-  designated_filtered %>%
  filter(IUCN_CAT == "Ia")
designated_filtered_Ib <-  designated_filtered %>%
  filter(IUCN_CAT == "Ib")
designated_filtered_II <-  designated_filtered %>%
  filter(IUCN_CAT == "II")
designated_filtered_III <-  designated_filtered %>%
  filter(IUCN_CAT == "III")
designated_filtered_IV <-  designated_filtered %>%
  filter(IUCN_CAT == "IV")
designated_filtered_V <-  designated_filtered %>%
  filter(IUCN_CAT == "V")
designated_filtered_VI <-  designated_filtered %>%
  filter(IUCN_CAT == "VI")
designated_filtered_na <-  designated_filtered %>%
  filter(IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported"))

established_filtered_Ia <-  established_filtered %>%
  filter(IUCN_CAT == "Ia")
established_filtered_Ib <-  established_filtered %>%
  filter(IUCN_CAT == "Ib")
established_filtered_II <-  established_filtered %>%
  filter(IUCN_CAT == "II")
established_filtered_III <-  established_filtered %>%
  filter(IUCN_CAT == "III")
established_filtered_IV <-  established_filtered %>%
  filter(IUCN_CAT == "IV")
established_filtered_V <-  established_filtered %>%
  filter(IUCN_CAT == "V")
established_filtered_VI <-  established_filtered %>%
  filter(IUCN_CAT == "VI")
established_filtered_na <-  established_filtered %>%
  filter(IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported"))

##### 1.2. Processing data #####

###### 1.2.1. Loading library & data and basic processing ######

# Importing MPAs' shapefiles
mpas_shp0 <- vect("./Data/WDPA/WDPA_WDOECM_Jan2024_Public_all_shp-polygons_0.shp")
mpas_shp0 <- subset(mpas_shp0, mpas_shp0$MARINE != "0")
mpas_shp0 <- subset(mpas_shp0, mpas_shp0$STATUS_YR >= 2013 & mpas_shp0$STATUS_YR <= 2019)
mpas_shp1 <- vect("./Data/WDPA/WDPA_WDOECM_Jan2024_Public_all_shp-polygons_1.shp")
mpas_shp1 <- subset(mpas_shp1, mpas_shp1$MARINE != "0")
mpas_shp1 <- subset(mpas_shp1, mpas_shp1$STATUS_YR >= 2013 & mpas_shp1$STATUS_YR <= 2019)
mpas_shp2 <- vect("./Data/WDPA/WDPA_WDOECM_Jan2024_Public_all_shp-polygons_2.shp")
mpas_shp2 <- subset(mpas_shp2, mpas_shp2$MARINE != "0")
mpas_shp2 <- subset(mpas_shp2, mpas_shp2$STATUS_YR >= 2013 & mpas_shp2$STATUS_YR <= 2019)

# Binding into one file
MPAs <- rbind(mpas_shp0, mpas_shp1, mpas_shp2)
rm(mpas_shp0, mpas_shp1, mpas_shp2)
MPAs <- st_as_sf(MPAs)
st_write(MPAs, "./Data/WDPA/MPAs.shp")
#MPAs <- vect("./Data/WDPA/MPAs.shp")
#MPAs <- st_as_sf(MPAs)

# Importing land's shapefile
land <- vect("./Data/WDPA/ne_10m_land.shp")
land <- st_as_sf(land)

# Cropping MPAs using land as mask
MPAs_cropped <- st_difference(MPAs,land) 
MPAs_cropped <- st_as_sf(MPAs_cropped)
st_is_valid(MPAs_cropped)
st_write(MPAs_cropped, "./Data/WDPA/MPAs_cropped.shp")
#MPAs_cropped <- vect("./Data/WDPA/MPAs_cropped.shp")
#MPAs_cropped <- st_as_sf(MPAs_cropped)


###### 1.2.2 Creating MPA layers depending on protection level and adding an area column ######

# All the created layers are sfc multipolygons
MPAs_levelIa <- MPAs_cropped[MPAs_cropped$IUCN_CAT=="Ia",]
MPAs_levelIa$AREA_KM2 <- st_area(MPAs_levelIa) / 10^6
MPAs_levelIa$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_levelIa$AREA_KM2))
#st_write(MPAs_levelIa, "./Data/WDPA/MPAs_Ia.shp", append = FALSE)

MPAs_levelIb <- MPAs_cropped[MPAs_cropped$IUCN_CAT=="Ib",]
MPAs_levelIb$AREA_KM2 <- st_area(MPAs_levelIb)  / 10^6
MPAs_levelIb$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_levelIb$AREA_KM2))
#st_write(MPAs_levelIb, "./Data/WDPA/MPAs_Ib.shp", append = FALSE)

MPAs_levelII <- MPAs_cropped[MPAs_cropped$IUCN_CAT=="II",]
MPAs_levelII <- MPAs_levelII[-72, ] # MPA 72 was excluded due to overlapping with IUCN Category Ia
MPAs_levelII$AREA_KM2 <- st_area(MPAs_levelII)  / 10^6
MPAs_levelII$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_levelII$AREA_KM2))
#st_write(MPAs_levelII, "./Data/WDPA/MPAs_II.shp", append = FALSE)

MPAs_levelIII <- MPAs_cropped[MPAs_cropped$IUCN_CAT=="III",]
MPAs_levelIII$AREA_KM2 <- st_area(MPAs_levelIII)  / 10^6
MPAs_levelIII$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_levelIII$AREA_KM2))
#st_write(MPAs_levelIII, "./Data/WDPA/MPAs_III.shp", append = FALSE)

MPAs_levelIV <- MPAs_cropped[MPAs_cropped$IUCN_CAT=="IV",]
MPAs_levelIV$AREA_KM2 <- st_area(MPAs_levelIV)  / 10^6
MPAs_levelIV$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_levelIV$AREA_KM2))
#st_write(MPAs_levelIV, "./Data/WDPA/MPAs_IV.shp", append = FALSE)

MPAs_levelV <- MPAs_cropped[MPAs_cropped$IUCN_CAT=="V",]
# MPA 555670040 crosses edge so it was divided into 2 on QGis
#MPAs_levelV <- vect("./Data/WDPA/MPAs_V.shp")
#MPAs_levelV <- st_as_sf(MPAs_levelV)
MPAs_levelV$WDPAID[140] <- 555670040
MPAs_levelV$AREA_KM2 <- st_area(MPAs_levelV)  / 10^6
MPAs_levelV$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_levelV$AREA_KM2))
#sum(area[140:141]) # 1508399
MPAs_levelV$AREA_KM2[c(140, 141)] <- 1508399
#st_write(MPAs_levelV, "./Data/WDPA/MPAs_V.shp", append = FALSE)

MPAs_levelVI <- MPAs_cropped[MPAs_cropped$IUCN_CAT=="VI",]
MPAs_levelVI <- MPAs_levelVI[-c(132, 138, 150, 157, 182), ] # MPA 182 was excluded due to overlapping with IUCN Category Ia
MPAs_levelVI$AREA_KM2 <- st_area(MPAs_levelVI)  / 10^6
MPAs_levelVI$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_levelVI$AREA_KM2))
#st_write(MPAs_levelVI, "./Data/WDPA/MPAs_VI.shp", append = FALSE)

MPAs_levelNA <- MPAs_cropped[MPAs_cropped$IUCN_CAT=="Not Applicable" |
                               MPAs_cropped$IUCN_CAT=="Not Reported"|
                               MPAs_cropped$IUCN_CAT=="Not Assigned",]
MPAs_levelNA <- MPAs_levelNA[-c(1056, 1213, 1215), ]
MPAs_levelNA$AREA_KM2 <- st_area(MPAs_levelNA)  / 10^6
MPAs_levelNA$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_levelNA$AREA_KM2))
#st_write(MPAs_levelNA, "./Data/WDPA/MPAs_NA.shp", append = FALSE)


###### 1.2.3. Creating buffers ######

MPAs_bufferIa <- st_buffer(MPAs_levelIa, dist = 10000)
MPAs_bufferIa <- st_difference(MPAs_bufferIa, MPAs_levelIa)
#MPAs_bufferIa <- vect("./Data/WDPA/MPAs_Ia_buffer_10km.shp")
#MPAs_bufferIa <- st_as_sf(MPAs_bufferIa)
MPAs_bufferIa$AREA_KM2 <- st_area(MPAs_bufferIa)  / 10^6
MPAs_bufferIa$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_bufferIa$AREA_KM2))
#st_write(MPAs_bufferIa, "./Data/WDPA/MPAs_Ia_buffer_10km.shp", append = FALSE)

MPAs_bufferIb <- st_buffer(MPAs_levelIb, dist = 10000)
MPAs_bufferIb <- st_difference(MPAs_bufferIb, MPAs_levelIb)
#MPAs_bufferIb <- vect("./Data/WDPA/MPAs_Ib_buffer_10km.shp")
#MPAs_bufferIb <- st_as_sf(MPAs_bufferIb)
MPAs_bufferIb$AREA_KM2 <- st_area(MPAs_bufferIb)  / 10^6
MPAs_bufferIb$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_bufferIb$AREA_KM2))
#st_write(MPAs_bufferIb, "./Data/WDPA/MPAs_Ib_buffer_10km.shp", append = FALSE)

MPAs_bufferII <- st_buffer(MPAs_levelII, dist = 10000)
MPAs_bufferII <- st_difference(MPAs_bufferII, MPAs_levelII)
#MPAs_bufferII <- vect("./Data/WDPA/MPAs_II_buffer_10km.shp")
#MPAs_bufferII <- st_as_sf(MPAs_bufferII)
MPAs_bufferII$AREA_KM2 <- st_area(MPAs_bufferII)  / 10^6
MPAs_bufferII$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_bufferII$AREA_KM2))
#st_write(MPAs_bufferII, "./Data/WDPA/MPAs_II_buffer_10km.shp", append = FALSE)

MPAs_bufferIII <- st_buffer(MPAs_levelIII, dist = 10000)
MPAs_bufferIII <- st_difference(MPAs_bufferIII, MPAs_levelIII)
#MPAs_bufferIII <- vect("./Data/WDPA/MPAs_III_buffer_10km.shp")
#MPAs_bufferIII <- st_as_sf(MPAs_bufferIII)
MPAs_bufferIII$AREA_KM2 <- st_area(MPAs_bufferIII)  / 10^6
MPAs_bufferIII$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_bufferIII$AREA_KM2))
#st_write(MPAs_bufferIII, "./Data/WDPA/MPAs_III_buffer_10km.shp", append = FALSE)

MPAs_bufferIV <- st_buffer(MPAs_levelIV, dist = 10000)
MPAs_bufferIV <- st_difference(MPAs_bufferIV, MPAs_levelIV)
#MPAs_bufferIV <- vect("./Data/WDPA/MPAs_IV_buffer_10km.shp")
#MPAs_bufferIV <- st_as_sf(MPAs_bufferIV)
#MPAs_bufferIV <- MPAs_bufferIV[-c(282,290,321,572), ] # For 1km
#MPAs_bufferIV <- MPAs_bufferIV[-c(282,289,290,321,424,572), ] # For 10km
#MPAs_bufferIV <- MPAs_bufferIV[-c(281,282,289,290,321,424,425,572), ] # For 20km
MPAs_bufferIV$AREA_KM2 <- st_area(MPAs_bufferIV)  / 10^6
MPAs_bufferIV$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_bufferIV$AREA_KM2))
#st_write(MPAs_bufferIV, "./Data/WDPA/MPAs_IV_buffer_10km.shp", append = FALSE)

MPAs_bufferV <- st_buffer(MPAs_levelV, dist = 10000)
MPAs_bufferV <- st_difference(MPAs_bufferV, MPAs_levelV)
#MPAs_bufferV <- vect("./Data/WDPA/MPAs_V_buffer_10km.shp")
#MPAs_bufferV <- st_as_sf(MPAs_bufferV)
MPAs_bufferV$WDPAID[140] <- 555670040
MPAs_bufferV$AREA_KM2 <- st_area(MPAs_bufferV)  / 10^6
MPAs_bufferV$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_bufferV$AREA_KM2))
#sum(area[140:141]) # 36008.914854 (for 1km), 90639.9653 (for 10km) and 149177.11 (for 20km)
#MPAs_bufferV$AREA_KM2[c(140, 141)] <- 36008.91 # For 1km
#MPAs_bufferV$AREA_KM2[c(140, 141)] <- 90639.97 # For 10km
#MPAs_bufferV$AREA_KM2[c(140, 141)] <- 149177.1 # For 20km
#st_write(MPAs_bufferV, "./Data/WDPA/MPAs_V_buffer_10km.shp", append = FALSE)

MPAs_bufferVI <- st_buffer(MPAs_levelVI, dist = 10000)
MPAs_bufferVI <- st_difference(MPAs_bufferVI, MPAs_levelVI)
#MPAs_bufferVI <- vect("./Data/WDPA/MPAs_VI_buffer_10km.shp")
#MPAs_bufferVI <- st_as_sf(MPAs_bufferVI)
#MPAs_bufferVI <- MPAs_bufferVI[-c(20,21,22,68,77,99,181,188,189,219), ] # For 1km
#MPAs_bufferVI <- MPAs_bufferVI[-c(20,21,22,37,68,77,99,181,188,189,219), ] # For 10km
#MPAs_bufferVI <- MPAs_bufferVI[-c(20,21,22,37,68,77,98,99,181,188,189,219), ] # For 20km
MPAs_bufferVI$AREA_KM2 <- st_area(MPAs_bufferVI)  / 10^6
MPAs_bufferVI$AREA_KM2 <- as.numeric(gsub("\\[.*\\]", "", MPAs_bufferVI$AREA_KM2))
#st_write(MPAs_bufferVI, "./Data/WDPA/MPAs_VI_buffer_10km.shp", append = FALSE)

###### 1.2.4. Binding MPAs and buffers ######

MPAs_bind <- rbind(MPAs_levelIa, MPAs_levelIb, MPAs_levelII, MPAs_levelIII,
                   MPAs_levelIV, MPAs_levelV, MPAs_levelVI)

st_write(MPAs_bind, "./Data/WDPA/MPAs_bind.shp",append = FALSE)


MPAs_buffer <- rbind(MPAs_bufferIa, MPAs_bufferIb, MPAs_bufferII, MPAs_bufferIII,
                     MPAs_bufferIV, MPAs_bufferV, MPAs_bufferVI)

st_write(MPAs_buffer, "./Data/WDPA/MPAs_buffer_10km.shp",append = FALSE)

###### 1.2.5. Importing files ######

rm(list = ls())

MPAs_bind <- vect("./Data/WDPA/MPAs_bind.shp")
MPAs_bind <- st_as_sf(MPAs_bind)
#MPAs_bind <- MPAs_bind[-c(552,560,591,842,1063,1064,1065,1111,1120,1142,1224,1231,1232,1262),] # For 1km
MPAs_bind <- MPAs_bind[-c(552,559,560,591,694,842,1063,1064,1065,1080,1111,1120,1142,1224,1231,1232,1262),] # For 10km
#MPAs_bind <- MPAs_bind[-c(551,552,559,560,591,694,695,842,1063,1064,1065,1080,1111,1120,1141,1142,1224,1231,1232,1262),] # For 20km

MPAs_buffer <- vect("./Data/WDPA/MPAs_buffer_10km.shp")
MPAs_buffer <- st_as_sf(MPAs_buffer)

##### 1.2. Plotting world map and MPAs #####

# Loading MPAs artificially bigger for zoomed maps
MPAs_bind_buf <- vect("./Data/WDPA/MPAs_bind_buf.shp")
MPAs_bind_buf <- st_as_sf(MPAs_bind_buf)
#MPAs_bind_buf <- MPAs_bind_buf[-c(552,560,591,842,1063,1064,1065,1111,1120,1142,1224,1231,1232,1262),] # For 1km
MPAs_bind_buf <- MPAs_bind_buf[-c(552,559,560,591,694,842,1063,1064,1065,1080,1111,1120,1142,1224,1231,1232,1262),] # For 10km
#MPAs_bind_buf <- MPAs_bind_buf[-c(551,552,559,560,591,694,695,842,1063,1064,1065,1080,1111,1120,1141,1142,1224,1231,1232,1262),] # For 20km
MPAs_bind_buf <- MPAs_bind_buf %>%
  mutate(IUCN_CAT = factor(IUCN_CAT, levels = c("VI", "V", "IV", "III", "II", "Ib", "Ia"))) %>%
  arrange(IUCN_CAT)

# Loading world and oceans' data
land <- ne_countries(scale = "medium", returnclass = "sf")

ocean <- ne_download(scale = 110, category = "physical", type = "ocean", returnclass = "sf")

# Creating shapefiles for zoom
zoom_latam <- data.frame(x = c(-118, -65, -59, -108, -118), y = c(31, 31, -4, -4, 31))
zoom_latam <- st_sf(geometry = st_sfc(st_polygon(list(cbind(zoom_latam$x, zoom_latam$y)))), crs = 4326)

zoom_eu <- data.frame(x = c(-8, 45, 30, -6, -8), y = c(65, 65, 35, 35, 65))
zoom_eu <- st_sf(geometry = st_sfc(st_polygon(list(cbind(zoom_eu$x, zoom_eu$y)))), crs = 4326)

zoom_aus <- data.frame(x = c(110, 155, 170, 117, 110), y = c(-11, -11, -43, -43, -11))
zoom_aus <- st_sf(geometry = st_sfc(st_polygon(list(cbind(zoom_aus$x, zoom_aus$y)))), crs = 4326)

zoom_jpn <- data.frame(x = c(135, 155, 134, 116, 135), y = c(45, 45, 25, 25, 45))
zoom_jpn <- st_sf(geometry = st_sfc(st_polygon(list(cbind(zoom_jpn$x, zoom_jpn$y)))), crs = 4326)

# Plotting MPAs' map for the world
ggplot() +
  geom_sf(data = land, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind, aes(fill = IUCN_CAT, size = 0.2)) +
  scale_fill_manual(values = c("Ia" = "#0868ac", "Ib" = "#2f8fc0", "II" = "#56b0c8",
                               "III" = "#7bccc4", "IV" = "#a5dcbf", "V" = "#ccebcb", "VI" = "#f0f9e8"),
                    name = "IUCN Category",
                    labels = c("Ia", "Ib", "II", "III", "IV", "V", "VI")) +
  
  # Adding squares for zoom
  geom_sf(data = zoom_latam, fill = "transparent", color = "black") +
  geom_sf(data = zoom_eu, fill = "transparent", color = "black") +
  geom_sf(data = zoom_aus, fill = "transparent", color = "black") +
  geom_sf(data = zoom_jpn, fill = "transparent", color = "black") +
  
  coord_sf(crs = "+proj=moll") +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in LatAm
map_latam <- st_crop(land, xmin = -63, xmax = -113, ymin = -4, ymax = 31)

ggplot() +
  geom_sf(data = map_latam, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_buf, aes(fill = IUCN_CAT, size = 0.2)) +
  scale_fill_manual(values = c("Ia" = "#0868ac", "Ib" = "#2f8fc0", "II" = "#56b0c8",
                               "III" = "#7bccc4", "IV" = "#a5dcbf", "V" = "#ccebcb", "VI" = "#f0f9e8")) +
  coord_sf(xlim = c(-63, -113), ylim = c(-4, 31)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Europe
map_eu <- st_crop(land, xmin = -5, xmax = 30, ymin = 35, ymax = 65)

ggplot() +
  geom_sf(data = map_eu, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_buf, aes(fill = IUCN_CAT, size = 0.2)) +
  scale_fill_manual(values = c("Ia" = "#0868ac", "Ib" = "#2f8fc0", "II" = "#56b0c8",
                               "III" = "#7bccc4", "IV" = "#a5dcbf", "V" = "#ccebcb", "VI" = "#f0f9e8")) +
  coord_sf(xlim = c(-5, 30), ylim = c(35, 65)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in the Australia
map_aus <- st_crop(land, xmin = 110, xmax = 160, ymin = -43, ymax = -11)

ggplot() +
  geom_sf(data = map_aus, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_buf, aes(fill = IUCN_CAT, size = 0.2)) +
  scale_fill_manual(values = c("Ia" = "#0868ac", "Ib" = "#2f8fc0", "II" = "#56b0c8",
                               "III" = "#7bccc4", "IV" = "#a5dcbf", "V" = "#ccebcb", "VI" = "#f0f9e8")) +
  coord_sf(xlim = c(110, 160), ylim = c(-43, -11)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Japan
map_jpn <- st_crop(land, xmin = 115, xmax = 150, ymin = 27, ymax = 42)

ggplot() +
  geom_sf(data = map_jpn, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_buf, aes(fill = IUCN_CAT, size = 0.2)) +
  scale_fill_manual(values = c("Ia" = "#0868ac", "Ib" = "#2f8fc0", "II" = "#56b0c8",
                               "III" = "#7bccc4", "IV" = "#a5dcbf", "V" = "#ccebcb", "VI" = "#f0f9e8")) +
  coord_sf(xlim = c(124, 146), ylim = c(26, 43)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

##### 1.3. Histograms #####

# Histogram of protection level
hist_protection <- ggplot() +
  geom_bar(data = MPAs_bind, aes(x = IUCN_CAT, fill = IUCN_CAT)) +
  scale_fill_manual(values = c("Ia" = "#0868ac", "Ib" = "#2f8fc0", "II" = "#56b0c8",
                               "III" = "#7bccc4", "IV" = "#a5dcbf", "V" = "#ccebcb", "VI" = "#f0f9e8")) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  #theme(panel.grid.major = element_line(color = "#D3D3D3")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#D3D3D3"),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 650)) +
  theme(legend.position = "none")
print(hist_protection)

# Histogram of size distribution
hist_size <- ggplot(data = MPAs_bind, aes(x = AREA_KM2, fill = IUCN_CAT)) +
  geom_histogram(breaks = 10^seq(0, log10(2e6), length.out = 20), position = "stack") +
  geom_histogram(breaks = 10^seq(0, log10(2e6), length.out = 20), position = "stack", fill = "transparent", color = "white") +
  scale_x_log10(labels = scales::comma_format()) +
  scale_fill_manual(values = c("Ia" = "#0868ac", "Ib" = "#2f8fc0", "II" = "#56b0c8",
                               "III" = "#7bccc4", "IV" = "#a5dcbf", "V" = "#ccebcb", "VI" = "#f0f9e8")) +
  labs(x = NULL, y = NULL, fill = "IUCN Category") +
  theme_minimal() +
  #theme(panel.grid.major = element_line(color = "#D3D3D3")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#D3D3D3"),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 110)) +
  theme(legend.position = "none")
print(hist_size)

# Histogram of implementation year
hist_year <- ggplot(data = MPAs_bind, aes(x = STATUS_YR, fill = IUCN_CAT)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("Ia" = "#0868ac", "Ib" = "#2f8fc0", "II" = "#56b0c8",
                               "III" = "#7bccc4", "IV" = "#a5dcbf", "V" = "#ccebcb", "VI" = "#f0f9e8")) +
  labs(x = NULL, y = NULL, fill = "IUCN Category") +
  theme_minimal() +
  #theme(panel.grid.major = element_line(color = "#D3D3D3")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#D3D3D3"),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 310)) +
  theme(legend.position = "none")
print(hist_year)


################################################################################

#### 2. Processing fishing effort data per year ####

##### 2.1. For 2020 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2020",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2020/",files,sep=""), read_csv)
fishing_2020 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2020 <- data.frame(fishing_2020) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours))

write_csv(summarized_fishing2020,"./Data/GFW/summarized_fishing_2020.csv")

## Creating raster
r_fishing_2020 <- summarized_fishing2020 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2020)

writeRaster(r_fishing_2020, "./Data/GFW/r_fishing_2020.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2020, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2020) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2020.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2020, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2020) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2020.csv")

##### 2.2. For 2019 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2019",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2019/",files,sep=""), read_csv)
fishing_2019 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2019 <- data.frame(fishing_2019) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours))

write_csv(summarized_fishing2019,"./Data/GFW/summarized_fishing_2019.csv")

## Creating raster
r_fishing_2019 <- summarized_fishing2019 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2019)

writeRaster(r_fishing_2019, "./Data/GFW/r_fishing_2019.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2019, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2019) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2019.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2019, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2019) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2019.csv")

##### 2.3. For 2018 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2018",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2018/",files,sep=""), read_csv)
fishing_2018 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2018 <- data.frame(fishing_2018) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours))

write_csv(summarized_fishing2018,"./Data/GFW/summarized_fishing_2018.csv")

## Creating raster
r_fishing_2018 <- summarized_fishing2018 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2018)

writeRaster(r_fishing_2018, "./Data/GFW/r_fishing_2018.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2018, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2018) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2018.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2018, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2018) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2018.csv")

##### 2.4. For 2017 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2017",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2017/",files,sep=""), read_csv)
fishing_2017 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2017 <- data.frame(fishing_2017) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours))

write_csv(summarized_fishing2017,"./Data/GFW/summarized_fishing_2017.csv")

## Creating raster
r_fishing_2017 <- summarized_fishing2017 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2017)

writeRaster(r_fishing_2017, "./Data/GFW/r_fishing_2017.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2017, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2017) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2017.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2017, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2017) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2017.csv")

##### 2.5. For 2016 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2016",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2016/",files,sep=""), read_csv)
fishing_2016 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2016 <- data.frame(fishing_2016) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours))

write_csv(summarized_fishing2016,"./Data/GFW/summarized_fishing_2016.csv")

## Creating raster
r_fishing_2016 <- summarized_fishing2016 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2016)

writeRaster(r_fishing_2016, "./Data/GFW/r_fishing_2016.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2016, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2016) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2016.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2016, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2016) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2016.csv")

##### 2.6. For 2015 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2015",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2015/",files,sep=""), read_csv)
fishing_2015 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2015 <- data.frame(fishing_2015) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours, na.rm = T))

write_csv(summarized_fishing2015,"./Data/GFW/summarized_fishing_2015.csv")

## Creating raster
r_fishing_2015 <- summarized_fishing2015 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2015)

writeRaster(r_fishing_2015, "./Data/GFW/r_fishing_2015.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2015, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2015) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2015.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2015, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2015) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2015.csv")

##### 2.7. For 2014 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2014",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2014/",files,sep=""), read_csv)
fishing_2014 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2014 <- data.frame(fishing_2014) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours))

write_csv(summarized_fishing2014,"./Data/GFW/summarized_fishing_2014.csv")

## Creating raster
r_fishing_2014 <- summarized_fishing2014 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2014)

writeRaster(r_fishing_2014, "./Data/GFW/r_fishing_2014.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2014, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2014) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2014.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2014, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2014) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2014.csv")

##### 2.8. For 2013 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2013",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2013/",files,sep=""), read_csv)
fishing_2013 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2013 <- data.frame(fishing_2013) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours))

write_csv(summarized_fishing2013,"./Data/GFW/summarized_fishing_2013.csv")

## Creating raster
r_fishing_2013 <- summarized_fishing2013 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2013)

writeRaster(r_fishing_2013, "./Data/GFW/r_fishing_2013.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2013, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2013) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2013.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2013, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2013) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2013.csv")

##### 2.9. For 2012 #####

## Reading all fishing effort files and joining them
files <-  list.files(path="./Data/GFW/fleet-daily-csvs-100-v2-2012",pattern="*.csv")
myfiles <-  lapply(paste("./Data/GFW/fleet-daily-csvs-100-v2-2012/",files,sep=""), read_csv)
fishing_2012 <- rbindlist(myfiles)
rm(files, myfiles)

summarized_fishing2012 <- data.frame(fishing_2012) %>%
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>%
  summarize(fishing_pressure=sum(fishing_hours))

write_csv(summarized_fishing2012,"./Data/GFW/summarized_fishing_2012.csv")

## Creating raster
r_fishing_2012 <- summarized_fishing2012 %>%
  select(cell_ll_lon, cell_ll_lat, fishing_pressure) %>%
  terra::rast(type = "xyz", crs = "wgs84")
print(r_fishing_2012)

writeRaster(r_fishing_2012, "./Data/GFW/r_fishing_2012.tif")

## Joining with mpas
fishing_mpa <- extract(r_fishing_2012, vect(MPAs_bind), fun = sum, na.rm = T)
fishing_mpa <- data.frame(WDPA_PID = MPAs_bind$WDPA_PID, fishing_mpa)
fishing_mpa <- left_join(fishing_mpa,
                         MPAs_bind %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                         by = "WDPA_PID") %>%
  mutate(TYPE = "MPA", YEAR = 2012) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_mpa,"./Data/Results/MPAs_WDPA/summarized_fishing_mpa_2012.csv")

## Joining with buffer
fishing_buffer <- extract(r_fishing_2012, vect(MPAs_buffer), fun = sum, na.rm = T)
fishing_buffer <- data.frame(WDPA_PID = MPAs_buffer$WDPA_PID, fishing_buffer)
fishing_buffer <- left_join(fishing_buffer,
                            MPAs_buffer %>% select(WDPA_PID, NAME, ISO3, IUCN_CAT, STATUS_YR, AREA_KM2), 
                            by = "WDPA_PID") %>%
  mutate(TYPE = "BUFFER", YEAR = 2012) %>%
  select(TYPE, YEAR, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2, everything()) %>%
  select(-geometry, -ID) %>%
  filter(row_number() != 1037 & row_number() != 1039)

write_csv(fishing_buffer,"./Data/Results/MPAs_WDPA/summarized_fishing_buffer_2012.csv")


################################################################################

##### 3. Analysis ####

files <-  list.files(path="./Data/Results/MPAs_WDPA",pattern="*.csv")
myfiles <-  lapply(paste("./Data/Results/MPAs_WDPA/",files,sep=""), read_csv)
total_fishing_pressure <- rbindlist(myfiles)
rm(files, myfiles)

# Creating the dataset with total fishing effort values for every year within and outside MPAs
fishing_pressure <- function(data) {
  
  fishing_pressure_before <- data %>%
    filter(YEAR < STATUS_YR) %>%
    group_by(TYPE, IUCN_CAT, WDPA_PID, NAME, ISO3, STATUS_YR, AREA_KM2) %>%
    summarize(fishing_pressure_before = mean(fishing_pressure, na.rm = TRUE))
  
  fishing_pressure_after <- data %>%  
    filter(YEAR > STATUS_YR) %>%
    group_by(TYPE, WDPA_PID) %>%
    summarize(fishing_pressure_after = mean(fishing_pressure, na.rm = TRUE))
  
  result <- left_join(fishing_pressure_before, fishing_pressure_after, by = c("TYPE", "WDPA_PID"))
  
  return(result)
}

fishing_pressure <- fishing_pressure(total_fishing_pressure)

# Normalizing fishing effort values per total MPA or buffer area
fishing_pressure <- fishing_pressure %>% 
  mutate(
    fishing_pressure_before_norm = fishing_pressure_before / AREA_KM2,
    fishing_pressure_after_norm = fishing_pressure_after / AREA_KM2,
    fishing_pressure_difference = fishing_pressure_after_norm - fishing_pressure_before_norm,
)
print(fishing_pressure)

write_csv(fishing_pressure,"./Data/Results/MPAs_WDPA/Summarized/total_fishing_pressure_10km.csv")
fishing_pressure <- read.csv("./Data/Results/MPAs_WDPA/Summarized/total_fishing_pressure_10km.csv")

##### 3.1. Log ratios - before/after inside ####

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  slice(-422) %>%
  mutate(fishing_pressure_after_norm_log=case_when(
    fishing_pressure_after_norm==0 ~ 0.001,
    TRUE~fishing_pressure_after_norm),
    fishing_pressure_before_norm_log=case_when(
      fishing_pressure_before_norm==0 ~ 0.001,
      TRUE~fishing_pressure_before_norm),
    log.ratio=log(fishing_pressure_after_norm_log/fishing_pressure_before_norm_log),
    IUCN_CAT=factor(IUCN_CAT,levels=rev(c("Ia","Ib","II","III","IV",
                                          "V","VI")))) %>%
  arrange(IUCN_CAT)


fishing10km.MPAs.summary <- fishing10km.MPAs %>%
  group_by(IUCN_CAT) %>%
  summarize(mean.log=mean(log.ratio),
            CI.log=sd(log.ratio)/sqrt(n())*1.96,
            mean.initial=mean(fishing_pressure_before_norm),
            pvalue=wilcox.test(log.ratio,mu=0,paired=FALSE)$p.value,
            sample=n(),
            lowCI=mean.log-CI.log,
            highCI=mean.log+CI.log)%>%
  mutate(IUCN_CAT=factor(IUCN_CAT,levels=rev(c("Ia","Ib","II","III","IV",
                                               "V","VI"))),
         significant=case_when(lowCI*highCI>0~"significant",
                               TRUE~"not significant"),
         fill=case_when(lowCI*highCI>0~"black",
                        TRUE~"white"))

###### 3.1.1. Plots ######

# Change in log ratios before/after inside
ggplot() +
  geom_point(data=fishing10km.MPAs, aes(x=IUCN_CAT, y=log.ratio),color="lightgrey",
             size=0.6)+
  geom_pointrange(data=fishing10km.MPAs.summary, 
                  aes(x=IUCN_CAT, y=mean.log, ymin=lowCI, 
                      ymax=highCI),size= 0.6,shape=21,fill=fishing10km.MPAs.summary$fill,
                  stroke=0.5)+
  coord_flip()+
  theme_classic()+
  geom_segment(aes(y=0,yend=0,x=0,xend=7.5), lty=2, size=0.2)+
  xlab("IUCN category") + ylab("Change in fishing effort (lnRR)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank() +
  geom_text(data=fishing10km.MPAs.summary, aes(label=sample,y=mean.log+0.2,x=(seq(1,7))-0.3), size=3)+
  labs(fill="significant")

# Scatter plot: change in fishing pressure x initial fishing pressure
bai <- ggplot() +
  geom_point(data=fishing10km.MPAs,aes(x=log.ratio,
                                       y=sqrt(sqrt(fishing_pressure_before_norm)),
                                       color=IUCN_CAT),
             size=1,alpha=0.7)+
  geom_point(data=fishing10km.MPAs.summary,aes(x=mean.log,
                                               y=sqrt(sqrt(mean.initial)),
                                               fill = IUCN_CAT),
             size=3,shape=21,stroke=0.5)+
  #,position=position_jitter(height = 2,width=1))+
  #geom_jitter(width = 0.2, height = 0) +
  theme_classic() +
  scale_fill_manual(name="IUCN category", values=c("Ia"= "#0868ac","Ib"="#2f8fc0","II"="#56b0c8",
                                                   "III"="#7bccc4","IV"="#a5dcbf","V"="#ccebcb","VI"="#f0f9e8")) +
  scale_color_manual(name="IUCN category", values=c("Ia"= "#0868ac","Ib"="#2f8fc0","II"="#56b0c8",
                                                    "III"="#7bccc4","IV"="#a5dcbf","V"="#ccebcb","VI"="#f0f9e8")) +
  guides(fill=guide_legend(reverse=TRUE),color=guide_legend(reverse=TRUE)) +
  #theme(legend.position = "none") +
  geom_vline(xintercept = 0, lty = 2, size = 0.2) +
  xlab("Change in fishing effort (lnRR)") + 
  ylab("Initial fishing pressure (hour km-2 year-1)")
print(bai)

bai_marginal <- ggMarginal(bai, type="histogram", bins=50, fill="black", color="white", size = 5)
print(bai_marginal)


###### 3.1.2. Stats ######

# For the initial fishing pressure
# Shapiro's test for normality
fishing_initial <- lm(fishing_pressure_before_norm ~ IUCN_CAT, data = fishing10km.MPAs)
shapiro.test(residuals(fishing_initial)) # not normal

# Welch test (ANOVA non-parametric equivalent)
welch_initial <- oneway.test(fishing_pressure_before_norm ~ IUCN_CAT, data = fishing10km.MPAs, var.equal = FALSE)
print(welch_initial) # no significant difference

# Games-Howell test (Tukey non-parametric equivalent)
games_howell_initial <- games_howell_test(data = fishing10km.MPAs, fishing_pressure_before_norm ~ IUCN_CAT)
print(games_howell_initial, n = 21) # No Groups

# For the change in fishing pressure
# Shapiro's test for normality
fishing_change <- lm(log.ratio ~ IUCN_CAT, data = fishing10km.MPAs)
shapiro.test(residuals(fishing_change)) # not normal

# Welch test (ANOVA non-parametric equivalent)
welch_change <- oneway.test(log.ratio ~ IUCN_CAT, data = fishing10km.MPAs, var.equal = FALSE)
print(welch_change) # significant change

# Games-Howell test (Tukey non-parametric equivalent)
games_howell_change <- games_howell_test(data = fishing10km.MPAs, log.ratio ~ IUCN_CAT)
print(games_howell_change, n = 21) # Groups: Ia-IV, Ia-V and Ia-VI


###### 3.1.3. Linear models ######

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  slice(-422) %>%
  mutate(fishing_pressure_after_norm_log=case_when(fishing_pressure_after_norm==0 ~ 0.001,
                                                   TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm_log=case_when(fishing_pressure_before_norm==0 ~ 0.001,
                                                    TRUE~fishing_pressure_before_norm),
         log.ratio=log(fishing_pressure_after_norm_log/fishing_pressure_before_norm_log),
         AGE = 2020 - STATUS_YR) %>%
  mutate(IUCN_CAT = as.factor(IUCN_CAT)) %>%
  mutate(CLASS = case_when(
    IUCN_CAT %in% c("Ia", "Ib") ~ "Highly regulated",
    IUCN_CAT %in% c("II", "III") ~ "Moderately regulated",
    TRUE ~ "Weakly regulated")) %>%
  mutate(CLASS = factor(CLASS, levels = c("Weakly regulated", "Moderately regulated", "Highly regulated"))) %>%
  arrange(CLASS)

fishing10km.MPAs.summary <- fishing10km.MPAs %>%
  group_by(CLASS) %>%
  do({
    model_age <- lm(log.ratio ~ AGE, data = .)
    pvalue_age <- summary(model_age)$coefficients[2, "Pr(>|t|)"]
    r2_age <- summary(model_age)$r.squared
    
    model_area <- lm(log.ratio ~ log(AREA_KM2), data = .)
    pvalue_area <- summary(model_area)$coefficients[2, "Pr(>|t|)"]
    r2_area <- summary(model_area)$r.squared
    
    data.frame(pvalue_age = pvalue_age, r2_age = r2_age, 
               pvalue_area = pvalue_area, r2_area = r2_area)
  }) %>%
  ungroup() %>%
  mutate(CLASS = factor(CLASS, levels = c("Weakly regulated", "Moderately regulated", "Highly regulated")),
         significant_age = case_when(pvalue_age < 0.05 ~ "significant", TRUE ~ "not significant"),
         linetype_age = case_when(pvalue_age < 0.05 ~ "solid", TRUE ~ "dashed"),
         significant_area = case_when(pvalue_area < 0.05 ~ "significant", TRUE ~ "not significant"),
         linetype_area = case_when(pvalue_area < 0.05 ~ "solid", TRUE ~ "dashed"))

fishing10km.MPAs.summary <- fishing10km.MPAs %>%
  left_join(fishing10km.MPAs.summary %>% select(CLASS, linetype_age, linetype_area), by = "CLASS")

# Age
summary(lm(log.ratio ~ AGE:CLASS, data = fishing10km.MPAs))

ggplot(fishing10km.MPAs, aes(x = AGE, y = log.ratio, color = CLASS)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(data = subset(fishing10km.MPAs.summary, linetype_age == "solid"), 
              aes(x = AGE, y = log.ratio, linetype = linetype_age),
              method = "lm", se = FALSE) +
  geom_smooth(data = subset(fishing10km.MPAs.summary, linetype_age == "dashed"), 
              aes(x = AGE, y = log.ratio, linetype = linetype_age),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Highly regulated" = "#0868ac",
                                "Moderately regulated" = "#56b0c8",
                                "Weakly regulated" = "#a5dcbf")) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                        labels = c("solid" = "Significant", "dashed" = "Not significant")) +
  labs(x = "Age (years)", y = "Fishing pressure (lnRR)", color = "Class of the MPA", linetype = "Significance") +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

# Size
summary(lm(log.ratio ~ log(AREA_KM2):CLASS, data = fishing10km.MPAs))

ggplot(fishing10km.MPAs, aes(x = log(AREA_KM2), y = log.ratio, color = CLASS)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(data = subset(fishing10km.MPAs.summary, linetype_area == "solid"), 
              aes(x = log(AREA_KM2), y = log.ratio, linetype = linetype_area),
              method = "lm", se = FALSE) +
  geom_smooth(data = subset(fishing10km.MPAs.summary, linetype_area == "dashed"), 
              aes(x = log(AREA_KM2), y = log.ratio, linetype = linetype_area),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Highly regulated" = "#0868ac",
                                "Moderately regulated" = "#56b0c8",
                                "Weakly regulated" = "#a5dcbf")) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                        labels = c("solid" = "Significant", "dashed" = "Not significant")) +
  labs(x = "Size (log km²)", y = "Fishing pressure (lnRR)", color = "Class of the MPA", linetype = "Significance") +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))


##### 3.2. Log ratios - before/after outside ####

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference) & WDPA_PID %in% fishing10km.MPAs$WDPA_PID) %>%
  mutate(fishing_pressure_after_norm_log=case_when(
    fishing_pressure_after_norm==0 ~ 0.001,
    TRUE~fishing_pressure_after_norm),
    fishing_pressure_before_norm_log=case_when(
      fishing_pressure_before_norm==0 ~ 0.001,
      TRUE~fishing_pressure_before_norm),
    log.ratio=log(fishing_pressure_after_norm_log/fishing_pressure_before_norm_log),
    IUCN_CAT=factor(IUCN_CAT,levels=rev(c("Ia","Ib","II","III","IV",
                                          "V","VI")))) %>%
  arrange(IUCN_CAT)


fishing10km.buffer.summary <- fishing10km.buffer %>%
  group_by(IUCN_CAT) %>%
  summarize(mean.log=mean(log.ratio),
            CI.log=sd(log.ratio)/sqrt(n())*1.96,
            mean.initial=mean(fishing_pressure_before_norm),
            pvalue=wilcox.test(log.ratio,mu=0,paired=FALSE)$p.value,
            sample=n(),
            lowCI=mean.log-CI.log,
            highCI=mean.log+CI.log)%>%
  mutate(IUCN_CAT=factor(IUCN_CAT,levels=rev(c("Ia","Ib","II","III","IV",
                                               "V","VI"))),
         significant=case_when(lowCI*highCI>0~"significant",
                               TRUE~"not significant"),
         fill=case_when(lowCI*highCI>0~"black",
                        TRUE~"white"))

###### 3.2.1. Plots ######

# Change in log ratios before/after outside
ggplot() +
  geom_point(data=fishing10km.buffer, aes(x=IUCN_CAT, y=log.ratio),color="lightgrey",
             size=0.6)+
  geom_pointrange(data=fishing10km.buffer.summary, 
                  aes(x=IUCN_CAT, y=mean.log, ymin=lowCI, 
                      ymax=highCI),size= 0.6,shape=21,fill=fishing10km.buffer.summary$fill,
                  stroke=0.5)+
  coord_flip()+
  theme_classic()+
  geom_segment(aes(y=0,yend=0,x=0,xend=7.5), lty=2, size=0.2)+
  xlab("IUCN category") + ylab("Change in fishing effort (lnRR)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_text(data=fishing10km.buffer.summary, aes(label=sample,y=mean.log+0.2,x=(seq(1,7))-0.3), size=3)+
  labs(fill="significant")

# Scatter plot: change in fishing pressure x initial fishing pressure
bac <- ggplot() +
  geom_point(data=fishing10km.buffer,aes(x=log.ratio,
                                         y=sqrt(sqrt(fishing_pressure_before_norm)),
                                         color=IUCN_CAT),
             size=1,alpha=0.7)+
  geom_point(data=fishing10km.buffer.summary,aes(x=mean.log,
                                                 y=sqrt(sqrt(mean.initial)),
                                                 fill = IUCN_CAT),
             size=3,shape=21,stroke=0.5)+
  #,position=position_jitter(height = 2,width=1))+
  #geom_jitter(width = 0.2, height = 0) +
  theme_classic() +
  scale_fill_manual(name="IUCN category", values=c("Ia"= "#0868ac","Ib"="#2f8fc0","II"="#56b0c8",
                                                   "III"="#7bccc4","IV"="#a5dcbf","V"="#ccebcb","VI"="#f0f9e8")) +
  scale_color_manual(name="IUCN category", values=c("Ia"= "#0868ac","Ib"="#2f8fc0","II"="#56b0c8",
                                                    "III"="#7bccc4","IV"="#a5dcbf","V"="#ccebcb","VI"="#f0f9e8")) +
  guides(fill=guide_legend(reverse=TRUE),color=guide_legend(reverse=TRUE)) +
  #theme(legend.position = "none") +
  geom_vline(xintercept=0, lty = 2, size = 0.2) +
  xlab("Change in fishing effort (lnRR)") + 
  ylab("Initial fishing pressure (hour km-2 year-1)")
print(bac)

bac_marginal <- ggMarginal(bac, type="histogram", bins=50, fill="black", color="white", size = 5)
print(bac_marginal)


###### 3.2.2. Stats ######

# For the initial fishing pressure
# Shapiro's test for normality
fishing_initial <- lm(fishing_pressure_before_norm ~ IUCN_CAT, data = fishing10km.buffer)
shapiro.test(residuals(fishing_initial)) # not normal

# Welch test (ANOVA non-parametric equivalent)
welch_initial <- oneway.test(fishing_pressure_before_norm ~ IUCN_CAT, data = fishing10km.buffer, var.equal = FALSE)
print(welch_initial) # significant difference

# Games-Howell test (Tukey non-parametric equivalent)
games_howell_initial <- games_howell_test(data = fishing10km.buffer, fishing_pressure_before_norm ~ IUCN_CAT)
print(games_howell_initial, n = 21) # Groups: III-IV, IV-VI

# For the change in fishing pressure
# Shapiro's test for normality
fishing_change <- lm(log.ratio ~ IUCN_CAT, data = fishing10km.buffer)
shapiro.test(residuals(fishing_change)) # not normal

# Welch test (ANOVA non-parametric equivalent)
welch_change <- oneway.test(log.ratio ~ IUCN_CAT, data = fishing10km.buffer, var.equal = FALSE)
print(welch_change) # significant change

# Games-Howell test (Tukey non-parametric equivalent)
games_howell_change <- games_howell_test(data = fishing10km.buffer, log.ratio ~ IUCN_CAT)
print(games_howell_change, n = 21) # Groups: Ia-V, Ia-VI, IV-VI


###### 3.2.3. Linear models ######

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference) & WDPA_PID %in% fishing10km.MPAs$WDPA_PID) %>%
  mutate(fishing_pressure_after_norm_log=case_when(fishing_pressure_after_norm==0 ~ 0.001,
                                                   TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm_log=case_when(fishing_pressure_before_norm==0 ~ 0.001,
                                                    TRUE~fishing_pressure_before_norm),
         log.ratio=log(fishing_pressure_after_norm_log/fishing_pressure_before_norm_log),
         AGE = 2020 - STATUS_YR) %>%
  mutate(IUCN_CAT = as.factor(IUCN_CAT)) %>%
  mutate(CLASS = case_when(
    IUCN_CAT %in% c("Ia", "Ib") ~ "Highly regulated",
    IUCN_CAT %in% c("II", "III") ~ "Moderately regulated",
    TRUE ~ "Weakly regulated")) %>%
  mutate(CLASS = factor(CLASS, levels = c("Weakly regulated", "Moderately regulated", "Highly regulated"))) %>%
  arrange(CLASS)

fishing10km.buffer.summary <- fishing10km.buffer %>%
  group_by(CLASS) %>%
  do({
    model_age <- lm(log.ratio ~ AGE, data = .)
    pvalue_age <- summary(model_age)$coefficients[2, "Pr(>|t|)"]
    r2_age <- summary(model_age)$r.squared
    
    model_area <- lm(log.ratio ~ log(AREA_KM2), data = .)
    pvalue_area <- summary(model_area)$coefficients[2, "Pr(>|t|)"]
    r2_area <- summary(model_area)$r.squared
    
    data.frame(pvalue_age = pvalue_age, r2_age = r2_age, 
               pvalue_area = pvalue_area, r2_area = r2_area)
  }) %>%
  ungroup() %>%
  mutate(CLASS = factor(CLASS, levels = c("Weakly regulated", "Moderately regulated", "Highly regulated")),
         significant_age = case_when(pvalue_age < 0.05 ~ "significant", TRUE ~ "not significant"),
         linetype_age = case_when(pvalue_age < 0.05 ~ "solid", TRUE ~ "dashed"),
         significant_area = case_when(pvalue_area < 0.05 ~ "significant", TRUE ~ "not significant"),
         linetype_area = case_when(pvalue_area < 0.05 ~ "solid", TRUE ~ "dashed"))


fishing10km.buffer.summary <- fishing10km.buffer %>%
  left_join(fishing10km.buffer.summary %>% select(CLASS, linetype_age, linetype_area), by = "CLASS")

# Age
summary(lm(log.ratio ~ AGE:CLASS, data = fishing10km.buffer))

ggplot(fishing10km.buffer, aes(x = AGE, y = log.ratio, color = CLASS)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(data = subset(fishing10km.buffer.summary, linetype_age == "solid"), 
              aes(x = AGE, y = log.ratio, linetype = linetype_age),
              method = "lm", se = FALSE) +
  geom_smooth(data = subset(fishing10km.buffer.summary, linetype_age == "dashed"), 
              aes(x = AGE, y = log.ratio, linetype = linetype_age),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Highly regulated" = "#0868ac",
                                "Moderately regulated" = "#56b0c8",
                                "Weakly regulated" = "#a5dcbf")) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                        labels = c("solid" = "Significant", "dashed" = "Not significant")) +
  labs(x = "Age (years)", y = "Fishing pressure (lnRR)", color = "Class of the MPA", linetype = "Significance") +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

# Size
summary(lm(log.ratio ~ log(AREA_KM2):CLASS, data = fishing10km.buffer))

ggplot(fishing10km.buffer, aes(x = log(AREA_KM2), y = log.ratio, color = CLASS)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(data = subset(fishing10km.buffer.summary, linetype_area == "solid"), 
              aes(x = log(AREA_KM2), y = log.ratio, linetype = linetype_area),
              method = "lm", se = FALSE) +
  geom_smooth(data = subset(fishing10km.buffer.summary, linetype_area == "dashed"), 
              aes(x = log(AREA_KM2), y = log.ratio, linetype = linetype_area),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Highly regulated" = "#0868ac",
                                "Moderately regulated" = "#56b0c8",
                                "Weakly regulated" = "#a5dcbf")) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                        labels = c("solid" = "Significant", "dashed" = "Not significant")) +
  labs(x = "Size (log km²)", y = "Fishing pressure (lnRR)", color = "Class of the MPA", linetype = "Significance") +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))


##### 3.3. Log ratios - inside/outside after ####

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  mutate(fishing_pressure_after_norm=case_when(
    fishing_pressure_after_norm==0 ~ 0.001,
    TRUE~fishing_pressure_after_norm),
    fishing_pressure_before_norm=case_when(
      fishing_pressure_before_norm==0 ~ 0.001,
      TRUE~fishing_pressure_before_norm))

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference)) %>%
  mutate(fishing_pressure_after_norm=case_when(
    fishing_pressure_after_norm==0 ~ 0.001,
    TRUE~fishing_pressure_after_norm),
    fishing_pressure_before_norm=case_when(
      fishing_pressure_before_norm==0 ~ 0.001,
      TRUE~fishing_pressure_before_norm)) 

fishing10km.log <-  fishing10km.MPAs %>%
  select(WDPA_PID, fishing_pressure_before_norm,fishing_pressure_after_norm,IUCN_CAT) %>%
  left_join(fishing10km.buffer %>% 
              select(WDPA_PID, fishing_pressure_before_norm,fishing_pressure_after_norm),
            by= "WDPA_PID",suffix=c(".in",".out")) %>%
  mutate(log.ratio = log(fishing_pressure_after_norm.in/fishing_pressure_after_norm.out),
         IUCN_CAT=factor(IUCN_CAT,levels=rev(c("Ia","Ib","II","III","IV",
                                               "V","VI")))) %>%
  arrange(IUCN_CAT)%>%
  filter(!is.na(log.ratio))


fishing10km.log.summary <- fishing10km.log %>%
  group_by(IUCN_CAT) %>%
  summarize(mean.log=mean(log.ratio),
            CI.log=sd(log.ratio)/sqrt(n())*1.96,
            mean.initial=mean(fishing_pressure_before_norm.in),
            pvalue=wilcox.test(log.ratio,mu=0,paired=FALSE)$p.value,
            sample=n(),
            lowCI=mean.log-CI.log,
            highCI=mean.log+CI.log)%>%
  mutate(IUCN_CAT=factor(IUCN_CAT,levels=rev(c("Ia","Ib","II","III","IV",
                                               "V","VI"))),
         significant=case_when(lowCI*highCI>0~"significant",
                               TRUE~"not significant"),
         fill=case_when(lowCI*highCI>0~"black",
                        TRUE~"white"))

###### 3.3.1. Plots ######

# Change in log ratios inside/outside after
ggplot() +
  geom_point(data=fishing10km.log, aes(x=IUCN_CAT, y=log.ratio),color="lightgrey",
             size=0.6)+
  geom_pointrange(data=fishing10km.log.summary, 
                  aes(x=IUCN_CAT, y=mean.log, ymin=lowCI, 
                      ymax=highCI),size= 0.6,shape=21,fill=fishing10km.log.summary$fill,
                  stroke=0.5)+
  coord_flip()+
  theme_classic()+
  geom_segment(aes(y=0,yend=0,x=0,xend=7.5), lty=2, size=0.2)+
  xlab("IUCN category") + ylab("Change in fishing effort (lnRR)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_text(data=fishing10km.log.summary, aes(label=sample,y=mean.log+0.2,x=(seq(1,7))-0.3), size=3)+
  labs(fill="significant")


###### 3.3.2. Stats ######

# For the change in fishing pressure
# Shapiro's test for normality
fishing_change <- lm(log.ratio ~ IUCN_CAT, data = fishing10km.log)
shapiro.test(residuals(fishing_change)) # not normal

# Welch test (ANOVA non-parametric equivalent)
welch_change <- oneway.test(log.ratio ~ IUCN_CAT, data = fishing10km.log, var.equal = FALSE)
print(welch_change) # significant change

# Games-Howell test (Tukey non-parametric equivalent)
games_howell_change <- games_howell_test(data = fishing10km.log, log.ratio ~ IUCN_CAT)
print(games_howell_change, n = 21) # Groups: Ia-IV, Ia-V and Ia-VI


###### 3.3.3. Linear models ######

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  mutate(fishing_pressure_after_norm=case_when(fishing_pressure_after_norm==0 ~ 0.001,
                                               TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(fishing_pressure_before_norm==0 ~ 0.001,
                                                TRUE~fishing_pressure_before_norm))

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference)) %>%
  mutate(fishing_pressure_after_norm=case_when(fishing_pressure_after_norm==0 ~ 0.001,
                                               TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(fishing_pressure_before_norm==0 ~ 0.001,
                                                TRUE~fishing_pressure_before_norm)) 

fishing10km.log <-  fishing10km.MPAs %>%
  select(WDPA_PID, fishing_pressure_before_norm,fishing_pressure_after_norm,IUCN_CAT,STATUS_YR,AREA_KM2) %>%
  left_join(fishing10km.buffer %>% 
              select(WDPA_PID, fishing_pressure_before_norm,fishing_pressure_after_norm),
            by= "WDPA_PID",suffix=c(".in",".out")) %>%
  mutate(log.ratio = log(fishing_pressure_after_norm.in/fishing_pressure_after_norm.out),
         AGE = 2020 - STATUS_YR) %>%
  mutate(IUCN_CAT = as.factor(IUCN_CAT)) %>%
  mutate(CLASS = case_when(
    IUCN_CAT %in% c("Ia", "Ib") ~ "Highly regulated",
    IUCN_CAT %in% c("II", "III") ~ "Moderately regulated",
    TRUE ~ "Weakly regulated")) %>%
  mutate(CLASS = factor(CLASS, levels = c("Weakly regulated", "Moderately regulated", "Highly regulated"))) %>%
  arrange(CLASS) %>%
  filter(!is.na(log.ratio))

fishing10km.log.summary <- fishing10km.log %>%
  group_by(CLASS) %>%
  do({
    model_age <- lm(log.ratio ~ AGE, data = .)
    pvalue_age <- summary(model_age)$coefficients[2, "Pr(>|t|)"]
    r2_age <- summary(model_age)$r.squared
    
    model_area <- lm(log.ratio ~ log(AREA_KM2), data = .)
    pvalue_area <- summary(model_area)$coefficients[2, "Pr(>|t|)"]
    r2_area <- summary(model_area)$r.squared
    
    data.frame(pvalue_age = pvalue_age, r2_age = r2_age, 
               pvalue_area = pvalue_area, r2_area = r2_area)
  }) %>%
  ungroup() %>%
  mutate(CLASS = factor(CLASS, levels = c("Weakly regulated", "Moderately regulated", "Highly regulated")),
         significant_age = case_when(pvalue_age < 0.05 ~ "significant", TRUE ~ "not significant"),
         linetype_age = case_when(pvalue_age < 0.05 ~ "solid", TRUE ~ "dashed"),
         significant_area = case_when(pvalue_area < 0.05 ~ "significant", TRUE ~ "not significant"),
         linetype_area = case_when(pvalue_area < 0.05 ~ "solid", TRUE ~ "dashed"))

fishing10km.log.summary <- fishing10km.log %>%
  left_join(fishing10km.log.summary %>% select(CLASS, linetype_age, linetype_area), by = "CLASS")

# Age
summary(lm(log.ratio ~ AGE:CLASS, data = fishing10km.log))

ggplot(fishing10km.log, aes(x = AGE, y = log.ratio, color = CLASS)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(data = subset(fishing10km.log.summary, linetype_age == "solid"), 
              aes(x = AGE, y = log.ratio, linetype = linetype_age),
              method = "lm", se = FALSE) +
  geom_smooth(data = subset(fishing10km.log.summary, linetype_age == "dashed"), 
              aes(x = AGE, y = log.ratio, linetype = linetype_age),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Highly regulated" = "#0868ac",
                                "Moderately regulated" = "#56b0c8",
                                "Weakly regulated" = "#a5dcbf")) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                        labels = c("solid" = "Significant", "dashed" = "Not significant")) +
  labs(x = "Age (years)", y = "Fishing pressure (lnRR)", color = "Class of the MPA", linetype = "Significance") +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

# Size
summary(lm(log.ratio ~ log(AREA_KM2):CLASS, data = fishing10km.log))

ggplot(fishing10km.log, aes(x = log(AREA_KM2), y = log.ratio, color = CLASS)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(data = subset(fishing10km.log.summary, linetype_area == "solid"), 
              aes(x = log(AREA_KM2), y = log.ratio, linetype = linetype_area),
              method = "lm", se = FALSE) +
  geom_smooth(data = subset(fishing10km.log.summary, linetype_area == "dashed"), 
              aes(x = log(AREA_KM2), y = log.ratio, linetype = linetype_area),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Highly regulated" = "#0868ac",
                                "Moderately regulated" = "#56b0c8",
                                "Weakly regulated" = "#a5dcbf")) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                        labels = c("solid" = "Significant", "dashed" = "Not significant")) +
  labs(x = "Size (log km²)", y = "Fishing pressure (lnRR)", color = "Class of the MPA", linetype = "Significance") +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))


##### 3.4. Log ratios - before/after inside/outside #####

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  mutate(fishing_pressure_after_norm=case_when(
    fishing_pressure_after_norm==0 ~ 0.001,
    TRUE~fishing_pressure_after_norm),
    fishing_pressure_before_norm=case_when(
      fishing_pressure_before_norm==0 ~ 0.001,
      TRUE~fishing_pressure_before_norm))

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference)) %>%
  mutate(fishing_pressure_after_norm=case_when(
    fishing_pressure_after_norm==0 ~ 0.001,
    TRUE~fishing_pressure_after_norm),
    fishing_pressure_before_norm=case_when(
      fishing_pressure_before_norm==0 ~ 0.001,
      TRUE~fishing_pressure_before_norm))  

fishing10km.baci <-  fishing10km.MPAs %>%
  select(WDPA_PID, fishing_pressure_before_norm,fishing_pressure_after_norm,IUCN_CAT) %>%
  left_join(fishing10km.buffer %>% 
              select(WDPA_PID, fishing_pressure_before_norm,fishing_pressure_after_norm),
            by= "WDPA_PID",suffix=c(".in",".out")) %>%
  mutate(log.ratio = log((fishing_pressure_after_norm.in/fishing_pressure_before_norm.in) /
                           (fishing_pressure_after_norm.out/fishing_pressure_before_norm.out)),
         IUCN_CAT=factor(IUCN_CAT,levels=rev(c("Ia","Ib","II","III","IV",
                                               "V","VI")))) %>%
  arrange(IUCN_CAT)%>%
  filter(!is.na(log.ratio)) #should remove only one line, for which no buffer value (WDPA #555682336, check why)

fishing10km.baci.summary <- fishing10km.baci %>%
  group_by(IUCN_CAT) %>%
  summarize(mean.baci=mean(log.ratio),
            CI.baci = sd(log.ratio)/sqrt(n())*1.96,
            mean.initial=mean(fishing_pressure_before_norm.in),
            pvalue=wilcox.test(mean.baci,mu=0,paired=FALSE)$p.value,
            sample=n(),
            lowCI=mean.baci-CI.baci,
            highCI=mean.baci+CI.baci)%>%
  mutate(IUCN_CAT=factor(IUCN_CAT,levels=rev(c("Ia","Ib","II","III","IV",
                                               "V","VI"))),
         significant=case_when(lowCI*highCI>0~"significant",
                               TRUE~"not significant"),
         fill=case_when(lowCI*highCI>0~"black",
                        TRUE~"white"))%>%
  arrange(IUCN_CAT)

###### 3.4.1. Plots ######

# Change in log ratios before/after inside/outside
ggplot() +
  geom_point(data=fishing10km.baci, aes(x=IUCN_CAT, y=log.ratio),color="lightgrey",
             size=0.6)+
  geom_pointrange(data=fishing10km.baci.summary, 
                  aes(x=IUCN_CAT, y=mean.baci, ymin=lowCI, 
                      ymax=highCI),size= 0.6,shape=21,fill=fishing10km.baci.summary$fill,
                  stroke=0.5)+
  coord_flip()+
  theme_classic()+
  geom_segment(aes(y=0,yend=0,x=0,xend=7.5), lty=2, size=0.2)+
  xlab("IUCN category") + ylab("Change in fishing effort (lnRR)")+
  #theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_text(data=fishing10km.baci.summary, aes(label=sample,y=mean.baci+0.1,x=(seq(1,7))-0.3), size=2.5)+
  labs(fill="significant")

# Scatter plot: change in fishing pressure x initial fishing pressure
baci <- ggplot() +
  geom_point(data=fishing10km.baci, aes(x=log.ratio,
                                        y=sqrt(sqrt(fishing_pressure_before_norm.in)),
                                        color=IUCN_CAT),
             size=1, alpha=0.7) +
  geom_point(data=fishing10km.baci.summary, aes(x=mean.baci,
                                                y=sqrt(sqrt(mean.initial)),
                                                fill=IUCN_CAT),
             size=3, shape=21, stroke=0.5) +
  theme_classic() +
  scale_fill_manual(name="IUCN category", values=c("Ia"= "#0868ac","Ib"="#2f8fc0","II"="#56b0c8",
                                                   "III"="#7bccc4","IV"="#a5dcbf","V"="#ccebcb","VI"="#f0f9e8")) +
  scale_color_manual(name="IUCN category", values=c("Ia"= "#0868ac","Ib"="#2f8fc0","II"="#56b0c8",
                                                    "III"="#7bccc4","IV"="#a5dcbf","V"="#ccebcb","VI"="#f0f9e8")) +
  guides(fill=guide_legend(reverse=TRUE), color=guide_legend(reverse=TRUE)) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_text(size = 12)) +
  geom_vline(xintercept=0, linetype=2, size=0.2) +
  xlab("Change in fishing effort (lnRR)") + 
  ylab("Initial fishing pressure (hour km-2 year-1)")
print(baci)

baci_marginal <- ggMarginal(baci, type="histogram", bins=50, fill="black", color="white", size = 5)
print(baci_marginal)


###### 3.4.2. Stats #####

# For the change in fishing pressure
# Shapiro's test for normality
fishing_change <- lm(log.ratio ~ IUCN_CAT, data = fishing10km.baci)
shapiro.test(residuals(fishing_change)) # not normal

# Welch test (ANOVA non-parametric equivalent)
welch_change <- oneway.test(log.ratio ~ IUCN_CAT, data = fishing10km.baci, var.equal = FALSE)
print(welch_change) # significant change

# Games-Howell test (Tukey non-parametric equivalent)
games_howell_change <- games_howell_test(data = fishing10km.baci, log.ratio ~ IUCN_CAT)
print(games_howell_change, n = 21) # Groups: Ia-IV, Ia-V and Ia-VI


###### 3.4.3. Linear models #####

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA" & !is.na(fishing_pressure_difference)) %>%
  mutate(fishing_pressure_after_norm=case_when(fishing_pressure_after_norm==0 ~ 0.001,
                                               TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(fishing_pressure_before_norm==0 ~ 0.001,
                                                TRUE~fishing_pressure_before_norm))

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER" & !is.na(fishing_pressure_difference)) %>%
  mutate(fishing_pressure_after_norm=case_when(fishing_pressure_after_norm==0 ~ 0.001,
                                               TRUE~fishing_pressure_after_norm),
         fishing_pressure_before_norm=case_when(fishing_pressure_before_norm==0 ~ 0.001,
                                                TRUE~fishing_pressure_before_norm)) 

fishing10km.baci <-  fishing10km.MPAs %>%
  select(WDPA_PID, fishing_pressure_before_norm,fishing_pressure_after_norm,IUCN_CAT,STATUS_YR,AREA_KM2) %>%
  left_join(fishing10km.buffer %>% 
              select(WDPA_PID, fishing_pressure_before_norm,fishing_pressure_after_norm),
            by= "WDPA_PID",suffix=c(".in",".out")) %>%
  mutate(log.ratio = log((fishing_pressure_after_norm.in/fishing_pressure_before_norm.in) /
                           (fishing_pressure_after_norm.out/fishing_pressure_before_norm.out)),
         AGE = 2020 - STATUS_YR) %>%
  mutate(IUCN_CAT = as.factor(IUCN_CAT)) %>%
  mutate(CLASS = case_when(
    IUCN_CAT %in% c("Ia", "Ib") ~ "Highly regulated",
    IUCN_CAT %in% c("II", "III") ~ "Moderately regulated",
    TRUE ~ "Weakly regulated")) %>%
  mutate(CLASS = factor(CLASS, levels = c("Weakly regulated", "Moderately regulated", "Highly regulated"))) %>%
  arrange(CLASS) %>%
  filter(!is.na(log.ratio))

fishing10km.baci.summary <- fishing10km.baci %>%
  group_by(CLASS) %>%
  do({
    model_age <- lm(log.ratio ~ AGE, data = .)
    pvalue_age <- summary(model_age)$coefficients[2, "Pr(>|t|)"]
    r2_age <- summary(model_age)$r.squared
    
    model_area <- lm(log.ratio ~ log(AREA_KM2), data = .)
    pvalue_area <- summary(model_area)$coefficients[2, "Pr(>|t|)"]
    r2_area <- summary(model_area)$r.squared
    
    data.frame(pvalue_age = pvalue_age, r2_age = r2_age, 
               pvalue_area = pvalue_area, r2_area = r2_area)
  }) %>%
  ungroup() %>%
  mutate(CLASS = factor(CLASS, levels = c("Weakly regulated", "Moderately regulated", "Highly regulated")),
         significant_age = case_when(pvalue_age < 0.05 ~ "significant", TRUE ~ "not significant"),
         linetype_age = case_when(pvalue_age < 0.05 ~ "solid", TRUE ~ "dashed"),
         significant_area = case_when(pvalue_area < 0.05 ~ "significant", TRUE ~ "not significant"),
         linetype_area = case_when(pvalue_area < 0.05 ~ "solid", TRUE ~ "dashed"))

fishing10km.baci.summary <- fishing10km.baci %>%
  left_join(fishing10km.baci.summary %>% select(CLASS, linetype_age, linetype_area), by = "CLASS")

# Age
summary(lm(log.ratio ~ AGE:CLASS, data = fishing10km.baci))

ggplot(fishing10km.baci, aes(x = AGE, y = log.ratio, color = CLASS)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(data = subset(fishing10km.baci.summary, linetype_age == "solid"), 
              aes(x = AGE, y = log.ratio, linetype = linetype_age),
              method = "lm", se = FALSE) +
  geom_smooth(data = subset(fishing10km.baci.summary, linetype_age == "dashed"), 
              aes(x = AGE, y = log.ratio, linetype = linetype_age),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Highly regulated" = "#0868ac",
                                "Moderately regulated" = "#56b0c8",
                                "Weakly regulated" = "#a5dcbf")) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                        labels = c("solid" = "Significant", "dashed" = "Not significant")) +
  labs(x = "Age (years)", y = "Fishing pressure (lnRR)", color = "Class of the MPA", linetype = "Significance") +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        #axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text = element_text(size = 12))

# Size
summary(lm(log.ratio ~ log(AREA_KM2):CLASS, data = fishing10km.baci))

ggplot(fishing10km.baci, aes(x = log(AREA_KM2), y = log.ratio, color = CLASS)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(data = subset(fishing10km.baci.summary, linetype_area == "solid"), 
              aes(x = log(AREA_KM2), y = log.ratio, linetype = linetype_area),
              method = "lm", se = FALSE) +
  geom_smooth(data = subset(fishing10km.baci.summary, linetype_area == "dashed"), 
              aes(x = log(AREA_KM2), y = log.ratio, linetype = linetype_area),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Highly regulated" = "#0868ac",
                                "Moderately regulated" = "#56b0c8",
                                "Weakly regulated" = "#a5dcbf")) +
  scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed"),
                        labels = c("solid" = "Significant", "dashed" = "Not significant")) +
  labs(x = "Size (log km²)", y = "Fishing pressure (lnRR)", color = "Class of the MPA", linetype = "Significance") +
  geom_hline(yintercept = 0, lty = 2, linewidth = 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        #axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text = element_text(size = 12))


################################################################################


