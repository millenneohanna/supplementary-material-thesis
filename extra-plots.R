################################################################################

# Contribution of marine protected areas to abate industrial fishing effort within and nearby their boundaries
# Millenne Ohanna S. M. S. Barreto, Juliette Jacquemont and Joachim Claudet

################################################################################

# Loading packages

library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggspatial)
library(scales)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)

################################################################################

#### 1. Data ####

# Loading MPAs' data
MPAs_bind <- vect("./Data/WDPA/MPAs_bind.shp")
MPAs_bind <- st_as_sf(MPAs_bind)
#MPAs_bind <- MPAs_bind[-c(552,560,591,842,1063,1064,1065,1111,1120,1142,1224,1231,1232,1262),] # For 1km
MPAs_bind <- MPAs_bind[-c(552,559,560,591,694,842,1063,1064,1065,1080,1111,1120,1142,1224,1231,1232,1262),] # For 10km
#MPAs_bind <- MPAs_bind[-c(551,552,559,560,591,694,695,842,1063,1064,1065,1080,1111,1120,1141,1142,1224,1231,1232,1262),] # For 20km

# MPAs artificially bigger for zoomed maps
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

################################################################################

#### 2. Spatial plots of the results ####

fishing_pressure <- read.csv("./Data/Results/MPAs_WDPA/Summarized/total_fishing_pressure_10km.csv")

##### 2.1. Before/after inside #####

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA") %>%
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

# Joining with MPAs_bind for world map
MPAs_bind_results <- left_join(MPAs_bind,
                               select(fishing10km.MPAs, WDPA_PID, log.ratio),
                               by = "WDPA_PID")
MPAs_bind_results <- MPAs_bind_results %>%
  mutate(change_fp = case_when(
    log.ratio < 0 ~ "decrease",
    log.ratio == 0 ~ "no_change",
    log.ratio > 0 ~ "increase",
    is.na(log.ratio) ~ "no_data"))

# Plotting MPAs' map showing the change in fishing effort
ggplot() +
  geom_sf(data = land, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe"),
                    name = "Change in fishing effort",
                    labels = c("decrease" = "Decrease", "no_change" = "No change", 
                               "increase" = "Increase", "no_data" = "No data"),
                    breaks = c("decrease", "no_change", "increase", "no_data")) +
  
  
  # Adding squares for zoom
  geom_sf(data = zoom_latam, fill = "transparent", color = "black") +
  geom_sf(data = zoom_eu, fill = "transparent", color = "black") +
  geom_sf(data = zoom_aus, fill = "transparent", color = "black") +
  geom_sf(data = zoom_jpn, fill = "transparent", color = "black") +
  
  coord_sf(crs = "+proj=moll") +
  theme_void() +
  theme(legend.position = "none")

# Joining with MPAs_bind_buf for zoomed maps
MPAs_bind_results <- left_join(MPAs_bind_buf,
                               select(fishing10km.MPAs, WDPA_PID, log.ratio),
                               by = "WDPA_PID")
MPAs_bind_results <- MPAs_bind_results %>%
  mutate(change_fp = case_when(
    log.ratio < 0 ~ "decrease",
    log.ratio == 0 ~ "no_change",
    log.ratio > 0 ~ "increase",
    is.na(log.ratio) ~ "no_data"))

# Plotting the map zooming in LatAm
map_latam <- st_crop(land, xmin = -63, xmax = -113, ymin = -4, ymax = 31)

ggplot() +
  geom_sf(data = map_latam, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(-63, -113), ylim = c(-4, 31)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Europe
map_eu <- st_crop(land, xmin = -5, xmax = 30, ymin = 35, ymax = 65)

ggplot() +
  geom_sf(data = map_eu, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(-5, 30), ylim = c(35, 65)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Australia
map_aus <- st_crop(land, xmin = 110, xmax = 160, ymin = -43, ymax = -11)

ggplot() +
  geom_sf(data = map_aus, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(110, 160), ylim = c(-43, -11)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Japan
map_jpn <- st_crop(land, xmin = 115, xmax = 150, ymin = 27, ymax = 42)

ggplot() +
  geom_sf(data = map_jpn, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(124, 146), ylim = c(26, 43)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")


##### 2.2. Before/after outside #####

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER") %>%
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

# Joining with MPAs_bind for world map
MPAs_bind_results <- left_join(MPAs_bind,
                               select(fishing10km.buffer, WDPA_PID, log.ratio),
                               by = "WDPA_PID")
MPAs_bind_results <- MPAs_bind_results %>%
  mutate(change_fp = case_when(
    log.ratio < 0 ~ "decrease",
    log.ratio == 0 ~ "no_change",
    log.ratio > 0 ~ "increase",
    is.na(log.ratio) ~ "no_data"))

# Plotting MPAs' map showing the change in fishing effort
ggplot() +
  geom_sf(data = land, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe"),
                    name = "Change in fishing effort",
                    labels = c("decrease" = "Decrease", "no_change" = "No change", 
                               "increase" = "Increase", "no_data" = "No data"),
                    breaks = c("decrease", "no_change", "increase", "no_data")) +
  
  # Adding squares for zoom
  geom_sf(data = zoom_latam, fill = "transparent", color = "black") +
  geom_sf(data = zoom_eu, fill = "transparent", color = "black") +
  geom_sf(data = zoom_aus, fill = "transparent", color = "black") +
  geom_sf(data = zoom_jpn, fill = "transparent", color = "black") +
  
  coord_sf(crs = "+proj=moll") +
  theme_void() +
  theme(legend.position = "none")

# Joining with MPAs_bind_buf for zoomed maps
MPAs_bind_results <- left_join(MPAs_bind_buf,
                               select(fishing10km.buffer, WDPA_PID, log.ratio),
                               by = "WDPA_PID")
MPAs_bind_results <- MPAs_bind_results %>%
  mutate(change_fp = case_when(
    log.ratio < 0 ~ "decrease",
    log.ratio == 0 ~ "no_change",
    log.ratio > 0 ~ "increase",
    is.na(log.ratio) ~ "no_data"))

# Plotting the map zooming in LatAm
map_latam <- st_crop(land, xmin = -63, xmax = -113, ymin = -4, ymax = 31)

ggplot() +
  geom_sf(data = map_latam, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(-63, -113), ylim = c(-4, 31)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Europe
map_eu <- st_crop(land, xmin = -5, xmax = 30, ymin = 35, ymax = 65)

ggplot() +
  geom_sf(data = map_eu, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(-5, 30), ylim = c(35, 65)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Australia
map_aus <- st_crop(land, xmin = 110, xmax = 160, ymin = -43, ymax = -11)

ggplot() +
  geom_sf(data = map_aus, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(110, 160), ylim = c(-43, -11)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Japan
map_jpn <- st_crop(land, xmin = 115, xmax = 150, ymin = 27, ymax = 42)

ggplot() +
  geom_sf(data = map_jpn, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(124, 146), ylim = c(26, 43)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")


##### 2.3. After inside/outside #####

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA") %>%
  mutate(fishing_pressure_after_norm=case_when(
    fishing_pressure_after_norm==0 ~ 0.001,
    TRUE~fishing_pressure_after_norm),
    fishing_pressure_before_norm=case_when(
      fishing_pressure_before_norm==0 ~ 0.001,
      TRUE~fishing_pressure_before_norm))

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER") %>%
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
  arrange(IUCN_CAT)

# Joining with MPAs_bind for world map
MPAs_bind_results <- left_join(MPAs_bind,
                               select(fishing10km.log, WDPA_PID, log.ratio),
                               by = "WDPA_PID")
MPAs_bind_results <- MPAs_bind_results %>%
  mutate(change_fp = case_when(
    log.ratio < 0 ~ "decrease",
    log.ratio == 0 ~ "no_change",
    log.ratio > 0 ~ "increase",
    is.na(log.ratio) ~ "no_data"))

# Plotting MPAs' map showing the change in fishing effort
ggplot() +
  geom_sf(data = land, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe"),
                    name = "Change in fishing effort",
                    labels = c("decrease" = "Decrease", "no_change" = "No change", 
                               "increase" = "Increase", "no_data" = "No data"),
                    breaks = c("decrease", "no_change", "increase", "no_data")) +
  
  # Adding squares for zoom
  geom_sf(data = zoom_latam, fill = "transparent", color = "black") +
  geom_sf(data = zoom_eu, fill = "transparent", color = "black") +
  geom_sf(data = zoom_aus, fill = "transparent", color = "black") +
  geom_sf(data = zoom_jpn, fill = "transparent", color = "black") +
  
  coord_sf(crs = "+proj=moll") +
  theme_void() +
  theme(legend.position = "none")

# Joining with MPAs_bind for zoomed maps
MPAs_bind_results <- left_join(MPAs_bind_buf,
                               select(fishing10km.log, WDPA_PID, log.ratio),
                               by = "WDPA_PID")
MPAs_bind_results <- MPAs_bind_results %>%
  mutate(change_fp = case_when(
    log.ratio < 0 ~ "decrease",
    log.ratio == 0 ~ "no_change",
    log.ratio > 0 ~ "increase",
    is.na(log.ratio) ~ "no_data"))

# Plotting the map zooming in LatAm
map_latam <- st_crop(land, xmin = -63, xmax = -113, ymin = -4, ymax = 31)

ggplot() +
  geom_sf(data = map_latam, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(-63, -113), ylim = c(-4, 31)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Europe
map_eu <- st_crop(land, xmin = -5, xmax = 30, ymin = 35, ymax = 65)

ggplot() +
  geom_sf(data = map_eu, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(-5, 30), ylim = c(35, 65)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Australia
map_aus <- st_crop(land, xmin = 110, xmax = 160, ymin = -43, ymax = -11)

ggplot() +
  geom_sf(data = map_aus, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(110, 160), ylim = c(-43, -11)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Japan
map_jpn <- st_crop(land, xmin = 115, xmax = 150, ymin = 27, ymax = 42)

ggplot() +
  geom_sf(data = map_jpn, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(124, 146), ylim = c(26, 43)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")


##### 2.4. Before/after inside/outside #####

fishing10km.MPAs <- fishing_pressure %>%
  filter(TYPE=="MPA") %>%
  mutate(fishing_pressure_after_norm=case_when(
    fishing_pressure_after_norm==0 ~ 0.001,
    TRUE~fishing_pressure_after_norm),
    fishing_pressure_before_norm=case_when(
      fishing_pressure_before_norm==0 ~ 0.001,
      TRUE~fishing_pressure_before_norm))

fishing10km.buffer <- fishing_pressure %>%
  filter(TYPE=="BUFFER") %>%
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
  arrange(IUCN_CAT)

# Joining with MPAs_bind for world map
MPAs_bind_results <- left_join(MPAs_bind,
                               select(fishing10km.baci, WDPA_PID, log.ratio),
                               by = "WDPA_PID")
MPAs_bind_results <- MPAs_bind_results %>%
  mutate(change_fp = case_when(
    log.ratio < 0 ~ "decrease",
    log.ratio == 0 ~ "no_change",
    log.ratio > 0 ~ "increase",
    is.na(log.ratio) ~ "no_data"))

# Plotting MPAs' map showing the change in fishing effort
ggplot() +
  geom_sf(data = land, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe"),
                    name = "Change in fishing effort",
                    labels = c("decrease" = "Decrease", "no_change" = "No change", 
                               "increase" = "Increase", "no_data" = "No data"),
                    breaks = c("decrease", "no_change", "increase", "no_data")) +
  
  # Adding squares for zoom
  geom_sf(data = zoom_latam, fill = "transparent", color = "black") +
  geom_sf(data = zoom_eu, fill = "transparent", color = "black") +
  geom_sf(data = zoom_aus, fill = "transparent", color = "black") +
  geom_sf(data = zoom_jpn, fill = "transparent", color = "black") +
  
  coord_sf(crs = "+proj=moll") +
  theme_void() +
  theme(legend.position = "none")

# Joining with MPAs_bind_buf for zoomed maps
MPAs_bind_results <- left_join(MPAs_bind_buf,
                               select(fishing10km.baci, WDPA_PID, log.ratio),
                               by = "WDPA_PID")
MPAs_bind_results <- MPAs_bind_results %>%
  mutate(change_fp = case_when(
    log.ratio < 0 ~ "decrease",
    log.ratio == 0 ~ "no_change",
    log.ratio > 0 ~ "increase",
    is.na(log.ratio) ~ "no_data"))

# Plotting the map zooming in LatAm
map_latam <- st_crop(land, xmin = -63, xmax = -113, ymin = -4, ymax = 31)

ggplot() +
  geom_sf(data = map_latam, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(-63, -113), ylim = c(-4, 31)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Europe
map_eu <- st_crop(land, xmin = -5, xmax = 30, ymin = 35, ymax = 65)

ggplot() +
  geom_sf(data = map_eu, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(-5, 30), ylim = c(35, 65)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Australia
map_aus <- st_crop(land, xmin = 110, xmax = 160, ymin = -43, ymax = -11)

ggplot() +
  geom_sf(data = map_aus, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(110, 160), ylim = c(-43, -11)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")

# Plotting the map zooming in Japan
map_jpn <- st_crop(land, xmin = 115, xmax = 150, ymin = 27, ymax = 42)

ggplot() +
  geom_sf(data = map_jpn, fill = "white", color = "white") +
  geom_sf(data = ocean, fill = "#ebf3fa", color = "#ebf3fa") +
  geom_sf(data = MPAs_bind_results, aes(fill = change_fp, size = 0.2)) +
  scale_fill_manual(values = c("decrease" = "#56b0c8", "no_change" = "#fff2cc",
                               "increase" = "#ff7f7f", "no_data" = "#bebebe")) +
  coord_sf(xlim = c(124, 146), ylim = c(26, 43)) +
  annotation_scale(location = "br", width_hint = 0.5, text_cex = 1) +
  theme_void() +
  theme(legend.position = "none")


################################################################################



