library(terra)
library(rnaturalearth)
library(sf)
library(dplyr)
library(raster)
library(geojsonio)
library(ggplot2)
library(patchwork)
library(exactextractr)
library(stargazer)

################# asia #####################

# conflict data
conflict <- readRDS("GEDEvent_v25_1.rds")

# first, Asia
conflict.asia <- conflict[conflict$region == "Asia" & conflict$year %in% c(2010:2020),]

# the function to plot map
plot_asian_map <- function(conflict, yr){
  # load nightlights data
  nightlights <- rast(paste0("Harmonized_DN_NTL_", yr, ".tif"))
  # border
  world <- ne_countries(scale = "medium", returnclass = "sf")
  asia <- world[world$continent == "Asia", ]
  asia <- st_transform(asia, crs(nightlights))
  # crop + mask to the map
  clipped_raster <- mask(crop(nightlights, extent(asia)), asia)
  # aggregate to lower the data size 
  clipped_raster <- aggregate(clipped_raster, 5, fun = 'mean', na.rm = T)
  nightlights.df <- as.data.frame(clipped_raster, xy = TRUE)
  
  nightlights.df <- rename(nightlights.df, 
                           lon = 1,
                           lat = 2,
                           nightlights = 3)
  
  
  conflicts.asia.events <- conflict |> 
    filter(year == yr) |> 
    dplyr::select(conflict_name, type_of_violence, country, adm_1, adm_2, latitude, longitude)
  
  conflicts.asia.geojson <- geojson_json(conflicts.asia.events, lat = "latitude", lon = "longitude")
  conflicts.asia.sf <- st_read(conflicts.asia.geojson)
  
  p1 <- ggplot() +
    # nightlight map
    geom_tile(data = nightlights.df , aes(x = lon, y = lat, fill = nightlights)) +
    scale_fill_viridis_c() +
    # Base map: country borders
    geom_sf(data = asia, fill = NA, color = "white", size = 0.5) +
    # scale_size_continuous(range = c(1, 3), name = "Nightlight") +
    theme_void() + 
    # labs(title = "Conflict Events and Nightlight Intensity in Asia (2020)") +
    coord_sf()
  
  p2 <- ggplot() +
    # night light map
    geom_tile(data = nightlights.df , aes(x = lon, y = lat, fill = nightlights)) +
    scale_fill_viridis_c() +
    # Base map: country borders
    geom_sf(data = asia, fill = NA, color = "white", size = 0.5) +
    # conflict points
    geom_sf(data = conflicts.asia.sf, aes(fill = "Conflict Event"), fill = "red", shape = 21, alpha = 0.3, size = 1, color = "black") +
    # scale_size_continuous(range = c(1, 3), name = "Nightlight") +
    theme_void() + 
    # labs(title = "Conflict Events and Nightlight Intensity in Asia (2020)") +
    coord_sf()
  
  p <- p1 + p2 + plot_layout(guides = "collect")
  
  # ggsave(paste0("map_asian_", yr, ".pdf"),p)
  ggsave(paste0("map_asian_", yr, ".png"),p, 
         height = 4.5,width = 8, 
         dpi = 300, units = "in", device='png'
  )
  
  return(p)
  
}

plot_asian_map(conflict.asia, 2020)
plot_asian_map(conflict.asia, 2010)

############## Thailand ################

plot_thailand_map <- function(conflict, yr){
  # load nightlights data
  nightlights <- rast(paste0("Harmonized_DN_NTL_", yr, ".tif"))
  # border
  thailand <- st_read("gadm41_THA_1.json") 
  # reproject vector to raster CRS
  thailand <- st_transform(thailand, crs(nightlights))
  # crop + mask to the map
  clipped_raster <- mask(crop(nightlights, extent(thailand)), thailand)
  # aggregate to lower the data size 
  nightlights.df <- as.data.frame(clipped_raster, xy = TRUE)
  
  nightlights.df <- rename(nightlights.df, 
                           lon = 1,
                           lat = 2,
                           nightlights = 3)
  
  
  conflicts.thailand.events <- conflict |> 
    filter(country == "Thailand" & year == yr) |> 
    dplyr::select(conflict_name, type_of_violence, country, adm_1, adm_2, latitude, longitude)
  
  conflicts.thailand.geojson <- geojson_json(conflicts.thailand.events, lat = "latitude", lon = "longitude")
  conflicts.thailand.sf <- st_read(conflicts.thailand.geojson)
  
  p1 <- ggplot() +
    # nightlight map
    geom_tile(data = nightlights.df , aes(x = lon, y = lat, fill = nightlights)) +
    scale_fill_viridis_c() +
    # Base map: country borders
    geom_sf(data = thailand, fill = NA, color = "white", size = 0.5) +
    # scale_size_continuous(range = c(1, 3), name = "Nightlight") +
    theme_void() + 
    # labs(title = "Conflict Events and Nightlight Intensity in Asia (2020)") +
    coord_sf()
  
  p2 <- ggplot() +
    # night light map
    geom_tile(data = nightlights.df , aes(x = lon, y = lat, fill = nightlights)) +
    scale_fill_viridis_c() +
    # Base map: country borders
    geom_sf(data = thailand, fill = NA, color = "white", size = 0.5) +
    # conflict points
    geom_sf(data = conflicts.thailand.sf, aes(fill = "Conflict Event"), fill = "red", shape = 21, alpha = 0.3, size = 3, color = "black") +
    # scale_size_continuous(range = c(1, 3), name = "Nightlight") +
    theme_void() + 
    # labs(title = "Conflict Events and Nightlight Intensity in Asia (2020)") +
    coord_sf()
  
  p <- p1 + p2 + plot_layout(guides = "collect")
  
  # ggsave(paste0("map_thailand_", yr, ".pdf"),p)
  ggsave(paste0("map_thailand_", yr, ".png"),p)
  
  return(p)
  
}

plot_thailand_map(conflict.asia, 2020)
plot_thailand_map(conflict.asia, 2010)


################ descriptive statistics of Thailand ###############
# data structure: panel data

thailand.df <- data.frame()

for (y in 2010:2020) {
  # nightlights
  nightlights <- rast(paste0("Harmonized_DN_NTL_",y , ".tif"))
  # border
  thailand <- st_read("gadm41_THA_1.json") 
  # reproject vector to raster CRS
  thailand <- st_transform(thailand, crs(nightlights))
  # summarize values by province 
  nightlight.state <- exact_extract(nightlights, thailand, 'mean')
  province.names <- thailand$NAME_1
  
  panel.data <- data.frame(
    province_name = province.names,
    nightlight_mean = nightlight.state,
    year = y
  )
  
  thailand.df <- rbind(thailand.df, panel.data)
}

# conflicts
conflict.thailand.event <- conflict.asia |>
  filter(country == "Thailand") |> 
  group_by(year, adm_1) |> 
  summarise(conflicts = n()) |> 
  mutate(province = str_trim(str_remove(adm_1, "province"))) 

conflict.thailand.event$province <- ifelse(conflict.thailand.event$province == "Sisaket", "SiSaket", conflict.thailand.event$province)

conflict.thailand.conflict <- conflict.asia |>
  filter(country == "Thailand") |> 
  group_by(year, adm_1, conflict_name) |> 
  summarise(conflicts = n()) 

conflict.thailand.conflict.no <- conflict.thailand.conflict |>
  group_by(year, adm_1) |> 
  summarise(conflicts.number = n()) |> 
  mutate(province = str_trim(str_remove(adm_1, "province")))

conflict.thailand.conflict.no$province <- ifelse(conflict.thailand.conflict.no$province == "Sisaket", "SiSaket", conflict.thailand.conflict.no$province)

conflict.thailand.sum <- merge(conflict.thailand.conflict.no, conflict.thailand.event, by = c("year","province"))
conflict.thailand.sum <- conflict.thailand.sum[!is.na(conflict.thailand.sum$province),]
conflict.thailand.sum <- conflict.thailand.sum[ -c(3, 5) ]

# merge with nightlights data
thailand.df <- merge(thailand.df, conflict.thailand.sum, 
                     by.x = c("year", "province_name"),
                     by.y = c("year", "province"), 
                     all.x = TRUE) |> 
  mutate(conflicts.number = if_else(is.na(conflicts.number), 0, conflicts.number)) |>
  mutate(conflicts = if_else(is.na(conflicts), 0, conflicts))


stargazer(thailand.df, type = "text", median = TRUE)
stargazer(thailand.df, type = "latex", median = TRUE)


