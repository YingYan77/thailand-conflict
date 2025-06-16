
library(highcharter)
library(countrycode)
library(sf)
library(dplyr)

# read the data 
conflict <- readRDS("GEDEvent_v25_1.rds")
# conflict <- readRDS("UcdpPrioConflict_v25_1.rds") # armed conflict

# first, Asia
conflict.asia <- conflict[conflict$region == "Asia" & conflict$year %in% c(2010:2020),]
# conflict.asia <- conflict[conflict$region == 3 & conflict$year %in% c(2010:2020),]

conflict.asia$Alpha3.code <- countrycode(conflict.asia$country, "country.name","iso3c")

# integrate to a conflict level data 
conflict.asia.panel <- conflict.asia |>
  group_by(year, country, conflict_name) |> 
  summarise(conflicts = n()) 

# how many conflicts event happened during the year for each country
conflict.asia.panel <- conflict.asia |>
  group_by(year, Alpha3.code) |> 
  summarise(conflicts = n()) 


# Read the GeoJSON file to obtain the list of Asian countries
asia.geo <- st_read("https://code.highcharts.com/mapdata/custom/asia.geo.json") 

asia.geo <- as.data.frame(asia.geo)
asia.geo <- asia.geo |> 
  select(name, subregion, iso.a3, iso.a2) |> 
  filter(iso.a3 != "-99")

# Create full panel
country_year <- expand.grid(iso.a3 = asia.geo$iso.a3, year = 2010:2020)
asia.geo <- merge(country_year, asia.geo, 
                  by = c("iso.a3"), 
                  all.x = TRUE)


conflicts.asia.country <- merge(asia.geo, conflict.asia.panel, 
                                by.x = c("iso.a3", "year"), 
                                by.y = c("Alpha3.code", "year"), 
                                all.x = TRUE) |> 
  mutate(conflicts = if_else(is.na(conflicts), 0, conflicts)) |> # fill na with zeros
  mutate(conflict_level = case_when(
    conflicts == 0 ~ 0,
    conflicts > 0 & conflicts <= 100 ~ 1,
    conflicts > 100 & conflicts <= 1000 ~ 2,
    conflicts > 1000 ~ 3
  )
  ) |> 
  arrange(year, name)

summary(conflicts.asia.country$conflicts)

conflicts.asia.country.2020 <- conflicts.asia.country[conflicts.asia.country$year==2020,]

# now we'll create the dataClasses
dta_clss <- conflicts.asia.country |> 
  # mutate(value = cumsum(!duplicated(tz))) |> 
  group_by(conflict_level) |> 
  summarise(value = unique(conflict_level)) |> 
  arrange(conflict_level) |> 
  rename(name = conflict_level, from = value) |> 
  mutate(to = from + 1) |> 
  mutate(label = c("0", "1-100", "100-1000",">1000"))|> 
  list_parse()

hcmap(
  map = "custom/asia", 
  data = conflicts.asia.country.2020, 
  joinBy = c("iso-a3", "iso.a3"), 
  name = c("Number of Conflicts"), 
  value = "conflict_level", 
  states = list(hover = list(color = "#BADA55")),
  tooltip = list(pointFormat = "{point.name} conflicts number: {point.conflicts}"),
  dataLabels = list(enabled = TRUE, format = "{point.name}"),
  events = list(
    load = JS("function() { this.reflow(); }")
  )
) |>
  hc_chart(animation = TRUE) |> 
  hc_title(text = "Asia Map") |> 
  hc_subtitle(text = "You can use the same functions to modify your map!") |>
  # hc_colorAxis(
  #   minColor = "#daecf3",
  #   maxColor = "#2c495e"
  # ) |>
  hc_colorAxis(
    dataClassColor = "category",
    dataClasses = dta_clss
  ) |> 
  hc_mapNavigation(enabled = TRUE)


# second, Thailand
conflict.thailand <- conflict.asia[conflict.asia$country == "Thailand",]

# Read the GeoJSON file to obtain the list of Asian countries
tha.geo <- st_read("gadm41_THA_3.json") 

library(jsonlite)
tha.map <- jsonlite::fromJSON("gadm41_THA_3.json", simplifyVector = FALSE)


ausmap <- highchart(type = "map") |>
  hc_add_series(mapData = tha.map, 
                showInLegend = FALSE)
ausmap
