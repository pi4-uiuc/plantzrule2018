library(dplyr)
library(tidyr)
library(traits)
library(sp)
library(rgeos)
library(leaflet)
year <- lubridate::year

betyurl <- "https://terraref.ncsa.illinois.edu/bety/"
betykey <- "9999999999999999999999999999999999999999"

## query and join tables
species <- (betydb_query(table = "species", limit = "none", betyurl = betyurl, key = betykey, api_version = "beta")
            %>% select(specie_id = id, scientificname, genus))

sites <- (betydb_query(table = "sites", limit = "none", sitename = "~Season 2 range",
                       betyurl = betyurl, key = betykey,
                       api_version = "beta"))

sites %>% group_by(city, state, country) %>% summarize(n())

# A simple plot of all site coordinates.
# Marker pins = sites with coords reported as a single point
# Red polygons = sites reporting full boundaries
site_geom <- (sites
              %>% filter(!is.na(geometry))
              %>% group_by(id)
              %>% do(parsed_geometry = readWKT(text = .$geometry, id = .$id))
              %>% mutate(geom_type = class(parsed_geometry)))
sites_poly <- do.call("rbind", filter(site_geom, geom_type == "SpatialPolygons")$parsed_geometry)
sites_point <- do.call("rbind", filter(site_geom, geom_type == "SpatialPoints")$parsed_geometry)


leaflet() %>% addTiles() %>% addPolygons(data = sites_poly, color = "red")
#%>% addMarkers(data = sites_point) # points removed by only querying Season 2

