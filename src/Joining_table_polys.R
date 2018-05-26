library(dplyr)
library(tidyr)
library(traits)
library(sp)
library(rgeos)
library(leaflet)
year <- lubridate::year

#generating polys

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
              %>% mutate(geom_type = class(parsed_geometry))) %>% 
  rename(site_id = id)
sites_poly <- do.call("rbind", filter(site_geom, geom_type == "SpatialPolygons")$parsed_geometry)
sites_point <- do.call("rbind", filter(site_geom, geom_type == "SpatialPoints")$parsed_geometry)


leaflet() %>% addTiles() %>% addPolygons(data = sites_poly, color = "red")


#downloading canopy cover data and saving them

options(betydb_key = readLines('~/.betykey', warn = FALSE),
        betydb_url = "https://terraref.ncsa.illinois.edu/bety/",
        betydb_api_version = 'beta')

canopy_cover <- betydb_query(table = 'search',
                             trait = "canopy_cover",
                             site  = "~Season 6",
                             limit = "none")

#write.csv(canopy_cover, file = 'data/canopy_cover.csv')


# joing canopy cover and poly data

CC_date <- canopy_cover %>% group_by(date)

CC_date_sites <- CC_date %>% left_join(site_geom, by = 'site_id') %>% 
  dplyr::select(date, raw_date, cultivar, method_name, canopy_cover = mean, sitename, parsed_geometry)

#readr::write_csv(CC_date_sites, 'data/canopy_cover_geoms.csv')

#visualizing 

leaflet() %>% addTiles() %>% 
  addPolygons(data = CC_date_sites$parsed_geometry, 
              color = "red")



