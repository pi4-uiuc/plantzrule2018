library(dplyr)
library(rgeos)
bety_src <- src_postgres(dbname = "bety", 
                         password = 'bety', 
                         host = 'bety.terraref', 
                         user = 'bety', 
                         port = 5432)

sites <- tbl(bety_src, 
             sql("select sitename, ST_AsGeoJSON(geometry) as geometry, 
                 id as site_id from sites")) %>% 
  collect() %>% 
  filter(grepl('Season 2', sitename))

leaflet()%>% addTiles()%>% 
  addPolygons(sites$geometry, color = "red")
