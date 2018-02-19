library(tidyverse)
library(waver)

cp = readOGR('shapefiles/box.shp')
cp_proj = spTransform(cp, CRS('+proj=longlat +init=epsg:26978'))
pt = SpatialPoints(matrix(c(-75.510698, 38.988955), ncol = 2), proj4string = CRS('+proj=longlat +init=epsg:26978'))
pt.df = data.frame(long = c(-75.510698), lat = c(38.988955), name = 'CP')

## Using 1500 m as approximate length of Coursey Pond
cp_fetch = fetch_len(p = pt, bearings = seq(0, 360, by = 5), shoreline = cp_proj, dmax = 1500)

cp_fetch = as.data.frame(cp_fetch)

cp_long = readOGR('shapefiles/CP_outline_utm.shp')
cp_long = spTransform(cp_long, CRS('+proj=longlat'))

cp_df = fortify(cp_long)

ggplot() + 
  geom_polygon(data = cp_df, aes(x = long, y = lat, group = group), color = 'black', fill = NA) + 
  geom_point(data = pt.df, aes(x = long, y = lat)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black"), panel.border = element_rect(color = "black")) + 
  labs(y = "Latitude", x = "Longitude")


write.csv(cp_fetch, 'csv/fetch.csv')
