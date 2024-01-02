library(sf)
library(terra)

# load depth data ---------------------------------------------------------

#depth data was downloaded from https://emodnet.ec.europa.eu/geoviewer/ (Dec 2023)
bath <- rast("data/map data/Mean depth rainbow colour (no land).geotif")
# reproject to UTM zone 33N
bath <- project(bath, "EPSG:25833")
# save raster
writeRaster(bath, "data/map data/wider Altafjorden croped depth raster.tif")

# load coastline data ---------------------------------------------------------

# land use vector files were downloaded from geonorge.no (N50) (Dec 2023)
pp    <- sf::st_read("data/map data/Basisdata_54_Troms_og_Finnmark_25833_N50Kartdata_FGDB.gdb")
# define boundary box
bbox  <- st_bbox(c(xmin = 759419, xmax = 845743, ymax = 7863405, ymin = 7768524), crs = st_crs(25833))
# crop to Altafjorden
coast <- st_crop(pp, bbox)

# extract ocean
ocean <- coast[coast$objtype %in% c("Havflate"),]
ocean <- st_union(st_combine(ocean), by_feature = T)
# save ocean polygon
st_write(ocean, "data/map data/wider Altafjorden croped ocean polygon_N50Kartdata.shp")

# extract rivers
rvr   <- coast[coast$objtype %in% c("Elv"),]
rvr   <- st_union(st_combine(rvr), by_feature = T)
rvr   <- st_cast(rvr, "POLYGON")
dist_to_coast <- as.numeric(st_distance(rvr, ocean))
rvr   <- rvr[dist_to_coast <= 0]
# save rivers polygon
st_write(rvr, "data/map data/wider Altafjorden croped river polygon_N50Kartdata.shp")

# remove all but ocean polygons and combine
coast <- coast[!coast$objtype %in% c("Havflate"),]
coast <- st_combine(coast)
coast <- st_union(coast, by_feature = T)
# save land polygon
st_write(coast, "data/map data/wider Altafjorden croped land polygon_N50Kartdata.shp")
