library(readxl)
library(sf)
# devtools::install_github("RichardPatterson/midlines")
library(midlines)
library(dplyr)

PATH <- 'data/'

# load mapping data -------------------------------------------------------

ocean <- st_read("data/map data/wider Altafjorden croped ocean polygon_N50Kartdata.shp")
ocean <- st_cast(ocean, "POLYGON")
ocean <- ocean[4,]

# load receiver locations -------------------------------------------------

receivers17 <- read_excel(paste0(PATH, 'Alta receiver network 2017.xlsx'))
receivers17 <- st_as_sf(receivers17, coords = c("longitude","latitude"), crs = 4326)
receivers17 <- st_transform(receivers17, st_crs(coast))
receivers17$group <- substr(receivers17$...7, 1, 3)
receivers17 <- receivers17[!receivers17$group %in% c("Tal", "Est"),]

receivers18 <- read_excel(paste0(PATH, 'Alta receiver network 2018.xlsx'))
receivers18$longitude <- as.numeric(receivers18$longitude)
receivers18$latitude  <- as.numeric(receivers18$latitude)
receivers18 <- receivers18[!is.na(receivers18$longitude) & !is.na(receivers18$latitude),]
receivers18 <- st_as_sf(receivers18, coords = c("longitude","latitude"), crs = 4326)
receivers18 <- st_transform(receivers18, st_crs(coast))
receivers18$group <- substr(receivers18$...6, 1, 3)
receivers18 <- receivers18[!receivers18$group %in% c("Tal", "Mun", "Alt", "Årø") &
                           !receivers18$...6 %in% c("AlfØ", "AlfV"),]

# 2017 polygons -----------------------------------------------------------

r17 <- st_union(st_buffer(receivers17, 500))
r17 <- st_cast(r17, "POLYGON")
r17 <- st_cast(r17, "LINESTRING")

p1      <- lwgeom::st_split(ocean, r17)
p1      <- st_collection_extract(p1, "POLYGON")
p1$id   <- 1:nrow(p1)
p1$area <- as.numeric(st_area(p1))/1000000
p1      <- p1[order(p1$area, decreasing = T),]
# p1      <- p1[c(1:8,9,11:18),]
p1      <- p1[c(1:8),]

p2      <- lwgeom::st_split(ocean, st_cast(p1[4,], "LINESTRING"))
p2      <- st_collection_extract(p2, "POLYGON")
p2$id   <- 40
p2$area <- as.numeric(st_area(p2))/1000000
p2      <- p2[order(p2$area),]
p2      <- p2[1,]

p17 <- rbind(p1,p2)

# 2018_1 polygons ---------------------------------------------------------

r18_1 <- st_union(st_buffer(receivers18[receivers18$period %in% c(0,1),], 500))
r18_1 <- st_cast(r18_1, "POLYGON")
r18_1 <- st_cast(r18_1, "LINESTRING")

p1      <- lwgeom::st_split(ocean, r18_1)
p1      <- st_collection_extract(p1, "POLYGON")
p1$id   <- 1:nrow(p1)
p1$area <- as.numeric(st_area(p1))/1000000
p1      <- p1[order(p1$area, decreasing = T),]
# p1      <- p1[c(1:8,10:14,16:19),]
p1      <- p1[c(1:8),]

p2      <- lwgeom::st_split(ocean, st_cast(p1[5,], "LINESTRING"))
p2      <- st_collection_extract(p2, "POLYGON")
p2$id   <- 40
p2$area <- as.numeric(st_area(p2))/1000000
p2      <- p2[order(p2$area),]
p2      <- p2[2,]

p3      <- lwgeom::st_split(ocean, st_cast(p1[2,], "LINESTRING"))
p3      <- st_collection_extract(p3, "POLYGON")
p3$id   <- 41
p3$area <- as.numeric(st_area(p3))/1000000
p3      <- p3[order(p3$area),]
p3      <- p3[2,]

p18_1 <- rbind(p1, p2, p3)

# 2018_2 polygons ---------------------------------------------------------

r18_2 <- st_union(st_buffer(receivers18[receivers18$period %in% c(0,2),], 500))
r18_2 <- st_cast(r18_2, "POLYGON")
r18_2 <- st_cast(r18_2, "LINESTRING")

p1      <- lwgeom::st_split(ocean, r18_2)
p1      <- st_collection_extract(p1, "POLYGON")
p1$id   <- 1:nrow(p1)
p1$area <- as.numeric(st_area(p1))/1000000
p1      <- p1[order(p1$area, decreasing = T),]
# p1      <- p1[c(1:8,10:14,16:19),]
p1      <- p1[1:8,]

p2      <- lwgeom::st_split(ocean, st_cast(p1[4,], "LINESTRING"))
p2      <- st_collection_extract(p2, "POLYGON")
p2$id   <- 40
p2$area <- as.numeric(st_area(p2))/1000000
p2      <- p2[order(p2$area),]
p2      <- p2[2,]

p3      <- lwgeom::st_split(ocean, st_cast(p1[2,], "LINESTRING"))
p3      <- st_collection_extract(p3, "POLYGON")
p3$id   <- 41
p3$area <- as.numeric(st_area(p3))/1000000
p3      <- p3[order(p3$area),]
p3      <- p3[2,]

p18_2 <- rbind(p1, p2, p3)

#  save -------------------------------------------------------------------

p17$id   <- 1:nrow(p17)
p18_1$id <- 1:nrow(p18_1)
p18_2$id <- 1:nrow(p18_2)

st_write(p17,   "data/2017 area polygons.shp")
st_write(p18_1, "data/2018_1 area polygons.shp")
st_write(p18_2, "data/2018_2 area polygons.shp")


png(paste0("figures/Determined polygons.png"), res=500, units="cm", width = 30, height = 10)
opar <- par(mar=c(0, 0, 2, 0), mfrow=c(1, 3))
plot(p17$geometry, main="2017", col = "lightblue", border = "white", lwd =.5)
text(st_coordinates(st_centroid(p17))[,1], st_coordinates(st_centroid(p17))[,2], as.character(p17$id))
plot(p18_1$geometry, main="2018_1", col = "lightblue", border = "white", lwd =.5)
text(st_coordinates(st_centroid(p18_1))[,1], st_coordinates(st_centroid(p18_1))[,2], as.character(p18_1$id))
plot(p18_2$geometry, main="2018_2", col = "lightblue", border = "white", lwd =.5)
text(st_coordinates(st_centroid(p18_2))[,1], st_coordinates(st_centroid(p18_2))[,2], as.character(p18_2$id))
par(opar)
dev.off()
