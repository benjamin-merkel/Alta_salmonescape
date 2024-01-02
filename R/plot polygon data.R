library(readxl)
library(sf)
library(terra)
library(raster)
library(tmap)

PATH <- 'data/'

# load mapping data -------------------------------------------------------

bath          <- rast("data/map data/wider Altafjorden croped depth raster.tif")
depth_contour <-  st_as_sf(as.contour(bath, nlevels = 21))
coast         <- st_read("data/map data/wider Altafjorden croped land polygon_N50Kartdata.shp")
rvr           <- st_read("data/map data/wider Altafjorden croped river polygon_N50Kartdata.shp")

# load polygon data -------------------------------

p17  <- readRDS(paste0(PATH, "Polygon summary 2017.RDS"))
p18_1<- readRDS(paste0(PATH, "Polygon summary 2018_1.RDS"))
p18_2<- readRDS(paste0(PATH, "Polygon summary 2018_2.RDS"))

# load fish farm locations ------------------------------------------------

cap_loc <- read_excel(paste0(PATH, 'fishfarm locations.xlsx'))
# define it as spatial object with coordinates and projection
sf_cap  <- st_as_sf(cap_loc, coords = c("longitude","latitude"), crs = 4326)
sf_cap  <- st_transform(sf_cap, st_crs(coast))

# load receiver locations -------------------------------------------------

receivers17 <- read_excel(paste0(PATH, 'Alta receiver network 2017.xlsx'))
receivers17 <- st_as_sf(receivers17, coords = c("longitude","latitude"), crs = 4326)
receivers17 <- st_transform(receivers17, st_crs(coast))

receivers18 <- read_excel(paste0(PATH, 'Alta receiver network 2018.xlsx'))
receivers18$longitude <- as.numeric(receivers18$longitude)
receivers18$latitude  <- as.numeric(receivers18$latitude)
receivers18 <- receivers18[!is.na(receivers18$longitude) & !is.na(receivers18$latitude),]
receivers18 <- st_as_sf(receivers18, coords = c("longitude","latitude"), crs = 4326)
receivers18 <- st_transform(receivers18, st_crs(coast))



map <- vector(mode = "list")
for(i in 1:6){

  if(i %in% c(1,2)) {
    pol2      <- p17
    receivers <- receivers17
    cap       <- sf_cap[sf_cap$Name == "Hesten",]
    main.title<- "2017"
  }
  if(i %in% c(3,4)) {
    pol2      <- p18_1
    receivers <- receivers18[receivers18$period %in% c(0,1),]
    cap       <- sf_cap[sf_cap$Name == "Bergsnes V",]
    main.title<- "2018_1"
  }
  if(i %in% c(5,6)) {
    pol2      <- p18_2
    receivers <- receivers18[receivers18$period %in% c(0,2),]
    cap       <- sf_cap[sf_cap$Name == "Bergsnes V",]
    main.title<- "2018_2"
  }
  if(i %in% c(1,3,5)) {
    pol2$var <- pol2$tsa
    tt       <- "cummulative TSA [days]"
    brks     <- seq(0,140,20)

    # pol2$var <- pol2$mean.tsa
    # tt       <- "mean TSA [days]"
    # brks     <- seq(0,8,2)
  } else {
    pol2$var <- pol2$unique_fish_occurence/max(pol2$unique_fish_occurence)
    tt       <- "prop of fish detected"
    brks     <- seq(0,1,0.2)
  }

  map[[i]] <-
    tm_shape(st_buffer(receivers17, 10000)) +
    tm_polygons(col = "white", border.col = "white") +

    tm_shape(st_union(st_buffer(receivers, 500))) +
    tm_polygons(col = "red", border.col = "transparent", alpha = 0.6) +

    tm_shape(pol2) +
    tm_fill(col = "var", border.col = "transparent",
            palette = "Blues", title = tt,
            n = 6,
            breaks = brks,
            style = "fixed") +

    tm_shape(coast) +
    tm_polygons(col = grey(0.95), border.col = grey(0.75), lwd = 0.8) +

    tm_shape(rvr) +
    tm_polygons(col = grey(0.5), border.col = grey(0.5)) +

    tm_shape(cap) +
    tm_symbols(col = "gold", size = 0.7, shape = 23, border.lwd = 0.8) +

    # tm_graticules(labels.inside.frame = F, labels.rot = c(0, 0),
    #               n.x = 3, n.y = 3,
    #               lwd = 0.6, labels.col = grey(0.5), col = grey(0.7)) +

    tm_add_legend(type = "symbol",
                  shape = c(21,23),
                  col = c("red","gold"),
                  alpha = c(0.5),
                  border.col = c("transparent",grey(0)),
                  border.lwd = c(1,0.8),
                  labels = c("Receiver coverage", "Release site")) +

    tm_layout(legend.bg.color = "white",
              title = main.title)

}

png(paste0("figures/polygon analysis.png"), res=500, units="cm", width = 18, height = 30)
print(
  tmap_arrange(map[[1]], map[[2]], map[[3]],
               map[[4]], map[[5]], map[[6]],
               ncol = 2)
)
dev.off()

