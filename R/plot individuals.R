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

# load individual tracks around land masses -------------------------------

dat17  <- readRDS(paste0(PATH, "2017 every minute subsampled dataset.RDS"))
path17 <- readRDS(paste0(PATH, "2017 Individual tracks around land masses every minute res.RDS"))
dat18  <- readRDS(paste0(PATH, "2018 every minute subsampled dataset.RDS"))
path18 <- readRDS(paste0(PATH, "2018 Individual tracks around land masses every minute res.RDS"))


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

# plot --------------------------------------------------------------------

for(yr in c(2017, 2018)){

  if(yr==2017) {
    dat2 <- dat17
    path2 <- path17
    receivers <- receivers17
  } else{
    dat2 <- dat18
    path2 <- path18
    receivers <- receivers18
  }

  ids <- unique(path2$name)
  for(id in ids){

    # dat2$time_since_release <- dat2$time_since_release/24

    path_select <- st_as_sf(path2[path2$name %in% id,])
    path_select <- path_select[path_select$dist >0,]

    if(yr == 2018){
      if(max(dat2$date[dat2$name %in% id], na.rm=T) > as.POSIXct("2018-07-25 12:00:00")) periods <- c(0,2) else periods <- c(0,1)
      receivers <- receivers18[receivers18$period %in% periods,]
    }

    png(paste0("figures/ind/",yr,"_",id,".png"), res=500, units="cm", width = 20, height = 21)

    print(
      tm_shape(st_buffer(receivers17, 7000)) +
        tm_polygons(col = "white", border.col = "white") +

        tm_shape(depth_contour) +
        tm_lines(col = 'level', palette = colorRampPalette(c(grey(0.95), grey(0.75)))(22), legend.col.show = F, lwd = 0.5) +

        tm_shape(st_union(st_buffer(receivers, 500))) +
        tm_polygons(col = "red", border.col = "transparent", alpha = 0.6) +

        tm_shape(coast) +
        tm_polygons(col = "beige", border.col = grey(0.75), legend.show=F) +

        tm_shape(rvr) +
        tm_polygons(col = grey(0.5), border.col = grey(0.5), legend.show=F) +

        tm_shape(path_select) +
        tm_lines(lwd = 1.8) +

        tm_shape(dat2[dat2$name %in% id,]) +
        tm_symbols(col = 'time_since_release', palette = "Blues",
                   n = 10, size = 0.6, border.lwd = 0.5,
                   title.col = "Time since release [hr]") +

        tm_shape(sf_cap[sf_cap$Name == dat2$Location[dat2$name %in% id][1],]) +
        tm_symbols(col = "gold", size = 0.7, shape = 23, border.lwd = 0.8) +

        tm_graticules(labels.inside.frame = F, labels.rot = c(0, 0),
                      n.x = 3, n.y = 3,
                      lwd = 0.6, labels.col = grey(0.5), col = grey(0.7)) +

        tm_add_legend(type = "symbol",
                      shape = c(21,23),
                      col = c("red","gold"),
                      alpha = c(0.5),
                      border.col = c("transparent",grey(0)),
                      border.lwd = c(1,0.8),
                      labels = c("Receiver coverage", "Release site")) +

        tm_add_legend(type = "line",
                      lwd = 2,
                      labels = c("Minimum pathway"))
    )

    dev.off()
  }
}
