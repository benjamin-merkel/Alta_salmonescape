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

p_all <- readRDS(paste0(PATH, "Polygon summary 2018_1.RDS"))
p_all <- p_all[,c(4)]

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


# load tsa data -----------------------------------------------------------

out3 <- readRDS(paste0(PATH, "Individual polygon summary.RDS"))

# polygon_connection     <- matrix(NA, nrow = 10, ncol = 10)
# polygon_connection[1,] <- c(NA, 0, 0, 0, 1, 1, 1, 0, 0, 0)
# polygon_connection[2,] <- c( 0,NA, 1, 1, 0, 0, 0, 0, 0, 1)
# polygon_connection[3,] <- c( 0, 1,NA, 0, 0, 0, 0, 1, 0, 0)
# polygon_connection[4,] <- c( 0, 0, 0,NA, 1, 1, 1, 0, 1, 0)
# polygon_connection[5,] <- c( 1, 0, 0, 1,NA, 0, 0, 0, 0, 0)
# polygon_connection[6,] <- c( 1, 0, 0, 1, 0,NA, 0, 0, 0, 0)
# polygon_connection[7,] <- c( 1, 0, 0, 1, 0, 0,NA, 0, 0, 0)
# polygon_connection[8,] <- c( 0, 0, 1, 0, 0, 0, 0,NA, 0, 0)
# polygon_connection[9,] <- c( 0, 0, 0, 1, 0, 0, 0, 0,NA, 0)
# polygon_connection[10,]<- c( 0, 1, 0, 0, 0, 0, 0, 0, 0,NA)
#
# dd <- graph_from_adjacency_matrix(polygon_connection)
#
# lo <- as.matrix(rbind(st_coordinates(st_centroid(p17)), st_coordinates(st_centroid(p18_1[10,]))))
# plot(dd, layout = lo)


# reshuffle polygon ids ---------------------------------------------------

# out3$polygon[out3$yr == "2018_1" & out3$polygon == 4] <- 0
# out3$polygon[out3$yr == "2018_1" & out3$polygon == 5] <- 4
# out3$polygon[out3$yr == "2018_1" & out3$polygon == 0] <- 5
out3$polygon[out3$yr == "2017" & out3$polygon == 5] <- 0
out3$polygon[out3$yr == "2017" & out3$polygon == 4] <- 5
out3$polygon[out3$yr == "2017" & out3$polygon == 0] <- 4
out3$polygon[out3$yr == "2018_2" & out3$polygon == 5] <- 0
out3$polygon[out3$yr == "2018_2" & out3$polygon == 4] <- 5
out3$polygon[out3$yr == "2018_2" & out3$polygon == 7] <- 4
out3$polygon[out3$yr == "2018_2" & out3$polygon == 6] <- 7
out3$polygon[out3$yr == "2018_2" & out3$polygon == 0] <- 6
out3$polygon <- factor(out3$polygon, levels = 1:10)


# summarise tsa data ------------------------------------------------------

d <- data.frame(
  polygon = levels(out3$polygon),
  no.fish.detected = as.numeric(table(out3$polygon)),
  sum.tsa     = tapply(out3$accum_time_spent_area, out3$polygon, sum)/24,
  min.tsa     = tapply(out3$accum_time_spent_area, out3$polygon, min)/24,
  max.tsa     = tapply(out3$accum_time_spent_area, out3$polygon, max)/24,
  mean.tsa    = tapply(out3$accum_time_spent_area, out3$polygon, mean)/24,
  median.tsa  = tapply(out3$accum_time_spent_area, out3$polygon, median)/24,
  sd.tsa      = tapply(out3$accum_time_spent_area, out3$polygon, sd)/24
)

p_all <- cbind(p_all, d)



plotting.variables <- c("TSA_mean", "TSA_max", "TSA_min", "TSA_median","TSA_accumulated","TSA_sd","prop_fish_detected")
for(x in plotting.variables){

  if(x == "TSA_mean") p_all$var <- p_all$mean.tsa
  if(x == "TSA_median") p_all$var <- p_all$median.tsa
  if(x == "TSA_min") p_all$var <- p_all$min.tsa
  if(x == "TSA_max") p_all$var <- p_all$max.tsa
  if(x == "TSA_sd") p_all$var <- p_all$sd.tsa
  if(x == "TSA_accumulated") p_all$var <- p_all$sum.tsa
  if(x == "prop_fish_detected") p_all$var <- p_all$no.fish.detected/max(p_all$no.fish.detected)

  if(x != "prop_fish_detected") tt <- paste(gsub("_"," ",x),"[days]") else tt <- gsub("_"," ",x)

  png(paste0("figures/",x,".png"), res=500, units="cm", width = 12, height = 13)
  print(

    tm_shape(st_buffer(receivers17, 10000)) +
      tm_polygons(col = "white", border.col = "white") +

      tm_shape(st_union(st_buffer(receivers18[receivers18$period %in% c(0,1),], 500))) +
      tm_polygons(col = "red", border.col = "transparent", alpha = 0.6) +

      tm_shape(p_all) +
      tm_fill(col = "var",
              border.col = "transparent",
              palette = "Blues",
              title = tt,
              n = 8) +

      tm_shape(coast) +
      tm_polygons(col = grey(0.95), border.col = grey(0.75), lwd = 0.8) +

      tm_shape(rvr) +
      tm_polygons(col = grey(0.5), border.col = grey(0.5)) +

      tm_shape(sf_cap) +
      tm_symbols(col = "gold", size = 0.35, shape = 23, border.lwd = 0.8) +

      tm_add_legend(type = "symbol",
                    shape = c(21,23),
                    col = c("red","gold"),
                    alpha = c(0.5),
                    border.col = c("transparent",grey(0)),
                    border.lwd = c(1,0.8),
                    labels = c("Receiver coverage", "Release site")) +

      tm_layout(legend.bg.color = "white",
                legend.text.size = 0.8)

  )
  dev.off()
}
