library(readxl)
library(sf)
library(terra)
library(raster)
library(tmap)
library(igraph)

PATH <- 'data/'

# load individual tracks around land masses -------------------------------

dat17  <- readRDS(paste0(PATH, "2017 every minute subsampled dataset.RDS"))
dat18  <- readRDS(paste0(PATH, "2018 every minute subsampled dataset.RDS"))

# load polygons -----------------------------------------------------------

p17   <- st_read("data/2017 area polygons.shp")
p18_1 <- st_read("data/2018_1 area polygons.shp")
p18_2 <- st_read("data/2018_2 area polygons.shp")

p17$id   <- 1:nrow(p17)
p18_1$id <- 1:nrow(p18_1)
p18_2$id <- 1:nrow(p18_2)


polygon_connection     <- matrix(NA, nrow = 10, ncol = 10)
polygon_connection[1,] <- c(NA, 0, 0, 0, 1, 1, 1, 0, 0, 0)
polygon_connection[2,] <- c( 0,NA, 1, 1, 0, 0, 0, 0, 0, 1)
polygon_connection[3,] <- c( 0, 1,NA, 0, 0, 0, 0, 1, 0, 0)
polygon_connection[4,] <- c( 0, 0, 0,NA, 1, 1, 1, 0, 1, 0)
polygon_connection[5,] <- c( 1, 0, 0, 1,NA, 0, 0, 0, 0, 0)
polygon_connection[6,] <- c( 1, 0, 0, 1, 0,NA, 0, 0, 0, 0)
polygon_connection[7,] <- c( 1, 0, 0, 1, 0, 0,NA, 0, 0, 0)
polygon_connection[8,] <- c( 0, 0, 1, 0, 0, 0, 0,NA, 0, 0)
polygon_connection[9,] <- c( 0, 0, 0, 1, 0, 0, 0, 0,NA, 0)
polygon_connection[10,]<- c( 0, 1, 0, 0, 0, 0, 0, 0, 0,NA)

dd <- graph_from_adjacency_matrix(polygon_connection)
plot(dd)

for(yr in c("2017","2018_1","2018_2")){
  if(yr == '2017'){
    dat2 <- dat17
    pol2 <- p17
  }
  if(yr == '2018_1'){
    dat2 <- dat18[dat18$date < as.POSIXct("2018-07-25 12:00:00"),]
    pol2 <- p18_1
  }
  if(yr == '2018_2'){
    dat2 <- dat18[dat18$date > as.POSIXct("2018-07-25 12:00:00"),]
    pol2 <- p18_2
  }

  polygon_cent_dist  <- st_distance(st_centroid(pol2))
  ids                <- unique(dat2$name)
  ids                <- ids[!is.na(ids)]

  for(j in 1:length(ids)){
    dat <- dat2[dat2$name == ids[j],]
    dat <- dat[!is.na(dat$date),]
    t   <- st_distance(dat, pol2)
    dat$polygon    <- NA
    dat$polygon[1] <- 9
    dat$delta_tsr  <- NA
    dat$pol_connected <- NA

    for(i in 2:nrow(dat)){

      dat$delta_tsr[i-1] <- dat$time_since_release[i] - dat$time_since_release[i-1]
      # dat$polygon[i]   <- which.min(t[i,])

      # if(as.numeric(t[i,9]) == 0) {
      #   #stay in same polygon
      #   dat$polygon[i] <- 9
      # }
      if(any(as.numeric(t[i,]) %in% 0)) {
        dat$polygon[i] <- which.min(as.numeric(t[i,]))
      }
      else {
        #move across polygon border
        border.btw     <- which(as.numeric(t[i,]) < 500)
        # determine into which polygon
        x <- as.numeric(names(which.max(polygon_cent_dist[dat$polygon[i-1], border.btw])))
        if(length(x) == 0) x <- as.numeric((which.max(polygon_cent_dist[dat$polygon[i-1], border.btw])))
        dat$polygon[i] <- x
      }
      dat$pol_connected[i-1] <- polygon_connection[dat$polygon[i-1], dat$polygon[i]]
      # # are the polygons connected
      # if(polygon_connection[dat$polygon[i-1],dat$polygon[i]] == 1) {
      #
      # } else {
      #
      # }
      #
      #   polygon_connection[dat$polygon[i-1],dat$polygon[i]]
      # polygon_cent_dist[dat$polygon[i-1], ]

    }

    out <- data.frame(tapply(dat$delta_tsr, dat$polygon, sum, na.rm = T))
    colnames(out) <- 'accum_time_spent_area'
    out$polygon <- rownames(out)
    out$id <- ids[j]
    out$yr <- yr

    if(j == 1) out2 <- out else out2 <- rbind(out2, out)
  }
  if(yr == '2017') out3 <- out2 else out3 <- rbind(out3, out2)
}

saveRDS(out3, file = paste0(PATH, "Individual polygon summary.RDS"))


out2 <- out3[out3$yr == "2017",]
out2$polygon <- factor(as.numeric(out2$polygon), levels = 1:9)
p17$tsa      <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, sum)/24)
p17$mean.tsa <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, mean)/24)
p17$max.tsa  <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, max)/24)
p17$min.tsa  <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, min)/24)
p17$sd.tsa   <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, sd)/24)
p17$unique_fish_occurence <- as.numeric(table(out2$polygon[!duplicated(paste(out2$id,out2$polygon))]))

out2 <- out3[out3$yr == "2018_1",]
out2$polygon  <- factor(as.numeric(out2$polygon), levels = 1:10)
p18_1$tsa      <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, sum)/24)
p18_1$mean.tsa <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, mean)/24)
p18_1$max.tsa  <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, max)/24)
p18_1$min.tsa  <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, min)/24)
p18_1$sd.tsa   <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, sd)/24)
p18_1$unique_fish_occurence <- as.numeric(table(out2$polygon[!duplicated(paste(out2$id,out2$polygon))]))

out2 <- out3[out3$yr == "2018_2",]
out2$polygon <- factor(as.numeric(out2$polygon), levels = 1:10)
p18_2$tsa <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, sum)/24)
p18_2$mean.tsa <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, mean)/24)
p18_2$max.tsa  <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, max)/24)
p18_2$min.tsa  <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, min)/24)
p18_2$sd.tsa   <- as.numeric(tapply(out2$accum_time_spent_area, out2$polygon, sd)/24)
p18_2$tsa[is.na(p18_2$tsa)] <- 0
p18_2$unique_fish_occurence <- as.numeric(table(out2$polygon[!duplicated(paste(out2$id,out2$polygon))]))


saveRDS(p17,  file = paste0(PATH, "Polygon summary 2017.RDS"))
saveRDS(p18_1,file = paste0(PATH, "Polygon summary 2018_1.RDS"))
saveRDS(p18_2,file = paste0(PATH, "Polygon summary 2018_2.RDS"))


