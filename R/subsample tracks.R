library(sf)
library(readxl)
library(lubridate)
library(rgdal)
library(terra)
library(geosphere)
library(gdistance)
library(raster)

# load file locations -----------------------------------------------------

PATH <- "data/"
xls  <- list.files(PATH, pattern = ".xlsx")

# load mapping data -------------------------------------------------------

bath  <- rast("data/map data/wider Altafjorden croped depth raster.tif")
coast <- st_read("data/map data/wider Altafjorden croped land polygon_N50Kartdata.shp")

# load fish farm locations ------------------------------------------------

cap_loc <- read_excel(paste0(PATH, 'fishfarm locations.xlsx'))
# define it as spatial object with coordinates and projection
sf_cap  <- st_as_sf(cap_loc, coords = c("longitude","latitude"), crs = 4326)
sf_cap  <- st_transform(sf_cap, st_crs(coast))

# build transition matrix for distance measure within fjords --------------



# rasterise land mass
ras_coast <- rasterize(vect(coast), bath)
ras_coast <- rast(raster::projectRaster(raster(ras_coast), res = 100, crs = projection(raster(ras_coast))))
ras_coast <- crop(ras_coast, vect(st_buffer(coast, -3000)))
ras_coast[is.na(ras_coast)] <- 0
ras_coast[ras_coast == 1] <- NA
ras_coast[ras_coast == 0] <- 1

# remove all disconnected pieces of fjord and open sea (small areas not connected to altafjorden in our cropped area)
clumps     <- raster::clump(raster(ras_coast), directions = 8)
clump_size <- table(clumps[])
clumps[clumps[] %in% which(clump_size < max(clump_size))] <- NA
clumps[clumps == which(clump_size == max(clump_size))] <- 1
ras_coast <- rast(clumps)
ras_coast[is.na(ras_coast)] <- 9999

# define all fish farm locations as water and not land
ras_cap <- rasterize(vect(sf_cap), ras_coast)
ras_cap[is.na(ras_cap)] <- 0
ras_coast <- ras_coast - ras_cap
ras_coast[ras_coast < 9999] <- 1

# calc transition matrix and geocorrect it for distance calculations
ras_tran <- transition(raster(ras_coast), function(x) 1/mean(x), 8)
ras_geo  <- geoCorrection(ras_tran)

# load fish capture data -------------------------------------------------------

cap17              <- read_excel(paste0(PATH, "Merking oppdrettslaks.xlsx"), sheet = "Merking 2017")
cap17$release_date <- as.POSIXct(strptime(paste(cap17$Dato, substr(cap17$`Release time`, 12, 20)), "%d.%m.%y %H:%M:%S"), tz = "UTC")
cap17$length       <- as.numeric(gsub(",", ".", cap17$`LF (cm)`)) * 10
cap17$mass         <- cap17$`Mass (kg)` * 1000
cap17$ID           <- cap17$`Tag ID`

cap18              <- read_excel(paste0(PATH, "Merking oppdrettslaks.xlsx"), sheet = "Merking 2018")
cap18$release_date <- as.POSIXct(strptime(paste(cap18$Dato, substr(cap18$`Slipptid`, 12, 20)), "%d.%m.%Y %H:%M:%S"), tz = "UTC")
cap18$length       <- cap18$`LF (mm)`
cap18$mass         <- cap18$`Vekt (g)`
cap18$Location     <- cap18$Sted

# load 2017 fish data -----------------------------------------------------

xls17 <- xls[grepl("2017", xls) & !grepl("receiver", xls)]
for(i in 1:length(xls17)){
  d <- read_excel(paste0(PATH, xls17[i]))
  if(i == 1) dat17 <- d else dat17 <- rbind(dat17, d)
}

# load 2018 fish data -----------------------------------------------------

xls18 <- xls[grepl("2018", xls) & !grepl("receiver", xls)]
for(i in 1:length(xls18)){
  d <- read_excel(paste0(PATH, xls18[i]))
  if(i == 1) dat18 <- d else dat18 <- rbind(dat18, d)
}

# run for each ID ---------------------------------------------------------

for(yr in c(2017,2018)){
if(yr == 2018) {
  ids <- unique(dat18$name)
  # remove ID 133, it only has one registration
  ids <- ids[ids != 133]
}
if(yr == 2017) ids <- unique(dat17$name)
for(i in 1:length(ids)){
  id  <- ids[i]

  if(yr == 2017) {
    cap <- cap17[cap17$ID == id,]
    dat <- dat17[dat17$name == id,]
  }
  if(yr == 2018) {
    cap <- cap18[cap18$ID == id,]
    dat <- dat18[dat18$name == id,]
  }
  # add capture location and time
  dat <- rbind(dat[1,], dat)
  dat$date[1] <- cap$release_date
  dat$longitude[1] <- cap_loc$longitude[cap_loc$Name == cap$Location]
  dat$latitude[1]  <- cap_loc$latitude[cap_loc$Name == cap$Location]
  dat$Receiver[1]  <- "capture"
  dat$bodylength   <- cap$length
  dat$Location     <- cap$Location

  # calculate time since release
  dat$time_since_release <- as.numeric(difftime(dat$date, dat$date[1], units = "hours"))

  # calculate time diff between registrations
  dat$dt <- NA
  dat$dt[2:nrow(dat)] <- as.numeric(difftime(tail(dat$date, -1), head(dat$date, -1), units = "secs"))

  # downsample to min every 30 min
  dat <- dat[dat$dt >= 60 | is.na(dat$dt),]
  dat$dt[2:nrow(dat)] <- as.numeric(difftime(tail(dat$date, -1), head(dat$date, -1), units = "secs"))
  # dat <- dat[dat$dt >= 60 * 30 | is.na(dat$dt),]
  # dat$dt[2:nrow(dat)] <- as.numeric(difftime(tail(dat$date, -1), head(dat$date, -1), units = "secs"))

  # calculate distance between registrations
  dat$dist <- NA
  dat$dist[2:nrow(dat)] <- distHaversine(cbind(dat$longitude, dat$latitude))
  dat$dist_body <- dat$dist * 1000 / dat$bodylength

  # transform to spatial object with coordinates and projection
  dat <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326)
  dat$longitude <- st_coordinates(dat)[,1]
  dat$latitude  <- st_coordinates(dat)[,2]
  dat <- st_transform(dat, st_crs(25833))

  # calculate distance around land masses for each step
  for(j in 1:(nrow(dat)-1)){
    cat("\r", "ID:", i, "of", length(ids), "- step:", j, "of", (nrow(dat)-1), "   ")
    s  <- shortestPath(ras_geo,
                           c(st_coordinates(dat)[j,]),
                           c(st_coordinates(dat)[j+1,]),
                           output="SpatialLines")
    if(j==1) path <- s else path <- rbind(path,s)

  }

  dat$dist2 <- NA
  dat$dist2[2:nrow(dat)] <- as.numeric(st_length(st_as_sf(path)))
  dat$dist2_body <- dat$dist2 * 1000 / dat$bodylength

  sum(st_length(st_as_sf(path)))

  dat$dist_release <- distHaversine(cbind(dat$longitude, dat$latitude), cbind(dat$longitude[1], dat$latitude[1]))
  dat$dist_release_body <- dat$dist_release * 1000 / dat$bodylength

  # calculate speed based on great circle distance (aka not taking into account coastline)
  dat$speedms   <- dat$dist/dat$dt
  dat$speedbody <- dat$dist_body/dat$dt
  dat$speed2ms   <- dat$dist2/dat$dt
  dat$speed2body <- dat$dist2_body/dat$dt

  # add data.frame to lines object
  path <- SpatialLinesDataFrame(path, data = data.frame(dat)[c(2:nrow(dat)),], match.ID = F)

  if(i == 1) path2 <- path else path2 <- rbind(path2, path)
  if(i == 1) dat2  <- dat  else dat2  <- rbind(dat2, dat)
}

saveRDS(dat2,  file = paste0(PATH, yr, " every minute subsampled dataset.RDS"))
saveRDS(path2, file = paste0(PATH, yr, " Individual tracks around land masses every minute res.RDS"))
}

plot(ras_coast)
plot(path2, add=T)



ceiling(tapply(dat2$time_since_release, dat2$name, max))

hist(dat2$speedbody)

plot(dat2$speedbody, dat2$dist_release)


plot(dat2$time_since_release, dat2$speed2body)
plot(dat2$time_since_release, dat2$dist2)

plot(dat2$time_since_release, dat2$dist_release, col="white")
for(i in 1:length(ids)) lines(dat2$time_since_release[dat2$name == ids[i]], dat2$dist_release[dat2$name == ids[i]])

summary(dat2)

plot(coastline$geometry, col = grey(0.8))
plot(dat2$geometry, add=T)

