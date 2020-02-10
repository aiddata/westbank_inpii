
box_loc <- '/Users/christianbaehr/Box Sync/westbank_inpii'

library(haven)
library(plyr)
library(sf)
library(raster)
library(rgdal)
library(stargazer)

#################################################

## Read in Wave III locations
# locations <- read.csv(paste0(box_loc, '/inputData/arabbarometer/WaveIII Locations.csv'), stringsAsFactors = F)

# locations <- st_read(paste0(box_loc, "/inputData/arabbarometer/waveiii_final.geojson"), stringsAsFactors=F)
# locations <- locations[!duplicated(locations$v05aENG),]

locations <- st_read(paste0(box_loc, "/inputData/arabbarometer_locations.geojson"), stringsAsFactors=F)
locations <- locations[!duplicated(locations$v05aENG),]

#################################################

wave3_locations <- paste0(box_loc, '/inputData/arabbarometer/ABWaveIIIPalestinewLocation.dta') %>%
  read_dta() %>%
  .[,1:20]

wave3_survey <- read.csv("/Users/christianbaehr/Downloads/ab_website_data/ABIII_English.csv", stringsAsFactors = F)
wave3_survey <- wave3_survey[wave3_survey$country=="Palestine", ]
# wave3_survey <- wave3_survey[, 1:50]

for(i in 1:nrow(locations)) {
  
  x <- wave3_locations$v05aENG[which(wave3_locations$qid==locations$qid[i])]
  
  ids <- wave3_locations$qid[which(wave3_locations$v05aENG==x)]
  
  rows <- wave3_survey[which(wave3_survey$qid %in% ids), ]
  locs <- data.frame(locations[rep(i, nrow(rows)),])
  
  if(i==1) {
    wave3 <- cbind(locs, rows)
    
  } else {
    a <- cbind(rows, locs)
    wave3 <- rbind(wave3, cbind(locs, rows))
  }
}

#################################################

wave4_survey <- read.csv("/Users/christianbaehr/Downloads/ab_website_data/ABIV_English.csv", stringsAsFactors = F)
wave4_survey <- wave4_survey[wave4_survey$country=="Palestine", ]
# wave4_survey <- wave4_survey[, 1:50]

locations$merge <- tolower(locations$v05aENG)
wave4_survey$merge <- tolower(wave4_survey$q2)

wave4 <- merge(locations, wave4_survey, by="merge")

#################################################

rm(list = setdiff(ls(), c("box_loc", "wave3", "wave4")))

## future economic relations btwn palestine/us
wave3$q7001 <- ifelse(wave3$q7001=="Become stronger than they were in previous years", 1, 
                      ifelse(wave3$q7001=="Become weaker than they were in previous years", -1, 
                             ifelse(wave3$q7001=="Remain the same as they were in previous years", 0, NA)))

## future economic relations btwn palestine/saudi
wave3$q7002 <- ifelse(wave3$q7002=="Become stronger than they were in previous years", 1, 
                      ifelse(wave3$q7002=="Become weaker than they were in previous years", -1, 
                             ifelse(wave3$q7002=="Remain the same as they were in previous years", 0, NA)))

## future economic relations btwn palestine/israel. Not asked in Wave IV
wave3$q7006 <- ifelse(wave3$q7006=="Become stronger than they were in previous years", 1, 
                      ifelse(wave3$q7006=="Become weaker than they were in previous years", -1, 
                             ifelse(wave3$q7006=="Remain the same as they were in previous years", 0, NA)))

## future security relations btwn palestine/us. Not asked in Wave IV
wave3$q700a1 <- ifelse(wave3$q700a1=="Become stronger than they were in previous years", 1, 
                       ifelse(wave3$q700a1=="Become weaker than they were in previous years", -1, 
                              ifelse(wave3$q700a1=="Remain the same as they were in previous years", 0, NA)))

## future security relations btwn palestine/saudi Not asked in Wave IV
wave3$q700a2 <- ifelse(wave3$q700a2=="Become stronger than they were in previous years", 1, 
                       ifelse(wave3$q700a2=="Become weaker than they were in previous years", -1, 
                              ifelse(wave3$q700a2=="Remain the same as they were in previous years", 0, NA)))

## future security relations btwn palestine/israel. Not asked in Wave IV
wave3$q700a6 <- ifelse(wave3$q700a6=="Become stronger than they were in previous years", 1, 
                       ifelse(wave3$q700a6=="Become weaker than they were in previous years", -1, 
                              ifelse(wave3$q700a6=="Remain the same as they were in previous years", 0, NA)))

## do you want more or less foreign aid? Not asked in Wave IV
wave3$q701c <- ifelse(wave3$q701c=="Increase a lot", 2,
                      ifelse(wave3$q701c=="Increase a little", 1,
                             ifelse(wave3$q701c=="Remain at its current level", 0,
                                    ifelse(wave3$q701c=="Decrease a little", -1,
                                           ifelse(wave3$q701c=="Decrease a lot", -2, NA)))))

## impact of foreign investment on people with similar econ conditions as you? Not asked in Wave IV
wave3$q701d6 <- ifelse(wave3$q701d6=="Very positive", 2,
                       ifelse(wave3$q701d6=="Somewhat positive", 1,
                              ifelse(wave3$q701d6=="Has no impact", 0,
                                     ifelse(wave3$q701d6=="Negative", -1,
                                            ifelse(wave3$q701d6=="Very negative", -2, NA)))))

## do you think americans are generally good people?
wave3$q707 <- ifelse(wave3$q707=="I agree", 1,
                     ifelse(wave3$q707=="I disagree", 0, NA))

## US intervention in the region justifies arms against the US
wave3$q706 <- ifelse(wave3$q706=="I strongly agree", 2,
                     ifelse(wave3$q706=="I agree", 1,
                            ifelse(wave3$q706=="I disagree", -1,
                                   ifelse(wave3$q706=="I strongly disagree", -2, NA))))

###

## future economic relations btwn palestine/us
wave4$q7001 <- ifelse(wave4$q7001=="Become stronger than they were in previous years", 1, 
                      ifelse(wave4$q7001=="Become weaker than they were in previous years", -1, 
                             ifelse(wave4$q7001=="Remain the same as they were in previous years", 0, NA)))

## future economic relations btwn palestine/saudi
wave4$q7002 <- ifelse(wave4$q7002=="Become stronger than they were in previous years", 1, 
                      ifelse(wave4$q7002=="Become weaker than they were in previous years", -1, 
                             ifelse(wave4$q7002=="Remain the same as they were in previous years", 0, NA)))

## do you think americans are generally good people?
wave4$q707 <- ifelse(wave4$q707=="Agree", 1,
                     ifelse(wave4$q707=="Disagree", 0, NA))

###

wave3 <- wave3[, c("qid", "v03", "v04", "v05aENG", "q7001", "q7002", "q7006", "q700a1", "q700a2", 
                   "q706", "q701c", "q701d6", "q707", "geometry")]

wave4 <- wave4[, c("psu", "a1", "q1", "q2", "q7001", "q7002", "q707", "geometry")]

wave3$geometry <- as(wave3$geometry, 'Spatial')
wave4$geometry <- as(wave4$geometry, 'Spatial')

wave3 <- SpatialPolygonsDataFrame(Sr=wave3$geometry, data = wave3, match.ID = F)
wave4 <- SpatialPolygonsDataFrame(Sr=wave4$geometry, data = data.frame(wave4), match.ID = F)

###

roads <- st_read(paste0(box_loc, "/inputData/inpii_roads.geojson"), stringsAsFactors = F)

roads <- SpatialLinesDataFrame(sl = as(roads$geometry, 'Spatial'), data = data.frame(roads), match.ID = F)

roads <- over(wave3, roads, returnList = T)

extract.dates <- function(x) {
  
  y <- as.Date(x[["date"]], tryFormats="%m/%d/%y", optional=T)
  
  return(as.character(y[!is.na(y)]))
  
}

roads <- lapply(roads, FUN = extract.dates)

wave3 <- as.data.frame(wave3, stringsAsFactors=F)

wave3$roads <- roads
wave3$geometry <- as(wave3$geometry, "Spatial")
wave3 <- SpatialPolygonsDataFrame(Sr=wave3$geometry, data = wave3, match.ID = F)

wave3$roads <- sapply(wave3$roads, length)

###


wave3$dmsp2007 <- raster(paste0(box_loc, "/inputData/ntl/dmsp_2007.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

wave3$dmsp2008 <- raster(paste0(box_loc, "/inputData/ntl/dmsp_2008.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

wave3$dmsp2009 <- raster(paste0(box_loc, "/inputData/ntl/dmsp_2009.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

wave3$dmsp2010 <- raster(paste0(box_loc, "/inputData/ntl/dmsp_2010.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

wave3$dmsp2011 <- raster(paste0(box_loc, "/inputData/ntl/dmsp_2011.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

wave3$dmsp2012 <- raster(paste0(box_loc, "/inputData/ntl/dmsp_2012.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

wave3$viirs2012max <- raster(paste0(box_loc, "/inputData/ntl/VIIRS_2012MAX.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

wave3$population <- raster(paste0(box_loc, "/inputData/population/population_2010_gpw.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

###

wave4 <- wave4[wave4$psu %in% wave3]

writeOGR(wave3[, names(wave3)!="geometry"], dsn = "/Users/christianbaehr/Desktop/wave3.geojson", layer = "qid", driver = "GeoJSON")
writeOGR(wave4[, names(wave4)!="geometry"], dsn = "/Users/christianbaehr/Desktop/wave4.geojson", layer = "qid", driver = "GeoJSON")

stargazer(wave3)















