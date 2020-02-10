
git_loc <- '/Users/christianbaehr/GitHub/westbank_inpii'
box_loc <- '/Users/christianbaehr/Box Sync/westbank_inpii'

library(haven)
library(plyr)
library(sf)

## Read in ArabBarometer Wave III locations. Adjust location names to correspond with survey names
waveiii <- st_read(paste0(box_loc, '/arabbarometer/waveiii_final.geojson'), stringsAsFactors = F)
waveiii$v05aENG[waveiii$v05aENG=='Al-Fandak (NEW)'] <- 'Al-Fandak'
waveiii$v05aENG[waveiii$v05aENG=='Aldahya (New)'] <- 'Aldahya'
waveiii$v05aENG[waveiii$v05aENG=='AL-Izarya'] <- 'Al_Izarya'
waveiii$v05aENG[waveiii$v05aENG=='Kalkleya'] <- 'Kalkelya'

## Read in ArabBarometer Wave III survey data. 
waveiii_survey <- paste0(box_loc, '/arabbarometer/ABWaveIIIPalestinewLocation.dta') %>%
  read_dta() %>%
  as.data.frame() %>%
  .[,1:20]

## Create unique ID for each Wave III survey location
waveiii_survey$reu_id <- paste0(waveiii_survey$v03, waveiii_survey$v04, waveiii_survey$v05aENG)

## Finding all observations in the survey that correspond to each location. Then merging the
## location data with each of those observations.
for(i in 1:nrow(waveiii)) {
  
  id <- waveiii_survey$reu_id[which(waveiii_survey$qid == waveiii$qid[i])]
  
  cols <- waveiii_survey[waveiii_survey$reu_id==id,]
  
  geo <- data.frame(waveiii[i,])
  geo[1:nrow(cols),] <- geo
  
  new_cols <- cbind(geo, cols)
  
  if(i==1) {
    df <- new_cols
  } else {
    
    df <- rbind(df, new_cols)
    
  }
}

names(df)[14] <- "v05aENG_2"

sum(df$v05aENG==df$v05aENG_2)

# View(df[df$v05aENG!=df$v05aENG_2, ])

df$location_id <- as.numeric(factor(df$reu_id))

library(sp)

library(tibble)
df$geometry <- do.call(rbind, st_geometry(df$geometry)) %>% 
  as_tibble() %>% 
  setNames(c("lon","lat")) %>%
  SpatialPoints(.)

df <- SpatialPointsDataFrame(df$geometry, df)

names(df) <- names(df)

library(rgdal)
writeOGR(df[,names(df)!="geometry"], paste0(box_loc, '/arabbarometer/waveiii_merged.geojson'), 
         "location_id", driver = "GeoJSON")









