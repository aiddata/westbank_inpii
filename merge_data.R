
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
  .[,1:5]

wave3_survey <- read.csv("/Users/christianbaehr/Downloads/ab_website_data/ABIII_English.csv", stringsAsFactors = F)
wave3_survey <- wave3_survey[wave3_survey$country=="Palestine", ]

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

# sum(wave3$v04 == wave3$q1)
# View(wave3[wave3$v04 != wave3$q1, c("v04", "q1")])
## checks out...only deviations are spelling issues

wave3 <- wave3[, !names(wave3) %in% c("a1", "q1", "q13")]
wave3$v03 <- "West Bank"

#################################################

wave4_survey <- read.csv("/Users/christianbaehr/Downloads/ab_website_data/ABIV_English.csv", stringsAsFactors = F)
wave4_survey <- wave4_survey[wave4_survey$country=="Palestine", ]

# locations$merge <- tolower(locations$v05aENG)
# wave4_survey$merge <- tolower(wave4_survey$q2)

locations$merge <- tolower(paste(locations$v04, locations$v05aENG))
wave4_survey$merge <- tolower(paste(wave4_survey$q1, wave4_survey$q2))

wave4 <- merge(locations, wave4_survey, by="merge")

sum(wave4$v04 == wave4$q1)
## checks out...all ADM2 and ADM3s match

wave4 <- wave4[, !names(wave4) %in% c("a1", "q1", "q2", "q13")]

#################################################

rm(list = setdiff(ls(), c("box_loc", "wave3", "wave4")))


#################################################
#################################################

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

# wave3 <- as.data.frame(wave3, stringsAsFactors=F)

wave3$roads <- roads
# wave3$geometry <- as(wave3$geometry, "Spatial")
# wave3 <- SpatialPolygonsDataFrame(Sr=wave3$geometry, data = wave3, match.ID = F)
wave3$roads <- sapply(wave3$roads, length)
wave3$treatment <- ifelse(wave3$roads>0, 1, 0)

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

wave3$dist_to_city <- raster(paste0(box_loc, "/inputData/city_access/accessibility_eujrc.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

test3 <- wave3
test4 <- wave4

#################################################

test3$age <- as.numeric(test3$q1001)

test3$male <- ifelse(test3$q1002=="Male", 1, 0)

test3$education <- ifelse(test3$q1003=="MA and above", 7,
                          ifelse(test3$q1003=="ba", 6,
                                 ifelse(test3$q1003=="Mid-level diploma (professional or technical)", 5,
                                        ifelse(test3$q1003=="Secondary", 4,
                                               ifelse(test3$q1003=="Prepartory/Basic", 3,
                                                      ifelse(test3$q1003=="Elementary", 2,
                                                             ifelse(test3$q1003=="Illiterate/No formal education", 1, NA)))))))

test3$married <- ifelse(test3$q1010=="Married", 1,
                        ifelse(test3$q1010=="Missing", NA, 0))

test3$muslim <- ifelse(test3$q1012=="Muslim", 1,
                       ifelse(test3$q1012=="Missing", NA, 0))

test3$christian <- ifelse(test3$q1012=="Christian", 1,
                          ifelse(test3$q1012=="Missing", NA, 0))

test3$income <- ifelse(test3$q1014=="No income", 0,
                       ifelse(test3$q1014=="Missing", NA, as.numeric(test3$q1014)))

test3$urban <- ifelse(test3$v13=="City", 1, 0)
test3$rural <- ifelse(test3$v13=="Village", 1, 0)
test3$refugee_camp <- ifelse(test3$v13=="Refugee camp", 1, 0)

test3$employed <- ifelse(test3$q1004=="Yes", 1,
                         ifelse(test3$q1004=="No", 0, NA))

test3$full_time <- ifelse(test3$q1006=="Full time (30 hours or more a week)", 1,
                          ifelse(test3$q1006=="Part time (less than 30 hours a week)", 0, NA))

test3$public_employee <- ifelse(test3$q1006a=="Public", 1,
                                ifelse(test3$q1006a=="Private", 0, NA))

test3$retired <- ifelse(test3$q1005=="Retired", 1,
                        ifelse(test3$q1005=="Missing", NA, 0))

test3$housewife <- ifelse(test3$q1005=="A housewife", 1,
                          ifelse(test3$q1005=="Missing", NA, 0))

test3$student <- ifelse(test3$q1005=="A student", 1,
                        ifelse(test3$q1005=="Missing", NA, 0))

test3$unemployed <- ifelse(test3$q1005=="Unemployed", 1,
                           ifelse(test3$q1005=="Missing", NA, 0))

test3$homeowner <- ifelse(test3$q1013 %in% c("Owned", "Owned with mortgage payments to a bank"), 1,
                          ifelse(test3$q1013=="Missing", NA, 0))

test3$own_a_computer <- ifelse(test3$q1011a=="Yes", 1,
                               ifelse(test3$q1011a=="No", 0, NA))

test3$own_a_car <- ifelse(test3$q1011b=="Yes", 1,
                          ifelse(test3$q1011b=="No", 0, NA))

test3$q7001 <- ifelse(test3$q7001=="Become stronger than they were in previous years", 1, 
                      ifelse(test3$q7001=="Become weaker than they were in previous years", -1, 
                             ifelse(test3$q7001=="Remain the same as they were in previous years", 0, NA)))

## future economic relations btwn palestine/saudi
test3$q7002 <- ifelse(test3$q7002=="Become stronger than they were in previous years", 1, 
                      ifelse(test3$q7002=="Become weaker than they were in previous years", -1, 
                             ifelse(test3$q7002=="Remain the same as they were in previous years", 0, NA)))

## future economic relations btwn palestine/israel. Not asked in Wave IV
test3$q7006 <- ifelse(test3$q7006=="Become stronger than they were in previous years", 1, 
                      ifelse(test3$q7006=="Become weaker than they were in previous years", -1, 
                             ifelse(test3$q7006=="Remain the same as they were in previous years", 0, NA)))

## future security relations btwn palestine/us. Not asked in Wave IV
test3$q700a1 <- ifelse(test3$q700a1=="Become stronger than they were in previous years", 1, 
                       ifelse(test3$q700a1=="Become weaker than they were in previous years", -1, 
                              ifelse(test3$q700a1=="Remain the same as they were in previous years", 0, NA)))

## future security relations btwn palestine/saudi Not asked in Wave IV
test3$q700a2 <- ifelse(test3$q700a2=="Become stronger than they were in previous years", 1, 
                       ifelse(test3$q700a2=="Become weaker than they were in previous years", -1, 
                              ifelse(test3$q700a2=="Remain the same as they were in previous years", 0, NA)))

## future security relations btwn palestine/israel. Not asked in Wave IV
test3$q700a6 <- ifelse(test3$q700a6=="Become stronger than they were in previous years", 1, 
                       ifelse(test3$q700a6=="Become weaker than they were in previous years", -1, 
                              ifelse(test3$q700a6=="Remain the same as they were in previous years", 0, NA)))

## do you want more or less foreign aid? Not asked in Wave IV
test3$q701c <- ifelse(test3$q701c=="Increase a lot", 2,
                      ifelse(test3$q701c=="Increase a little", 1,
                             ifelse(test3$q701c=="Remain at its current level", 0,
                                    ifelse(test3$q701c=="Decrease a little", -1,
                                           ifelse(test3$q701c=="Decrease a lot", -2, NA)))))

## impact of foreign investment on people with similar econ conditions as you? Not asked in Wave IV
test3$q701d6 <- ifelse(test3$q701d6=="Very positive", 2,
                       ifelse(test3$q701d6=="Somewhat positive", 1,
                              ifelse(test3$q701d6=="Has no impact", 0,
                                     ifelse(test3$q701d6=="Negative", -1,
                                            ifelse(test3$q701d6=="Very negative", -2, NA)))))

## do you think americans are generally good people?
test3$q707 <- ifelse(test3$q707=="I agree", 1,
                     ifelse(test3$q707=="I disagree", 0, NA))

## US intervention in the region justifies arms against the US
test3$q706 <- ifelse(test3$q706=="I strongly agree", 2,
                     ifelse(test3$q706=="I agree", 1,
                            ifelse(test3$q706=="I disagree", -1,
                                   ifelse(test3$q706=="I strongly disagree", -2, NA))))


#################################################

test3$dmsp_pretrend <- NA

for(i in unique(test3$qid)) {
  dmsp <- as.numeric(test3[test3$qid==i, paste0("dmsp", 2007:2012)]@data[1,])
  trend <- lm(dmsp ~ c(1:6))
  test3$dmsp_pretrend[test3$qid==i] <- trend$coefficients[2]
}


sum(test4$v05aENG %in% test3$v05aENG)
sum(test3$v05aENG %in% test4$v05aENG)

unique(test3$v05aENG[which(!test3$v05aENG %in% test4$v05aENG)])

test3$v05aENG[test3$v05aENG=="Old-City-Nablus"] <- "Nablus City"

# test3$v05aENG[test3$v05aENG=="Al-faraa Refugee"] <- "Al-Faraa Refugee"
# test3$v05aENG[test3$v05aENG=="Askar Refugee"] <- "Askar refugee"

unique(test3$v05aENG[!test3$v05aENG %in% test4$v05aENG])
test3 <- test3[which(test3$v05aENG %in% test4$v05aENG),]

unique(test4$q2[!test4$v05aENG %in% test3$v05aENG])
test4 <- test4[test4$v05aENG %in% test3$v05aENG,]

out_names <- c("age",
               "male",
               "education",
               "married",
               "muslim",
               "christian",
               "income",
               "employed",
               "own_a_car")

out_labels <- c("Age",
                "Male (dummy)",
                "Level of Educ., 0=None, 7=MA+",
                "Married (dummy)",
                "Muslim (dummy)",
                "Christian (dummy)",
                "Income",
                "Employed (dummy)",
                "Owns a car (dummy)")

stargazer(test3@data[, out_names], type = "latex", title = "Wave III Demographics (Full Sample)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_full.tex")

stargazer(test3@data[test3$treatment==1, out_names], type = "latex", title = "Wave III Demographics (Treatment Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_treatment.tex")

stargazer(test3@data[test3$treatment==0, out_names], type = "latex", title = "Wave III Demographics (Control Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_control.tex")

###

out_names <- c("treatment",
               "roads",
               "dmsp_pretrend",
               "viirs2012max",
               "population",
               "dist_to_city",
               "urban",
               "rural",
               "refugee_camp")

out_labels <- c("Treatment (dummy)",
                "# of treatments",
                "NTL Pre-trend (DMSP, 2007-12)",
                "VIIRS 2012 (max)",
                "Population (CIESIN)",
                "Distance to nearest city (pop>50k)",
                "Urban (dummy)",
                "Rural (dummy)",
                "Refugee Camp (dummy)")

stargazer(test3@data[, out_names], type = "latex", title = "Wave III Covariates (Full Sample)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/covariates_full.tex", 
          digits.extra = 0)

stargazer(test3@data[test3$treatment==1, out_names], type = "latex", title = "Wave III Covariates (Treatment Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/covariates_treatment.tex",
          digits.extra = 0)

stargazer(test3@data[test3$treatment==0, out_names], type = "latex", title = "Wave III Covariates (Control Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/covariates_control.tex",
          digits.extra = 0)

###

out_names <- c("q7001",
               "q7002",
               "q7006",
               "q700a1",
               "q700a2",
               "q700a6",
               "q706",
               "q701c")

out_labels <- c("Do you prefer economic relations btwn Pal./US improve",
                "Do you prefer economic relations btwn Pal./Saudi improve",
                "Do you prefer economic relations btwn Pal./Israel improve",
                "Do you prefer security relations btwn Pal./US improve",
                "Do you prefer security relations btwn Pal./Saudi improve",
                "Do you prefer security relations btwn Pal./Israel improve",
                "Do you agree: US interference justifies arms against the US?",
                "Should foreign aid to your country increase?")

stargazer(test3@data[, out_names], type = "latex", title = "Wave III Survey Questions (Full Sample)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_full.tex", 
          digits.extra = 0)

stargazer(test3@data[test3$treatment==1, out_names], type = "latex", title = "Wave III Survey Questions (Treatment Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_treatment.tex",
          digits.extra = 0)

stargazer(test3@data[test3$treatment==0, out_names], type = "latex", title = "Wave III Survey Questions (Control Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_control.tex",
          digits.extra = 0)

###

writeOGR(obj = test3[names(test3)!="geometry"], dsn = "/Users/christianbaehr/Desktop/wave3.geojson",
         layer = "qid", driver = "GeoJSON")




#################################################





stargazer(test3@data, type = "html", title = "Wave III Summary Statistics (Full Sample)",
          keep = c("urban", "rural", "refugee_camp", "q7001", "q7002", "q7006", "q700a1", "q700a2", "q700a6", "q706", "q701c", 
                   "viirs2012max", "population", "dist_to_city", "dmsp_pretrend"),
          covariate.labels = c("Urban", "Rural", "Refugee camp",
                               "Do you prefer economic relations btwn Pal./US improve",
                               "Do you prefer economic relations btwn Pal./Saudi improve",
                               "Do you prefer economic relations btwn Pal./Israel improve",
                               "Do you prefer security relations btwn Pal./US improve",
                               "Do you prefer security relations btwn Pal./Saudi improve",
                               "Do you prefer security relations btwn Pal./Israel improve",
                               "Do you agree: US interference justifies arms against the US?",
                               "Should foreign aid to your country increase?",
                               "VIIRS Baseline (2012 Max)",
                               "Population (CIESIN)",
                               "Distance to nearest city (EU-JRC)",
                               "DMSP Pre-trend (2007-12)"),
          out = "/Users/christianbaehr/Desktop/test.html")

stargazer(test3@data[test3$treatment==1, ], type = "html", title = "Wave III Summary Statistics (Treated Only)",
          keep = c("urban", "rural", "refugee_camp", "q7001", "q7002", "q7006", "q700a1", "q700a2", "q700a6", "q706", "q701c", 
                   "viirs2012max", "population", "dist_to_city", "dmsp_pretrend"),
          covariate.labels = c("Urban", "Rural", "Refugee camp",
                               "Do you prefer economic relations btwn Pal./US improve",
                               "Do you prefer economic relations btwn Pal./Saudi improve",
                               "Do you prefer economic relations btwn Pal./Israel improve",
                               "Do you prefer security relations btwn Pal./US improve",
                               "Do you prefer security relations btwn Pal./Saudi improve",
                               "Do you prefer security relations btwn Pal./Israel improve",
                               "Do you agree: US interference justifies arms against the US?",
                               "Should foreign aid to your country increase?",
                               "VIIRS Baseline (2012 Max)",
                               "Population (CIESIN)",
                               "Distance to nearest city (EU-JRC)",
                               "DMSP Pre-trend (2007-12)"),
          out = "/Users/christianbaehr/Desktop/test_treated.html")

stargazer(test3@data[test3$treatment==0, ], type = "html", title = "Wave III Summary Statistics (Control Only)",
          keep = c("urban", "rural", "refugee_camp", "q7001", "q7002", "q7006", "q700a1", "q700a2", "q700a6", "q706", "q701c", 
                   "viirs2012max", "population", "dist_to_city", "dmsp_pretrend"),
          covariate.labels = c("Urban", "Rural", "Refugee camp",
                               "Do you prefer economic relations btwn Pal./US improve",
                               "Do you prefer economic relations btwn Pal./Saudi improve",
                               "Do you prefer economic relations btwn Pal./Israel improve",
                               "Do you prefer security relations btwn Pal./US improve",
                               "Do you prefer security relations btwn Pal./Saudi improve",
                               "Do you prefer security relations btwn Pal./Israel improve",
                               "Do you agree: US interference justifies arms against the US?",
                               "Should foreign aid to your country increase?",
                               "VIIRS Baseline (2012 Max)",
                               "Population (CIESIN)",
                               "Distance to nearest city (EU-JRC)",
                               "DMSP Pre-trend (2007-12)"),
          out = "/Users/christianbaehr/Desktop/test_control.html")









#################################################
#################################################

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

## Wave IV

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

wave3$urban <- ifelse(wave3$v13=="City", 1, 0)
wave3$rural <- ifelse(wave3$v13=="Rural", 1, 0)
wave3$refugee_camp <- ifelse(wave3$v13=="Refugee camp", 1, 0)

wave4$urban <- ifelse(wave4$q13=="Urban", 1, 0)
wave4$rural <- ifelse(wave4$q13=="Rural", 1, 0)
wave4$refugee_camp <- ifelse(wave4$q13=="Refugee camp", 1, 0)

###

# wave3 <- wave3[, c("qid", "sex", "v13", "urban", "rural", "refugee_camp", "v03", "v04", "v05aENG", "q7001", "q7002", "q7006", "q700a1", "q700a2", "q700a6", 
#                    "q706", "q701c", "q701d6", "q707", "geometry")]

# wave4 <- wave4[, c("psu", "q13", "urban", "rural", "refugee_camp", "a1", "q1", "q2", "q7001", "q7002", "q707", "geometry")]

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
# wave3$geometry <- as(wave3$geometry, "Spatial")
wave3 <- SpatialPolygonsDataFrame(Sr=wave3$geometry, data = wave3, match.ID = F)
wave3$roads <- sapply(wave3$roads, length)
wave3$treatment <- ifelse(wave3$roads>0, 1, 0)

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

wave3$dist_to_city <- raster(paste0(box_loc, "/inputData/city_access/accessibility_eujrc.tif")) %>%
  extract(., wave3, fun=mean) %>%
  as.numeric(.)

###

# wave4 <- wave4[wave4$psu %in% wave3]

writeOGR(wave3[, names(wave3)!="geometry"], dsn = "/Users/christianbaehr/Desktop/wave3.geojson", layer = "qid", driver = "GeoJSON")
writeOGR(wave4[, names(wave4)!="geometry"], dsn = "/Users/christianbaehr/Desktop/wave4.geojson", layer = "qid", driver = "GeoJSON")

test3 <- wave3
test4 <- wave4

test3$dmsp_pretrend <- NA

for(i in unique(test3$qid)) {
  
  dmsp <- as.numeric(test3[test3$qid==i, paste0("dmsp", 2007:2012)]@data[1,])
  
  trend <- lm(dmsp ~ c(1:6))
  
  test3$dmsp_pretrend[test3$qid==i] <- trend$coefficients[2]
}


###

sum(test4$q2 %in% test3$v05aENG)
sum(test3$v05aENG %in% test4$q2)
test3$v05aENG[test3$v05aENG=="Al-faraa Refugee"] <- "Al-Faraa Refugee"
test3$v05aENG[test3$v05aENG=="Askar Refugee"] <- "Askar refugee"

unique(test3$v05aENG[!test3$v05aENG %in% test4$q2])
test3 <- test3[test3$v05aENG %in% test4$q2,]

unique(test4$q2[!test4$q2 %in% test3$v05aENG])
test4 <- test4[test4$q2 %in% test3$v05aENG,]

stargazer(test3@data, type = "html", title = "Wave III Summary Statistics (Full Sample)",
          keep = c("urban", "rural", "refugee_camp", "q7001", "q7002", "q7006", "q700a1", "q700a2", "q700a6", "q706", "q701c", 
                   "viirs2012max", "population", "dist_to_city", "dmsp_pretrend"),
          covariate.labels = c("Urban", "Rural", "Refugee camp",
                               "Do you prefer economic relations btwn Pal./US improve",
                               "Do you prefer economic relations btwn Pal./Saudi improve",
                               "Do you prefer economic relations btwn Pal./Israel improve",
                               "Do you prefer security relations btwn Pal./US improve",
                               "Do you prefer security relations btwn Pal./Saudi improve",
                               "Do you prefer security relations btwn Pal./Israel improve",
                               "Do you agree: US interference justifies arms against the US?",
                               "Should foreign aid to your country increase?",
                               "VIIRS Baseline (2012 Max)",
                               "Population (CIESIN)",
                               "Distance to nearest city (EU-JRC)",
                               "DMSP Pre-trend (2007-12)"),
          out = "/Users/christianbaehr/Desktop/test.html")

stargazer(test3@data[test3$treatment==1, ], type = "html", title = "Wave III Summary Statistics (Treated Only)",
          keep = c("urban", "rural", "refugee_camp", "q7001", "q7002", "q7006", "q700a1", "q700a2", "q700a6", "q706", "q701c", 
                   "viirs2012max", "population", "dist_to_city", "dmsp_pretrend"),
          covariate.labels = c("Urban", "Rural", "Refugee camp",
                               "Do you prefer economic relations btwn Pal./US improve",
                               "Do you prefer economic relations btwn Pal./Saudi improve",
                               "Do you prefer economic relations btwn Pal./Israel improve",
                               "Do you prefer security relations btwn Pal./US improve",
                               "Do you prefer security relations btwn Pal./Saudi improve",
                               "Do you prefer security relations btwn Pal./Israel improve",
                               "Do you agree: US interference justifies arms against the US?",
                               "Should foreign aid to your country increase?",
                               "VIIRS Baseline (2012 Max)",
                               "Population (CIESIN)",
                               "Distance to nearest city (EU-JRC)",
                               "DMSP Pre-trend (2007-12)"),
          out = "/Users/christianbaehr/Desktop/test_treated.html")

stargazer(test3@data[test3$treatment==0, ], type = "html", title = "Wave III Summary Statistics (Control Only)",
          keep = c("urban", "rural", "refugee_camp", "q7001", "q7002", "q7006", "q700a1", "q700a2", "q700a6", "q706", "q701c", 
                   "viirs2012max", "population", "dist_to_city", "dmsp_pretrend"),
          covariate.labels = c("Urban", "Rural", "Refugee camp",
                               "Do you prefer economic relations btwn Pal./US improve",
                               "Do you prefer economic relations btwn Pal./Saudi improve",
                               "Do you prefer economic relations btwn Pal./Israel improve",
                               "Do you prefer security relations btwn Pal./US improve",
                               "Do you prefer security relations btwn Pal./Saudi improve",
                               "Do you prefer security relations btwn Pal./Israel improve",
                               "Do you agree: US interference justifies arms against the US?",
                               "Should foreign aid to your country increase?",
                               "VIIRS Baseline (2012 Max)",
                               "Population (CIESIN)",
                               "Distance to nearest city (EU-JRC)",
                               "DMSP Pre-trend (2007-12)"),
          out = "/Users/christianbaehr/Desktop/test_control.html")


stargazer(test4@data, type = "html", 
          keep = c("q7001", "q7002", "q7006", "q700a1", "q700a2", "q700a6", "q706", "q701c", "dmsp_pretrend", 
                   "viirs2012_max", "population"),
          covariate.labels = c("Do you prefer economic relations btwn Pal./US improve",
                               "Do you prefer economic relations btwn Pal./Saudi improve",
                               "Do you prefer economic relations btwn Pal./Israel improve",
                               "Do you prefer security relations btwn Pal./US improve",
                               "Do you prefer security relations btwn Pal./Saudi improve",
                               "Do you prefer security relations btwn Pal./Israel improve",
                               "Do you agree: US interference justifies arms against the US?",
                               "Should foreign aid to your country increase?",
                               "DMSP Pre-trend (2007-12)",
                               "VIIRS Baseline (2012 Max)",
                               "Population (CIESIN)"),
          out = "/Users/christianbaehr/Desktop/test.html")









