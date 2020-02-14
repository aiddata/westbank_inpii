
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
wave3roads <- over(wave3, roads, returnList = T)

extract.dates <- function(x) {
  y <- as.Date(x[["date"]], tryFormats="%m/%d/%y", optional=T)
  return(as.character(y[!is.na(y)]))
}

wave3roads <- lapply(wave3roads, FUN = extract.dates)

# wave3 <- as.data.frame(wave3, stringsAsFactors=F)

wave3$roads <- wave3roads
# wave3$geometry <- as(wave3$geometry, "Spatial")
# wave3 <- SpatialPolygonsDataFrame(Sr=wave3$geometry, data = wave3, match.ID = F)
wave3$roads <- sapply(wave3$roads, length)
wave3$treatment <- ifelse(wave3$roads>0, 1, 0)


wave4roads <- over(wave4, roads, returnList = T)
wave4roads <- lapply(wave4roads, FUN = extract.dates)

wave4$roads <- wave4roads
wave4$roads <- sapply(wave4$roads, length)
wave4$treatment <- ifelse(wave4$roads>0, 1, 0)

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

test4$age <- as.numeric(test4$q1001)
test4$male <- ifelse(test4$q1002=="Male", 1, 0)

test4$education <- ifelse(test4$q1003=="MA and above", 7,
                          ifelse(test4$q1003=="BA", 6,
                                 ifelse(test4$q1003=="Mid-level diploma/professional or technical", 5,
                                        ifelse(test4$q1003=="Secondary", 4,
                                               ifelse(test4$q1003=="Preparatory/Basic", 3,
                                                      ifelse(test4$q1003=="Elementary", 2,
                                                             ifelse(test4$q1003=="Illiterate/No formal education", 1, NA)))))))

test4$married <- ifelse(test4$q1010=="Married", 1, 0)

test4$muslim <- ifelse(test4$q1012=="Muslim", 1, 0)

test4$christian <- ifelse(test4$q1012=="Christian", 1, 0)

### No explicit income measure in test4

test4$urban <- ifelse(test4$v05=="city", 1, 0)
test4$rural <- ifelse(test4$v05 %in% c("Village/Town", "Village/ Town"), 1, 0)
test4$refugee_camp <- ifelse(test4$v05=="Refugee Camp", 1, 0)

test4$employed <- ifelse(test4$q1004=="Yes", 1, 0)

test4$full_time <- ifelse(test4$q1006=="Full time (30 hours or more a week)", 1, 0)

test4$public_employee <- ifelse(test4$q1006a=="Public", 1,
                                ifelse(test4$q1006a=="", NA, 0))

test4$retired <- ifelse(test4$q1005=="Retired", 1,
                        ifelse(test4$q1005=="Decline to answer (Do not read)", NA, 0))

test4$housewife <- ifelse(test4$q1005=="Housewife", 1,
                          ifelse(test4$q1005=="Decline to answer (Do not read)", NA, 0))

test4$student <- ifelse(test4$q1005=="Student", 1,
                        ifelse(test4$q1005=="Decline to answer (Do not read)", NA, 0))

test4$student <- ifelse(test4$q1005=="Student", 1,
                        ifelse(test4$q1005=="Decline to answer (Do not read)", NA, 0))

test4$unemployed <- ifelse(test4$q1005=="Unemployed", 1,
                           ifelse(test4$q1005=="Decline to answer (Do not read)", NA, 0))

### No home ownership question in Wave IV

test4$own_a_computer <- ifelse(test4$q1011a=="Yes", 1, 0)
test4$own_a_car <- ifelse(test4$q1011b=="Yes", 1, 0)

## future economic relations btwn palestine/US
test4$q7001 <- ifelse(test4$q7001=="Become stronger than they were in previous years", 1,
                      ifelse(test4$q7001=="Remain the same as they were in previous years", 0,
                             ifelse(test4$q7001=="Become weaker than they were in previous years", -1, NA)))

## future economic relations btwn palestine/Saudi
test4$q7002 <- ifelse(test4$q7002=="Become stronger than they were in previous years", 1,
                      ifelse(test4$q7002=="Remain the same as they were in previous years", 0,
                             ifelse(test4$q7002=="Become weaker than they were in previous years", -1, NA)))


## influence of US on development of democracy in your country. Not asked in Wave III
test4$q7011 <- ifelse(test4$q7011=="Very positive", 2,
                      ifelse(test4$q7011=="Somewhat positive", 1,
                             ifelse(test4$q7011=="Neither positive nor negative", 0,
                                    ifelse(test4$q7011=="Somewhat negative", -1,
                                           ifelse(test4$q7011=="Very negative", -2, NA)))))

## do you think (ordinary) americans are generally good people? Slight variation of Wave III
test4$q707 <- ifelse(test4$q707=="Agree", 1,
                     ifelse(test4$q707=="Disagree", 0, NA))

## American and Western culture have positive aspects? Not asked in Wave III
test4$q708 <- ifelse(test4$q708=="Strongly agree", 2,
                     ifelse(test4$q708=="Agree", 1,
                            ifelse(test4$q708=="Disagree", -1,
                                   ifelse(test4$q708=="Strongly disagree", -2, NA))))

## Do you support the two-state Arab-Israeli solution?
test4$q709a <- ifelse(test4$q709a=="Support", 1,
                      ifelse(test4$q709a=="Oppose", 0, NA))


## Is western infuence an obstacle to reform in your country?
test4$q7114 <- ifelse(test4$q7114=="Agree to a large extent", 2,
                      ifelse(test4$q7114=="Agree to some extent", 1,
                             ifelse(test4$q7114=="Don’t agree", -1,
                                    ifelse(test4$q7114=="Don’t agree at all", -2, NA))))

## What country poses the greatest threat to stability in your country? (US dummy)
test4$q714us <- ifelse(test4$q714=="The United States", 1, 
                       ifelse(test4$q714 %in% c("Decline to answer (Do not read)", "Don't know (Do not read)"), NA, 0))

## What country poses the greatest threat to stability in your country? (SA dummy)
test4$q714sa <- ifelse(test4$q714=="Saudi Arabia", 1, 
                       ifelse(test4$q714 %in% c("Decline to answer (Do not read)", "Don't know (Do not read)"), NA, 0))

## What country poses the greatest threat to stability in your country? (ISR dummy)
test4$q714is <- ifelse(test4$q714=="Israel", 1, 
                       ifelse(test4$q714 %in% c("Decline to answer (Do not read)", "Don't know (Do not read)"), NA, 0))

## to what degree you would describe yourself as feeling angry toward the US?
test4$q8341 <- ifelse(test4$q8341=="Very angry", 4,
                      ifelse(test4$q8341=="Somewhat angry", 3,
                             ifelse(test4$q8341=="Not very angry", 2,
                                    ifelse(test4$q8341=="Not angry at all", 1, NA))))


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

unique(test4$v05aENG[!test4$v05aENG %in% test3$v05aENG])
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
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_full_wave3.tex")

stargazer(test3@data[test3$treatment==1, out_names], type = "latex", title = "Wave III Demographics (Treatment Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_treatment_wave3.tex")

stargazer(test3@data[test3$treatment==0, out_names], type = "latex", title = "Wave III Demographics (Control Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_control_wave3.tex")

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
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/covariates_full_wave3.tex", 
          digits.extra = 0)

stargazer(test3@data[test3$treatment==1, out_names], type = "latex", title = "Wave III Covariates (Treatment Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/covariates_treatment_wave3.tex",
          digits.extra = 0)

stargazer(test3@data[test3$treatment==0, out_names], type = "latex", title = "Wave III Covariates (Control Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/covariates_control_wave3.tex",
          digits.extra = 0)

###

out_names <- c("q7001",
               "q7002",
               "q7006",
               "q700a1",
               "q700a2",
               "q700a6",
               "q706",
               "q707",
               "q701c")

out_labels <- c("Do you prefer economic relations btwn Pal./US improve",
                "Do you prefer economic relations btwn Pal./Saudi improve",
                "Do you prefer economic relations btwn Pal./Israel improve",
                "Do you prefer security relations btwn Pal./US improve",
                "Do you prefer security relations btwn Pal./Saudi improve",
                "Do you prefer security relations btwn Pal./Israel improve",
                "Do you agree: US interference justifies arms against the US?",
                "Americans are generally good people",
                "Should foreign aid to your country increase?")

stargazer(test3@data[, out_names], type = "latex", title = "Wave III Survey Questions (Full Sample)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_full_wave3.tex", 
          digits.extra = 0)

stargazer(test3@data[test3$treatment==1, out_names], type = "latex", title = "Wave III Survey Questions (Treatment Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_treatment_wave3.tex",
          digits.extra = 0)

stargazer(test3@data[test3$treatment==0, out_names], type = "latex", title = "Wave III Survey Questions (Control Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_control_wave3.tex",
          digits.extra = 0)

###

writeOGR(obj = test3[names(test3)!="geometry"], dsn = "/Users/christianbaehr/Desktop/wave3.geojson",
         layer = "qid", driver = "GeoJSON")

#################################################

out_names <- c("age",
               "male",
               "education",
               "married",
               "muslim",
               "christian",
               "employed",
               "own_a_car")

out_labels <- c("Age",
                "Male (dummy)",
                "Level of Educ., 0=None, 7=MA+",
                "Married (dummy)",
                "Muslim (dummy)",
                "Christian (dummy)",
                "Employed (dummy)",
                "Owns a car (dummy)")

stargazer(test4@data[, out_names], type = "latex", title = "Wave IV Demographics (Full Sample)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_full_wave4.tex")

stargazer(test4@data[test4$treatment==1, out_names], type = "latex", title = "Wave IV Demographics (Treatment Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_treatment_wave4.tex")

stargazer(test4@data[test4$treatment==0, out_names], type = "latex", title = "Wave IV Demographics (Control Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/demographics_control_wave4.tex")


###

out_names <- c("q7001",
               "q7002",
               "q7011",
               "q707",
               "q708",
               "q709a",
               "q7114",
               "q714us",
               "q714sa",
               "q714is",
               "q8341")

out_labels <- c("Do you prefer economic relations btwn Pal./US improve",
                "Do you prefer economic relations btwn Pal./Saudi improve",
                "How does US influence democracy development in Pal.",
                "*Ordinary* Americans are generally good people",
                "American and Western culture has positive aspects",
                "Do you support the two-state Arab-Israeli solution", 
                "Is western influence an obstacle to reform in Pal.?",
                "What country poses greatest threat to your country (US Dummy)",
                "What country poses greatest threat to your country (Saudi Dummy)",
                "What country poses greatest threat to your country (ISR Dummy)",
                "To what degree do you feel angry towards the US?")

stargazer(test4@data[, out_names], type = "latex", title = "Wave IV Survey Questions (Full Sample)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_full_wave4.tex", 
          digits.extra = 0)

stargazer(test4@data[test4$treatment==1, out_names], type = "latex", title = "Wave IV Survey Questions (Treatment Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_treatment_wave4.tex",
          digits.extra = 0)

stargazer(test4@data[test4$treatment==0, out_names], type = "latex", title = "Wave IV Survey Questions (Control Only)",
          covariate.labels = out_labels, out = "/Users/christianbaehr/Desktop/survey_control_wave4.tex",
          digits.extra = 0)


###

writeOGR(obj = test4[names(test4)!="geometry"], dsn = "/Users/christianbaehr/Desktop/wave4.geojson",
         layer = "qid", driver = "GeoJSON")














#################################################
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









