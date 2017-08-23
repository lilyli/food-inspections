#### STEP 1: get community area of each restaurant ####

#code adapted from https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile
library(rgeos)
library(sp)
library(rgdal)
library(jsonlite)

#food inspection data, from Chicago Data Portal
fooddata <- read.csv('~/Desktop/food-inspections/data/Food_Inspections.csv')

#need to delete observations without location
fooddata <- fooddata[fooddata$Location != "", ]

chicagomap <- readOGR(dsn = path.expand("~/Desktop/food-inspections/data/Boundaries"))
coordinates(fooddata) <- ~ Longitude + Latitude
proj4string(fooddata) <- proj4string(chicagomap)
proj4string(fooddata) <- CRS("+proj=longlat")
fooddata <- spTransform(fooddata, proj4string(chicagomap))
fooddata$neighborhood <- over(fooddata, chicagomap)

fooddata2 <- as.data.frame(fooddata)

#need to flatten df because it's nested, since neighborhood variables were from another df
fooddata3 <- flatten(fooddata2, recursive = TRUE)
fooddata3 <- fooddata3[, -c(18, 20:23, 25:26)]

write.csv(fooddata3, "Food_Inspections_with_Community.csv")

#### STEP 2: sort crime data by community area ####

#sorting crime data
crimedata <- read.csv('~/Desktop/food-inspections/data/Crimes_2009_to_2015.csv')

library(dplyr)

#clean repetitive crime types
crimedata$Primary.Type <- gsub("(.*)NON(.*)CRIMINAL(.*)","NON-CRIMINAL", crimedata$Primary.Type)
crimedata$Primary.Type <- as.character(crimedata$Primary.Type)
crimedata$num <- 1

aggregcrimedata <- crimedata %>%
  group_by(Primary.Type, Community.Area, Year) %>%
  summarize(crimenum = sum(num))

#census data from https://www.cityofchicago.org/content/dam/city/depts/zlup/Zoning_Main_Page/Publications/Census_2010_Community_Area_Profiles/Census_2010_and_2000_CA_Populations.pdf

censusdata <- read.csv('~/Desktop/food-inspections/data/comm_census_data.csv')
censusdata$population <- as.numeric(gsub(",", "", censusdata$population))

crime_names_list <- unique(aggregcrimedata$Primary.Type)
crime <- as.character(rep(crime_names_list, each = 7*77))
year <- c(rep(2009:2015, times = 77*31))
comm_area <- c(rep(1:77, times = 31, each = 7))
aggregcrimedata_frame <- data_frame(year, comm_area, crime)

colnames(aggregcrimedata)[1:3] <- c('crime', 'comm_area', 'year')

aggregcrimedata2 <- left_join(aggregcrimedata_frame, aggregcrimedata, by = c("year", "comm_area", "crime"))

aggregcrimedata2[is.na(aggregcrimedata2)] <- 0

aggregcrimedata_crime <- left_join(aggregcrimedata2, censusdata, 'comm_area')
aggregcrimedata_crime$crimerate_per_thousand <- (aggregcrimedata_crime$crimenum/aggregcrimedata_crime$population)*1000

list2env(split(aggregcrimedata_crime, crime), envir = .GlobalEnv)

ARSON$ARSON_crimerate <- ARSON$crimerate_per_thousand
ASSAULT$ASSAULT_crimerate <- ASSAULT$crimerate_per_thousand
BATTERY$BATTERY_crimerate <- BATTERY$crimerate_per_thousand
BURGLARY$BURGLARY_crimerate <- BURGLARY$crimerate_per_thousand
`CONCEALED CARRY LICENSE VIOLATION`$CONC_CARRY_crimerate <- `CONCEALED CARRY LICENSE VIOLATION`$crimerate_per_thousand
`CRIM SEXUAL ASSAULT`$SEXUAL_ASSAULT_crimerate <- `CRIM SEXUAL ASSAULT`$crimerate_per_thousand
`CRIMINAL DAMAGE`$CRIM_DAMAGE_crimerate <- `CRIMINAL DAMAGE`$crimerate_per_thousand
`CRIMINAL TRESPASS`$CRIM_TRESPASS_crimerate <- `CRIMINAL TRESPASS`$crimerate_per_thousand
`DECEPTIVE PRACTICE`$DECEP_PRACT_crimerate <- `DECEPTIVE PRACTICE`$crimerate_per_thousand
GAMBLING$GAMBLING_crimerate <- GAMBLING$crimerate_per_thousand
HOMICIDE$HOMICIDE_crimerate <- HOMICIDE$crimerate_per_thousand
`HUMAN TRAFFICKING`$HUMAN_TRAFF_crimerate <- `HUMAN TRAFFICKING`$crimerate_per_thousand
`INTERFERENCE WITH PUBLIC OFFICER`$INTERF_PUB_OFF_crimerate <- `INTERFERENCE WITH PUBLIC OFFICER`$crimerate_per_thousand
INTIMIDATION$INTIMIDATION_crimerate <- INTIMIDATION$crimerate_per_thousand
KIDNAPPING$KIDNAPPING_crimerate <- KIDNAPPING$crimerate_per_thousand
`LIQUOR LAW VIOLATION`$LIQUOR_crimerate <- `LIQUOR LAW VIOLATION`$crimerate_per_thousand
`MOTOR VEHICLE THEFT`$MOTOR_THEFT_crimerate <- `MOTOR VEHICLE THEFT`$crimerate_per_thousand
NARCOTICS$NARCOTICS_crimerate <- NARCOTICS$crimerate_per_thousand
`NON-CRIMINAL`$NON_CRIM_crimerate <- `NON-CRIMINAL`$crimerate_per_thousand
OBSCENITY$OBSCENITY_crimerate <- OBSCENITY$crimerate_per_thousand
`OFFENSE INVOLVING CHILDREN`$OFFENSE_CHILDREN_crimerate <- `OFFENSE INVOLVING CHILDREN`$crimerate_per_thousand
`OTHER NARCOTIC VIOLATION`$OTHER_NARCOTIC_crimerate <- `OTHER NARCOTIC VIOLATION`$crimerate_per_thousand
`OTHER OFFENSE`$OTHER_OFFENSE_crimerate <- `OTHER OFFENSE`$crimerate_per_thousand
PROSTITUTION$PROSTITUTION_crimerate <- PROSTITUTION$crimerate_per_thousand
`PUBLIC INDECENCY`$PUBLIC_INDEC_crimerate <- `PUBLIC INDECENCY`$crimerate_per_thousand
`PUBLIC PEACE VIOLATION`$PUB_PEACE_crimerate <- `PUBLIC PEACE VIOLATION`$crimerate_per_thousand
ROBBERY$ROBBERY_crimerate <- ROBBERY$crimerate_per_thousand
`SEX OFFENSE`$SEX_OFFENSE_crimerate <- `SEX OFFENSE`$crimerate_per_thousand
STALKING$STALKING_crimerate <- STALKING$crimerate_per_thousand
THEFT$THEFT_crimerate <- THEFT$crimerate_per_thousand
`WEAPONS VIOLATION`$WEAPONS_VIOL_crimerate <- `WEAPONS VIOLATION`$crimerate_per_thousand

crime_dfs <- list(ARSON, ASSAULT, BATTERY, BURGLARY, `CONCEALED CARRY LICENSE VIOLATION`, `CRIM SEXUAL ASSAULT`, `CRIMINAL DAMAGE`, `CRIMINAL TRESPASS`, `DECEPTIVE PRACTICE`, GAMBLING, HOMICIDE, `HUMAN TRAFFICKING`, `INTERFERENCE WITH PUBLIC OFFICER`, INTIMIDATION, KIDNAPPING, `LIQUOR LAW VIOLATION`, `MOTOR VEHICLE THEFT`, NARCOTICS, `NON-CRIMINAL`, OBSCENITY, `OFFENSE INVOLVING CHILDREN`, `OTHER NARCOTIC VIOLATION`, `OTHER OFFENSE`, PROSTITUTION, `PUBLIC INDECENCY`, `PUBLIC PEACE VIOLATION`, ROBBERY, `SEX OFFENSE`, STALKING, THEFT, `WEAPONS VIOLATION`)

for (i in 1:length(crime_dfs)) {
  crime_dfs[[i]] <- crime_dfs[[i]][, -c(3:6)]
}

library(data.table)

#from http://stackoverflow.com/questions/2209258/merge-several-data-frames-into-one-data-frame-with-a-loop/2209371
crime_complete <- Reduce(function(x, y) merge(x, y, all=T, by=c("year", "comm_area")), crime_dfs, accumulate=F)

write.csv(crime_complete, '~/Desktop/food-inspections/data/aggregated_crime_data.csv')


#### STEP 3: construct past inspections variable ####

fooddata <- read.csv('~/Desktop/food-inspections/data/Food_Inspections_with_Community.csv', as.is=TRUE)

library(tidyverse)
library(lubridate)

fooddata2 <- fooddata %>%
  mutate(Inspection.Date = mdy(Inspection.Date)) %>%
  arrange(License.., Inspection.Date) %>%
  group_by(License.., DBA.Name) %>%
  mutate(total_inspect = 1:n()) %>%
  mutate(fail_result = ifelse(Results == "Fail", 1, 0)) %>%
  mutate(failed_inspect = cumsum(fail_result)) %>%
  select(-fail_result) %>%
  arrange(Inspection.Date)

fooddata3 <- fooddata2 %>%
  mutate(past_inspect = ifelse(total_inspect == 0, 0, total_inspect - 1)) %>%
  mutate(past_failed_inspect = ifelse(Results == "Fail" & failed_inspect != 0, failed_inspect - 1, failed_inspect)) %>%
  select(-total_inspect, -failed_inspect)

names(fooddata3)[names(fooddata3) == 'total_inspect'] <- 'num_past_inspections'
names(fooddata3)[names(fooddata3) == 'fail_inspect'] <- 'num_past_failed_inspections'

write.csv(fooddata3, '~/Desktop/food-inspections/data/Food_Inspections_with_Community_updated.csv')


#### STEP 4: add 311 data to food inspections ####

library(dplyr)

rodent_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Rodent_Baiting.csv')
rodent_data <- rodent_data[-1, ]
rodent_data <- rodent_data[!grepl(" - Dup", rodent_data$Status),] #delete duplicates
rodent_data <- rodent_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(rodent_data)[3:4] <- c("num_rodent_complaints", "avg_rodent_complaint_length")

tree_debris_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Tree_Debris.csv')
tree_debris_data <- tree_debris_data[-1, ]
tree_debris_data <- tree_debris_data[!grepl(" - Dup", tree_debris_data$Status),]
tree_debris_data <- tree_debris_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(tree_debris_data)[3:4] <- c("num_tree_debris_complaints", "avg_tree_debris_complaint_length")

abandoned_vehicles_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Abandoned_Vehicles.csv')
abandoned_vehicles_data <- abandoned_vehicles_data[-1, ]
abandoned_vehicles_data <- abandoned_vehicles_data[!grepl(" - Dup", abandoned_vehicles_data$Status),]
abandoned_vehicles_data <- abandoned_vehicles_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(abandoned_vehicles_data)[3:4] <- c("num_abandoned_vehicles_complaints", "avg_abandoned_vehicles_complaint_length")

alley_lights_out_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Alley_Lights_Out.csv')
alley_lights_out_data <- alley_lights_out_data[-1, ]
alley_lights_out_data <- alley_lights_out_data[!grepl(" - Dup", alley_lights_out_data$Status),]
alley_lights_out_data <- alley_lights_out_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(alley_lights_out_data)[3:4] <- c("num_alley_lights_out_complaints", "avg_alley_lights_out_complaint_length")

garbage_carts_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Garbage_Carts.csv')
garbage_carts_data <- garbage_carts_data[-1, ]
garbage_carts_data <- garbage_carts_data[!grepl(" - Dup", garbage_carts_data$Status),]
garbage_carts_data <- garbage_carts_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(garbage_carts_data)[3:4] <- c("num_garbage_carts_complaints", "avg_garbage_carts_complaint_length")

graffiti_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Graffiti_Removal.csv')
graffiti_data <- graffiti_data[-1, ]
graffiti_data <- graffiti_data[!grepl(" - Dup", graffiti_data$Status),]
graffiti_data <- graffiti_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(graffiti_data)[3:4] <- c("num_graffiti_complaints", "avg_graffiti_complaint_length")

pot_holes_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Pot_Holes_Reported.csv')
pot_holes_data <- pot_holes_data[-1, ]
pot_holes_data <- pot_holes_data[!grepl(" - Dup", pot_holes_data$STATUS),]
pot_holes_data <- pot_holes_data %>%
  mutate(CREATION.DATE = mdy(CREATION.DATE)) %>%
  mutate(COMPLETION.DATE = mdy(COMPLETION.DATE)) %>%
  mutate(year = year(CREATION.DATE)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = COMPLETION.DATE-CREATION.DATE) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(pot_holes_data)[3:4] <- c("num_pot_holes_complaints", "avg_pot_holes_complaint_length")

sanitation_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Sanitation_Code_Complaints.csv')
sanitation_data <- sanitation_data[-1, ]
sanitation_data <- sanitation_data[!grepl(" - Dup", sanitation_data$Status),]
sanitation_data <- sanitation_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(sanitation_data)[3:4] <- c("num_sanitation_complaints", "avg_sanitation_complaint_length")

lights_all_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Street_Lights_All_Out.csv')
lights_all_data <- lights_all_data[-1, ]
lights_all_data <- lights_all_data[!grepl(" - Dup", lights_all_data$Status),]
lights_all_data <- lights_all_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(lights_all_data)[3:4] <- c("num_lights_all_complaints", "avg_lights_all_complaint_length")

lights_one_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Street_Lights_One_Out.csv')
lights_one_data <- lights_one_data[-1, ]
lights_one_data <- lights_one_data[!grepl(" - Dup", lights_one_data$Status),]
lights_one_data <- lights_one_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(lights_one_data)[3:4] <- c("num_lights_one_complaints", "avg_lights_one_complaint_length")

tree_trims_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Tree_Trims.csv')
tree_trims_data <- tree_trims_data[-1, ]
tree_trims_data <- tree_trims_data[!grepl(" - Dup", tree_trims_data$Status),]
tree_trims_data <- tree_trims_data %>%
  mutate(Creation.Date = mdy(Creation.Date)) %>%
  mutate(Completion.Date = mdy(Completion.Date)) %>%
  mutate(year = year(Creation.Date)) %>%
  filter(year <= 2015 & year >= 2011) %>%
  mutate(complaint_length = Completion.Date-Creation.Date) %>%
  mutate(num = 1) %>%
  select(-c(1:3)) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaint_length = sum(complaint_length), num_complaints = sum(num)) %>%
  mutate(avg_complaint_length = total_complaint_length/num_complaints) %>%
  select(-c(3))
colnames(tree_trims_data)[3:4] <- c("num_tree_trims_complaints", "avg_tree_trims_complaint_length")

abandoned_buildings_data <- read.csv('~/Desktop/food-inspections/data/311 Data/311_Vacant_and_Abandoned_Buildings_Reported.csv')
abandoned_buildings_data <- abandoned_buildings_data[-1, ]
abandoned_buildings_data <- abandoned_buildings_data %>%
  mutate(DATE.SERVICE.REQUEST.WAS.RECEIVED = mdy(DATE.SERVICE.REQUEST.WAS.RECEIVED)) %>%
  mutate(year = year(DATE.SERVICE.REQUEST.WAS.RECEIVED)) %>%
  filter(year <= 2015 & year >= 2010) %>%
  mutate(num = 1) %>%
  group_by(Community.Area, year) %>%
  summarise(total_complaints = sum(num))
colnames(abandoned_buildings_data)[3] <- c("num_abandoned_buildings_complaints")

fooddata <- read.csv('~/Desktop/food-inspections/data/Food_Inspections_with_Community_updated.csv')
names(fooddata)[names(fooddata) == 'neighborhood.area_numbe'] <- 'Community.Area'

#first create year variable in food inspections
fooddata$year <- substr(fooddata$Inspection.Date, 1, 4)
fooddata$year <- as.numeric(fooddata$year)

fooddata <- fooddata[fooddata$year <= 2016, ]
fooddata <- fooddata[fooddata$year >= 2010, ]

fooddata_new <- list(fooddata, abandoned_buildings_data, abandoned_vehicles_data , alley_lights_out_data, garbage_carts_data, graffiti_data, lights_all_data, lights_one_data, pot_holes_data, rodent_data, sanitation_data, tree_debris_data, tree_trims_data) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1, dtf2, by = c('Community.Area', 'year')),.) #from http://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list

#create missing variable indicator
#1 if variable missing, 0 if it's there
#replace NA values with 1000

for(i in 25:47) {
  fooddata_new <- cbind(fooddata_new, fooddata_new$X.1+(fooddata_new$X.1*i))
  colnames(fooddata_new)[i+23] <- paste(colnames(fooddata_new)[i], "miss", sep="_")
}

for(i in 48:70) {
  fooddata_new[[i]] <- ifelse(is.na(fooddata_new[[i-23]]), 1, 0)
  fooddata_new[[i-23]][is.na(fooddata_new[[i-23]])] <- 1000
}

write.csv(fooddata_new, '~/Desktop/food-inspections/data/Food_Inspections_with_Community_311.csv')


#### STEP 5: add crime data to food inspections ####

fooddata <- read.csv('~/Desktop/food-inspections/data/Food_Inspections_with_Community_311.csv', as.is=TRUE)

#need to distinguish between inspection and crime rate
colnames(fooddata)[colnames(fooddata) == 'year'] <- 'inspect_year'

fooddata$year <- fooddata$inspect_year - 1

aggregcrimedata <- read.csv('~/Desktop/food-inspections/data/aggregated_crime_data.csv')
aggregcrimedata <- aggregcrimedata[, -1]

library(dplyr)

colnames(fooddata)[colnames(fooddata) == 'Community.Area'] <- 'comm_area'
fooddata_crime <- left_join(fooddata, aggregcrimedata, by = c('comm_area', 'year'))

colnames(fooddata_crime)[colnames(fooddata_crime) == 'year'] <- 'crime_year'

write.csv(fooddata_crime, '~/Desktop/food-inspections/data/food_inspections_crime.csv')


#### STEP 6: add socioeconomic indicators to food inspections ####

library(dplyr)

fooddata <- read.csv('~/Desktop/food-inspections/data/food_inspections_crime.csv', as.is=TRUE)
socialdata <- read.csv('~/Desktop/food-inspections/data/Census_Data_Selected_socioeconomic_indicators.csv', as.is=TRUE)

colnames(socialdata)[1] <- c("comm_area")

fooddata_social <- left_join(fooddata, socialdata, by = "comm_area")

write.csv(fooddata_social, '~/Desktop/food-inspections/data/food_inspections_final.csv')


#### STEP 7: some final data cleaning ####

fooddata <- read.csv('~/Desktop/food-inspections/data/food_inspections_final.csv', as.is=TRUE)

fooddata2 <- fooddata
fooddata2 <- fooddata2[, -c(1:8, 12:14, 19, 20:23, 74, 106)]
#replace inspection date with day, month, year
library(tidyverse)
library(lubridate)
fooddata2 <- fooddata2 %>%
  mutate(Inspection.Date = ymd(Inspection.Date)) %>%
  mutate(month = month(Inspection.Date)) %>%
  mutate(day = day(Inspection.Date))
fooddata2 <- fooddata2[, -c(5)]
fooddata2 <- na.omit(fooddata2)

#clean facility types
fooddata2$Facility.Type <- tolower(fooddata2$Facility.Type)
library(stringr)
fooddata2$Facility.Type <- str_replace(str_trim(fooddata2$Facility.Type), "\\s+", " ")
fooddata2$Facility.Type <- trimws(fooddata2$Facility.Type, which = c("both"))
fooddata2$Facility.Type[agrep("daycare", fooddata2$Facility.Type)] <- "daycare"
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, " and ", "/")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, " & ", "/")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "& ", "/")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, " &", "/")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, " /", "/")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, " / ", "/")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "/ ", "/")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "\\(", "")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "\\)", "")
fooddata2$Facility.Type <- gsub("long-term care facility|assissted living|long term care|1005 nursing home|assisted living|long-term care|supporting living|adult family care center|supportive living facility", "nursing home", fooddata2$Facility.Type)
fooddata2$Facility.Type <- gsub("hooka lounge|hooka bar", "hookah lounge", fooddata2$Facility.Type)
fooddata2$Facility.Type <- gsub("rooftop|roof tops|roof top|rooftops", "rooftop", fooddata2$Facility.Type)
fooddata2$Facility.Type[agrep("frozen dessert", fooddata2$Facility.Type)] <- "frozen dessert pushcarts"
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "commiasary", "commissary")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "convnience", "convenience")
fooddata2$Facility.Type <- gsub("restuarant|rstaurant|reataurant", "restaurant", fooddata2$Facility.Type)
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "shcool", "school")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "herbal life", "herbalife")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "linited", "limited")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "trem", "term")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "conncession", "concession")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "prepacakaged", "prepackaged")
fooddata2$Facility.Type <- gsub("tavern|liquor|bar|liquore", "bar", fooddata2$Facility.Type)
fooddata2$Facility.Type[agrep("profit", fooddata2$Facility.Type)] <- "nonprofit"
fooddata2$Facility.Type[agrep("banquet", fooddata2$Facility.Type)] <- "banquet"
fooddata2$Facility.Type[agrep("herbalife", fooddata2$Facility.Type)] <- "herbalife"
fooddata2$Facility.Type[agrep("1023", fooddata2$Facility.Type)] <- "children’s services facility"
fooddata2$Facility.Type <- gsub("childern activity facility|childrens services facility|childern's service facility|childern's services  facility|children’s services facility", "children's services facility", fooddata2$Facility.Type)
fooddata2$Facility.Type[agrep("mobil", fooddata2$Facility.Type)] <- "food cart"
fooddata2$Facility.Type[agrep("paleteria", fooddata2$Facility.Type)] <- "paleteria"
fooddata2$Facility.Type <- gsub("[[:punct:]]$", "", fooddata2$Facility.Type)
fooddata2$Facility.Type <- gsub("chinese herbs|herbal remedy|herbal store| herbal", "herbal medicine", fooddata2$Facility.Type)
fooddata2$Facility.Type <- gsub("grocery/restaurant|restaurant/grocery store|grocery store/restaurant", "restaurant/grocery", fooddata2$Facility.Type)
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "bar/bar", "bar")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "church kitchen", "religious")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "prepackage meal distributor 1006 retail", "prepackaged foods")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "convenient store", "convenience store")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "convenience", "convenience store")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "gas station/mini mart", "gas station")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "after school care", "after school program")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "art gallery w/wine/beer", "art gallery")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "bar consumption on premises", "bar")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "bar store", "bar")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "bar store/bar", "bar")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "bar-bar", "bar")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "bar/bar", "bar")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "bar/1006", "bar")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "catered events", "catered")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "charter school cafeteria", "charter school")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "charter school/cafeteria", "charter school")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "church/special events", "church")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "catered", "catering")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "coffee kiosk", "coffee shop")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "coffee roaster", "coffee shop")
fooddata2$Facility.Type <- gsub("coffee cart|frozen dessert pushcarts|hot dog cart| push carts", "food cart", fooddata2$Facility.Type)
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "push carts", "food cart")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "illegal vendor", "unlicensed facility")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "np-kiosk", "navy pier kiosk")
fooddata2$Facility.Type <- gsub("frozen dessert pushcarts|hot dog cart| push carts", "food cart", fooddata2$Facility.Type)
fooddata2$Facility.Type[agrep("coffee", fooddata2$Facility.Type)] <- "coffee"
fooddata2$Facility.Type[agrep("convenience store", fooddata2$Facility.Type)] <- "convenience store"
fooddata2$Facility.Type[agrep("dollar", fooddata2$Facility.Type)] <- "dollar store"
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "general store", "dollar store")
fooddata2$Facility.Type[agrep("drug", fooddata2$Facility.Type)] <- "drug store"
fooddata2$Facility.Type[agrep("gas", fooddata2$Facility.Type)] <- "gas station"
fooddata2$Facility.Type <- sub('[/].*', '', fooddata2$Facility.Type)
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "before", "after school program")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "butcher shop", "butcher")
fooddata2$Facility.Type[agrep("candy", fooddata2$Facility.Type)] <- "candy store"
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "culinary arts school", "culinary school")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "golf course concession stand", "golf course")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "grocery store", "grocery")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "grocerysushi prep", "grocery")
fooddata2$Facility.Type[agrep("health", fooddata2$Facility.Type)] <- "health store"
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "herbal medicine", "herbal")
fooddata2$Facility.Type[agrep("ice cream", fooddata2$Facility.Type)] <- "ice cream"
fooddata2$Facility.Type[agrep("juice", fooddata2$Facility.Type)] <- "juice"
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "nursing home facility", "nursing home")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "produce vendor", "produce stand")
fooddata2$Facility.Type[agrep("retail", fooddata2$Facility.Type)] <- "retail"
fooddata2$Facility.Type[agrep("rooftop", fooddata2$Facility.Type)] <- "rooftop"
fooddata2$Facility.Type[agrep("riverwalk", fooddata2$Facility.Type)] <- "riverwalk"
fooddata2$Facility.Type[agrep("school", fooddata2$Facility.Type)] <- "school"
fooddata2$Facility.Type[agrep("shared kitchen", fooddata2$Facility.Type)] <- "shared kitchen"
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "wholesale bakery", "wholesale")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "school cafeteria", "school")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "city of chicago college", "college")
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "university cafeteria", "college")
fooddata2$Facility.Type[agrep("thea", fooddata2$Facility.Type)] <- "movie theater"
fooddata2$Facility.Type[agrep("rest", fooddata2$Facility.Type)] <- "restaurant"
fooddata2$Facility.Type <- str_replace(fooddata2$Facility.Type, "religious", "church")

newdf <- count(fooddata2, Facility.Type)
newdf <- newdf[order(-newdf$n), ]

acceptable_facilities <- list("restaurant", "grocery", "school", "daycare", "bakery", "children's services facility", "food cart", "bar", "nursing home", "catering", "wholesale", "golden diner", "hospital", "gas station", "shared kitchen", "banquet", "special event", "convenience store", "kiosk", "rooftop", "shelter", "movie theater", "church", "navy pier kiosk", "coffee", "live poultry", "cafeteria", "ice cream", "college", "stadium", "butcher")
row_unaccept_1 <- which(!(fooddata2$Facility.Type %in% acceptable_facilities))
fooddata2$Facility.Type[row_unaccept_1] <- "other"


#clean results
fooddata2$Results[fooddata2$Results == "Pass w/ Conditions"] <- "Pass"
fooddata2$Results[fooddata2$Results == "Out of Business" | fooddata2$Results == "Business Not Located" | fooddata2$Results == "No Entry" | fooddata2$Results == "Not Ready"] <- NA #IS THIS APPROPRIATE
fooddata2 <- na.omit(fooddata2)
fooddata2$Results <- as.factor(fooddata2$Results)


#clean inspection types
fooddata2$Inspection.Type <- tolower(fooddata2$Inspection.Type)
fooddata2$Inspection.Type <- trimws(fooddata2$Inspection.Type, which = c("both"))
fooddata2$Inspection.Type[agrep("task", fooddata2$Inspection.Type)] <- "task force"
fooddata2$Inspection.Type[agrep("license", fooddata2$Inspection.Type)] <- "license"
fooddata2$Inspection.Type <- str_replace(fooddata2$Inspection.Type, "out ofbusiness", "out of business")
fooddata2$Inspection.Type <- str_replace(fooddata2$Inspection.Type, "two people ate and got sick.", "suspected food poisoning")
fooddata2$Inspection.Type[fooddata2$Inspection.Type == "canvass for rib fest" | fooddata2$Inspection.Type == "canvass school/special event" | fooddata2$Inspection.Type == "canvass special events" | fooddata2$Inspection.Type == "canvass/special event"] <- NA
fooddata2$Inspection.Type <- str_replace(fooddata2$Inspection.Type, "canvass re inspection of close up", "canvass re-inspection")
acceptable_types <- list("canvass", "license", "canvass re-inspection", "complaint", "short form complaint", "complaint re-inspection", "task force", "suspected food poisoning", "consultation", "tag removal", "out of business", "recent inspection", "complaint-fire", "suspected food poisoning re-inspection", "short form fire-complaint", "no entry", "special events (festivals)", "complaint-fire re-inspection", "package liquor 1474", "not ready")
row_unaccept <- which(!(fooddata2$Inspection.Type %in% acceptable_types))
fooddata2$Inspection.Type[row_unaccept] <- "other"

newdf <- count(fooddata2, Inspection.Type)
types <- list(newdf$Inspection.Type)

write.csv(fooddata2, '~/Desktop/food-inspections/data/food_inspections_final_updated.csv')
