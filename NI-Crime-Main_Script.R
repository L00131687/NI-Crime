df2 <- readRDS("PostCode_Data/NIPCode.rds")

Files <- list.files('data/', recursive = TRUE, full.names = TRUE)
AllNICrimeData <- do.call(rbind, lapply(Files, read.csv))

nrow(AllNICrimeData)

names(AllNICrimeData)
Remove_Cols <- c("Crime.ID","Reported.by","Falls.within","LSOA.code","LSOA.name","Last.outcome.category","Context")
AllNICrimeData <- AllNICrimeData[,!(names(AllNICrimeData) %in% Remove_Cols)]

#(d)
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
AllNICrimeData[AllNICrimeData$Location == "", c("Location")] <- NA 


find_a_postcode <- function(Location_Col){
  #Import NI postcode data
  NIPostCode <- readRDS("PostCode_Data/NIPCode.rds")
  NIPostCode <- NIPostCode %>% 
    count(Primary.Thorfare, b) %>%
    slice(which.max(n))
  # If Location column is not factor then convert
  if(!is.factor(Location_Col)){
    Location_Col <- as.factor(Location_Col)
  }
  
  # Extract levels from location column
  List_of_levels <- as.list(levels(Location_Col))
  
  # Initiate counter for looping through a list
  counter <- 1
  # Loop through each item in list of locations (Factor levels from location column)
  for (i in List_of_levels){
    #Extract matching locations with postcode that are NOT NA
    temp <- NIPostCode[NIPostCode$Primary.Thorfare == toupper(i) & !is.na(NIPostCode$Primary.Thorfare), c("Primary.Thorfare","Postcode")]
    # If location is matched then proceed with contents of if statement
    if(nrow(temp) > 0){
      indx <- tail(names(sort(table(temp$Postcode))),1) #Extracts the most popular Postcode for given location
      Location_Match <- temp[temp$Postcode == indx,][1,] #Extracts row with popular postcode and matched location
      if( counter == 1 ){ #If first loop iteration create and fill the dataframe
        Locations_Postcodes <- Location_Match
      } else {
        Locations_Postcodes <- rbind(Locations_Postcodes, Location_Match)
      }
    }
    rm(temp) # Removing Temp file for next iteration
    counter <- counter + 1 #Keeping count of iterations, as i is a list of level attributes
  }
  return(Locations_Postcodes)
}

Postcodes <- find_a_postcode(AllNICrimeData$Location)

write.csv(Postcodes, file = "Matched_loc_postcodes.csv")

AllNICrimeData$Location <- toupper(AllNICrimeData$Location)

AllNICrimeData <- merge(AllNICrimeData, Postcodes, by.x = "Location", by.y = "Primary.Thorfare", all.x = TRUE, sort = TRUE)
str(AllNICrimeData)

tidy_location <- function(Data){
  Data <- Data[is.na(Data$Location),]
  return(Data)
}
tidy_location(AllNICrimeData)
library(ggmap)
revgeocode(c(AllNICrimeData$Longitude[1], AllNICrimeData$Latitude[1]))[2]

install.packages("geosphere")

Missing <- AllNICrimeData[is.na(AllNICrimeData$Location), c("Longitude","Latitude")]
Not_Missing <- AllNICrimeData[!is.na(AllNICrimeData$Location), c("Longitude","Latitude","Location")]

library(geosphere)

library(dplyr)
# Take the avg long and lat values for data without missing locations (to improve speed)
Long_Lat_Avg <- Not_Missing %>% group_by(Location) %>% summarise_each(funs(mean)) %>% ungroup()

start <- Sys.time()
# creates distance matrix, distance of missing locations to locations not missing
Distances <- distm(Missing[,c('Longitude','Latitude')], Long_Lat_Avg[,c('Longitude','Latitude')], fun=distHaversine)
stop <- Sys.time()
# assign the name to the point in list1 based on shortest distance in the matrix
Missing <- Missing %>% mutate(Location_new = Long_Lat_Avg$Location[max.col(-Distances)])

# create distance matrix
Distances <- distm(Missing[,c('Longitude','Latitude')], Not_Missing[,c('Longitude','Latitude')], fun=distVincentyEllipsoid)

# assign the name to the point in list1 based on shortest distance in the matrix
list1$locality <- list2$locality[max.col(-mat)]



