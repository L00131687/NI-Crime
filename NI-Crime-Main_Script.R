# A
Files <- list.files('data/', recursive = TRUE, full.names = TRUE)
AllNICrimeData <- do.call(rbind, lapply(Files, read.csv))

nrow(AllNICrimeData)

# B
Remove_Cols <- c("Crime.ID","Reported.by","Falls.within","LSOA.code","LSOA.name","Last.outcome.category","Context")
AllNICrimeData <- AllNICrimeData[,!(names(AllNICrimeData) %in% Remove_Cols)]
str(AllNICrimeData)

# C
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
str(AllNICrimeData)

# D
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
AllNICrimeData[AllNICrimeData$Location == "", c("Location")] <- NA 
str(AllNICrimeData)

# E

find_a_postcode <- function(Location_Col){
  library(dplyr)
  #Import NI postcode data
  NIPostCode <- readRDS("PostCode_Data/NIPCode.rds")
  #Extract most frequent postcode for location (Speeds up later processing)
  NIPostCode <- NIPostCode %>% group_by(Primary.Thorfare) %>% summarize (Postcode =names(which.max(table(Postcode))))
  
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
    Location_Match <- NIPostCode[NIPostCode$Primary.Thorfare == toupper(i) & !is.na(NIPostCode$Primary.Thorfare), c("Primary.Thorfare","Postcode")]
    # If location is matched then proceed with contents of if statement
    if(nrow(Location_Match) > 0){
      if( counter == 1 ){ #If first loop iteration create and fill the dataframe
        Locations_Postcodes <- Location_Match
      } else {
        Locations_Postcodes <- rbind(Locations_Postcodes, Location_Match)
      }
    }
    rm(Location_Match) # Removing Temp file for next iteration
    counter <- counter + 1 #Keeping count of iterations, as i is a list of level attributes
  }
  return(Locations_Postcodes)
}

Postcodes <- find_a_postcode(AllNICrimeData$Location)
str(Postcodes)
nrow(Postcodes)

#Saving matched postcodes for matched locations on NI Crime data
write.csv(Postcodes, file = "Matched_loc_postcodes.csv")

# F
AllNICrimeData$Location <- toupper(AllNICrimeData$Location)

AllNICrimeData <- merge(AllNICrimeData, Postcodes, by.x = "Location", by.y = "Primary.Thorfare", all.x = TRUE, sort = TRUE)
str(AllNICrimeData)

# G
tidy_location <- function(Data){
  Missing <- Data[is.na(Data$Location), c("Longitude","Latitude")]
  Not_Missing <- Data[!is.na(Data$Location), c("Longitude","Latitude","Location")]
  
  library(geosphere)
  library(dplyr)
  # Take the avg long and lat values for data without missing locations (to improve speed)
  Long_Lat_Avg <- Not_Missing %>% group_by(Location) %>% summarise_each(funs(mean)) %>% ungroup()
  
  start <- Sys.time()
  # creates distance matrix, distance of missing locations to locations not missing
  Distances <- distm(Missing[,c('Longitude','Latitude')], Long_Lat_Avg[,c('Longitude','Latitude')], fun=distHaversine)
  stop <- Sys.time()
  # assign the Location to the point in Missing location data based on shortest distance in the matrix
  Missing <- Missing %>% mutate(Location_new = Long_Lat_Avg$Location[max.col(-Distances)])
  Missing[!is.na(Missing$Location_new),] <- Long_Lat_Avg$Location[max.col(-Distances)]
  return(Missing)
}

Missing_Locations <- tidy_location(AllNICrimeData)
str(Missing_Locations)
nrow(Missing_Locations)

#H
NIPostCode <- readRDS("PostCode_Data/NIPCode.rds")
AllNICrimeData <- merge(AllNICrimeData, NIPostCode[,c("Primary.Thorfare","Town","County","Postcode")], 
                        by.x = "Location", by.y = "Primary.Thorfare", sort = TRUE)
AllNICrimeData$Town <- as.factor(AllNICrimeData$Town) 
AllNICrimeData$County  <- as.factor(AllNICrimeData$County)

str(AllNICrimeData)

#I
write.csv(AllNICrimeData, file = "FinalNICrimeData.csv")

#J

AllNICrimeData[AllNICrimeData$Town == 'Strabane' & AllNICrimeData$Postcode == 'BT82',][1:10]








