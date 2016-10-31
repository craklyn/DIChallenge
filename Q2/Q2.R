
setwd("/Users/danielrb/Dropbox/Applications/Data Incubator/Challenge Problems/Q2")
source("parseCitibike.R")

# Keep data in another directory so my dropbox doesn't get stuffed
setwd("~/Documents/DIdata/Q2")
options(digits=10)


# Okay, let's load all the data up and put it into the dataframe "allData"
fileData <- list()
for (month in 1:12) {
  fileName <- sprintf("2015%02d-citibike-tripdata", month)
#  download.file(paste0("https://s3.amazonaws.com/tripdata/",fileName,".zip"), paste0("./",fileName,".zip"))
#  unzip(paste0("./",fileName,".zip"))

  data <- parseTable(paste0(fileName,".csv"))
  fileData[[month]] <- data
}

allData <- data.frame()
for(month in 1:12) {
  allData <- rbind(allData, as.data.frame(fileData[[month]]))
}
fileData <- NULL



# Okay, files are all loaded into fileData DF.

# What is the median trip duration, in seconds?
medianTripDuration <- median(allData$tripduration)
print(paste("Median trip duration", medianTripDuration))

# Result:
# [1] 629


# What fraction of rides start and end at the same station?
stationStartEndIsSame <- table((allData$start.station.id == allData$end.station.id))
numStartEndIsSame <- stationStartEndIsSame[names(stationStartEndIsSame)==TRUE]
fractionStartEndIsSame <- numStartEndIsSame / length(allData$start.station.id)
print(paste("Fraction of rides start and end at the station:", fractionStartEndIsSame))
print(fractionStartEndIsSame)

# Result:
# 0.02235839134


# We say a bike has visited a station if it has a ride that either started 
# or ended at that station. Some bikes have visited many stations; others 
# just a few. What is the standard deviation of the number of stations visited 
# by a bike?

# Make a list of all bikes
bikeIDs <- unique(allData$bikeid)

bikeNstationsVisited = c()
for (i in 1:length(bikeIDs)) {
  if(i%%250 == 0) {
    print(paste("Processing bike #",i,"out of",length(bikeIDs)))
  }
  bikeRecords <- which(allData$bikeid == bikeIDs[i])
  # Make a list of unique stations visited by bike i
  stationsVisited <- unique(c(allData$start.station.id[bikeRecords], 
                              allData$end.station.id[bikeRecords]))
  bikeNstationsVisited = c(bikeNstationsVisited, length(stationsVisited))
}

bikesNstationsVisitedStdDev <- sd(bikeNstationsVisited)
print(paste("StdDev of number of stations visited:", bikesNstationsVisitedStdDev))
print(bikesNstationsVisitedStdDev)

# Result:
# 54.54511387


# What is the average length, in kilometers, of a trip? Assume trips follow great 
# circle arcs from the start station to the end station. Ignore trips that start 
# and end at the same station, as well as those with obviously wrong data.

# Use the form of the great-circle equation that's well-behaved for 
# short distances due to use of the haversine formula:
allData$deltaPhi <- allData$start.station.latitude - allData$end.station.latitude
allData$cosPhi1 <- cos(allData$start.station.latitude)
allData$cosPhi2 <- cos(allData$end.station.latitude)
allData$deltaLambda <- allData$start.station.longitude - allData$end.station.longitude

allData$deltaSigma <- 2*asin(sqrt(sin(allData$deltaPhi/2)^2) + 
                                allData$cosPhi1*allData$cosPhi2*(sin(allData$deltaLambda/2)^2))
# Treat earth radius as 6371km, as per recommendation of:
# https://en.wikipedia.org/wiki/Great-circle_distance#Radius_for_spherical_Earth
allData$dist <- 6371 * allData$deltaSigma

# Remove trips that have the same start and end location
# Assume trips longer than 24 hours are obviously wrong
# Assume trips shorter than 1 meter are obviously wrong
# (Not assuming very small trips, like those seen with valets, are in error.)
goodDistData <- allData[-which(allData$start.station.id == allData$end.station.id |
                               allData$tripduration > 24*60*60 |
                               allData$dist < 0.001),]
nrow(allData) - nrow(goodDistData)

print("The mean (great circle) distance travelled of good trips is:")
print(mean(goodDistData$dist))

# Result:
# [1] 1.344494719



# Calculate the average duration of trips for each month in the year. 
# (Consider a trip to occur in the month in which it starts.) What is the 
# difference, in seconds, between the longest and shortest average durations?

tripDur = c()
for(month in 1:12) {
  tripThisMonth <- which(month(allData$starttime) == month)
  tripDur[month] <- mean(allData[tripThisMonth,]$tripduration)
}
diffOfAvgDur <- range(tripDur)[2] - range(tripDur)[1]
print(paste("Difference between longest and shortest monthly avg duration:",diffOfAvgDur))
print(diffOfAvgDur)

# Result:
# [1] 430.5702960


# Let us define the hourly usage fraction of a station to be the fraction of 
# all rides starting at that station that leave during a specific hour. A 
# station has surprising usage patterns if it has an hourly usage fraction 
# for an hour significantly different from the corresponding hourly usage 
# fraction of the system as a whole. What is the largest ratio of station 
# hourly usage fraction to system hourly usage fraction (hence corresponding 
# to the most "surprising" station-hour pair)?

overallUsage <- prop.table(table(factor(hour(allData$starttime), levels=0:23)))
stationIDs <- unique(allData$start.station.id)

perStationUsage = list()
for(i in 1:length(stationIDs)) {
  if(i %% 25 == 0) {
    print(paste("Now processing station", i,"of",length(stationIDs)))
  }
  
  station <- stationIDs[i]
  
  stationEntries <- which(allData$start.station.id == station)
  stationData <- allData[stationEntries,]

  perStationUsage[[i]] <- prop.table(table(factor(hour(stationData$starttime), levels=0:23)))
}

usageRatios <- c()
for(i in 1:length(stationIDs)) {
  for(j in 1:24) {
    ratio <- perStationUsage[[i]][j] / overallUsage[j]
    usageRatios[(i-1)*24 + j] <- ratio
  }
}

print("The maximum ratio of station hourly usage to overall hourly usage is:")
print(max(usageRatios))
# Result:
# 11.42168405


# There are two types of riders: "Customers" and "Subscribers." Customers buy 
# a short-time pass which allows 30-minute rides. Subscribers buy yearly 
# passes that allow 45-minute rides. What fraction of rides exceed their 
# corresponding time limit?

# Calculate time limit in seconds for subscribers and customers
subscrTimeLimit <- 45 * 60 
customerTimeLimit <- 30 * 60
subscribers <- which(allData$usertype == "Subscriber")

numExceed <- sum((allData[subscribers,])$tripduration > subscrTimeLimit) + 
             sum((allData[-subscribers,])$tripduration > customerTimeLimit)

fractionExceed <- numExceed / nrow(allData)
print(paste("Fraction of riders exceeding their time limit:",fractionExceed))
print(fractionExceed)

# Result:
# 0.03810678017


# Most of the time, a bike will begin a trip at the same station where its 
# previous trip ended. Sometimes a bike will be moved by the program, either 
# for maintenance or to rebalance the distribution of bikes. What is the 
# average number of times a bike is moved during this period, as detected by 
# seeing if it starts at a different station than where the previous ride ended?

# Make a list of all bikes
bikeIDs <- unique(allData$bikeid)
numMovesPerBike <- c()

print(paste("Number of bikes to process:",length(bikeIDs)))

for(i in 1:length(bikeIDs)) {
  bike <- bikeIDs[i]
  
  if(i %% 100 == 0) {
    print(paste("Now processing bike", i,"of",length(bikeIDs)))
  }
  bikeEntries <- which(allData$bikeid == bike)
  bikeData <- allData[bikeEntries,]
  
  numMoves <- 0
  prevEndStation <- bikeData[1,"start.station.id"]
#  print(paste("bike:"),bike)
  for(j in 1:length(bikeEntries)) {
#    print(paste("j:",j,"startStation:",bikeData[j,"start.station.id"],"endStation"))
    curStartStation <- bikeData[j,"start.station.id"]
    if(prevEndStation != curStartStation) {
      numMoves <- numMoves + 1
    }
    prevEndStation <- bikeData[j,"end.station.id"]
  }
  numMovesPerBike <- c(numMovesPerBike, numMoves) 
}

print("Mean number of bike moves per year:")
print(mean(numMovesPerBike))
# Result: 
# [1] 155.4053321


