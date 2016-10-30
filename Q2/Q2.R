setwd("/Users/danielrb/Dropbox/Applications/Data Incubator/Challenge Problems/Q2")
source("parseCitibike.R")

# Keep data in another directory so my dropbox doesn't get stuffed
setwd("~/Documents/DIdata/Q2")

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


# What fraction of rides start and end at the same station?
stationStartEndIsSame <- table((allData$start.station.id == allData$end.station.id))
numStartEndIsSame <- stationStartEndIsSame[names(stationStartEndIsSame)==TRUE]
fractionStartEndIsSame <- numStartEndIsSame / length(allData$start.station.id)
print(paste("Fraction of rides start and end at the station:", fractionStartEndIsSame))


# We say a bike has visited a station if it has a ride that either started 
# or ended at that station. Some bikes have visited many stations; others 
# just a few. What is the standard deviation of the number of stations visited 
# by a bike?








