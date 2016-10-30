parseTable <- function(fileName) {
  
  # Begin by processing data like this blog does:
  # http://faculty.baruch.cuny.edu/smanzan/AEDR/_book/citibike-trips.html
  
#  data <- fread("~/Documents/DIdata/Q2/201510-citibike-tripdata.csv", 
#                header=TRUE, showProgress=TRUE, 
#                stringsAsFactors=FALSE, data.table = FALSE)  

  data <- fread(fileName, header=TRUE, showProgress = FALSE, 
                stringsAsFactors = FALSE, data.table = FALSE)
  
  # Rename columns with . instead of spaces
  names(data) <- make.names(names(data))

  # Change the columns to more appropriate data types
  data$tripduration <- as.integer(data$tripduration)
  data$birth.year <- as.integer(data$birth.year)
  # Repeat same approach to some other data types:
  data$usertype   <- as.factor(data$usertype)
  # Gender (Zero=unknown; 1=male; 2=female)
  data$gender     <- as.factor(data$gender)
  levels(data$gender) <- c("UNKNOWN", "MALE", "FEMALE")
  
  require(lubridate)
  
  # Sigh, this date data is pretty badly formed.  Let's clean it up...
#  for(row in 1:nrow(data)) {
#    if(row %% 50000 == 0) {
#      print(paste0("Processing row #",row))
#    }
#    
#    # First: Starttime
#    pos = regexpr(' ', data$starttime[row])
#    date = substr(data$starttime[row], 0, pos-1)
#    time = substr(data$starttime[row], pos+1, nchar(data$starttime[row]))
#    if(nchar(time) == 4) {
#      time = paste0("0",time)
#      data$starttime[row] = paste0(date, " ", time)
#    }
#    
#    # Second: endtime
#    pos = regexpr(' ', data$stoptime[row])
#    date = substr(data$stoptime[row], 0, pos-1)
#    time = substr(data$stoptime[row], pos+1, nchar(data$stoptime[row]))
#    if(nchar(time) == 4) {
#      time = paste0("0",time)
#      data$stoptime[row] = paste0(date, " ", time)
#    }
#  }

  # Count number of colons to decide if the time is Hour-Minute-Second or Hour-Minute.
  nColons <- nchar(data$starttime[1]) - nchar(gsub(":","",data$starttime[1]))
  if(nColons == 2) {
    data$starttime <- mdy_hms(data$starttime)
    data$stoptime  <- mdy_hms(data$stoptime)
  }
  else {
    data$starttime <- mdy_hm(data$starttime)
    data$stoptime  <- mdy_hm(data$stoptime)
  }
  # difference between two dates ...
  data$myduration <- data$stoptime - data$starttime
  
  
  # function difftime allows to select the unit of the difference in time (secs, mins, hours, ...)
  data$myduration1 <- difftime(data$stoptime, data$starttime, units="secs")
  head(data[,c('tripduration', 'myduration', 'myduration1')])
  
  # create the weekday and hour variable with the day and hour of trip start time
  #data <- mutate(data, weekday = wday(starttime, label=TRUE), hour=as.factor(hour(starttime))) 
  data$weekday <- wday(data$starttime, label=TRUE)
  data$hour    <- as.factor(hour(data$starttime))
  
  data
}