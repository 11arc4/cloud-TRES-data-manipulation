


#I'm just going to improve the band data record really quickly


library(lubridate)

fixUpBandData <- function(banddata) {
  # add Julian date to band data.
  # also, sort it by year (and by boxID within the year)

  #Adding a usable year column.
  for (i in 1:length(banddata$Date)){
    date <- banddata$Date[i]
    if(!is.na(date)) {
      date = as.integer(as.character(date))
      if (is.na(date)) {
        message("band data date entry ", i, " is NA")
        next
      }
      if (date > 100) {
        date = date %/% 10000
      }
      if (date < 2000) {
        banddata$Year[i] <- date + 1900 # turn 78 into 1978
      } else {
        banddata$Year[i] <- date
      }
    }
  }
  #sort band data by year
  banddata <- banddata[order(banddata$Year), ]

  #I also may need a nice useable Non Julian date for each of the band entries
  #But all my other dates are julian dates! Maybe I'll want one of those available as well!
  banddata$NJDate <- as.Date(NA)
  banddata$JDate <- as.Date(NA)
  a = 0
  for (date in banddata$Date) {
    a = a + 1
    if (!is.na(date)) {
      if (nchar(as.character(date)) == 6) {
        # 6 characters:yymmdd
        banddata$NJDate[a] <-
          as.Date (as.character(date), format = ("%y%m%d"))
      } else if (nchar(as.character(date)) == 2) {
        # 2 characters...yy
        banddata$NJDate[a] <- NA
      } else if (nchar(as.character(date)) == 8) {
        # 8 characters - yyyy
        banddata$NJDate[a] <-
          as.Date (as.character(date), format = ("%Y%m%d"))
      } else {
        warning("unexpectected band data date '", data, "' (treated as NA)")
      }
    }
  }

  banddata$JDate <- yday(banddata$NJDate)
  return(banddata)
}

if ("Amelia" == Sys.getenv("USERNAME")) {
  topLevelDir <- "~/Masters Thesis Project/Tree Swallow Data"
  bandDataDir = paste(sep = "/", topLevelDir, "TRES data", 
                      "Data Sets I really need to ADD to my current dataset")
  bandDataFile = "TRESBAND_75-01.csv"

  resultdir <- paste(sep = "/", topLevelDir, "Amelia TRES data 1975-2016",
                     "Improved and Cleaned Data")
  banddata <- read.csv(paste(bandDataDir, bandDataFile, sep = "/"),
                       as.is=TRUE, na.strings = c("", "NA"))

} else {
  topLevelDir <- "~/GitHub/cloud-TRES-data-manipulation/testData"
  bandDataFile <- "banddata.csv"
  resultdir <- paste(sep = "/", topLevelDir,
                     "cleanedBandData")
  banddata <- read.csv(paste(topLevelDir, bandDataFile, sep = "/"),
                       as.is=TRUE, na.strings = c("", "NA"))
}


banddata2 <- fixUpBandData(banddata)

filename<- "Updated band data 1975-2001.csv"

if (! dir.exists(resultdir)) {
  dir.create(resultdir, recursive = TRUE)
}
write.csv(banddata2, file=paste(resultdir, filename, sep="/"),
          row.names = FALSE)


#This code is to take care of a weird issue in the Place column--It appears that
#a couple of the entries are empty instead of NA which fucks up some of the
#later code that I try to run. So I've fixed it!

