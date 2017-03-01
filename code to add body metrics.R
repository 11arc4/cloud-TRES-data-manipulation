
banddir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics"
bandfile<-paste(banddir, "TRESBAND_75-01.csv", sep="/")
band<-read.csv(bandfile,  as.is=TRUE, na.strings = c("", "NA"))

nestDataDir = "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/2 all nestlings added"
resultDir = "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/3 adult morphometrics added"

if ( ! dir.exists(resultDir)) {
  dir.create(resultDir, recursive=TRUE)
}

nestDataFiles <- list.files(nestDataDir)
for (fn in nestDataFiles) {
  # regular expression for file name:
  #   - string start (^)
  #   - one or more non-digits ([^0-9]+)
  #   - one or more digits
  #   - ending in ".csv"
  nameExp = "^[^0-9]+([0-9]+)+[^0-9]+\\.csv$"
  if (0 == length(grep(nameExp, fn))) {
    next
  }
  print(paste("processing nest data", fn))

  year = as.integer(unlist(regmatches(fn, gregexpr("[[:digit:]]+", fn))))

  if ( year < 2000) {
    year = year - 1900
  }
  inputNestDataCsvFile <- paste(nestDataDir, fn, sep="/")
  nest_data <- read.csv(inputNestDataCsvFile, as.is=TRUE, na.strings = c("NA", ""))
  
  updatedNestData <- updateNestData(nest_data, year, band)
  
  resultFile = paste("updated nest data ", as.character(year), ".csv", sep="")
  output = paste(resultDir, resultFile, sep="/")
  write.csv(updatedNestData, file=output, na="", row.names=FALSE)
}
