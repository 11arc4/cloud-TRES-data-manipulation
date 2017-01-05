
setwd("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics")
band<-read.csv("TRESBAND_75-01.csv", as.is=TRUE, na.strings = c("", "NA"))

setwd("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics")

nestDataDir = "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/1 All available adult band IDs added"
resultDir = "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/3 adult morphometrics added"

if ( ! dir.exists(resultDir)) {
  dir.create(resultDir, recursive=TRUE)
}

nestDataFiles <- list.files(nestDataDir)
for (fn in nestDataFiles) {
  print(paste("processing nest data", fn))
  year = as.integer(unlist(regmatches(fn, gregexpr("[[:digit:]]+", fn))))
  if ( year < 2000) {
    year = year - 1900
  }
  fileName = paste(nestDataDir, fn, sep="/")
  d = updateNestData(fileName, year, band)
  resultFile = paste("updated nest data ", as.character(year), ".csv", sep="")
  output = paste(resultDir, resultFile, sep="/")
  write.csv(d, file=output, na="", row.names=FALSE)
}
#d = updateNestData("Nest level data 1983.csv", 83, band)
