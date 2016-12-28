
setwd("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics")
band<-read.csv("TRESBAND_75-01.csv", as.is=TRUE, na.strings = c("", "NA"))

setwd("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/R Script for adding in  adult body metrics")

nestDataDir = "C:/Users/Amelia/Documents/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/FINAL QC DONE"
resultDir = "./mangled"

if ( ! dir.exists(resultDir)) {
  dir.create(resultDir, recursive=TRUE)
}

nestDataFiles <- list.files(nestDataDir)
for (fn in nestDataFiles) {
  if (0 == length(grep("^[^0-9]+([0-9]+)\\.csv$", fn))) {
    next
  }
  print(paste("processing nest data", fn))
  year = as.integer(gsub("^[^0-9]+([0-9]+)\\.csv$", "\\1", fn,perl=TRUE))
  if ( year < 2000) {
    year = year - 1900
  }
  fileName = paste(nestDataDir, fn, sep="/")
  d = updateNestData(fileName, year, band)
  resultFile = paste("updated nest data ", as.character(year), ".csv", sep="")
  output = paste(resultDir, resultFile, sep="/")
  write.csv(d, file=output, na="")
}
#d = updateNestData("Nest level data 1983.csv", 83, band)
