#Use the updatenestlings function to update all the different files

if ("amelia" == Sys.getenv("USER")) {
  topLevelDir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016"
  # input files...
  nestDataInDir <- paste(topLevelDir, 
                         "Improved and Cleaned Data", 
                         "1 All available adult band IDs added",
                         sep = "/")
  updatedResultDir <- paste(topLevelDir, "Improved and Cleaned Data",
                            "2 all nestlings added",
                            sep = "/")
  bandDataDir <- paste(sep = "/",
                       topLevelDir,
                       "Amelia TRES data 1975-2016",
                       "Improved and Cleaned Data")
  bandDataFile <- "Updated band data 1975-2001.csv"
  
} else {
  topLevelDir <- "~/GitHub/cloud-TRES-data-manipulation/testData"
  
  nestDataInDir <- paste(sep = "/", topLevelDir, "updatedNestData")
  updatedResultDir <- paste(sep = "/", topLevelDir, "2 nestinglings added")
  bandDataDir <- paste(sep = "/", topLevelDir, "cleanedBandData")
  bandDataFile <- "Updated band data 1975-2001.csv"
}

banddata<-read.csv(paste(sep = "/", bandDataDir, bandDataFile),
                   as.is=TRUE, na.strings = c("", "NA"))

if (!dir.exists(updatedResultDir)) {
  message("creating output directory ", updatedResultDir)
  dir.create(updatedResultDir, recursive = TRUE)
}
updateNestings_directory(sourcedir = nestDataInDir,
                         resultdir = updatedResultDir,
                         bandata = bandata) 
