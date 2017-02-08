#Use the updatenestlings function to update all the different files

if ("Amelia" == Sys.getenv("USERNAME")) {
  topLevelDir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"
  # input files...
  nestDataInDir <- paste(topLevelDir,
                         "1 All available adult band IDs added",
                         sep = "/")
  updatedResultDir <- paste(topLevelDir, 
                            "2 all nestlings added",
                            sep = "/")
  bandDataDir <-  topLevelDir
                      
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

# 
# updateAdult_directory <- function(sourcedir,
#                          resultdir ,
#                          bandDataTable ) {
#   listfiles <- list.files(sourcedir)
#   for (file in listfiles){
#     nestdata <- read.csv (paste (sourcedir, file, sep="/" ), as.is=TRUE, na.strings= c("NA", ""))
#     updatedNestData <- updateNestData(nest_data =  nestdata, currentYear = nestdata$Year[1], bandDataTable = bandDataTable)
#     filename = paste ("Nestdata with nestlings ", nestdata$Year[1], ".csv", sep="")
#     write.csv(updatedNestData, file=paste (resultdir, filename, sep="/"),  na="", row.names = FALSE)
#   }
# }

updateNestings_directory(sourcedir = nestDataInDir,
                         resultdir = updatedResultDir,
                         bandata = banddata) 
