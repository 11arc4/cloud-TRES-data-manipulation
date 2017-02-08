processNestDataFiles <- function(inputDir, outputDir, banddata) {
  listfiles<-list.files(inputDir)
  
  for (f in listfiles){
    message("Processing ", f)
    filename<-paste(inputDir, "/", f, sep="" )
    initialnestdata <- read.csv(filename, as.is=TRUE, na.strings = c("", "NA"))
    #Add the siteID ased on the function for renests--needed to check in the band data
    #Add the renest column based on the function I wrote. 
    nestdata <- AssignRenestStatus(initialnestdata)
    Filledinnestdata <- fillinbandID(nestdata, banddata)
    
    outputFileName <- paste("Nest Data", Filledinnestdata$Year[1], 
                            "with all possible adult bands.csv")
    updatedfilename <- paste(outputDir, outputFileName, sep = "/")
    write.csv(Filledinnestdata, file=updatedfilename, row.names = FALSE, na=""  )
    message("updated ", f, " with all possible band IDs", sep=" ")
  }
}

if ("Amelia" == Sys.getenv("USERNAME")) {
  topLevelDir <- "~/Masters Thesis Project/Tree Swallow Data"
  bandDataDir = paste(sep = "/", topLevelDir, "Amelia TRES data 1975-2016", "Improved and Cleaned Data")
  bandDataFile = "Updated band data 1975-2001.csv"
  
  resultdir <- paste(sep = "/", topLevelDir, "Amelia TRES data 1975-2016",
                     "Improved and Cleaned Data")
  banddata <- read.csv(paste(bandDataDir, bandDataFile, sep = "/"),
                       as.is=TRUE, na.strings = c("", "NA"))
  
} else {
  topLevelDir <- "~/GitHub/cloud-TRES-data-manipulation/testData"
  bandDataFile <- "banddata.csv"
  resultdir <- paste(sep = "/", topLevelDir,
                     "Improved and Cleaned Data")
  banddata <- read.csv(paste(topLevelDir, bandDataFile, sep = "/"),
                       as.is=TRUE, na.strings = c("", "NA"))
  banddata <- fixUpBandData(banddata)
  
}



if ("Amelia" == Sys.getenv("USERNAME")) {
  topLevelDir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016"
  # input files...
  nestDataInDir <- paste(topLevelDir, "FINAL QC DONE", sep = "/")
  updatedResultDir <- paste(topLevelDir, "Improved and Cleaned Data/1 All available adult band IDs added",
                            sep = "/")
} else {
  
  nestDataInDir <- paste(sep = "/", topLevelDir, "inputNestData")
  updatedResultDir <- paste(sep = "/", topLevelDir, "updatedNestData")
}

processNestDataFiles(inputDir = nestDataInDir,
                     outputDir = updatedResultDir,
                     banddata = banddata)

