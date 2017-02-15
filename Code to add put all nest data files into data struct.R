if ("Amelia" == Sys.getenv("USERNAME")) {
  outerdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
  
  
} 

if ("Lab_Users" == Sys.getenv("USERNAME")) {
  outerdir <- "~/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
} 

listfilenames <- list.files(outerdir)


library(beepr)

globalData <- GlobalBirdData()

for (filename in listfilenames){
  file <- paste( outerdir, filename, sep="/")
  nestdata<- read.csv(file, as.is=TRUE, na.strings=c("", "NA"))
  nestdata$F.Day.measured <- as.character(nestdata$F.Day.measured)
  nestdata$M.Day.measured <- as.character(nestdata$M.Day.measured)
  year <- nestdata$Year[1]
  InputNestDatatoClassStructure(nestdata=nestdata, globalData = globalData)
  message("****added", year, " to the global database")
}
beep(1)
#save your nest global data somewhere so that you can access it again if necessary.... You may well need it if your stuff breaks...
globalDataNest<- globalData$copy
#as.list(globalData$nests)
#ls(all.names=TRUE, globalData$nests)
#pulls out all the stuff in nest_hash (the actual stuff not just the names)


#save the super slow data structure in something for later use....
#WARNING: THIS DOES NOT SEEM TO WORK, OR AT LEAST IT TAKES A STUPIDLY LONG TIME TO RUN, SO IT NOT WORTH IT. 
#saveRDS(globalData, "globalNestData.rds")


# if you want to reload the globalData object as globalData
#globalData <- readRDS("globalData.rds")
