outerdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
listfilenames <- list.files(outerdir)




globalData <- GlobalBirdData()

for (j in 1:length(listfilenames)){
  filename <- listfilenames[j]
  file <- paste( outerdir, filename, sep="/")
  nestdata<- read.csv(file, as.is=TRUE, na.strings=c("", "NA"))
  year <- nestdata$Year[1]
  InputNestDatatoClassStructure(nestdata=nestdata, globalData = globalData)
  message("****added", year, " to the global database")
}


#as.list(globalData$nests)
ls(all.names=TRUE, globalData$nests)
#pulls out all the stuff in nest_hash (the actual stuff not just the names)


#save the super slow data structure in something for later use....
saveRDS(globalData, "globalData.rds")


# if you want to reload the globalData object as globalData
#globalData <- readRDS("globalData.rds")
