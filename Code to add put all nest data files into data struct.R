outerdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
listfilenames <- list.files(outerdir)
filename <- paste( outerdir, listfilenames[5], sep="/")
nestdata<- read.csv(filename, as.is=TRUE, na.strings=c("", "NA"))





globalData <- GlobalBirdData()

for (filename in listfilenames){
  file <- paste( outerdir, filename, sep="/")
  nestdata<- read.csv(file, as.is=TRUE, na.strings=c("", "NA"))
  year <- nestdata$Year[1]
  InputNestDatatoClassStructure(nestdata=nestdata, globalData = globalData)
  message("****added", year, " to the global database")
}


#as.list(globalData$nests)
ls(all.names=TRUE, globalData$nests)
#pulls out all the stuff in nest_hash (the actual stuff not just the names)