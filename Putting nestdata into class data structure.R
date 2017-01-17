
nestdatadir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/
                Improved and Cleaned Data/4 malaria status added"
listfiles <- list.files(nestdatadir)


for (filename in listfiles){
  nestdata <- read.csv ( paste (nestdatadir, filename, sep="/"))
  for (i in 1: length(nestdata$Year)){
    #Need to create the list of nestling band IDs to feed into the function
    nestband<-c("band.1", "band.2", "band.3", "band.4", "band.5", 
                "band.6", "band.7", "band.8", "band.9", "band.10")
  for( a in 1:length(nestband)){
    nestlingbandIDlist<-append(nestdata[i, nestband[a]], nestlingbandIDlist)
  }
  configureFromNestDataFile (nestDataFrame =nestdata, line=i )
  }
}
