#Merging all my nestdata into one giant file

createMasterNestdataFile <-function(){
  library(plyr)
  #require this library for the "rbind.fill" function
  inputdir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
  filelist<-list.files(inputdir)
  
  
  for (i in 1:length(filelist)){
    nestdata <- read.csv(paste(inputdir, filelist[i], sep="/"), as.is=TRUE, na.strings = c("", "NA"))
    for (j in length(nestdata$F.Day.measured)){
      #Dealing with the inconsistant formatting of the dates. 
      if (grepl("\\/", nestdata$F.Day.measured[j])){
        nestdata$F.Day.measured <- as.Date(x=nestdata$F.Day.measured, format= "%m/%d/%Y")
        nestdata$M.Day.measured <- as.Date(x=nestdata$M.Day.measured, format= "%m/%d/%Y")
      } else {
        nestdata$F.Day.measured <- as.Date(x=nestdata$F.Day.measured, format= "%Y-%m-%d")
        nestdata$M.Day.measured <- as.Date(x=nestdata$M.Day.measured, format= "%Y-%m-%d")
      }
      
    }
    
 
    print(nestdata$Year[1])
    if(i==1){
      MasterNestdata <- nestdata
    } else {
      MasterNestdata<-rbind.fill(as.data.frame(nestdata), as.data.frame(MasterNestdata))
    }
  }
  
  return(MasterNestdata)
}
#YAY YOU FINALLY HAVE A GIANT DATASET!!!!!!!!!!! That's so excting