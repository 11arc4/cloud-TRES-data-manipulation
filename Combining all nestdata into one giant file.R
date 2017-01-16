#Merging all my nestdata into one giant file


library(plyr)
#require this library for the "rbind.fill" function



inputdir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/4 malaria status added"
filelist<-list.files(inputdir)


for (i in 1:length(filelist)){
  if(!exists("MasterNestdata")){
    MasterNestdata <- read.csv(paste(inputdir, filelist[i], sep="/"), as.is=TRUE, na.strings = c("", "NA"))
  } else{
    nestdata <- read.csv(paste(inputdir, filelist[i], sep="/"), as.is=TRUE, na.strings = c("", "NA"))
    MasterNestdata<-rbind.fill(as.data.frame(nestdata), as.data.frame(MasterNestdata))
    
  }
  
}

#YAY YOU FINALLY HAVE A GIANT DATASET!!!!!!!!!!! That's so excting