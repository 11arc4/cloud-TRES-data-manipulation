


#I'm just going to improve the band data record really quickly


library(lubridate)

banddata<-read.csv("~/Masters Thesis Project/Tree Swallow Data/TRES data/Data Sets I really need to ADD to my current dataset/TRESBAND_75-01.csv", as.is=TRUE, na.strings = c("", "NA"))
#Adding a usable year column. 
for (i in 1:length(banddata$Date)){
  date <- banddata$Date[i]
  if(!is.na(date)) {
    date = as.integer(as.character(date))
    if (is.na(date)) {
      print(sprintf("entry %d is NA", i))
      next
    }
    if (date > 100) {
      date = date %/% 10000
    }
    if (date < 2000) {
      banddata$Year[i] <- date + 1900 # turn 78 into 1978
    } else {
      banddata$Year[i] <- date
    }
  }
}
#now band data is all sorted by year
banddata <- banddata[order(banddata$Year), ]

#I also may need a nice useable Non Julian date for each of the band entries
#But all my other dates are julian dates! Maybe I'll want one of those available as well!
banddata$NJDate <- as.Date(NA)
banddata$JDate<-as.Date(NA)
a=0
for (date in banddata$Date){
  a=a+1
  if(!is.na(date)){
    if(nchar(as.character(date))==6){
      banddata$NJDate[a] <- as.Date (as.character(date), format=("%y%m%d"))
    } else if(nchar(as.character(date))==2){
      banddata$NJDate[a] <- NA
    } else if(nchar(as.character(date))==8){
      banddata$NJDate[a] <- as.Date (as.character(date), format=("%Y%m%d")) 
    }
   
  }
}

banddata$JDate<-yday(banddata$NJDate)

#This code is to take care of a weird issue in the Place column--It appears that
#a couple of the entries are empty instead of NA which fucks up some of the
#later code that I try to run. So I've fixed it!



resultdir<-"~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"
filename<- "Updated band data 1975-2001.csv"
write.csv(banddata, file=paste(resultdir, filename, sep="/"),  row.names = FALSE)
