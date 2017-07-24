#Flag any fledge or fail dates that make no sense 
ToCheck <- as.data.frame(matrix(nrow = 300, ncol = 4))
colnames(ToCheck)<- c("year", "nest", "renestStatus", "DoubleCheck")
failday <- c()
fledgeday <- c()
incdays<- c()
abanddays <- c()
a<- 0
for (nest in as.list(globalData$nests)){
  if(!is.na(nest$fledgeSize) & !is.na(nest$hatchDate) & !is.na(nest$fledgeDate)){
    #nestlings failed
    b<- nest$fledgeDate- nest$hatchDate
    if( nest$fledgeSize==0){
      
      failday[length(failday)+1]<-  b
      #Check to make sure that the dates even make sense for failing birds!
      if(b<0    | b>18){
        a<- a +1
        ToCheck$year[a]<- nest$year
        ToCheck$nest[a]<- nest$siteID
        ToCheck$renestStatus[a]<- nest$renestStatus
        if(b<0){
          ToCheck$DoubleCheck[a]<- "nest listed to fail before the birds hatched"
        } else {
          ToCheck$DoubleCheck[a]<- "nestlings had enough time to fledge instead of failing"
        }
      }
      
    } else {
      #We know nestlings have fledged
      fledgeday[length(fledgeday)+1 ] <- b
      
      #Again we'd like to check to make sure these dates make sense for a fledging bird
      if (b<13 | b>30){
        a<- a +1
        ToCheck$year[a]<- nest$year
        ToCheck$nest[a]<- nest$siteID
        ToCheck$renestStatus[a]<- nest$renestStatus
        if(b<16){
          ToCheck$DoubleCheck[a]<- "nestlings only had 12 days and might have actually failed"
        } else {
          ToCheck$DoubleCheck[a]<- "fledge date is too far away from the hatch date"
        }
      }
    }
    
    
  }
  
  #If we know first and last egg date do those dates make sense
  if(!is.na(nest$lastEggDate) & !is.na(nest$firstEggDate) ){
    c<- nest$lastEggDate-nest$firstEggDate
    if(c<0){
      a<- a +1
      ToCheck$year[a]<- nest$year
      ToCheck$nest[a]<- nest$siteID
      ToCheck$renestStatus[a]<- nest$renestStatus 
      ToCheck$DoubleCheck[a]<- "First or last egg date is wrong"
      
    } 
  }
  
  #IF the bird hatched eggs, do the dates between the last egg day and the hatching make sense?
  if(!is.na(nest$lastEggDate) & !is.na(nest$hatchSize)){
    #IF the bird hatched eggs, do the dates between the hatch date and last egg date make sense?
    if(nest$hatchSize>0 & !is.na(nest$hatchDate)){
      d<- nest$hatchDate-nest$lastEggDate
      incdays[length(incdays)+1]<- d
      if(d<12 |d>18){
        a<- a +1
        ToCheck$year[a]<- nest$year
        ToCheck$nest[a]<- nest$siteID
        ToCheck$renestStatus[a]<- nest$renestStatus 
        ToCheck$DoubleCheck[a]<- "hatchdate or last egg date is wrong"
      }
    }

  }
  
}




order
ToCheck <-ToCheck[order(ToCheck$year),]

failday2 <- failday [which(failday>0)]

failday3 <- failday [which(failday>0 & failday<18 )]
hist(failday2, breaks=20, xlim=c(0, 20))
abline(v=mean(failday2))
abline(v=median(failday2), col="red")

mean(failday2)
median(failday2)



hist(fledgeday)



hist(abanddays)

hist(incdays)
hist(incdays[which(incdays<40 & incdays>0)], xlim=c(0, 20), breaks=30)
