#extract adult bird data and their FIRST nest of the year. 
totalbirds = 0

band <- c()
year <- c()
sex <- c()
age <- c()
mass <- c()
tarsus <- c()
wingChord <- c()
ninethPrim <- c()
malaria <- c() 
dateMeas <- c()
nestID <- c()
clutchsize <- c()
laydate <-c()
incdate <- c()
hatchsize <- c()
hatchdate <- c()
fledgedate <- c()
fledgesize <- c()
reasonforfailure <- c()


for (bird in as.list(globalData$birds)){
  Adult=0
  for (Year in bird$yearsSeen$as.list()){
    #pick the nest to get breeding statistics from
    if(Year$nest$length==0){
      nID<- NA
      clsize <- NA
      ldate <-NA
      idate <- NA
      hsize <- NA
      hdate <- NA
      fdate <- NA
      fsize <- NA
      rffailure <- NA
    } else {
      if(Year$nest$length==1){
        nestKey <-Year$nest$buffer[[1]]
        nest <-get(nestKey$m_key, nestKey$m_hash)
      } else {
        
        for (nestKey in Year$nest$as.list()){
          nestKey <-Year$nest$buffer[[1]]
          nest <-get(nestKey$m_key, nestKey$m_hash)
          if(nest$renestStatus=="First"){
            break
          }
        }
      }
      nID<- nest$siteID
      clsize <- nest$clutchSize
      ldate <- nest$firstEggDate
      idate <- nest$lastEggDate
      hsize <- nest$hatchSize
      hdate <- nest$hatchDate
      fdate <- nest$fledgeDate
      fsize <- nest$fledgeSize
      rffailure <- nest$reasonforFailure
    }
    #If this wasn't a year that they hatched
    if(is.na(Year$hatchNest$m_key)){
      Adult <-  Adult +1
      if(Year$observations$length>0){
        for(obs in Year$observations$as.list()){
          l <- length(sex)+1
          if(obs$type=="bodymeasurement"){
            dateMeas[l]<- obs$date
            mass[l] <- obs$mass
            tarsus[l] <- obs$tarsus
            wingChord[l] <- obs$wingChord
            ninethPrim[l] <- obs$ninthPrimary
            nestID[l]<- nID
            clutchsize[l] <- clsize
            laydate[l] <-ldate
            incdate[l] <-idate
            hatchsize[l] <-hsize
            hatchdate[l] <- hdate
            fledgedate[l] <- fdate
            fledgesize[l] <-fsize
            reasonforfailure[l] <- rffailure
            year[l]<- Year$year
            sex[l] <- bird$sex
            band[l] <- bird$bandID
            age[l]<- Year$age
          }
          
        }
      } else {
        dateMeas[l]<- NA
        mass[l] <- NA
        tarsus[l] <- NA
        wingChord[l] <- NA
        ninethPrim[l] <- NA
        nestID[l]<- nID
        clutchsize[l] <- clsize
        laydate[l] <-ldate
        incdate[l] <-idate
        hatchsize[l] <-hsize
        hatchdate[l] <- hdate
        fledgedate[l] <- fdate
        fledgesize[l] <-fsize
        reasonforfailure[l] <- rffailure
        year[l]<- Year$year
        sex[l] <- bird$sex
        band[l] <- bird$bandID
        age[l]<- Year$age
      }
      
    }
  }
  if(Adult>0){
    totalbirds = totalbirds + 1
  }
  
}

AdultMeas <- data.frame(band ,
                        year ,
                        sex ,
                        age ,
                        mass ,
                        tarsus ,
                        wingChord ,
                        ninethPrim ,
                        dateMeas ,
                        nestID ,
                        clutchsize,
                        laydate ,
                        incdate ,
                        hatchsize ,
                        hatchdate ,
                        fledgedate ,
                        fledgesize ,
                        reasonforfailure)
outputdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Extracted Data for Analysis"
write.csv(x=AdultMeas, file= paste (outputdir, "Adult Measurements with First nest data.csv", sep="/"), na="", row.names = F)
