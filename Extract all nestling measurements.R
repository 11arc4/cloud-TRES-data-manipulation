#Extracting all Nestling Observations for analysis
mass <- c()
tarsus <- c()
ninprim <- c()
age <- c()
year <- c()
nestID <- c()
nestlingID <- c()
hatchsize <- c()
hatchdate <- c()
fledgedate <- c()
fledgesize <- c()
growthrates <- c()

for (nestling in as.list(globalData$nestlings)){
  nest <- nestling$fromNest$m_key
  ID <- nestling$nestlingCode
  if (exists (nest, globalData$nests)){
    fromNest <- get(nest, globalData$nests)
    year_ <- fromNest$year
    hdate <- fromNest$hatchDate
    hsize <- fromNest$hatchSize
    fsize <- fromNest$fledgeSize
    fdate <- fromNest$fledgeDate
  } else {
    year_ <- NA
    hdate <- NA
    hsize <- NA
    fsize <- NA
    fdate <- NA
  }
  if(length(nestling$measurements$as.list()) >=1){
    for (meas in nestling$measurements$as.list()){
      if(!is.null(meas)){
        l <- length(age)+1
        age[l] <- meas$age
        ninprim[l] <- meas$ninthPrimary
        tarsus[l] <- meas$tarsus
        mass[l] <- meas$mass
        year[l] <- year_
        nestID[l] <- nest
        nestlingID[l] <- ID
        hatchsize[l] <- hsize
        hatchdate[l] <- hdate
        fledgesize[l] <- fsize
        fledgedate[l] <- fdate
      }
    }
  }
  growthrates[length(growthrates)+1]<- nestling$growthRateMass
}

nstgMeas <- data.frame(nestlingID, nestID, year, age, hatchdate, hatchsize, fledgedate, fledgesize,  mass, tarsus, ninprim)
outputdir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Extracted Data for Analysis"
write.csv(x=nstgMeas, file= paste (outputdir, "Nestling Measurements for Analysis.csv", sep="/"), na="", row.names = F)
