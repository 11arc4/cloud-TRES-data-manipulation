#Fixing ages, fixing sex where appropriate and adding return status
exactAges <- c("HY", "SY", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "10Y", "11Y" )
estimateAges <- c("ASY", "A3Y", "A4Y", "A5Y", "A6Y", "A7Y", "A8Y", "A9Y", "A10Y", "A11Y")


AllBirds <- as.list(globalData$birds)

for (bird in AllBirds){
  #need to sort the yearsSeen based on year (that way we can just run though
  #that list and it will all be wonderfully sorted and I will have an easy way
  #to assign return and age)
  bird$yearsSeen <- bird$yearsSeen[order(sapply(bird$yearsSeen,function(x) x$year ))]
  create a vector of sexes 
  sexes <- vector(mode = "character")
  #not sure if I need to reinput them all (may just do this automatically because RC is mutable)
  t=0
  for (year in bird$yearsSeen){
    sexes[length(sexes)+1] <- year$sex
    t=t+1
    if (t==1){
      if (is.na (year$hatchNest$m_key)){
        year$returnStatus <- "New"
        #year age stays the same!
        
        #QUick fixes to check to see that everyone is consistent about how they write ages
        if( year$age=="2Y") {
          year$age <- "SY"
        }
        if(year$age == "A2Y"){
          year$age <- "ASY"
        }
      } else {
        year$returnStatus <- "Nestling"
        year$age <- "HY"
      }
    } else {
      if (t== 2){
        if (prev$returnStatus == "Nestling"){
          year$returnStatus <- "Recruit"
          year$age <- "SY"
        } else {
          year$returnStatus <- "Return"
          agevec <- substring (year$age, first=1, last=1)
          if(agevec == "A"){
            year$age <- estimateAges[which(estimateAges==prev$age)+1]
          } else {
            year$age <- exactAges[which(exactAges==prev$age)+1]
          }
          
        }
        
      } else {
        year$returnStatus <- "Return"
        agevec <- substring (year$age, first=1, last=1)
        if(agevec == "A"){
          year$age <- estimageAges[which(estimageAges==prev$age)+1]
        } else {
          year$age <- exactAges[which(exactAges==prev$age)+1]
        }
      }
    }
    #Setting this iteration to be the previous year's info for the next iteration
    prev <- year
  }
  
  
}



