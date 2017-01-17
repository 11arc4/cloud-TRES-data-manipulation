NestData <- setRefClass("NestData",
                        
                        fields = list(
                          
                          year = "numeric",
                          lineNumber = "numeric",
                          male = "TreeSwallow",
                          female = "TreeSwallow",
                          nestlings = "list",  #Nestlings needs to be a list of tree swallow entries
                          
                          FirstEggDate = "Date",
                          LastEggDate="Date",
                          HatchDate= "Date",
                          FledgeDate= "Date",
                          
                          ClutchSize= "integer",
                          HatchSize= "integer",
                          FledgeSize= "integer",
                          ReasonforFailure = "character")
                        
)

##########

Observation <- setRefClass("Observation",
                           fields = list(
                             date = "Date",
                             type = "character",
                             bird = "TreeSwallow"
                           ))



BodyMeasurements <- setRefClass("BodyMeasurements",
                           contains = "Observation",
                           fields = list (
                             WingChord = "numeric",
                             NinthPrimary = "numeric",
                             Mass = "numeric",
                             Tarsus = "numeric"
                           ))

EggMass<-setRefClass("EggMass", 
                     contains="Observation", 
                     fields= list(
                       EggMass = "numeric"
                     ))

MalariaCheck <- setRefClass("MalariaCheck",
                            contains = "Observation",
                            fields = list(
                              status = "character"
                            ))





#########
TreeSwallow <- setRefClass("TreeSwallow",
                         fields = list(
                            bandId = "character",
                            sex = "character",
                            HatchNest = "NestData", 
                            # the nestling record from the nest where I hatched                            nestList = "list", #(nestData, nestData)
                            observations = "list") 
                         #all of the things I know about an individual bird based on year
)



