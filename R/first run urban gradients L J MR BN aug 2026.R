library(ggplot2)
library(tidyverse)

setwd("C:/Dropbox/work/2024 LEPMON/Urban gradients/Urban gradient data 2026-08-26/urban gradient")
# must fix file first by importing to excel. is actually tab delimited
L <- read.csv("LeipzigUrbanGradient1.csv")
J <- read.csv("JenaUrbanGradient1.csv"); dim(J)
MR<- read.csv("MarburgUrbanGradient1.csv"); dim(MR)
BN<- read.csv("BonnUrbanGradient1.csv"); dim(BN)

ALL<- rbind(L,J,MR,BN)


# check which sites to include, cause some may have changed name

ALL$Date <- as.Date((ALL$eventStart), format= "%d/%m/%Y")
ALL$Year <- format(as.Date((ALL$eventStart), format= "%d/%m/%Y"), "%Y")
head(ALL)



aggregate(ALL, Date~  Year+locationName    , function(x){length(unique(x))})

aggregate(subset(ALL, Year == 2025 ), Date~  Year+locationName    , function(x){length(unique(x))})

# a bit messy, but not catastrophic 
ALL$locationName[ALL$locationName == "Botanical garden in the Eurasian steppe, Leipzig"  & ALL$Year == 2025   ] <- "Garden Uwe Wallberg, Leipzig"
ALL$locationName[ALL$locationName == "Rheinstra\xdfe, Sankt Augustin"] <- "Rheinstrasse, Sankt Augustin"

# full deployment data, includung non leps
allDeploymentDays <- aggregate(ALL, Date~  Year+locationName    , function(x){length(unique(x))}) # subset(ALL, Year == 2025 )
names(allDeploymentDays)[names(allDeploymentDays) == "Date" ] <- "nbDeploymentDays"

# remove sites with low sampling effort
allDeploymentDays <- subset(allDeploymentDays, nbDeploymentDays>50)



ALLleps$hour <- cat(hour(J$eventStart), "\n")

library(lubridate)
cat(minute("22/06/2025 23:00" )) 













dim(ALL)

table(ALL$locationName)


# select correct sites first 
Lsites<- data.frame(locationName = c("Markkleeberg Allotment garden Matthias Albert, Leipzig" ,  
                                     "Car repair shop, highly sealed (appr. 85%), Leipzig" ,                                                                   
                                      "Quedlinburgerstrasse Leipzig, Leipzig"           ,                                                                       
                                       "Auwaldkran, appr. 0% sealed surface, Leipzig"   ,                                                                        
                                       "Garden Uwe Wallberg, Leipzig"        ),
                    sealing = c(20, 80, 40, 0, 60), 
                    City = "Leipzig", 
                    Location = c("Allotment garden Markkleeberg",
                                 "Car repair shop",
                                 "Garden Gohlis", 
                                 "Auwaldkran",
                                 "Garden Wahren"))

Jsites<- data.frame(locationName = c("Garten Gunnar Brehm, Jena",
                                     "Haeckelhaus, Jena", 
                                     "Im Planer, Jena", 
                                     "Imaginata, Jena" ,
                                     "Waldstandort Remderoda, Jena"),
                    sealing = c(40, 60, 20, 80, 0) , 
                    City = "Jena", 
                    Location = c("Garden",
                                 "Haeckelhaus", 
                                 "Im Planer", 
                                 "Imaginata" ,
                                 "Waldstandort Remderoda"))

MRsites<- data.frame(locationName = c("Lena Lerb's Garten, Marburg", 
                                      "Marburg Open Forest near Caldern, Marburg", 
                                      "Robert Junker's garden, Marburg",
                                      "Parking bay near generator building of Stadtwerke Marburg,", 
                                      "Urban courtyard of geography department,"
                                      ),
                    sealing = c(40, 0, 20, 80, 60) , 
                    City = "Marburg", 
                    Location = c("Garden east", 
                    "Forest near Caldern", 
                    "Garden south",
                    "Stadtwerke Marburg,", 
                    "Geography department,"))

BNsites<- data.frame(locationName = c("Mondorfer Str, Bonn" , 
                                      "Museum Koenig, Bonn", 
                                      "Noeggerathstr., Bonn",
                                      "FJW Holtorf, Bonn", 
                                      "Rheinstrasse, Sankt Augustin"
                                      ),
                                  sealing = c(40, 60, 80, 0, 20) , # UNSURE HERE
                                  City = "Bonn", 
                     Location = c("Mondorfer Str" , 
                                      "Museum Koenig", 
                                      "Noeggerathstr",
                                      "Holtorf", 
                                      "Sankt Augustin"
                     ))



ALLsites<- rbind(Lsites, MRsites, Jsites, BNsites)           



ALLleps<- ALL[grep("Lepidoptera", ALL$higher_taxa), ]; dim(ALLleps) 

dim(ALLleps)


# maybe should exclude all micrlep families? 

macros<- c("Noctuidae", "Erebidae", "Notodontidae", "Nolidae", "Geometridae", 
"Uraniidae","Bombycoidea", "Saturniidae", "Sphingidae", "Bombycidae", "Endromididae", "Drepanidae", "Lasiocampidae", "Mimallonidae")


ALLleps<- ALL[grepl(paste(macros, collapse = "|"), ALL$higher_taxa), ]
dim(ALLleps) # 1/5 of ALL lepidoptera! 
ALLleps<- merge(ALLleps, ALLsites)
dim(ALLleps) # 1/5 of ALL lepidoptera! 


# remove poorly classified species 
ALLleps<- subset(ALLleps, classificationProbability >700000)




# check if sampling effort is comparable among sites withing each city 
head(ALLleps)

ALLleps$hour <- cat(hour(J$eventStart), "\n")

library(lubridate)
cat(minute("22/06/2025 23:00" )) 

daysWLeps<- aggregate(ALLleps, Date~ locationName +City + Year  , function(x){length(unique(x))})

daysWLeps<- merge(daysWLeps, allDeploymentDays )





# summarize species number per year per site 
unique(ALLleps$scientificName) # 480 macros

# number of detections
ALLdetections<- aggregate(ALLleps, scientificName ~ Year + locationName + sealing + City, length)
names(ALLdetections)[names(ALLdetections) == "scientificName"] <- "detections"
ALLdetections

ALLdetections <-  merge(ALLdetections, daysWLeps)


arrange(ALLdetections, Year , City, sealing)

ALLdetections$detectionsPerDay <- ALLdetections$detections / ALLdetections$nbDeploymentDays



ggplot(ALLdetections, aes(x = sealing, y = detectionsPerDay, color = City ))+
  geom_point()+
  geom_line()+
  ylab("Lepidoptera detections per night")+
  facet_wrap(.~Year, ncol = 2, scales = 'free_y')

reshape2::dcast(subset(ALLleps, Year == 2025 & City == "Jena"), Date~locationName  , fill = -999 , length)

# for bonn, jena and MR have very unequal sampling durations



ALLspec<- aggregate(subset(ALLleps, City == "Leipzig"), 
                    scientificName~locationName+sealing+ City + Year, function(x) {length(unique(x))})


ggplot(ALLspec, aes(x = sealing, y = scientificName, linetype = Year))+
  geom_point(size = 3)+
  geom_line(, linewidth = 2)+ 
  ylab ("Number of species per year")




#Make accumulation curves 

library(dplyr)
library(tidyr)
library(iNEXT)

# long data with columns: site, species
wide <- subset(ALLleps, City == "Leipzig" & Year == 2025) %>%
  count(sealing,   scientificName) %>%          # counts per species per site
  pivot_wider(names_from = sealing, values_from = n, values_fill = 0)

# convert to the list format iNEXT wants: species x site matrix -> list of vectors
sp_matrix <- as.data.frame(wide[,-1])  # drop species name column
rownames(sp_matrix) <- wide$scientificName

# iNEXT can actually take the data.frame/matrix directly (each column = one assemblage)
out <- iNEXT(sp_matrix, q = 0, datatype = "abundance")
ggiNEXT(out, type = 1)



