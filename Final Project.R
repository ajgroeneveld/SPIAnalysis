####Final Project#####

####Getting SPI Data####

library(gdata)
#Read excel file for spatial price data
spatialPriceData = read.xls("2016-Alberta-Spatial-Price-Survey-Data-Tables.xlsx", 
                            sheet=6)
View(spatialPriceData)

##Gets correct column Names
colnames(spatialPriceData) <- as.character(unlist(spatialPriceData[2,]))

#Remove the first two rows since they don't have data
spatialPriceData <- spatialPriceData[-c(1,2),]

##Isolate for only the categories we are interested in
spatialPriceData[5:16] <- NULL

#Rename some variables manually
names(spatialPriceData)[6] = "Transportation"
names(spatialPriceData)[2] = "OverallSPI"


####Getting Population and Income####

#Import labour income data
profileData = read.xls("FamilyTables-16AB-20112015-03-12.xls")

View(profileData)

#Remove the first 3 rows since they don't have data
profileData <- profileData[-c(1,2,3),]

#Remove unnecessary columns. 
profileData[1:4] <- NULL
Sys.sleep(1)
profileData[2:6] <- NULL
Sys.sleep(1)
profileData <- profileData[1:5]

#Rename the columns
names(profileData) <- c("Community","NumFamilies","MedianFamilyIncome","MedianPersonIncome","NumPersons")

#Merge dataframes
spatialPriceData$Community <-toupper(spatialPriceData$Community)
allData <- merge(spatialPriceData,profileData,by="Community", all=FALSE)

View(allData)


####Getting community coordinates####

library(ggmap)
#Specify Alberta for google maps lookup
allData$Community <- lapply(allData$Community, paste0, ", ALBERTA")
allData$Community <- as.character(allData$Community)
#Execute lookup
CoordDF<-cbind(allData$Community,geocode(allData$Community,source="google"))

View(CoordDF)

#Rename columns
names(CoordDF)[1] = "Community"
names(CoordDF)[2] = "Longitude"
names(CoordDF)[3] = "Latitude"

#Merge latitude and longitude with other data
allData <- merge(allData,CoordDF,by="Community", all=FALSE)

#Convert dataframe types to numeric
communities <- allData$Community
allData <- data.frame(sapply(allData, function(x) as.numeric(as.character(x))))
allData$Community <- communities


####Mapping#####

#Install libraries required for mapping
install.packages("ggmap")
install.packages("mapproj")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("directlabels")
library(ggmap)
library(ggrepel)
library(mapproj)
library(ggplot2)
library(grid)
library(directlabels)


####Map of communities###

#Get map of Alberta from Google API
ABmap <- get_map(location = c(-126,47.249,-102.679,58.2), source = 'google')
plot(ABmap)

#Plot the locations of the communities in the data

Communities1 <- ggmap(ABmap) +
  geom_point(data = allData, aes(x = Longitude, y = Latitude, label = allData$Community), fill = "black",
             alpha = 0.8, size = 2, shape = 21) + 
  theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
  xlab('')+ ylab('') 

plot(Communities1)


####Map of SPIs####

#Plot cities with sizes based on price data of all items
SPImap <- ggmap(ABmap)+ geom_point(data = allData, color= "darksalmon", 
           alpha = 0.7,
           aes(x = Longitude, y = Latitude, size = OverallSPI))+ 
  labs(x="Longitude", y="Latitude",title="Community Spacial Price Index")

plot(SPImap)


####Map of Median Family Income####

#Plot cities with sizes based on median family income
IncomeMap <-  ggmap(ABmap)+ 
  geom_point(data = allData, color= "darkslategray", alpha = 0.6, 
             aes(x = Longitude, y = Latitude, size = MedianFamilyIncome)) + 
  labs(x="Longitude", y="Latitude",title="Community Median Family Income")
plot(IncomeMap)


####Map of Population Size####

#Plot cities with sizes based on population size
PopSizeMap = ggmap(ABmap)+ 
  geom_point(data = allData, color= "rosybrown3", 
             aes(x = Longitude, y = Latitude, size = NumPersons))+ 
  labs(x="Longitude", y="Latitude",title="Community Population Size")

plot(PopSizeMap)

####Overall SPI Regression####

#See if there is correlation between variables that predict SPI
library(MASS)
library(dplyr) 

RegresData <- allData[complete.cases(allData), ]

View(RegresData)
RegresData[3:9] <- NULL
Sys.sleep(1)
RegresData[7:8] <- NULL
RegresData[1] <- NULL

##Create Pairwise scatterplot
attach(RegresData)
plot(RegresData, pch = 15, col = "blue")

##Overall SPI Regression
SPIrelation <- lm(OverallSPI ~ MedianFamilyIncome + NumPersons, data = RegresData)
summary(SPIrelation)

####SPI Sub-Category Regressions####

#Food SPI
attach(allData)
FoodRelation <- lm(Food ~ MedianFamilyIncome + NumPersons, data = allData)
summary(FoodRelation)

#Non-Food SPI
NonFoodRelation <- lm(Non.food ~ MedianFamilyIncome + NumPersons, data = allData)
summary(NonFoodRelation)

#Recreation
RecRelation <- lm(Recreation...Leisure ~ MedianFamilyIncome + NumPersons, data = allData)
summary(RecRelation)

#Transportation
TransportRelation <- lm(Transportation ~ MedianFamilyIncome + NumPersons, data = allData)
summary(TransportRelation)

#Clothing
ClothRelation <- lm(Clothing ~ MedianFamilyIncome + NumPersons, data = allData)
summary(ClothRelation)

#Shelter
ShelterRelation <- lm(Shelter ~ MedianFamilyIncome + NumPersons, data = allData)
summary(ShelterRelation)

#Utilities 
UtilitiesRelation <- lm(Utilities ~ MedianFamilyIncome + NumPersons, data = allData)
summary(UtilitiesRelation)




