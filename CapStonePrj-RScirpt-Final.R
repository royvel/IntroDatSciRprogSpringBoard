.libPaths("C:/Users/Rekha/Documents/R/win-library/3.4")


install.packages("ggplot2")
library(ggplot2)
library(corrplot)
library(dplyr)
library(gvlma)

#1. Load each years datafile to a datastructure and replace any null values in columns with NA.

mydata15 <- read.csv("C:\\Users\\Rekha\\Documents\\REKHA\\DataScience\\SpringBoard\\CapStoneProject\\DATA\\CSV\\2015FE.csv",na.strings = c("", " ", "NA"))
mydata16 <- read.csv("C:\\Users\\Rekha\\Documents\\REKHA\\DataScience\\SpringBoard\\CapStoneProject\\DATA\\CSV\\2016FE.csv",na.strings = c("", " ", "NA"))
mydata17 <- read.csv("C:\\Users\\Rekha\\Documents\\REKHA\\DataScience\\SpringBoard\\CapStoneProject\\DATA\\CSV\\2017FE.csv",na.strings = c("", " ", "NA"))
mydata18 <- read.csv("C:\\Users\\Rekha\\Documents\\REKHA\\DataScience\\SpringBoard\\CapStoneProject\\DATA\\CSV\\2018FE.csv",na.strings = c("", " ", "NA"))


#2. Load function that combines datastructures with different number of columns                       
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}


#3. Remove columns with all missing/null values
mydata15c <- mydata15[, !apply(is.na(mydata15), 2, all)]
mydata16c <- mydata16[, !apply(is.na(mydata16), 2, all)]
mydata17c <- mydata17[, !apply(is.na(mydata17), 2, all)]
mydata18c <- mydata18[, !apply(is.na(mydata18), 2, all)]



#4. combine the dataframes
mydata1516c <- rbind.all.columns(mydata15c, mydata16c)
mydata1718c <- rbind.all.columns(mydata17c, mydata18c)
mydataf <-  rbind.all.columns(mydata1516c, mydata1718c)

#5. Remove columns that are not needed
mydatafc <- mydataf[, -c(13:18, 24, 30:32, 39:44, 47:65, 75:80, 88:103, 106:108, 119:122, 128:129)]


#6.cleanup(remove/delete) datastructures not needed
rm(mydataf)
rm(mydata15)
rm(mydata16)
rm(mydata17)
rm(mydata18)

#7. change colnames for easy use

colnames(mydatafc)[1]<- c( "ModelYear")                                                                                              
colnames(mydatafc)[2]<- c( "Mfrname")
colnames(mydatafc)[5]<- c("Mfrcd")

colnames(mydatafc)[6]<- c( "ModelTypeIndex")                                                                                
colnames(mydatafc)[7]<- c( "EngDispl")                                                                                               
colnames(mydatafc)[8]<- c( "Cylinders")                                                                                                  
colnames(mydatafc)[10]<- c( "CityFuel")                                                                     
colnames(mydatafc)[11]<- c( "HwyFuel")                                                                      
colnames(mydatafc)[12]<- c( "CombFuel")                                                                     
colnames(mydatafc)[14]<- c( "AirAspirMethod")                                                                                        
colnames(mydatafc)[15]<- c( "AirAspirMethodDesc")                                                                              
colnames(mydatafc)[16]<- c( "Trans")                                                                                                   
colnames(mydatafc)[17]<- c( "TransDesc")    

colnames(mydatafc)[18]<- c( "Gears")                                                                                                
colnames(mydatafc)[19]<- c( "LockupTorqueConverter")                                                                                 
colnames(mydatafc)[20]<- c( "TransCreeperGear")                                                                                      
colnames(mydatafc)[21]<- c( "DriveSys")                                                                                               
colnames(mydatafc)[22]<- c( "DriveDesc")                                                                                              
colnames(mydatafc)[23]<- c( "FuelUsageFuel")                                                                         
colnames(mydatafc)[24]<- c( "FuelUsageDesc")                                                                    
colnames(mydatafc)[25]<- c( "FuelUnit")                                                                           
colnames(mydatafc)[26]<- c( "FuelUnitDesc")                                                                      
colnames(mydatafc)[27]<- c( "GasGuzzlerExempt")                                          
colnames(mydatafc)[28]<- c( "GasGuzzlerExemptDesc")                                     
colnames(mydatafc)[29]<- c( "AnnualFuelCost")                                                                   
colnames(mydatafc)[30]<- c( "EPAAnnualFuelCost")
colnames(mydatafc)[31]<- c( "DescriptorModelType")                                                               
colnames(mydatafc)[32]<- c( "IntakeValves")                                                                                   
colnames(mydatafc)[33]<- c( "ExhaustValves")                                                                                  
colnames(mydatafc)[34]<- c( "CarlineClass")                                                                                           
colnames(mydatafc)[35]<- c( "CarlineClassDesc")                                                                                      
colnames(mydatafc)[36]<- c( "CarTruckCategory")                                                            
colnames(mydatafc)[37]<- c( "CalcApproachDesc")                                                                                      
colnames(mydatafc)[38]<- c( "ReleaseDate")                                                                                            
colnames(mydatafc)[39]<- c( "EPAFELabelID")                                                                                 
colnames(mydatafc)[40]<- c( "CommentsMfr")                                                                                 
colnames(mydatafc)[41]<- c( "CylDeact")                                                                                              
colnames(mydatafc)[42]<- c( "CylDeactDesc")                                                                                          
colnames(mydatafc)[43]<- c( "VarValTiming")                                                                                       
colnames(mydatafc)[44]<- c( "VarValTimingDesc")                                                                                  
colnames(mydatafc)[45]<- c( "VarValLift")                                                                                         
colnames(mydatafc)[46]<- c( "VarValLiftDesc")                                                                                     
colnames(mydatafc)[47]<- c( "FuelMtrSysCd")                                                                                    
colnames(mydatafc)[48]<- c( "FuelMtrSysDesc")                                                                                 
colnames(mydatafc)[49]<- c( "OilViscosity")                                                                                           
colnames(mydatafc)[50]<- c( "StopStartSysCd")                                                       
colnames(mydatafc)[51]<- c( "StopStartSysDesc")                                               
colnames(mydatafc)[52]<- c( "ModelTypeDesc")                                                                           
colnames(mydatafc)[53]<- c( "MFRCalcGasGuzzlerMPG")                                                                          
colnames(mydatafc)[54]<- c( "FERating")                                                                        
colnames(mydatafc)[55]<- c( "GHGRating")                                                                     
colnames(mydatafc)[56]<- c( "GHGRatingEPA")                                                             
colnames(mydatafc)[57]<- c( "SmogRatingTestGrp")                                                                              
colnames(mydatafc)[58]<- c( "MfrSmogRating")                                    
colnames(mydatafc)[59]<- c( "Savingsfuelcosts5yrs")                           
colnames(mydatafc)[60]<- c( "Spentfuelcosts5yrs")               
colnames(mydatafc)[61]<- c( "CityCO2Adj")                                                                               
colnames(mydatafc)[62]<- c( "HwyCO2Adj")                                                                                
colnames(mydatafc)[63]<- c( "CombCO2Adj")

#write.csv(mydata15c, "C:\\Users\\Rekha\\Documents\\REKHA\\DataScience\\SpringBoard\\CapStoneProject\\DATA\\CSV\\mydata15cnewc.csv")

# Plot to show distribution of cylinders

ggplot(data = mydatafc) +
  geom_bar(mapping = aes(x = Cylinders)) 

# Plot to show distribution of Engine Displacement

ggplot(data = mydatafc) +
  geom_bar(mapping = aes(x = EngDispl))

# Plot to show distribution of Transmission

ggplot(data = mydatafc) +
  geom_bar(mapping = aes(x = TransDesc)) +
  coord_flip()

# Plot to show distribution of Drives

ggplot(data = mydatafc) +
  geom_bar(mapping = aes(x = DriveDesc)) +
  coord_flip()

# Plot to show distribution of Carlines  

ggplot(data = mydatafc) +
  geom_bar(mapping = aes(x = CarlineClassDesc)) +
  coord_flip()


## Relation of variables on fuel consumption

# Plot showing relationship of city fuel and cylinders. The data for cylinders less than 4 was mostly null so removed those few rows

myprunedatafc <- mydatafc[-which(mydatafc$Cylinders <= 3), ]
myprunedatafc <- myprunedatafc[-which(is.na(myprunedatafc$Cylinders)),]
ggplot(myprunedatafc, aes(x = Cylinders, y = CityFuel)) +
  geom_point() +
  geom_smooth()

## Plot showing relationship of city fuel and Transmission

ggplot(data = mydatafc, mapping = aes(x = Trans, y = CityFuel)) +
  geom_boxplot()

# Plot showing relationship of city fuel and carlines

ggplot(data = mydatafc) +
  geom_boxplot(mapping = aes(x = reorder(CarlineClassDesc, CityFuel, FUN = median), y = CityFuel)) +
  coord_flip()

# Plot showing relationship of city fuel and Drive Systems

ggplot(data = mydatafc) +
  geom_boxplot(mapping = aes(x = reorder(DriveSys, CityFuel, FUN = median), y = CityFuel)) +
  coord_flip()


# multivariable distribution

# Plot shows the distribution of drive system withing each car line. 

ggplot(data = mydatafc,aes(CarlineClassDesc)) +
  geom_bar(aes(fill = DriveDesc)) +
  coord_flip()

# Plot shows the distribution of Cylinders withing each car line. 

ggplot(myprunedatafc, aes(CarlineClassDesc)) +
  geom_bar(aes(fill = factor(Cylinders))) + 
  coord_flip()

# Plot showing relationship fuel consumption and Green House Rating 
ggplot(data = mydatafc) +
  geom_line(mapping = aes(x = CityFuel, y = GHGRating))

#  Statistical Analysis 

# linear regression of cylinders and Cityfuel. 
#Plot showing scatter plot showing the visual comparison of City fuel (mpg) and cylinders. 

ggplot(myprunedatafc, aes(x = Cylinders,y = CityFuel)) +
  geom_point(aes(color = CarlineClassDesc)) +
  stat_smooth(method = "lm")


# Finding correlations between different variables. 

# Cleanup the GHGrating column that has non numeric values. The valid values are 1 thru 10.
mydatafc$GHGRating <- ifelse(mydatafc$GHGRating %in% c('Mod', 'mod'), NA, mydatafc$GHGRating)
mydatafc$MfrSmogRating <- ifelse(mydatafc$MfrSmogRating %in% c('Mod', 'mod'), NA, mydatafc$MfrSmogRating)

# Add catergorical variable with 2 levels of transmission
mydatafc <- mydatafc %>% dplyr::mutate(TransNum = ifelse(mydatafc$Trans == "M", 0, 1))

myvars <- c("ModelYear", "Cylinders", "Gears", "EngDispl",  "CityFuel", "TransNum", "GHGRating", "MfrSmogRating", "AnnualFuelCost")
mynewdata <- mydatafc[myvars]

# Visualize  correlations of features that city fuel depends on
correlations <- cor(mynewdata, use = "complete.obs")
corrplot(correlations, na.label = "square")

# Correlation between Engine Displacement and City Fuel
cor(mydatafc$CityFuel, mydatafc$EngDispl)

##  Linear model of all variables on mpg

# Remove data when city fuel is empty
mydatafc <- mydatafc[!is.na(mydatafc$CityFuel), ]

# density plot of City fuel to check normal distribution
plot(density(mydatafc$CityFuel), main="Density Plot: CityFuel in MPG", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydatafc$CityFuel), 2)))

# Run log transformation
mydatafcT <- mydatafc
mydatafcT$CityFuel <- log10(mydatafcT$CityFuel)
mydatafcT$EngDispl <- log10(mydatafcT$EngDispl)

# density plot of City fuel to check normal distribution
plot(density(mydatafcT$CityFuel), main="Density Plot: CityFuel in MPG", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydatafcT$CityFuel), 2)))

# Scale all numeric variables in the dataset
invisible(mydatafcT %>% mutate_if(is.numeric, scale))


# Results of Single Linear Regression (Engine Displacment on City Fuel) 

fuelmodel <- lm(CityFuel ~ EngDispl, data = mydatafcT)
summary(fuelmodel)
par(mfrow=c(2,2))
plot(fuelmodel)
gvlma(fuelmodel)

# Results of Multiple Regression model(Engine Displacment and Transmission on City Fuel )

fuelmodel1 <- lm(CityFuel ~ EngDispl + TransNum, data = mydatafcT)
summary(fuelmodel1)
par(mfrow=c(2,2))
plot(fuelmodel1)
gvlma(fuelmodel1)

#  Results of Multiple Regression model(Engine Displacment and Transmission and Gears on City Fuel )

fuelmodel2 <- lm(CityFuel ~ EngDispl + TransNum + Gears, data = mydatafcT)
summary(fuelmodel2)
par(mfrow=c(2,2))
plot(fuelmodel2)
gvlma(fuelmodel2)


   
  