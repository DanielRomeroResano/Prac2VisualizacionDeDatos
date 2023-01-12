#########################################
# title: "PRAC2 Visualización de datos" #
# author: "Daniel Romero Resano"        #
# date: "2023-01-12"                    #
#########################################  


# Read CSV

data<-read.csv("./US_Accidents_Dec21_updated.csv",header=T,sep=",")

head(data)
str(data)


library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(stringr)
library(lubridate)


#####################################################################
#State and Cities map
#####################################################################


#check states
data$State <- as.factor(data$State)
levels(data$State)


#Create freq table for each of the states
dfState <- as.data.frame(table(data$State))
write.csv(dfState, "StateCases.csv", row.names=FALSE)



#Repeat for each of the top 100 Cities
dfCity <- as.data.frame(table(data$City))
dfTopCity <- tail(dfCity[
  with(dfCity, order(Freq)),
],100)

for(c in dfTopCity$Var1){
  dfTopCity$lon[dfTopCity$Var1==c] <- mean(data$Start_Lng[data$City==c])
  dfTopCity$lat[dfTopCity$Var1==c] <- mean(data$Start_Lat[data$City==c])
  #calculate a mean for each city lat and long
}

write.csv(dfTopCity, "TopCities.csv", row.names=FALSE) #save CSV




#####################################################################
#Cities Hist (Only top 30 cities)
#####################################################################



dfCity <- as.data.frame(table(data$City))
dfTopCity <- tail(dfCity[
  with(dfCity, order(Freq)),
],30)

for(c in dfTopCity$Var1){
  dfTopCity$lon[dfTopCity$Var1==c] <- mean(data$Start_Lng[data$City==c])
  dfTopCity$lat[dfTopCity$Var1==c] <- mean(data$Start_Lat[data$City==c])
  #calculate a mean for each city lat and long in order to plot the points
}

write.csv(dfTopCity, "TopCities.csv", row.names=FALSE) #save CSV





#####################################################################
#Severity Pie Chart
#####################################################################


dfSev <- as.data.frame(table(data$Severity))


write.csv(dfSev, "Severity.csv", row.names=FALSE) #save CSV




#####################################################################
#Type of street (Complex solution due to format, to be continued)
#####################################################################



dfSt <- as.data.frame(table(word(sub('-', ' ', data$Street,1),1)))

dfTopSt <- tail(dfSt[
  with(dfSt, order(Freq)),
],30)

write.csv(dfTopSt, "Street.csv", row.names=FALSE) #save CSV





#####################################################################
#Year Hist
#####################################################################



Date <- as.POSIXct(data$Start_Time, format="%Y-%m-%d  %H:%M:%S")

dfYear <- as.data.frame(table(format(Date, format="%Y")))


write.csv(dfYear, "dfYear.csv", row.names=FALSE) #save CSV




#####################################################################
#Month with more accidents
#####################################################################


Date <- as.POSIXct(data$Start_Time, format="%Y-%m-%d  %H:%M:%S")

dfMonth <- as.data.frame(table(format(Date, format="%m"),data$Severity))


write.csv(dfMonth, "dfMonth.csv", row.names=FALSE) #save CSV



#####################################################################
#Hours with more accidents
#####################################################################



dfHour <- as.data.frame(table(format(Date, format="%H")))


write.csv(dfHour, "dfHour.csv", row.names=FALSE) #save CSV


#####################################################################
#Hours-Days with more accidents
#####################################################################



dfHourDay <- as.data.frame.matrix(table(wday(Date, label=TRUE, abbr=FALSE),format(Date, format="%H")))


write.csv(dfHourDay, "dfHourDay.csv", row.names=TRUE) #save CSV





#####################################################################
# Accident Heatmap Severity
#####################################################################

accidentDataAux=subset(data, select=c(Start_Lat,Start_Lng,Severity))

sample <- sample(c(TRUE, FALSE), nrow(accidentDataAux), replace=TRUE, prob=c(0.1,0.9))
accidentDataSample  <- accidentDataAux[sample, ]


write.csv(accidentDataSample, "Points.csv", row.names=FALSE) #save CSV


#####################################################################
# Accident Heatmap (City and Street)
#####################################################################

accidentDataAux=subset(data, select=c(Start_Lat,Start_Lng,City,Street))

sample <- sample(c(TRUE, FALSE), nrow(accidentDataAux), replace=TRUE, prob=c(0.1,0.9))
accidentDataSample  <- accidentDataAux[sample, ]

accidentDataSample$size <- 1


write.csv(accidentDataSample, "Points.csv", row.names=FALSE) #save CSV




####################################################################
# Contributing factors
####################################################################



dfPrecip <- as.data.frame.matrix(table(data$Precipitation.in.,data$Severity))


write.csv(dfPrecip, "dfPrecip.csv", row.names=TRUE) #save CSV




dfVisibility <- as.data.frame.matrix(table(data$Visibility.mi.,data$Severity))


write.csv(dfVisibility, "dfVisibility.csv", row.names=TRUE) #save CSV
