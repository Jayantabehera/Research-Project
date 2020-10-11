####Data imputation by Multiple Imputation by Chained Equations (MICE) package
setwd("D:/Research Project/Datasets/Flood intensity/data url/Final Data/")
data <- read.csv("after_dropping_column.csv", stringsAsFactors = TRUE)
data_bck <- data
data
data <- data[, -c(1,2,4,5,6,7,8,9,19)]

#missing values for Item_Weight
sum(is.na(data))

####plotting missing value 
library(Amelia)
missmap(data, main = "Missing values vs observed")


#####ceck average values of columns
library(dplyr)
summarise(data, Average = mean(Day0_Temperature, na.rm = T))
summarise(data, Average = mean(Day.1_Temperature, na.rm = T))
summarise(data, Average = mean(Day.2_Temperature, na.rm = T))
summarise(data, Average = mean(Day.3_Temperature, na.rm = T))
summarise(data, Average = mean(Day.4_Temperature, na.rm = T))

#convert categorical valriables as factor
data$Day0_Wind <- factor(data$Day0_Wind)
data$Day0_Condition  <- factor(data$Day0_Condition)
data$Day.1_Wind <- factor(data$Day.1_Wind)
data$Day.1_Condition <- factor(data$Day.1_Condition)
data$Day.2_Wind <- factor(data$Day.2_Wind)
data$Day.2_Condition <- factor(data$Day.2_Condition)
data$Day.3_Wind <- factor(data$Day.3_Wind)
data$Day.3_Condition <- factor(data$Day.3_Condition)
data$Day.3_Wind <- factor(data$Day.3_Wind)
data$Day.4_Condition <- factor(data$Day.4_Condition)

####check summary of dataframe
summary(data)

## missing data patterns
library(mice)
md.pattern(data)

install.packages("VIM")
library(VIM)
mice_plot <- aggr(data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

####impute with 3 iterations with random forest
imputed_Data <- mice(data, m=3, maxit = 3, method = 'rf', seed = 123)
summary(imputed_Data)
imputed_Data$imp$Day0_Temperature
typeof(imputed_Data$imp$Day.1_Temperature)

##par(mfrow=c(2,2)) # allows us to put plots into a 2 x 2 grid -- makes it easier to compare
##hist(data$Day0_Temperature, freq=F, main='Temp0: Original', col='red', ylim=c(0,0.04))
##imputed_Data$imp$Day.1_Temperature = as.double(imputed_Data$imp$Day.1_Temperature)
##hist(imputed_Data$imp$Day.1_Temperature, freq=F, main='Temp0: Imputed', col='red', ylim=c(0,0.04))


imputed_Data$imp$Day0_Temperature
imputed_Data$imp$Day.1_Temperature

#get complete data ( 3nd out of 3)
completeData <- complete(imputed_Data,2)

#add deleted columns to completeData -1,2,4,5,6,7,8,9,19
completeData$ID <- data_bck$ID
completeData$Country <- data_bck$Country       
completeData$Began <- data_bck$Began     
completeData$Ended <- data_bck$Ended 
completeData$Dead  <- data_bck$Dead 
completeData$Displaced <- data_bck$Displaced         
completeData$MainCause <- data_bck$MainCause         
completeData$Severity  <- data_bck$Severity   
completeData$Duration <- data_bck$Duration


##exporting the imputaed data as csv file
write.csv(completeData,"D:\\Research Project\\Datasets\\Flood intensity\\data url\\Final Data\\Imputed_Data.csv", row.names = FALSE)
