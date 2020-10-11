#####Feature Selection
library(Boruta)
setwd("D:/Research Project/Datasets/Flood intensity/data url/Final Data/")
traindata <- read.csv("Check_for_Significance_before_one_hot.csv", header = T, stringsAsFactors = F)
traindata_bck <- traindata
str(traindata)
#names(traindata) <- gsub("_", "", names(traindata))
###drop unrequired columns
traindata <- traindata[, -c(1,3,4,5)]
summary(traindata)
###check for Null values
sum(is.na(traindata)) 


###convert categorical variables into factor data type.
convert <- c(7,8, 20:24, 50:54)
traindata[,convert] <- data.frame(apply(traindata[convert], 2, as.factor))


# create dummy variables for category variables
library(data.table)
library(mltools)
traindata <- one_hot(as.data.table(traindata~.-Severity))
summary(traindata)

#####implement and check the performance of boruta package
set.seed(123)
#boruta.train <- Boruta(Area_Affected_per_day~.-ID, data = traindata, doTrace = 2)
boruta.train <- Boruta(Severity~.-ID, data = traindata, doTrace = 2)
print(boruta.train)


plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

####Decision on tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

###list of confirmed attributes
getSelectedAttributes(final.boruta, withTentative = F)


####create a data frame of the final result derived from Boruta.
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

library(dplyr) 
final_data <- traindata %>% select(getSelectedAttributes(final.boruta, withTentative = F))
final_data$ID <- traindata_bck$ID
final_data$Severity <- traindata_bck$Severity
str(final_data)

##exporting the feature selection data using boruta as csv file
write.csv(final_data,"D:\\Research Project\\Datasets\\Flood intensity\\data url\\Final Data\\Classification\\Boruta_Feature_Selection_Classification.csv", row.names = FALSE)


