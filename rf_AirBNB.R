######################################################################
# Goal of script: create a random forest model on AirBNB data to predict country of visit
#
#
# date of start: 01/09/2016
#
#
######################################################################

## load libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)
library(lubridate)
library(randomForest)
#library(ROCR)

## load input from "exploration_AirBNB.R"
filename <- "dfTrain_sessionStats1.csv"
dfSessionStats <- read.csv(filename)

## load other data
dfCountries <- read.csv("../data/countries.csv")

## merge data sets
df <- full_join(x = dfSessionStats, y = dfCountries) #, by = c("country_destination"="country_destination")

# clean up the data formats
df$date_account_created <- ymd(df$date_account_created)
df$date_first_booking <- ymd(df$date_first_booking)
df$country_destination <- as.factor(df$country_destination)
colsExclude <- c("X", "id", "date_first_booking")
goodCols <- !(names(df) %in% colsExclude)

#####################################################################
## split into training, validation, and test sets
set.seed(1245)
trainFraction <- 0.7
trainIndex <- createDataPartition(df$country_destination, p = trainFraction, list = FALSE)
dfTrain <- df[trainIndex,goodCols]
dfTest <- df[-trainIndex,goodCols]



####################################################################
## train random forest
tr <- na.omit(dfTrain)
tr$country_destination <- as.factor(as.character(tr$country_destination))
rf <- randomForest(country_destination ~ ., data = tr, 
                   mtry=2, 
                   ntree=1000, 
                   keep.forest = TRUE, 
                   importance = TRUE, 
                   test = dfTest)
