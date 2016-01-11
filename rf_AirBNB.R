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
df <- mutate(df, travel = as.factor(1.*(country_destination != "NDF")))
colsExclude <- c("X", "id", "date_first_booking")
goodCols <- !(names(df) %in% colsExclude)

naCols <- names(which(colSums(is.na(df))>0))
notNACols <- !unname(colSums(is.na(df))>0)
y1Cols <- (names(df) %in% "country_destination")
#travelCol <- (names(df) %in% "travel")

#####################################################################
## split into training, validation, and test sets
set.seed(1245)
trainFraction <- 0.7
trainIndex <- createDataPartition(df$country_destination, p = trainFraction, list = FALSE)
dfTrain1 <- df[trainIndex,goodCols & notNACols] # & !y1Cols]
dfTest1 <- df[-trainIndex,] # & !y1Cols] #
travel1Col <- (names(dfTrain1) %in% "travel")
y1Col <- (names(dfTrain1) %in% "country_destination")


####################################################################
## train random forest on travel/no travel
# tr <- na.omit(dfTrain)
# find best value of "mtry"
# Start the clock!
ptm <- proc.time()

# sample the full data set to check how long it will run
nsamp <- dim(dfTrain1)[1]#200
samp <- sample(1:dim(dfTrain1)[1], nsamp)
dfTrain1_s <- dfTrain1[samp,!travel1Col]
dfTrain1_s$country_destination <- as.factor(as.character(dfTrain1_s$country_destination))
y1Col_s <- (names(dfTrain1_s) %in% "country_destination")

# 
computeMtry <- FALSE
if (computeMtry == TRUE){
  bestMtry <- tuneRF(dfTrain1_s[,!y1Col_s], dfTrain1_s[,y1Col_s], 
                     mtryStart = 5, ntreeTry = 100, stepFactor = 1.5, improve = 0.10)
  m1 <- bestMtry[which(bestMtry[,"OOBError"] == min(bestMtry[,"OOBError"])), "mtry"]
} else {
  m1 <- 4
}


ntree <- 200
#rf <- randomForest(dfTrain1_s[,!y1Col_s], dfTrain1_s[,y1Col_s], data = dfTrain1_s, 

# use classwt to weight the classes
freq <- table(dfTrain1_s$country_destination)
wt <- unname(1/freq)

# train random forest
rf <- randomForest(country_destination ~ ., data = dfTrain1_s, 
                   mtry=m1, 
                   classwt=wt,
                   ntree=ntree, 
                   keep.forest = TRUE, 
                   importance = TRUE, 
                   test = dfTest1)
varImpPlot(rf)

p1tr <- predict(rf, dfTrain1_s)
table(p1tr, dfTrain1_s$country_destination)

p1prob <- predict(rf, dfTest1, type="prob")
p1 <- predict(rf, dfTest1)
table(p1, dfTest1$country_destination)
# Stop the clock
proc.time() - ptm


## format data for export to CSV
p1df <- data.frame(country_destination = dfTest1$country_destination)
p1df <- cbind(p1df, data.frame(p1prob))
# head(p1df)

## generate top 5 predictions
p1prob_5 <- t(apply(p1prob, 1, predictionSort))
df_p1prob_5 <- data.frame(p1prob_5)
names(df_p1prob_5) <- c("C1", "C2", "C3", "C4", "C5") #names of top 5 countries
df1_p1prob_5 <- mutate(df_p1prob_5, country_destination = dfTest1$country_destination)
#evals <- t(apply(df1_p1prob_5[,1:5], 1, DCG, df1_p1prob_5[,6]))
evals <- (apply(df1_p1prob_5, 1, DCG))
print(paste("mean DCG: ", mean(evals)))

# ####################################################################
# ## train random forest
# tr <- na.omit(dfTrain)
# tr$country_destination <- as.factor(as.character(tr$country_destination))
# rf <- randomForest(country_destination ~ ., data = tr, 
#                    mtry=2, 
#                    ntree=1000, 
#                    keep.forest = TRUE, 
#                    importance = TRUE, 
#                    test = dfTest)
