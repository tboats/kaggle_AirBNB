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
statCols <- c("sum", "mean", "sd", "max", "min")
dfSessionStats[dfSessionStats$N == 1, statCols] <- 0
dfSessionStats[dfSessionStats$N == 2, "sd"] <- 0

## load other data
dfCountries <- read.csv("../data/countries.csv")

## merge data sets
df <- full_join(x = dfSessionStats, y = dfCountries) #, by = c("country_destination"="country_destination")

# clean up the data formats
df$date_account_created <- ymd(df$date_account_created)
df$date_first_booking <- ymd(df$date_first_booking)
df$country_destination <- as.factor(df$country_destination)
df <- mutate(df, travel = as.factor(1.*(country_destination != "NDF")))
df <- mutate(df, age = as.numeric(age))
df$age[is.na(df$age)] <- 0
df$timestamp_first_active <- ymd_hms(df$timestamp_first_active)
colsExclude <- c("X", "id", "date_first_booking", "lat_destination", "lng_destination",
                 "distance_km", "destination_km2", "destination_language", 
                 "language_levenshtein_distance", "travel") #,"timestamp_first_active"
goodCols <- !(names(df) %in% colsExclude)

#naCols <- names(which(colSums(is.na(df))>0))
#notNACols <- !unname(colSums(is.na(df))>0)
y1Cols <- (names(df) %in% "country_destination")
#travelCol <- (names(df) %in% "travel")

#####################################################################
## split into training, validation, and test sets
set.seed(1245)
trainFraction <- 0.7
trainIndex <- createDataPartition(df$country_destination, p = trainFraction, list = FALSE)
dfTrain1 <- df[trainIndex,goodCols] # & !y1Cols] & notNACols
dfTest1 <- df[-trainIndex,] # & !y1Cols] #
idTrain <- df[trainIndex, "id"]
idTest <- df[-trainIndex, "id"]
travel1Col <- (names(dfTrain1) %in% "travel")
y1Col <- (names(dfTrain1) %in% "country_destination")


####################################################################
## train random forest on travel/no travel
# tr <- na.omit(dfTrain)
# find best value of "mtry"
# Start the clock!
ptm <- proc.time()

# sample the full data set to check how long it will run
nsamp <- dim(dfTrain1)[1]#200 #10000 #
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


ntree <- 100
#rf <- randomForest(dfTrain1_s[,!y1Col_s], dfTrain1_s[,y1Col_s], data = dfTrain1_s, 

# use classwt to weight the classes
freq <- table(dfTrain1_s$country_destination)
wt <- unname(1/freq)

# impute missing ages
# cs <- dfTrain1_s[,!(names(dfTrain1_s) %in% c("date_account_created"))]
# dfTrain1_s.imputed <- rfImpute(country_destination ~ ., data = cs)
# dfTrain1_s.imputed <- mutate(dfTrain1_s.imputed, date_account_created = dfTrain1_s$date_account_created)
# dfTrain1_s.imputed <- dfTrain1_s
# dfTrain1_s.imputed$age[is.na(dfTrain1_s.imputed$age)] <- 0

# train random forest
rf <- randomForest(country_destination ~ ., data = dfTrain1_s, 
                   mtry=m1, 
                   classwt=wt,
                   ntree=ntree, 
                   keep.forest = TRUE, 
                   importance = TRUE) #, test = dfTest1




# plot the most important variables
varImpPlot(rf)

# how well does classifier perform on training set?
p1tr <- predict(rf, dfTrain1_s)
table(p1tr, dfTrain1_s$country_destination)

# how well does classifier perform on test set?

# impute age
# cs <- dfTest1[,!(names(dfTest1) %in% c("date_account_created"))]
# dfTest1.imputed <- rfImpute(country_destination ~ ., data = cs)
# dfTest1.imputed <- mutate(dfTest1.imputed, date_account_created = dfTest1$date_account_created)

p1prob <- predict(rf, dfTest1, type="prob")
p1 <- predict(rf, dfTest1)
table(p1, dfTest1$country_destination)

# Stop the clock
proc.time() - ptm


## create data frame to save predictions
p1df <- data.frame(country_destination = dfTest1$country_destination)
p1df <- cbind(p1df, data.frame(p1prob))
# head(p1df)

## generate top 5 predictions
p1prob_5 <- t(apply(p1prob, 1, predictionSort))
df_p1prob_5 <- data.frame(p1prob_5)
names(df_p1prob_5) <- c("C1", "C2", "C3", "C4", "C5") #names of top 5 countries
df1_p1prob_5_ans <- mutate(df_p1prob_5, country_destination = dfTest1$country_destination)
#evals <- t(apply(df1_p1prob_5[,1:5], 1, DCG, df1_p1prob_5[,6]))
evals <- (apply(df1_p1prob_5_ans, 1, DCG))
print(paste("mean DCG: ", mean(evals)))

######################################################################
## format for text output
dfOut <- mutate(df_p1prob_5, id=idTest)
dfOutm <- melt(dfOut, id = c("id"))
dfOutm <- with(dfOutm, dfOutm[order(id, variable),])
saveCols <- c("id", "value")
dfOutm <- dfOutm[,names(dfOutm) %in% saveCols]

source('E:/Dropbox/R/general/timestamp.R')
ts <- timestamp()
outputName <- paste("output_", ts, '.txt', sep="")
fileConn <- file(outputName)
# writeLines(c("id,country\n"), fileConn)
cat(c("id,country"), file=fileConn,sep="\n")

nLines <- dim(dfOutm)[1]
for (i in 1:100){
  line <- paste(dfOutm[i,"id"], dfOutm[i,"value"],sep=",")
  #writeLines(line, fileConn)
  cat(line, file=outputName, sep="\n", append=TRUE)
  if (i %% 1000 == 0){
    print(paste("line ", i, sep=""))
  }
}
close(fileConn)

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
