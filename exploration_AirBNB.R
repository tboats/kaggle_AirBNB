######################################################################
# Goal of script: explore AirBNB data
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


## load training users data
if (!exists("dfTrain")){
  dfTrain <- read.csv("../data/train_users_2.csv")
}

if (!exists("dfSessions")){
  dfSessions <- read.csv("../data/sessions.csv")
}

## load other data
dfCountries <- read.csv("../data/countries.csv")
dfAgeGender <- read.csv("../data/age_gender_bkts.csv")

## look at the first user


uoi <- "d1mm9tcy42" # user of interest
test <- filter(dfSessions, user_id == uoi)


## how does the length of time spent on the site affect the probability of travel?

#dfSubSessions <- dfSessions[1:1000,]
# aggregate the time spent
# operation takes: 200 s
if (!exists("dfSessions_byUser")){
  dfSessions_byUser <- ddply(dfSessions, c("user_id"), summarise, 
                           sum = sum(secs_elapsed, na.rm = TRUE), 
                           mean = mean(secs_elapsed, na.rm = TRUE), 
                           sd = sd(secs_elapsed, na.rm = TRUE), 
                           N = length(secs_elapsed),
                           max = max(secs_elapsed),
                           min = min(secs_elapsed)
                             )
}

# merge data sets
# operation takes: 7 s
# Start the clock!
ptm <- proc.time()
if (!exists("df_merged1")){
  #dfSessions_byUser_sub <- dfSessions_byUser#[1:1000,]
  df_merged1 <- inner_join(x = dfTrain, y = dfSessions_byUser, by = c("id"="user_id"))
}
# Stop the clock
proc.time() - ptm

# plot histograms of the various sessions statistics
colsOI <- c("id", "country_destination", "sum", "mean", "sd", "N")
df_merged2 <- df_merged1[,colsOI]
mdata <- melt(df_merged2, id = "id")

# add column to indicate whether travel occurred or not
mdata <- mutate(mdata, travel = 1.*(value != "NDF"))
df_merged2 <- mutate(df_merged2, travel = 1.*(country_destination != "NDF"))

# plot: N
q <- ggplot(data=df_merged2, aes(x = N, fill = as.factor(travel)))
q + geom_histogram(alpha = 0.5, position = 'dodge', binwidth = 20) +
  xlim(c(0,500))

# plot: sum
q <- ggplot(data=df_merged2, aes(x = sum, fill = as.factor(travel)))
q + geom_histogram(alpha = 0.5, position = 'dodge', binwidth = 1e+05) + 
  xlim(c(0,0.5e+07))

# plot: mean
q <- ggplot(data=df_merged2, aes(x = mean, fill = as.factor(travel)))
q + geom_histogram(alpha = 0.5, position = 'dodge', binwidth = 2000) + xlim(c(0,1e5))

# plot: sd
q <- ggplot(data=df_merged2, aes(x = sd, fill = as.factor(travel)))
q + geom_histogram(alpha = 0.5, position = 'dodge', binwidth = 2000) + xlim(c(0,2e5))
