# Goal: model the evaluation metric for AirBNB kaggle
# metric: NDCG (Normalized discounted cumulative gain) @k where k =5


DCG <- function(df){
  countries <- df[1:5]
  actual <- df[6]
  rel <- countries == actual
  nc <- length(countries)
  sum <- 0
  for(i in 1:nc){
    sum = sum + (2^rel[i] - 1) / (log(i + 1, base = 2))
  }
  sum
}

# # test code
# countries <- c("US", "US","NDF", "GB", "FR")
# actual <- "PT"
# 
# DCG(countries, actual)
