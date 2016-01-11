# Goal: format a probability list from a random forest prediction into a format that can be sent to DCG

predictionSort <- function(df1, k=5){
  df1sort <- sort(df1, decreasing=TRUE, index.return=TRUE)
  topNames <- names(df1)[df1sort$ix]
  topNames[1:k]
}

# # test code
# df1 <- p1prob[1,]
#predictionSort(df1)
