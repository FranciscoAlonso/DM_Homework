tareaWine <- function()
{
  library(C50)
  library(dplyr)
  library(caret)
  wine <- as.data.frame(read.csv("winequality-red.csv", sep = ";"))
  wine$quality <- as.factor(wine$quality)
  write.csv(x = wine, file = "wine.csv")
#   View(wine[,1])
#   View(wine$fixed.acidity)
  
  set.seed(42)
  trainingIndex <- sample(seq_len(nrow(wine)), size = (nrow(wine) * 0.8))
  
  trainWine <- wine[trainingIndex, ]
  testWine <- wine[-trainingIndex, ]
  
  #wineTree <- C5.0(x = trainWine[, -10], y = trainWine$quality)
  wineTree <- C5.0(trainWine$quality~., data = trainWine)
  
#   wineTree <- train(trainWine$quality ~ .
#                     , data = trainWine
#                     , method = "C5.0"
#                     #, tuneGrid = c50Grid
#                     #, trControl = ctrl
#                     #, metric = "Accuracy" # not needed it is so by default
#                     #, importance=TRUE # not needed
#                     #, preProc = c("center", "scale")
#                     ) 
  #print(text[pos])
  #wineTree$output
  #summary(wineTree)
  #prediction <- predict(wineTree, trainWine)
  #summary(prediction)
  
  
}