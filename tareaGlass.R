tareaGlass <- function()
{
  library(C50)
  library(dplyr)
  library(caret)
  library(PerformanceAnalytics)
  library(foreign)
  library(RWeka)
  
#   seismic <- as.data.frame(read.arff("seismic-bumps.arff"))
#   seismic$class <- as.factor(seismic$class)
#   write.csv(x = seismic, file = "seismic.csv")
#   #View(seismic)
#   
#   set.seed(42)
#   trainingIndex <- sample(seq_len(nrow(seismic)), size = (nrow(seismic) * 0.8))
#   trainSeismic <- seismic[trainingIndex, ]
#   testSeismic <- seismic[-trainingIndex, ]
#   
#   seismicTreeC50 <- C5.0(trainSeismic$class~., data = trainSeismic)
#   predictionC50 <- predict.C5.0(seismicTreeC50, testSeismic)
#   #print(confusionMatrix(predictionC50, testSeismic$class))
#   
#   seismicRIPPER <- RWeka::JRip(trainSeismic$class~., data = trainSeismic)
#   predictionRIPPER <- predict(seismicRIPPER, testSeismic)
#   #print(confusionMatrix(predictionRIPPER, testSeismic$class))
#   
#   seismicSMOTED <- SMOTE(class ~ ., data = seismic, perc.over = 500, k = 5)
#   write.csv(x = seismicSMOTED, file = "seismicSMOTED.csv")
#   trainingIndex <- sample(seq_len(nrow(seismicSMOTED)), size = (nrow(seismicSMOTED) * 0.8))
#   trainSeismic <- seismicSMOTED[trainingIndex, ]
#   testSeismic <- seismicSMOTED[-trainingIndex, ]
# 
#   seismicTreeC50 <- C5.0(trainSeismic$class~., data = trainSeismic, trials = 20)
#   predictionC50 <- predict.C5.0(seismicTreeC50, testSeismic)
#   #print(summary(seismicTreeC50))
#   #print(confusionMatrix(predictionC50, testSeismic$class))
# 
#   seismicRIPPER <- RWeka::JRip(trainSeismic$class~., data = trainSeismic)
#   predictionRIPPER <- predict(seismicRIPPER, testSeismic)
#   print(summary(seismicRIPPER))
#   print(confusionMatrix(predictionRIPPER, testSeismic$class))

  
  glass <- as.data.frame(read.table("glass.data", sep = ","))
  glass <- select(glass, -V1)
  names(glass) <- c("RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "Type")
  glass$Type <- as.factor(glass$Type)
  write.csv(x = glass, file = "glass.csv")
  
  set.seed(42)
  trainingIndex <- sample(seq_len(nrow(glass)), size = (nrow(glass) * 0.8))
   
  trainGlass <- glass[trainingIndex, ]
  testGlass <- glass[-trainingIndex, ]
#    View(trainGlass)   
#    View(testGlass)
  glassTree <- C5.0(trainGlass$Type~., data = trainGlass, trials = 5)
  #summary(glassTree)
   
  prediction <- predict.C5.0(glassTree, testGlass)
  table(True = testGlass$Type, predicted = prediction)
  confusionMatrix(prediction, testGlass$Type)
}