library("mlbench")
library("caret")


data(Satellite) #carregamento da base Satellite do pacote
str(Satellite) #visualizacao dos dados principais


set.seed(7)

indices <- createDataPartition(Satellite$classes, p=0.8, list=FALSE) #particao em base de treino 80%

treino <- Satellite[indices,]

teste <- Satellite[-indices,]

#RANDOM FOREST

rf <- caret::train(classes~., data=treino, method="rf")

predicoes.rf <- predict(rf, teste)

matriz.rf <- caret::confusionMatrix(predicoes.rf, teste$classes)
matriz.rf


#
