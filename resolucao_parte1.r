library("mlbench")
library("caret")


data(Satellite) #carregamento da base Satellite do pacote
str(Satellite) #visualizacao dos dados principais

valores_centrais <- Satellite [,c(17,18,19,20,37)]
head(valores_centrais)

set.seed(7)

indices <- createDataPartition(valores_centrais$classes, p=0.8, list=FALSE) #particao em base de treino 80%

treino <- valores_centrais[indices,]

teste <- valores_centrais[-indices,]


#RANDOM FOREST

rf <- caret::train(classes~., data=treino, method="rf")

predicoes.rf <- predict(rf, teste)

matriz.rf <- caret::confusionMatrix(predicoes.rf, teste$classes)
matriz.rf


#SVM

svm <- caret::train(classes ~ ., data=treino, method="svmRadial")

predicoes.svm <- predict(svm, teste)

matriz.svm <- caret::confusionMatrix(predicoes.svm, teste$classes)
matriz.svm


#RNA

rna <- caret::train(classes~., data=treino, method="nnet")
predicoes.rna <- predict(rna, teste)

matriz.rna <- caret::confusionMatrix(predicoes.rna, teste$classes)
matriz.rna

