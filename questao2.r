# Pacotes carregados abaixo com o comando library foram instalados previamente >

# Carrega as bibliotecas que serão usadas

library("ggplot2")
library("lattice")
library("mlbench")
library("caret")
library("randomForest")
library("kernlab")

# Carrega arquivo que foi previamente baixado do site
dados <- read.csv2("Volumes.csv" , header=T)

# Remove coluna NR
dados_new = dados[ , 2:5]

# Faz a separação entre dados de treinamento e teste
indices <-createDataPartition(dados_new$VOL,p=0.80, list=FALSE)
treino <- dados_new[indices,]
teste <- dados_new[-indices,]

# Treinamento dos modelos
rf <- train(VOL~.,data=treino,method="rf")
svm <- train(VOL~.,data=treino,method="svmRadial")
rna <- train(VOL~.,data=treino,method="nnet")

# Predições
predicoes.rf <- predict(rf,teste)
predicoes.svm <- predict(svm,teste)
predicoes.rna <- predict(rna,teste)

# Comparando resultados
confusionMatrix(predicoes.rf, teste$VOL)
confusionMatrix(predicoes.svm, teste$VOL)
confusionMatrix(predicoes.rna, teste$VOL)

