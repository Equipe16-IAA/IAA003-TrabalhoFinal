# Pacotes carregados abaixo com o comando library foram instalados previamente utilizando o comando install.packages("[nome_do_pacote]")

# Carrega as bibliotecas que serão usadas
library("ggplot2")
library("lattice")
library("mlbench")
library("caret")
library("randomForest")
library("kernlab")

# Carrega a base de dados Satellite
data(Satellite)

# Mostra que a base foi carregada
df<-Satellite[,c(17,18,19,20,37)]

# Define uma semente para manter os resultados sempre iguais em cada execução
set.seed(10)

# Faz a separação entre dados de treinamento e teste
indices <-createDataPartition(df$classes,p=0.80, list=FALSE)
treino <- Satellite[indices,]
teste <- Satellite[-indices,]

# Treinamento dos modelos
rf <- train(classes~.,data=treino,method="rf")
svm <- train(classes~.,data=treino,method="svmRadial")
rna <- train(classes~.,data=treino,method="nnet")

# Predições
predicoes.rf <- predict(rf,teste)
predicoes.svm <- predict(svm,teste)
predicoes.rna <- predict(rna,teste)

# Comparando resultados
confusionMatrix(predicoes.rf, teste$classes)
confusionMatrix(predicoes.svm, teste$classes)
confusionMatrix(predicoes.rna, teste$classes)





