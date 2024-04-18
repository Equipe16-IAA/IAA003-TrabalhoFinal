# Pacotes carregados abaixo com o comando library foram instalados previamente

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
dados_new <- dados[ , 2:5]

# Define uma semente para manter os resultados sempre iguais em cada execução
set.seed(7)

# Faz a separação entre dados de treinamento e teste
indices <-createDataPartition(dados_new$VOL,p=0.80, list=FALSE)
treino <- dados_new[indices,]
teste <- dados_new[-indices,]

# Treinamento dos modelos
rf <- caret::train(VOL~.,data=treino,method="rf")
svm <- caret::train(VOL~.,data=treino,method="svmRadial")
redeneural <- caret::train(VOL~.,data=treino,method="neuralnet")
# Modelo alometrico
alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, dados_new, start=list(b0=0.5,b1=0.5))
 
# Predições
predicoes.rf <- predict(rf,teste)
predicoes.svm <- predict(svm,teste)
predicoes.redeneural <- predict(redeneural,teste)
predicoes.alom <- predict(alom,teste)



#coeficiente de determinação

r2 <- function(valor_real, valor_predito){
  
  r2 <- 1 - (sum((valor_real - valor_predito)^2)/sum((valor_real - mean(valor_predito))^2))
  
  return(r2)
}

#erro padrão da estimativa

syx <- function(valor_real, valor_predito){
  
  syx <- sqrt(sum((valor_real - valor_predito)^2)/(length(valor_real)-2))
  
  return(syx)
}

#porcentagem do erro padrao

porsyx <- function(valor_real, valor_predito){
  
  porsyx <- (syx(valor_real, valor_predito)/mean(valor_real))*100
  
  return(porsyx)
}


###comparação entre resultados

#rf
cat("---------------------RANDOM FOREST---------------------------\n\n")
cat("R2\n")
r2(teste$VOL, predicoes.rf)
cat("\n")
cat("Erro Padrão de Estimativa (Syx)\n")
syx(teste$VOL, predicoes.rf)
cat("\n")
cat("Erro Padrão de Estimativa (Syx) percentual\n")
porsyx(teste$VOL, predicoes.rf)
cat("\n")
cat("RMSE\n")
RMSE(predicoes.rf, teste$VOL)

#svm
cat("---------------------SVM---------------------------\n\n")
cat("R2\n")
r2(teste$VOL, predicoes.svm)
cat("\n")
cat("Erro Padrão de Estimativa (Syx)\n")
syx(teste$VOL, predicoes.svm)
cat("\n")
cat("Erro Padrão de Estimativa (Syx) percentual\n")
porsyx(teste$VOL, predicoes.svm)
cat("\n")
cat("RMSE\n")
RMSE(predicoes.svm, teste$VOL)

#redes neurais
cat("---------------------REDES NEURAIS---------------------------\n\n")
cat("R2\n")
r2(teste$VOL, predicoes.redeneural)
cat("\n")
cat("Erro Padrão de Estimativa (Syx)\n")
syx(teste$VOL, predicoes.redeneural)
cat("\n")
cat("Erro Padrão de Estimativa (Syx) percentual\n")
porsyx(teste$VOL, predicoes.redeneural)
cat("\n")
cat("RMSE\n")
RMSE(predicoes.redeneural, teste$VOL)

#modelo alometrico de Spurr
cat("---------------------ALOMETRICO---------------------------\n\n")
cat("R2\n")
r2(teste$VOL, predicoes.alom)
cat("\n")
cat("Erro Padrão de Estimativa (Syx)\n")
syx(teste$VOL, predicoes.alom)
cat("\n")
cat("Erro Padrão de Estimativa (Syx) percentual\n")
porsyx(teste$VOL, predicoes.alom)
cat("\n")
cat("RMSE\n")
RMSE(predicoes.alom, teste$VOL)
