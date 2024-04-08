library("mlbench")
library("caret")

df <- read.csv2("http://www.razer.net.br/datasets/Volumes.csv", header=T, sep=";", dec=",")


dados <- subset(df, select= -NR)

head(dados)

set.seed(7)

indices <- createDataPartition(dados$VOL, p=0.8, list=FALSE) #particao em base de treino 80%


treino <- dados[indices,]
teste <- dados[-indices,]


#RANDOM FOREST

rf <- caret::train(VOL ~ ., data=treino, method="rf")

predicoes.rf <- predict(rf, teste)

#SVM

svm <- caret::train(VOL ~ ., data=treino, method="svmRadial")

predicoes.svm <- predict(svm, teste)

#REDES NEURAIS

redeneural <- caret::train(VOL ~ ., data=treino, method="neuralnet")

predicoes.redeneural <- predict(redeneural, teste)

#MODELO ALOMETRICO DE SPURR !!!!!CONFERIR IMPLEMENTAÇÃO!!!!!

alom <-	nls(VOL	~ b0 + b1* DAP * DAP * HT,	data=treino, start=list(b0=0.5, b1=0.5))

predicoes.alom <- predict(alom, teste)

######UDFs

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
r2(teste$VOL, predicoes.rf)
syx(teste$VOL, predicoes.rf)
porsyx(teste$VOL, predicoes.rf)
RMSE(predicoes.rf, teste$VOL)

#svm
r2(teste$VOL, predicoes.svm)
syx(teste$VOL, predicoes.svm)
porsyx(teste$VOL, predicoes.svm)
RMSE(predicoes.svm, teste$VOL)

#redes neurais
r2(teste$VOL, predicoes.redeneural)
syx(teste$VOL, predicoes.redeneural)
porsyx(teste$VOL, predicoes.redeneural)
RMSE(predicoes.redeneural, teste$VOL)

#modelo alometrico de Spurr
r2(teste$VOL, predicoes.alom)
syx(teste$VOL, predicoes.alom)
porsyx(teste$VOL, predicoes.alom)
RMSE(predicoes.alom, teste$VOL)
