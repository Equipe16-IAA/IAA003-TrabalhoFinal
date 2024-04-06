
data(Satellite)
str(Satellite)

set.seed(7)
indices <- createDataPartition(Satellite$classes, p=0.8, list=FALSE)

treino <- Satellite[indices,]

teste <- Satellite[-indices,]