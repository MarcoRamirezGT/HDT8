library(nnet)
library(neuralnet)
library(ggplot2)

data<-read.csv('train.csv')

#Cambio de tipo de columnas

data$SalePrice<-as.numeric(data$SalePrice)
data$GrLivArea<-as.numeric(data$GrLivArea)
data$GarageCars<-as.numeric(data$GarageCars)
data$YearBuilt<-as.numeric(data$YearBuilt)
data$GarageArea<-as.numeric(data$GarageArea)
data$X1stFlrSF<-as.numeric(data$X1stFlrSF)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



porcentaje<-0.7
set.seed(123)
datos2<-data.frame(data$GrLivArea,data$GarageCars,data$YearBuilt,data$GarageArea,data$X1stFlrSF,data$SalePrice)
datos <- as.data.frame(lapply(datos2, normalize))


corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

#-------------------------------------------------
# Red Neuronal con nnet
#-------------------------------------------------

modelo.nn2 <- nnet(as.numeric(data.SalePrice)~.,data = datos,subset = corte, size=2, rang=0.1,decay=5e-4, maxit=200) 
prediccion1 <- predict(modelo.nn2, newdata = test,na.rm=TRUE)
prediccion1

minvec <- sapply(datos2,min)
maxvec <- sapply(datos2,max)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}

datos<-as.data.frame(Map(denormalize,datos,minvec,maxvec))
prediccion1_r<-(prediccion1)*(max(datos2)-min(datos2))+min(datos2)

plot(datos$data.SalePrice)
plot(prediccion1_r)

