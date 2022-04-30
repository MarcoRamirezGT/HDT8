library(caret)
library(nnet)
library(neural)
library(dummy)
library(neuralnet)



data<-read.csv('train.csv')



#Quitar nulos
data[is.na(data)] <- 0
#Calculo de percentiles
percentil <- quantile(data$SalePrice)
#Percentiles
estado<-c('Estado')
data$Estado<-estado
#Economica=0
#Intermedia=1
#Cara=2
data <- within(data, Estado[SalePrice<=129975] <- 'Economica')
data$Estado[(data$SalePrice>129975 & data$SalePrice<=163000)] <- 'Intermedia'
data$Estado[data$SalePrice>163000] <- 'Cara'


#Cambio de tipo de columnas

data$SalePrice<-as.numeric(data$SalePrice)
data$GrLivArea<-as.numeric(data$GrLivArea)
data$GarageCars<-as.numeric(data$GarageCars)
data$YearBuilt<-as.numeric(data$YearBuilt)
data$GarageArea<-as.numeric(data$GarageArea)
data$X1stFlrSF<-as.numeric(data$X1stFlrSF)

data$Estado<-as.factor(data$Estado)

porcentaje<-0.7
set.seed(123)
datos<-data.frame(data$SalePrice,data$GrLivArea,data$GarageCars,data$YearBuilt,data$GarageArea,data$X1stFlrSF,data$Estado)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

#-------------------------------------------------
# Red Neuronal con nnet
#-------------------------------------------------

modelo.nn2 <- nnet(data.Estado~.,data = datos,subset = corte, size=2, rang=0.1,
                   decay=5e-4, maxit=200) 
prediccion2 <- as.data.frame(predict(modelo.nn2, newdata = test[,1:6]))
columnaMasAlta<-apply(prediccion2, 1, function(x) colnames(prediccion2)[which.max(x)])
test$prediccion2<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción

modelo1<-confusionMatrix(as.factor(test$prediccion2),test$data.Estado)


#-------------------------------------------------
# Red Neuronal con caret
#-------------------------------------------------

modeloCaret <- train(data.Estado~., data=train, method="nnet", trace=F)
test$prediccionCaret<-predict(modeloCaret, newdata = test[,1:6])
modelo2<-confusionMatrix(test$prediccionCaret,test$data.Estado)




