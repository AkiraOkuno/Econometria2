install.packages("fpp")
install.packages("zoo")
install.packages("xts")
install.packages("ggfortify")
install.packages("ggplot2")
install.packages("BETS")
install.packages("dynlm")
install.packages("vars")
install.packages("forecast")


library(fpp)
library(zoo)
library(xts)
library(ggfortify)
library(ggplot2)
library(BETS)
library(dynlm)
library(vars)

#filtro HP tira a sazonalidade mas deixa a tendencia

#ibovespa

ibovespa = BETS.get(code=7832)
temp = ibovespa
start(temp)
end(temp)
frequency(temp)

#commodities

commodities = BETS.get(code=20048)
temp = commodities
start(temp)
end(temp)
frequency(temp)

#IPCA

IPCA = BETS.get(code=433)
temp = IPCA
start(temp)
end(temp)
frequency(temp)

#Juros - CDI

CDI = BETS.get(code=4391)
temp = CDI
start(temp)
end(temp)
frequency(temp)

#Indústria

Ind = BETS.get(code=21859)
temp = Ind
start(temp)
end(temp)
frequency(temp)

#Remover arquivo temporário
rm(temp)

#Criar dummies temporais para o VAR
dummies = ts(seasonaldummy(Ind), start = start(Ind), freq=12)

#Constroi o Dataset
dataset = ts.intersect(Ind, IPCA, ibovespa, CDI, commodities)
colnames(dataset) = c('Indústria', 'IPCA', 'Ibovespa', 'Juros', 'Commodities')

#Summary of variables
summary(dataset)

var = VAR(dataset, p=1, type = 'both', exogen = dummies)

#summary of VAR
summary(var)

#seleção da ordem do modelo

max_lag = 24

AIC_Matrix = matrix(0, max_lag, 1)

for(k in 1:max_lag){
  var = VAR(dataset, p=k, type = 'both', exogen=dummies)
  AIC_Matrix[k,1]=AIC(var)
}

#choose the best model by IC

AIC_best = which(AIC_Matrix==min(AIC_Matrix), arr.ind = TRUE)
plot(AIC_Matrix)

#Fixa o modelo de acordo com IC
var = VAR(dataset, p = AIC_best[,1], type = 'both', exogen = dummies)

#deu 24 lags pois nao estamos usando series estacionarias
#Let's check stationarity of the series and correct it

#CDI

plot(CDI)
adf.test(CDI)

plot(diff(log(CDI)))
CDI_S = diff(log(CDI))
adf.test(s)

#COMMODITIES

plot(commodities)
adf.test(commodities)
plot(diff(commodities))
adf.test(diff(commodities))
COMM_S = diff(commodities)

#IBOVESPA

plot(ibovespa)
adf.test(ibovespa)
plot(diff(ibovespa))
adf.test(diff(ibovespa))
IBOV_S = diff(ibovespa)

#Ind

plot(Ind)
adf.test(Ind)
plot(diff(Ind))
adf.test(diff(Ind))
IND_S = diff(Ind)

#IPCA

plot(IPCA)
adf.test(IPCA)
plot(diff(IPCA))
adf.test(diff(IPCA))
IPCA_S = diff(IPCA)

#Do it all over again for stationary series

dummies_s = ts(seasonaldummy(IND_S), start = start(Ind), freq=12)
dataset_s = ts.intersect(IND_S, IPCA_S, IBOV_S, CDI_S, COMM_S)
colnames(dataset_s) = c('Indústria', 'IPCA', 'Ibovespa', 'Juros', 'Commodities')
var = VAR(dataset_s, p=1, type = 'both', exogen = dummies_s)

summary(dataset_s)
var_s = VAR(dataset_s, p=1, type = 'both', exogen = dummies)
summary(var_s)

max_lag_s = 24

AIC_Matrix_s = matrix(0, max_lag_s, 1)

for(k in 1:max_lag_s){
  var_s = VAR(dataset_s, p=k, type = 'both', exogen=dummies_s)
  AIC_Matrix_s[k,1]=AIC(var_s)
}

plot(AIC_Matrix_s)
AIC_best_S = which(AIC_Matrix_s==min(AIC_Matrix_s), arr.ind = TRUE)

##########################################################################

#CROSS VALIDATION


#example of prediction
#Industria, h=1
dataset = ts.intersect(Ind, IPCA, ibovespa, CDI, commodities)
colnames(dataset) = c('Indústria', 'IPCA', 'Ibovespa', 'Juros', 'Commodities')
fit <- VAR(dataset, p=1, type = 'both')
pred = predict(fit, n.ahead = 1)
pred$fcst$Indústria[1]

#função de predição VAR
varPredict <- function(dataset,lags, n_ahead){
  fit <- VAR(dataset, p=lags, type='both')
  pred = predict(fit, n.ahead = n_ahead)

  pred_matrix = matrix(0, 1, 5)
  
  pred_matrix[1] = pred$fcst$Indústria[1]
  pred_matrix[2] = pred$fcst$IPCA[1]
  pred_matrix[3] = pred$fcst$Ibovespa[1]
  pred_matrix[4] = pred$fcst$Juros[1]
  pred_matrix[5] = pred$fcst$Commodities[1]
  
  colnames(pred_matrix) <- c('Indústria', 'IPCA', 'Ibovespa', 'Juros', 'Commodities')
  
  return(pred_matrix)
}


#cross valid. time

#lag_max: até que ponto voltar para fazer a validacao cruzada
#ex: se há 100 obs., so volto ate a obs 60 se lagmax=40

#ahead_max: horizonte de previsao maximo

#model_lag_max: quantos lags o VAR incorpora

crossValid <- function(dataset, lag_max, ahead_max, model_lag_max){
  
  error_matrix = array(0, dim=c(5, model_lag_max, ahead_max))
  
  #dimension1 = endogenous variable
  #dimension2 = lags in VAR model
  #dimension3 = horizonte de previsao da validação cruzada

  for(i in 1:model_lag_max){
    for(j in 1:ahead_max){
      for(k in 1:lag_max){
        
        sum_error2ind = 0
        
        data <- dataset[1:(188-k),]
        P <- varPredict(dataset, i, j) 
          
          if((188-k+j) <= 188){
        
          true_value = dataset[(188-k+j),]
          
          error2ind = (true_value[1]-P[1])^2
          error2ipca = (true_value[2]-P[2])^2
          error2ibov = (true_value[3]-P[3])^2
          error2juros = (true_value[4]-P[4])^2
          error2comm = (true_value[5]-P[5])^2
          
          error_matrix[1,i,j] = error_matrix[1,i,j] + error2ind
          error_matrix[2,i,j] = error_matrix[2,i,j] + error2ipca
          error_matrix[3,i,j] = error_matrix[3,i,j] + error2ibov
          error_matrix[4,i,j] = error_matrix[4,i,j] + error2juros
          error_matrix[5,i,j] = error_matrix[5,i,j] + error2comm
        }
      }
    }
  }
  
  return(error_matrix)
  
}


error = crossValid(dataset, 20, 5, 5)

#find best for industry
best1 = which(error == min(error[1,,]), arr.ind = TRUE)
best1

#matrix to store best parameters

best = matrix(0,2,5)

for(i in 1:2){
  for(j in 1:5){
    
    best[i,j]= which(error == min(error[j,,]), arr.ind = TRUE)[1,(i+1)]
    
  }
}

best


