library(ggplot2)
library(ggfortify)
library(zoo)
library(fpp)
library(xts)

#carregar e renomear os dados do PIB
data = read.csv2("PIB.csv", header = T)
PIB = ts(data$PIB, start = c(1996,1), frequency = 4)

#remove o dataframe que nao interessa
rm(data)

#roda o HP
filtrohp <- function(y, lambda){
  id <- diag(length(y))
  d <- diff(id, d=2)
  tendhp <- solve(id+lambda*crossprod(d),y)
  as.ts(tendhp)
}

#filtro HP
PIB1 = filtrohp(PIB, 1600)
PIB1 = ts(PIB1, start=c(1996,1),frequency=4)
autoplot(PIB1) + autolayer(PIB)

#media movel
mapib <- moving_average(PIB,3)
mapib <- ts(mapib, start=c(1996,1), frequency = 4)
autoplot(PIB)+autolayer(mapib)

#============
#Seleção de Modelo
PIB_janela <- window(PIB, end=2014)
fit <- auto.arima(PIB_janela)

#estrutura do fit
str(fit)
summary(fit)

#residuos
res <- ts(fit$residuals, start = start(PIB_janela), frequency = 
            frequency(PIB_janela))
autoplot(res)

#residuos 2 -> ver se há heteroscedasticidade aparente
res2 <- ts(fit$residuals**2,start = start(PIB_janela), frequency =
             frequency(PIB_janela))
autoplot(res2)

#ACF e PACF -> indicativo de que não há estrutura de correlação
acf(res)
pacf(res)

#teste de ljung-box
Box.test(res, lag=2, type="Ljung-Box")

#ver os residuos quadrados
acf(res2)
pacf(res2)

#histograma
hist(res)

#teste de jarque bera
jarque.bera.test(res)

#checks
checkresiduals(fit)

#================
#Previsão

set.seed(23082017)

#n observações
n=12*10

#modelo
modelo = list(ar = c(0.5), ma=c(-0.3,0.7)) #ARMA(1,2)

#constante
c=5

#media incondicional
mu = c/(1-sum(modelo$ar))

#cria serie y
y = mu + arima.sim(modelo, n, sd = 0.25)
y = ts(y, start=c(2000,1),frequency=12)

#plot da serie
autoplot(y)

#serie de treino
treino <- window(y, end=c(2008,12))

#cria serie de teste
teste <- window(y, start=2009)

#forecast naive
fc <- naive(treino, h=12)

#plot
autoplot(fc) + autolayer(teste)

#cross validation
#horizonte
hf=3
#ordem
K=2

#funcao quadrados dos residuos
sq <- function(u){u^2}

#criar array
C = array(dim = c(K+1,K+1, hf))

for (i in 0:K){
  for (j in 0:K){
    fit <- function(x,h){
      forecast(arima(x, order=c(i,0,j)), h=)}
    
    for (h in 1:hf){
      out <- tsCV(treino, forecastfunction = fit, h=h)
      outsq = sq(out)
      meansqmean <- mean(outsq, na.rm=T)
      C[i+1, j+1,h] <- meansqmean
    }
  }
}

#verificar so o horizonte h=1
View(C[,,1])
