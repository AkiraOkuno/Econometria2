#MONITORIA 03

#SÉRIES

#PIB: Tendência
#IPCA: Sazonalidade
#DESEMPREGO: Filtragem

library(zoo)
library(forecast)
library(ggplot2)
library(ggfortify)
library(mFilter)

#carregar dados do PIB
dados <- read.csv2("PIB.csv", header = TRUE)
PIB = ts(dados$PIB, start=c(1996,1),frequency=4)

#remover o objeto dados
rm(dados)

#OLS no braço
T = length(PIB)
Y <- matrix(PIB,T,1)
tendencia = 1:T
X <- matrix(1,T,2)
X[,2] <- tendencia

#betas e residuos
XXprime <- solve(t(X) %*% X)
beta <- XXprime %*% t(X) %*% Y
residuo_braco <- Y - X%*%beta

plot(residuo_braco)

#OLS do R
reg = lm(formula = PIB ~ tendencia)
res = ts(reg$residuals, start = start(PIB), frequency = frequency(PIB))

summary(reg)
str(reg)

#plot dos residuos
autoplot(res)

# ===
#Parte 2

dados = read.csv2("IPCA.csv", header = T)
IPCA = ts(dados$IPCA, start = c(1995,1), frequency = 12)
IPCA = window(IPCA, start = c(2004,1))

#Plot de sazonalidade
windows()
seasonplot(IPCA)

#Indícios de sazonalidade em junho e julho

#tendencia IPCA
T = length(IPCA)
tendencia = 1:T

#LM do IPCA na tendencia
reg = lm(formula = IPCA ~ tendencia)
res =ts(reg$residuals, start=start(IPCA), frequency = frequency(IPCA))

#sumário da regressão
summary(reg)
#coeficiente nao significativo = sinal de que não há tendência

#regressão com as dummies
dummy = factor(cycle(IPCA))
reg = lm(formula = IPCA ~ dummy) #intercepto = janeiro, efeito = em relacao a janeiro
res = ts(reg$residuals, start = start(IPCA), frequency = frequency(IPCA))
#só os meses 6,7,8,9 são significantes

#sumário da regressão
summary(reg)

#Criar dummies
d6 = 1*(cycle(IPCA)==6)
d7 = 1*(cycle(IPCA)==7)
d8 = 1*(cycle(IPCA)==8)
d9 = 1*(cycle(IPCA)==9)

#regressão das dummies
reg = lm(formula = IPCA ~ d6+d7+d8+d9) 
res = ts(reg$residuals, start = start(IPCA), frequency = frequency(IPCA))

#sumário da regressão
summary(reg)
autoplot(res)

#plot da correlação dos resíduos
lag.plot(res, lags=13)

#acf dos residuos
acf = acf(res)
pacf = pacf(res)

correl_res = cor(res[2:T],res[1:T-1])

#==

#Parte 3

#filtro HP
plot(hpfilter(IPCA,freq = 1600))
plot(hpfilter(res,freq = 1600))

#moving average


moving_average <- function(data, n) {
  div = 1/(2*n+1)
  ma <- c()
  for (i in 1:length(data)){
    if (i <= n){
      ma <- c(ma, data[i])
    }
    else if (length(data)-i+1 <=3){
      ma <- c(ma, data[i])
    }
    else {
      smooth <- sum(data[(i-n):(i+n)])
      ma <- c(ma, smooth*div)
    }
  }
  return(ts(ma))
}

autoplot(a <- moving_average(IPCA, 3))
autoplot(moving_average((moving_average(IPCA, 3)),5))
