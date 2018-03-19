library("BETS")
library("dynlm")
library("fpp")
library("zoo")
library("xts")
library("ggfortify")
library("ggplot2")
library("xts")
library("urca")
library("sandwich")
library("lmtest")

##### BAIXANDO E LIMPANDO OS DADOS ######

exp = read.table("exp", sep =";")

des <- read.csv("desemprego.csv", sep=";", dec = ",")
ipca <- read.csv("IPCA.csv", sep=";", dec=".")

#ajustar tamanho das series

des1 <- read.csv("desemprego.csv", sep=";", dec = ",")
des_date = as.matrix(des1[,1])
data = as.matrix(des1[,2])
des_ts = xts(data, as.Date(des_date, "%d-%m-%Y"))

#colocar formato time series
des_date <- as.matrix(des[c(122:nrow(des)),1])

des_ts <- xts(des)

##### MONTAR MATRIZ DE LAGS #####

lags.ipca <- matrix(0, nrow(ipca)-12, ncol = 12)

for(i in 1:12){
  lags.ipca[,i] <- ipca[(13-i):(nrow(ipca)-i)]
}

lags.desemprego <- matrix(, nrow(des)-12, ncol=12)

for(i in 1:12){
  lags.desemprego[,i] <- des[(13-i):(nrow(des)-i)]
}

#cada coluna seria um lag, ou seja, para a data 1= linha 1, o lag 1 seria
#a coluna 1, o lag 2 é a coluna 2, e assim por diante

##### BACKWARD LOOKING REGRESSION #####
##### INFLAÇÃO CORRENTE ~ DESEMPREGO, LAG INFL., LAG DES. #####


##### MONTAR VARIAVEIS CORRENTES #####

ipca.corrente <- as.matrix(ipca[13:(nrow(ipca)), 1])
desemprego.corrente <- as.matrix(des[c(13:nrow(des)),1])

matinf <- matrix(0, 12, 12)

for(i in 1:12){
  for(j in 1:12){
    reg <- dynlm(ipca.corrente ~ desemprego.corrente + 
                   lags.ipca[,c(1:i)] + lags.desemprego[,c(1:j)])
    matinf[i,j] <- BIC(reg)
  }
}

ordem <- which(mating == min(matinf))
ordem.ipca <- ordem %% 12
ordem.desemprego <- ceiling(ordem/12)

if(ordem%%12 ==0){
  ordem.desemprego=12
}

##### regressão #####

regf <- lm(ipca.corrente ~ desemprego.corrente + lags.ipca[, c(1:ordem.ipca)])+
  lags.desemprego[,c(1:ordem.desemprego)]

summary(regf)

#testar se ainda há correlação nos residuos pelo teste breusch godfrey
#ordem 4 pq eh a ordem do newey west, q corrige a ordem de correlacao
#dos residuos ate 4

bgtest(ipca.corrente ~ desemprego.corrente + lags.ipca[, c(1:ordem.ipca)]+
  lags.desemprego[,c(1:ordem.desemprego)], order= 4)

##### DICKEY FULLER #####

#Pode ter raiz unitaria
#desemprego esta sempre numa banda => n pode ter variancia explosiva por
#consequencia da definicao de raiz unitaria

dickey <- ur.df(ipca, lags = ordem.ipca, type = "trend")
summary(dickey)

##### forward looking #####


#ajustar o tamanho das series novamente

ipca.corrente2 <- as.matrix(ipca.corrente[85:nrow(ipca.corrente)])
desemprego.corrente2 <- as.matrix(desemprego.corrente[85: nrow(ipca.corrente)])

############
exp <- exp[c(13:(nrow(exp)-10)),] #pq -10?
exp[,1] <- NULL #A primeira coluna só continha datas

#retirar os 12 primeiros lags
ipca.corrente3 <- (ipca.corrente2[13:length(ipca.corrente2)])
ipca.corrente3 <- as.matrix(ipca.corrente3)

desemprego.corrente3 <- desemprego.corrente2[13:length(desemprego.corrente2)]
desemprego.corrente3 <- as.matrix(desemprego.corrente3)

#matriz de lags

lags.ipca2 = matrix(0, length(ipca.corrente3), ncol = 12)

for(i in 1:12){
  lags.ipca2[,i] <- ipca.corrente2[(13-i):(length(ipca.corrente2)-i)]
}

lags.ipca2 = as.matrix(lags.ipca2)

lags.desemprego2 <- matrix(0, length(desemprego.corrente3), ncol=12)

for(i in 1:12){
  lags.desemprego2[,i] <- desemprego.corrente2[(13-i):(length(desemprego.corrente2)-i)]
}

lags.desemprego2 <- as.matrix(lags.desemprego2)


##### computar criterios de informação #####

bics <- array(1, dim = c(12,12,12))

for(i in 1:12){
  for(j in 1:12){
    for(k in 1:12){
      reg <- lm(ipca.corrente3 ~ desemprego.corrente3 + lags.ipca2[,(1:i)]+
                  lags.desemprego2[,(1:j)] + exp[,(1:k)])
      bics[i,j,k]=BIC(reg)
    }
  }
}

best.fit <- which(best.fit == min(best.fit), arr.ind = TRUE) -1


##### regressao final #####

reg.forward=lm(ipca.corrente3 ~ desemprego.corrente3 + lags.ipca2[,ipca.ordem2]+
                 lags.desemprego2[,desemprego.ordem2]+
                 exp[,exp.ordem])

bgtest(ipca.corrente3 ~ desemprego.corrente3 + lags.ipca2[,ipca.ordem2]+
         lags.desemprego2[,desemprego.ordem2]+
         exp[,exp.ordem], order = 4)

##### OLS ESTÁTICO #####

modelo = lm((ipca.corrente) ~ as.ts(desemprego.corrente))
modelo$residuals = coeftest(modelo$residuals, vcov = NeweyWest(modelo$residuals, lag =4))
