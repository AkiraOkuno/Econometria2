install.packages("fArma")
library(fArma)

data <- read.table(file = "serie.txt", header=T)

# serie do masini
dados <- as.ts(data$value)

acf(as.ts(res))
pacf(as.ts(res))

resultado <- arima(dados, order=c(1,0,2))

aic_resultado <- AIC(dados)
bic_resultado <- BIC(dados)
#hq_resultado <- HQ(dados)

#criar uma vetor que contém os criterios de informação AIC para cada par
#(i,j) de ordens AR,MA

info <- c()
for (i in 0:8){
  for (j in 0:8){
    mod <- arima(dados,  order=c(i,0,j))
    aic <-AIC(mod)
    info = c(info, aic)
  }
}


#transformar esse vetor em uma matriz cujas linhas são as ordens AR
#e as colunas são as ordens MA
#Obs: as ordens começam em 0

info_matrix <- matrix(info, nrow = 9, ncol = 9, byrow = TRUE)
info_matrix

#encontrar o menor valor do criterio AIC e encontrar os respectivos indices

m <- which(info_matrix == min(info_matrix), arr.ind = TRUE)
m

ar = m[1,1]-1
ma = m[1,2]-1

ar #=2
ma #=1

#criar uma vetor que contém os criterios de informação BIC para cada par
#(i,j) de ordens AR,MA

info2 <- c()
for (i in 0:8){
  for (j in 0:8){
    mod2 <- arima(dados,  order=c(i,0,j))
    bic <-BIC(mod)
    info2 = c(info2, bic)
  }
}

#transformar esse vetor em uma matriz cujas linhas são as ordens AR
#e as colunas são as ordens MA
#Obs: as ordens começam em 0

info_matrix2 <- matrix(info2, nrow = 9, ncol = 9, byrow = TRUE)
info_matrix2

#encontrar o menor valor do criterio AIC e encontrar os respectivos indices

m2 <- which(info_matrix2 == min(info_matrix2), arr.ind = TRUE)
m2

ar2 = m[1,1]-1
ma2 = m[1,2]-1

ar2
ma2

arma <- armaFit( ~ arma(2,1), dados, method ="ML")
arma
res = residuals(arma)
jarqueberaTest(res)

acf(as.ts(res))
pacf(as.ts(res))

resultado <- arima(dados, order=c(1,0,2))

#TUTORIAL 04 COMEÇA AQUI

p=2
q=1
#verificar se residuos tem media 0
mean(res)

#fazer residuos centrados
res_c <- res - mean(res)
mean(res_c)

#selecionar T+m elementos
T = length(res)
m = 20

all = c()

k=0
while (k<100){
  
  list = c()
  allk = c()
  
  i=0
  while(i<T+m){
    s = sample(res_c,1)
    list = c(list,s)
    i = i+1
  }
  allk = list
  all = c(all, allk)
  k =k+1
}

matriz = matrix(all, ncol = T+m, byrow = T)

  #Obter Yt*
  p = 1
  q = 2
  ar_coef = c(0.7525,-0.5545)
  ma_coef = c(0.7970)
  intercept = 3.5304
  
  n = 100
  h=3
  vector = c()

  for (k in 1:100){
  
  t=1
  Yt = c()
  
  while (t <= max(p,q)){
    yt = dados[t]
    Yt = c(Yt, yt)
    t = t+1
  }
  
  i=max(p,q)+1
  while (i<T+m+1){
    
    arsum = 0
    for (j in 1:p){
      ar = ar_coef[j]*Yt[h-j]
      arsum = arsum + ar
    }
    
    masum=0
    for (j in 1:q){
      ma = ma_coef[j]*matriz[k,][i-j]
      masum = masum + ma
    }
    
    y = arsum + masum + intercept + matriz[k,][i]
    Yt = c(Yt, y)
    
    i = i+1
  }
  
  #jogar os m primeiros elementos fora
  Yt = Yt[(m+1):length(Yt)]
  
  #fazer previsao para um horizonte h usando Yt*
  h=3
  
  yt1 = intercept + ar_coef[1]*Yt[length(Yt)] + ar_coef[2]*Yt[length(Yt)-1]+ma_coef*res[length(res)]
  
  Yt_predict = Yt
  res_predict = res
  bootstrap = c()
  
  
  for (i in 1:h){
    arsum = 0
    for (j in 1:length(ar_coef)){
      ar = ar_coef[j]*Yt_predict[length(Yt)+i-j]
      arsum = arsum + ar
    }
    masum=0
    for (j in 1:length(ma_coef)){
      ma = ma_coef[j]*res_predict[length(res)+i-j]
      masum = masum + ma
    }
    yt_hat = arsum + masum + intercept
    #nao sei se res_hat esta certo
    res_hat = yt_hat - intercept - ma_coef[1]*res_predict[length(res)+1-(i)] - ar_coef[1]*Yt_predict[120+1-(i)]+ar_coef[2]*Yt_predict[120-(i)]
    
    bootstrap=c(bootstrap,yt_hat)
    Yt_predict = c(Yt_predict, yt_hat)
    res_predict = c(res_predict, res_hat)
  }
  vector = c(vector,bootstrap)
}

matrix( vector , nrow = n, ncol = h, byrow = T)
