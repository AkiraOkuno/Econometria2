
data <- read.table(file = "serie.txt", header=T)

# serie do masini
dados <- as.ts(data$value)

p=2
q=1
ar_coef = c(0.7525,-0.5545)
ma_coef = c(0.7970)
intercept = 3.5304
media = intercept/(1-ar_coef[1]-ar_coef[2])

#verificar se residuos tem media 0
mean(res)

#fazer residuos centrados
res_c <- res - mean(res)
mean(res_c)

#selecionar T+m elementos
T = length(res)
m = 20
h=3
R=1000
M = matrix(NA, R, h)

for (k in 1:R){
  all = c()
  
  res_s = sample(res_c,T+m, replace =  T)
  
  t=1
  Yt = c(dados, rep(NA,m+h))
  
  res_s = c(res_s, rep(0,h))
  
  for (i in (1+max(p,q)):(T+m+h)){
    Yt[i] = ar_coef[1]*Yt[i-1] + ar_coef[2]*Yt[i-2] + ma_coef[1]*res_s[i-1] + intercept + res_s[i]
  }
  
  M[k,] = t(Yt[(T+m+1):(T+m+h)])
}

