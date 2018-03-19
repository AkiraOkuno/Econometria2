to.POSIXct <- function(year, monthNumber){
  ## Function to create a POSIXct time series 
  ## object from a year.month format
  
  ## Create a character vector from the numeric input
  dateStr <- paste(as.character(year), "-",
                   as.character(monthNumber), "-",
                   "01", sep = "")
  ## Return the POSIXct time series object
  as.POSIXct( strptime(dateStr, "%Y-%m-%d"))
}

order.month <- function(x){
  ## Function to make Month column an ordered factor.
  x <- substr(x, 1, 3) ## Use just the first three letters
  factor(x, 
         levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
         ordered = TRUE)
}

read.dairy <- function(path='.',filename='cadairydata.csv'){
  filePath <- file.path(path, filename)
  df <- read.csv(filePath, header = T, stringsAsFactors=F)
  df$dateTime <- to.POSIXct(df$Year, df$Month.Number)
  df$Month <- order.month(df$Month)
  df <- df[,-1:-3]
}

dairy <- read.dairy()

head(dairy)
library(repr)
options(repr.plot.width=8, repr.plot.height=8)

dairy.plot <- function(df, col = 'Milk.Prod'){
  require(ggplot2)
  ggplot(df, aes_string('dateTime', col)) +
    geom_line() +
    ggtitle(paste('Time series of', col)) +
    geom_smooth() +
    xlab('Time in years') +
    ylab('Milk Production')
}
dairy.plot(dairy)

dairy.acf <- function(df, col = 'remainder'){
  temp <- df[, col]
  temp = ts(temp, start = 1995, frequency = 12)
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
}
dairy.acf(dairy, col = 'Milk.Prod')

hist.ts = function(df, col = 'Milk.Prod', bins = 40){
  temp = df[,col]
  hist(temp, breaks = 30, col='gray', main = paste('Distribution of ', col), xlab = col)
}
hist.ts(dairy)

dairy.ma <- function(df, col = 'Milk.Prod', order = 12){
  temp = df[, col]
  end = length(temp) - 1
  out = rep(0, length(temp))
  out[1] = temp[1]
  for(i in 2:end){
    if(i - order <= 1) j = 1 
    else j = j + 1
    out[i + 1] = sum(temp[j:i])/(i - j + 1)
  }
  out
}

dairy.seasons <- function(df, col = 'Milk.Prod'){
  y = df[, col]
  fit = lm(y ~ 0 + Month, data = df)
  predict(fit, newdata = df)
}

decomp.dairy <- function(df,  col = 'Milk.Prod', multiplicative = TRUE, order = 12){
  if(multiplicative) {
    temp = log(df[, col])
    df[, col] = temp
  } else { 
    temp = df[, col] 
  }
  trend = dairy.ma(df, col = col, order = order)
  temp = temp - trend
  df[, col] = temp
  seasonal = dairy.seasons(df, col = col)
  remainder = temp - seasonal
  data.frame(trend = trend, seasonal = seasonal, remainder = remainder)
}
decomp <- decomp.dairy(dairy, order = 12)

head(decomp)

decomp.plot <- function(df){
  require(ggplot2)
  install.packages("gridExtra")
  require(gridExtra)
  df$x = 1:nrow(df)
  ycols = c('trend', 'seasonal', 'remainder')
  p <- lapply(ycols, function(y){
    ggplot(df, aes_string('x', y)) + 
      geom_line() +
      ylab(y)
  })
  grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 3)
}
decomp.plot(decomp)

dairy.acf(decomp)

dairy.decomp <- function(df, col = 'Milk.diff', span = 0.5, Mult = TRUE){
  if(Mult) {temp <- ts(log(df[, col]), frequency=12, start=1)
  } else {temp <- ts(df[, col], frequency=24, start=1)}
  span = span * length(temp)  
  dairyFit <- stl(temp, s.window = "periodic", t.window = span)
  plot(dairyFit, main = 'Decompositon of dairy produciton')
  cbind(df, as.data.frame(dairyFit$time.series))
}
dairyMult = dairy.decomp(dairy, col = 'Milk.Prod', span = 0.2)

dairy.acf(dairyMult)
hist.ts(dairyMult, col = 'remainder')

dairy.box <- function(df, col = 'remainder'){
  require(ggplot2)
  p <- ggplot(df, aes_string('Month', col)) +
    geom_boxplot() +
    ggtitle('Variation of remainder component of dairy production by month')
  print(p)
}
dairy.box(dairyMult)
