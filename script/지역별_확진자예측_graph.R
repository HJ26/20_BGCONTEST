library(dplyr)
library(forecast)
rmse <- function(x,y){sqrt(mean((x-y)^2))}


train <- TP_province[1:2757,]
test <- TP_province[2758:2771,]

for( i in unique(train$province)){
  TP_province<-train
  ts_prac <- TP_province%>%
    subset(province==i)%>%
    select(day_confirmed_all)%>%
    as.data.frame()

  ts_prac <- rbind(t,ts_prac)
  ts_prac <- ts_prac%>%
    select(day_confirmed_all)%>%
    ts(start=c(2020,1),frequency = 365.25)

  arima_1.3 <- Arima(ts_prac,order = c(3,0,0))
  for_1.3 <- forecast(arima_1.3,h=14)
  
  for_1.3$mean <- ifelse(for_1.3$mean<0,0,for_1.3$mean)
  
  png(paste0('./pic/region_confirmed/',i,'.png'))
  autoplot(for_1.3)+autolayer(for_1.3$fitted,series = 'fitted')
  dev.off()
  

}









