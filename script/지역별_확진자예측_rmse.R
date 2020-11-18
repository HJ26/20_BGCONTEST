rmse <- function(x,y){sqrt(mean((x-y)^2))}


train <- TP_province[1:2757,]
test <- TP_province[2758:2771,]


rslt_rmse_3 <- data.frame()
rslt_rmse_5_2 <- data.frame()


for( i in unique(rslt_TP$province)){
  TP_province<-train
  ts_prac <- TP_province%>%
    subset(province==i)%>%
    select(day_confirmed_all)%>%
    as.data.frame()

  ts_prac <- rbind(t,ts_prac)
  ts_prac <- ts_prac%>%
    select(day_confirmed_all)%>%
    ts(start=c(2020,1),frequency = 365.25)

  arima_1.3 <- Arima(ts_prac,order = c(3,0,2))
  for_1.3 <- forecast(arima_1.3,h=14)
  
  arima_1.4 <- Arima(ts_prac,order = c(5,2,0))
  for_1.4 <- forecast(arima_1.4,h=14)
 
  for_v_3 <- ifelse(for_1.3$mean<0,0,for_1.3$mean)
  for_v_4 <- ifelse(for_1.4$mean<0,0,for_1.4$mean)
  
  rmse_3 <- rmse(for_1.3$mean,test$day_confirmed_all)
  rmse_5_2 <- rmse(for_1.4$mean,test$day_confirmed_all)
  
  rmse_3.df <- data.frame(province=i,rmse=rmse_3)
  rmse_5_2.df <- data.frame(province=i,rmse=rmse_5_2)

  rslt_rmse_3 <- rbind(rslt_rmse_3,rmse_3.df)
  rslt_rmse_5_2 <-rbind(rslt_rmse_5_2,rmse_5_2.df)
}










train%>%
  subset()