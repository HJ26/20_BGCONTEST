library(xts)
library(TSstudio)
library(dplyr)
library(forecast)
library(urca)


#2020-01-19까지 임의 데이터 생성하여 TP_day수정
t<-data.frame(seq(as.Date("2020-01-01"),as.Date("2020-01-19"), by = "day"),seq(0,0,1))
names(t)<-c('date','day_confirmed_all')
t$date <- as.Date(t$date)

(TP_province<-rslt_TP %>% group_by(date,province) %>% summarise(day_confirmed_all=sum(day_confirmed)))

ts_prac <- TP_province%>%
  subset(province=='Busan')%>%
  select(day_confirmed_all)%>%
  as.data.frame()

ts_prac <- rbind(t,ts_prac)
ts_prac <- ts_prac%>%
  select(day_confirmed_all)%>%
  ts(start=c(2020,1),frequency = 365.25)

ggtsdisplay(ts_prac)
arima_1.2 <- Arima(ts_prac,order = c(3,0,0))
checkresiduals(arima_1.2)
Box.test(arima_1.2$residuals,lag=10,type='Ljung-Box')

for_1.2 <- forecast(arima_1.2,h=5)
autoplot(for_1.2)+autolayer(for_1.2$fitted,series = 'fitted')

