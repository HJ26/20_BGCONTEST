#library()
library(dplyr)
#시계열 분석 필요 라이브러리
library(TTR)
library(fpp2)
library(urca)
library(forecast)
library(ggplot2)

#파일 불러오기
setwd('c:/R_corona/')
TP <- read.csv("TimeProvince.csv")

#누적확진자 수를 일별 확진자 수로 풀어주기
rslt_TP <- data.frame()
for( i in unique(TP$province)){
  TP_city <- TP%>%
    subset(province==i)%>%
    arrange(desc(date))
  
  TP_city$day_confirmed <- 0
  for( i in 1:(length(TP_city$confirmed)-1) ){
    TP_city$day_confirmed[i] <- TP_city$confirmed[i]-TP_city$confirmed[i+1]
  }
  
  rslt_TP <- rbind(rslt_TP,TP_city)
  
}

#날짜변수 변환
rslt_TP$date<-as.Date(rslt_TP$date)

#날짜별로로 합치기
TP_day<-rslt_TP %>% group_by(date) %>% summarise(day_confirmed_all=sum(day_confirmed))

#날짜별,지역별 확진자 수 정렬
TP_province<-rslt_TP %>% group_by(date,province) %>% summarise(day_confirmed_all=sum(day_confirmed))




#시계열 데이터로 만들기
library(xts)
Time_ts<-xts(TP_day$day_confirmed_all, order.by = TP_day$date, "%Y%m%d")
class(Time_ts)

class(Time_ts)
index(Time_ts)
plot(Time_ts)

window(Time_ts, start="2020-2-17",end='2020-4-13')

#2020-01-19까지 임의 데이터 생성하여 TP_day수정
t<-data.frame(seq(as.Date("2020-01-01"),as.Date("2020-01-19"), by = "day"),seq(0,0,1))
names(t)<-c('date','day_confirmed_all')
TP_day<-rbind(t,TP_day)

#시계열
day_ts<-ts(TP_day$day_confirmed_all,c(2020,1),frequency = 365.25)
plot.ts(day_ts,xlab='date')
end(day_ts)
frequency(day_ts)




#데이터 살펴보기
autoplot(day_ts) +
  ggtitle("2020-01-01~2020-06-30 전국 신규 확진자") +
  xlab("Date") +
  ylab("확진자수")


####이벤트1. 신천치 : 2/4~2/21 관측
#데이터 생성 및 잔차 분석, 시계열 정상성 확인
sincheonji<-window(day_ts,start=c(2020,35),end=c(2020,52))
ggtsdisplay(sincheonji)
summary(ur.kpss(sincheonji)) #검정 통계량 : 0.3731 시계열이 정상성을 가짐.

##모델생성
auto.arima(sincheonji) #ARIMA(0,2,0) 제공함

##모델학습 및 예측
sincheonji_120_F<-forecast(Arima(sincheonji,c(1,2,0)),h=7) 
          #잔차 분석과 test data와의 rmse차이를 비교하며 ARIMA(p,d,q) 설정.
autoplot(sincheonji_120_F)+autolayer(sincheonji_120_F$fitted,series = 'fitted')

#적합성진단(잔차분포 확인)
checkresiduals(sincheonji_120_F) #p=0.3501

##잔차 자기상관성 확인
Box.test(sincheonji_120_F$residuals, lag=10, type = "Ljung-Box") 
                        #p-value 0.3063; 잔차의 자기상관성이 없음
##test예측

test1<-window(day_ts,start=c(2020,53),end=c(2020,59))
                    #[1] 229 169 231 144 284 505 571
accuracy(sincheonji_120_F,test1) #RMSE: Training 6.920722, Test 93.617783





####이벤트2. 이태원 : 4/22~5/9 관측
#데이터 생성 및 잔차 분석, 시계열 정상성 확인
iteawon<-window(day_ts,start=c(2020,113),end=c(2020,130))
ggtsdisplay(iteawon)
summary(ur.kpss(iteawon)) #검정 통계량 : 0.3591 시계열이 정상성을 가짐.

#모델 생성
auto.arima(iteawon) #ARIMA(0,0,0)제공

##모델학습 및 예측
iteawon_111_F<-Arima(iteawon,c(1,1,1))
      #잔차 분석과 test data와의 rmse차이를 비교하며 ARIMA(p,d,q) 설정.
iteawon_111_F<-forecast(Arima(iteawon,c(1,1,1)),h=7)

#적합성진단(잔차분포 확인)
checkresiduals(iteawon_111_F) #p-value 0.3438. 잔차 적합함.

##잔차 자기상관성 확인
Box.test(iteawon_111_F$residuals, lag=10, type = "Ljung-Box") 
                    #p-value 0.6781; 잔차의 자기상관성이 없음

#그래프 확인
autoplot(iteawon_111_F)+autolayer(iteawon_111_F$fitted,series = 'fitted')
test2<-window(day_ts,start=c(2020,131),end=c(2020,137))
                              #[1] 28 32 26 25 29 26 11
accuracy(iteawon_111_F,test2) #RMSE: Training 4.330502, Test 9.882367
