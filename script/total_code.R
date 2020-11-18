library(dplyr)
library(ggplot2)

# 데이터 읽어오기
patient <- read.csv('./data/PatientInfo.csv')

# 데이터 형태 확인
summary(patient)

# id factor형으로 변환
patient$patient_id <- as.factor(patient$patient_id)

# 한국에서 발생한 것만 추출
patient_ko <- patient%>%
  filter(country == 'Korea')
summary(patient_ko)


##############################
############################## 분석
##############################


# 지역별 확진자 중 성별 비율
rslt_rate <- data.frame()

for( i in unique(patient_ko$city) ){
  
  df <- patient_ko%>%
    subset(city==i)
  
  df$sex <- as.character(df$sex)
  df$sex <- ifelse(df$sex=="","Unknown",df$sex)
  
  n <- df%>%
    select(sex)%>%
    table()%>%
    as.data.frame()
  
  if( !('female' %in% n$.) ) {n <- rbind(n,data.frame(.='female',Freq=0))}
  if( !('male' %in% n$.) ) {n <- rbind(n,data.frame(.='male',Freq=0))}
  if( !('Unknown' %in% n$.) ) {n <- rbind(n,data.frame(.='Unknown',Freq=0))}
  
  
  nname_n <- paste0('n_',n$.)
  ratename_n <- paste0('rate_',n$.)
  colname_n <- c(nname_n,ratename_n)
  
  n$rate <- n$Freq/sum(n$Freq)
  
  n.df <- data.frame(0)
  n.df$city <- i
  n.df$n_female <- n$Freq[1]
  n.df$n_male <- n$Freq[2]
  n.df$n_unknown <- n$Freq[3]
  
  n.df$rate_female <- n$rate[1]
  n.df$rate_male <- n$rate[2]
  n.df$rate_unknown <- n$rate[3]
  
  n.df$X0 <- NULL
  
  n.df[is.na(n.df)] <- 0
  
  colnames(n.df) <- c('city',colname_n)
  
  rslt_rate <- rbind(rslt_rate,n.df) 
}

# 지역별 그래프 저장
path = "E://공모전/교내/20/20_BGContest/pic/rate_sex/"
for( i in 1:nrow(rslt_rate)){
  df <- rslt_rate[i,c(1,5,6,7)]
  df <- t(df[,-1])
  df <- as.data.frame(df)
  df$class <- rownames(df)
  colnames(df) <- c("rate",'class')
  rownames(df) <- NULL
  df$place <- 0.6*df$rate - cumsum(df$rate) + 1
  df <- df[!(df$rate == 0 ), ]
  
  ggplot(df,aes(x=2,y=rate,fill=class))+
    geom_bar(width=1,stat='identity',color='white')+
    coord_polar(theta = 'y',direction = 1)+
    geom_text(aes(y=place,label=paste0(round(rate*100),"%")),size=6,color='white')+
    xlim(0.5,2.5) +
    ggtitle(rslt_rate[i,]$city)+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(path,rslt_rate[i,]$city,".png"))
  
}



# 지역별 확진자 중 요일 비유

patient_dow <- patient_ko
patient_dow$confirmed_date <- as.Date(patient_dow$confirmed_date)
patient_dow$diff <- as.numeric(as.Date('2020-11-15')-patient_dow$confirmed_date) %% 7
patient_dow$dow <- ifelse(patient_dow$diff == 0, 'Sun',
                          ifelse(patient_dow$diff == 1, 'Mon',
                                 ifelse(patient_dow$diff == 2, 'Tue',
                                        ifelse(patient_dow$diff == 3, 'Wed',
                                               ifelse(patient_dow$diff == 4, 'Thu',
                                                      ifelse(patient_dow$diff == 5, 'Fri','Sat'))))))
patient_dow$dow <- ifelse(is.na(patient_dow$dow),'Unknown',patient_dow$dow)

sum(is.na(patient_dow$dow))

rslt_time <- data.frame()
for( i in unique(patient_ko$city) ){
  df <- patient_dow%>%
    subset(city==i)
  
  n <- df%>%
    select(dow)%>%
    table()%>%
    as.data.frame()
  
  if( !('Sun' %in% n$.) ) {n <- rbind(n,data.frame(.='Sun',Freq=0))}
  if( !('Mon' %in% n$.) ) {n <- rbind(n,data.frame(.='Mon',Freq=0))}
  if( !('Tue' %in% n$.) ) {n <- rbind(n,data.frame(.='Tue',Freq=0))}
  if( !('Wed' %in% n$.) ) {n <- rbind(n,data.frame(.='Wed',Freq=0))}
  if( !('Thu' %in% n$.) ) {n <- rbind(n,data.frame(.='Thu',Freq=0))}
  if( !('Fri' %in% n$.) ) {n <- rbind(n,data.frame(.='Fri',Freq=0))}
  if( !('Sat' %in% n$.) ) {n <- rbind(n,data.frame(.='Sat',Freq=0))}
  if( !('Unknown' %in% n$.) ) {n <- rbind(n,data.frame(.='Unknown',Freq=0))}
  
  
  
  nname_n <- paste0('n_',n$.)
  ratename_n <- paste0('rate_',n$.)
  colname_n <- c(nname_n,ratename_n)
  
  n$rate <- n$Freq/sum(n$Freq)
  
  n.df <- data.frame(0)
  n.df$city <- i
  n.df$n_Mon <- n$Freq[1]
  n.df$n_Tue <- n$Freq[2]
  n.df$n_Wed <- n$Freq[3]
  n.df$n_Thu <- n$Freq[4]
  n.df$n_Fri <- n$Freq[5]
  n.df$n_Sat <- n$Freq[6]
  n.df$n_Sun <- n$Freq[7]
  n.df$n_Unknown <- n$Freq[8]
  
  
  n.df$rate_Mon <- n$rate[1]
  n.df$rate_Tue <- n$rate[2]
  n.df$rate_Wed <- n$rate[3]
  n.df$rate_Thu <- n$rate[4]
  n.df$rate_Fri <- n$rate[5]
  n.df$rate_Sat <- n$rate[6]
  n.df$rate_Sun <- n$rate[7]
  n.df$rate_Unknown <- n$rate[8]
  
  n.df$X0 <- NULL
  
  colnames(n.df) <- c('city',colname_n)
  
  rslt_time <- rbind(rslt_time,n.df) 
}



# 지역별 그래프 저자
path = "E://공모전/교내/20/20_BGContest/pic/rate_time/"
for( i in 1:nrow(rslt_rate)){
  df <- rslt_time[i,c(1,10,11,12,13,14,15,16,17)]
  df <- t(df[,-1])
  df <- as.data.frame(df)
  df$class <- rownames(df)
  colnames(df) <- c("rate",'class')
  rownames(df) <- NULL
  df$place <- 0.6*df$rate - cumsum(df$rate) + 1
  df <- df[!(df$rate == 0 ), ]
  
  if('rate_Unknown' %in% df$class){
    place.Wed <- df$place[7]
    df$place[7] <- df$place[8]+0.06
    df$place[8] <- place.Wed+0.06
  }
  
  ggplot(df,aes(x=2,y=rate,fill=class))+
    geom_bar(width=1,stat='identity',color='white')+
    coord_polar(theta = 'y',direction = 1)+
    geom_text(aes(y=place,label=paste0(round(rate*100),"%")),size=6,color='white')+
    xlim(0.5,2.5) +
    ggtitle(rslt_time[i,]$city)+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0(path,rslt_time[i,]$city,".png"))
  
}




##############################
############################## 예측
##############################


#2020-01-19까지 임의 데이터 생성하여 TP_day수정
t<-data.frame(seq(as.Date("2020-01-01"),as.Date("2020-01-19"), by = "day"),seq(0,0,1))
names(t)<-c('date','day_confirmed_all')
t$date <- as.Date(t$date)

# 지역별 총확진자를 더하여 지역-총확진자 데이터set 생성
(TP_province<-rslt_TP %>% group_by(date,province) %>% summarise(day_confirmed_all=sum(day_confirmed)))

# 서울을 기준으로 예측
# 이를 활용하여 지역별 확진자 수 예측 모델 생성
ts_prac <- TP_province%>%
  subset(province=='Seoul')%>%
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




# train/test 나누기
train <- TP_province[1:2757,]
test <- TP_province[2758:2771,]


# 지역별 확진자 예측 - rmse 확인
rslt_rmse_3 <- data.frame()

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

  for_v_3 <- ifelse(for_1.3$mean<0,0,for_1.3$mean)
  
  rmse_3 <- rmse(for_1.3$mean,test$day_confirmed_all)
  
  rmse_3.df <- data.frame(province=i,rmse=rmse_3)

  rslt_rmse_3 <- rbind(rslt_rmse_3,rmse_3.df)

}





# 지역별 확진자 수 예측-그래프 저
for( i in unique(train$province)){
  TP_province2<-train
  ts_prac <- TP_province2%>%
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
  
  rmse(arima_1.3$fitted,test$day_confirmed_all)
  png(paste0('./pic/region_confirmed/',i,'.png'))
  autoplot(for_1.3)+autolayer(for_1.3$fitted,series = 'fitted')
  dev.off()
  
  
}

