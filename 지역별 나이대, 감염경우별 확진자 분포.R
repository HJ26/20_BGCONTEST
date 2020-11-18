#파일 불러오기
setwd('C:/R_corona')
patient<-read.csv('PatientInfo.csv')

##전반적인 데이터 분포 확인
#지역에 따른 나이별 확진자 분포 시각화
library(dplyr)
CITY_AGE<-patient %>% group_by(city,age) %>% summarise(num=n())%>%arrange(city)

install.packages("treemap")
library(treemap)
treemap(CITY_AGE,index=c("city",'age'),vSize = 'num',vColor="age",title = "Distribution of confirmed cases by age group by region")


#지역에 따른 감염경우별 확진자 분포 시각화
library(dplyr)
CITY_infection_case<-patient %>% group_by(city,infection_case) %>% summarise(num=n())%>%arrange(city)

install.packages("treemap")
library(treemap)
treemap(CITY_infection_case,index=c("city","infection_case"),vSize = 'num',vColor="age",title = "Distribution of confirmed cases by region of infection")

#파일 불러오기
setwd('C:/R_corona')
patient<-read.csv('PatientInfo.csv')

#지역별 나이대별 확진 비율
CITY_AGE_df<-data.frame(CITY_AGE)
CITY_AGE_df[1,2]



#1.나이대별
#age 데이터 프레임 만들기이
length(unique(patient$city))
unique_city<-unique(patient$city)
a<-rep(c('0s','10s','20s','30s','40s','50s','60s','70s','80s','90s','100s'),164)
AGE_df<-data.frame(a)
AGE_df<-rename(AGE_df,'age'='a')

#city명 나이대에 맞게 반복되기
c<-c()
b<-c()
for (i in unique_city){
  b<-rep(i,11)
  c<-append(c,b)
  }
 
#age와 city반복 벡터 합쳐 데이터프레임 만들기
AGE_df$city<-c
AGE_df<-AGE_df[,c("city","age")] #열 순서 조정

CITY_AGE<-patient %>% group_by(city,age) %>% summarise(num=n())%>%arrange(city)

CITY_AGE_df<-full_join(AGE_df,CITY_AGE,id=c("city","age")) #데이터 프레임 join.(city와 age가 일치하도록)



#age를 모른다면 unknown 넣기
CITY_AGE_df$age<-as.character(CITY_AGE_df$age)
for (i in 1805:1873){
  CITY_AGE_df[i,2]<-'unknown'}
#city를 모른다면 unknowun 넣기
for (i in 320:330){
  CITY_AGE_df[i,1]<-'unknown'}

#지역에 따른 나이대 확진자 비율
CITY_AGE_df[is.na(CITY_AGE_df)]<-0

city_age_rate <- CITY_AGE_df %>%
  group_by(city) %>%
  mutate(total=sum(num))%>%
  mutate(pct = round(num/sum(num)*100, 2))


library(ggplot2)
##지역별 나이대 확진자 파이차트 자동 생성
for (i in unique(city_age_rate$city)){
  city_age_rate["city"== i]
  city_data<-data.frame(filter(city_age_rate,city==i))
  city_data$place <- 0.5*city_data$pct - cumsum(city_data$pct) +100 #레이블 위치 값 설정
  ggplot(city_data, aes(x = "", y = pct, fill = age)) +
   geom_bar(width = 1, stat = "identity", color = "white") +
   coord_polar("y", start = 0)+
   geom_text(aes(y=place,label = paste0(pct,"%")), color = "black")+
   labs(title = i )
  ggsave(paste( i , c("_age.png"))) 
}
  




#2.감염경우별
#지역별 감염경우
#지역에 따른 감염경우별 확진자 분포 시각화
library(dplyr)
CITY_infection_case<-patient %>% group_by(city,infection_case) %>% summarise(num=n())%>%arrange(city)
unique(patient$infection_case)

#city명 모르면 unknown넣기
CITY_infection_case$city<-as.character(CITY_infection_case$city)
for (i in 1:5){
  CITY_infection_case[i,"city"]<-'unknown'
  }

#infection_case를 모르면 unknown넣기
CITY_infection_case<-data.frame(CITY_infection_case)
CITY_infection_case$infection_case<-as.character(CITY_infection_case$infection_case)
CITY_infection_case[CITY_infection_case$infection_case =='','infection_case'] <- 'unknown'
CITY_infection_case[is.na(CITY_infection_case$infection_case),"infection_case"] <- 'unknown'

#비율값 구하기
city_infect_rate <- CITY_infection_case %>%
  group_by(city) %>%
  mutate(total=sum(num))%>%
  mutate(pct = round(num/sum(num)*100, 2))


#treemap으로 작성
library(treemap)
install.packages('treemapify')
library(treemapify)

for(i in unique(city_infect_rate$city)){
  infect_df<-data.frame(filter(city_infect_rate,city==i))
  ggplot(infect_df,aes(area=num,fill=infection_case,label=infection_case))+
    geom_treemap()+
    geom_treemap_text(
      fontface = "italic",
      colour = "black",
      place = "centre",
      grow = T
    )+
    ggtitle(paste( i , c("'s infection case")))
    ggsave(paste( i , c("_infection_case.png")))







