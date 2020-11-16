library(dplyr)
library(ggplot2)
case <- read.csv("./data/case.csv")

# 지역별 누적합 풀기

city <- case$city
city <- unique(city)

length(city)

case2 <- data.frame()

for(i in city){
  rslt <- case%>%subset(city==i)
  rslt <- arrange(rslt,-confirmed)
  
  rslt$cp <- rslt$confirmed
  
  if(nrow(rslt)>1){
    for( j in range(2,nrow(rslt)) ){
      rslt$cp[j-1] <- rslt$confirmed[j-1] - rslt$confirmed[j]
    }
  }
  
  #assign(i,rslt)
  case2 <- rbind(case2,rslt)
}

#save.image('./RData/case_지역별_일일확진자.RData')



# 지역별 성별


ggplot(case2,aes(x=city,y=cp,color=))