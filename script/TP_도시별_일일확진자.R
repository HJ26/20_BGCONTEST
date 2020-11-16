library(dplyr)

TP <- read.csv("./data/TimeProvince.csv")

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


#save.image("./RData/TP_도시별_일일확진자.RData")