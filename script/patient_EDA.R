library(dplyr)
library(ggplot2)


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

rslt_rate%>%
  subset(city=='Dalseong-gun')
#save.image('./RData/지역별비율_그래프까지그림.RData')





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

#save.image('./RData/지역별_요일비율.RData')


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
# 그림크기 : 8,56*7.36

#save.image('./RData/지역별_성별+요일_비율_결과만.RData')
