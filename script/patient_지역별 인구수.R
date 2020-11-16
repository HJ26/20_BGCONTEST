city <- read.csv("./data/city.csv")
city2 <- read.csv("./data/city2.csv")

city <- rbind(city,city2)
city$X <- NULL

#인구수 수치형으로 바꿔주기
city$total<-as.numeric(gsub(",","",city$total))
city$female<-as.numeric(gsub(",","",city$female))
city$male<-as.numeric(gsub(",","",city$male))

#save.image("./RData/도시별_인구수.RData")
