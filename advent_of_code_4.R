setwd("Advent_code/4/")
library(dplyr)
data_original<-read.csv("ac_4_1", header = FALSE)

data <-tidyr::separate(data_original, col = V1, into =  c("1","2"), sep = "-") 
data <-tidyr::separate(data, col = V2, into =  c("3","4"), sep = "-")


#data2 <- mutate_all(data, function(x) as.numeric(as.character(x)))

for (i in 1:nrow(data)){
  data$first[i]<-toString(c(data$`1`[i]: data$`2`[i]))
  data$second[i]<-toString(c(data$`3`[i]: data$`4`[i]))
  data$overlap_2en1[i]<-all(c(data$`3`[i]: data$`4`[i]) %in% c(data$`1`[i]: data$`2`[i]))
  data$overlap_1en2[i]<-all(c(data$`1`[i]: data$`2`[i]) %in% c(data$`3`[i]: data$`4`[i]))
}
for (i in 1:nrow(data)){
  if (data$overlap_2en1[i]==TRUE | data$overlap_1en2[i]==TRUE){
    data$results[i]<- print ("TRUE")
  }
  else{
    data$results[i]<- print ("FALSE")
    }
    }
data_final<-cbind(data, data_original)

table(data$results)


###Second part

data_original<-read.csv("ac_4_1", header = FALSE)

data <-tidyr::separate(data_original, col = V1, into =  c("1","2"), sep = "-") 
data <-tidyr::separate(data, col = V2, into =  c("3","4"), sep = "-")


#data2 <- mutate_all(data, function(x) as.numeric(as.character(x)))

for (i in 1:nrow(data)){
  data$first[i]<-toString(c(data$`1`[i]: data$`2`[i]))
  data$second[i]<-toString(c(data$`3`[i]: data$`4`[i]))
  data$overlap_2en1[i]<-c(data$`3`[i]: data$`4`[i]) %in% c(data$`1`[i]: data$`2`[i])
  data$overlap_1en2[i]<-c(data$`1`[i]: data$`2`[i]) %in% c(data$`3`[i]: data$`4`[i])
}
for (i in 1:nrow(data)){
  if (data$overlap_2en1[i]==TRUE | data$overlap_1en2[i]==TRUE){
    data$results[i]<- print ("TRUE")
  }
  else{
    data$results[i]<- print ("FALSE")
  }
}
data_final<-cbind(data, data_original)

table(data$results)
