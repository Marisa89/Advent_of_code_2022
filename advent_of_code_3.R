setwd("Advent_code/3/")
data<-read.csv("ac_3_1", header = FALSE)
nchar(data[1,])
data2<-data.frame(matrix(ncol = 3, nrow = 300))
#split into two 
for (i in 1:nrow(data)){
data2[i,]<-transform(data[i,], first = substr(data[i,], 1, (nchar(data[i,])/2)), second = substr(data[i,],  (nchar(data[i,])/2)+1,  (nchar(data[i,]))))
}
#find common character
for (i in 1:nrow(data)){
  a<-unlist(strsplit(data2$X2[i], split = ""))
  b<-unlist(strsplit(data2$X3[i], split = ""))
  data2$character[i]<-intersect(a, b)
}
#coun the points
#Lowercase item types a through z have priorities 1 through 26.
#Uppercase item types A through Z have priorities 27 through 52.
points<-as.data.frame(cbind(c(letters, LETTERS), c(1:52)))
colnames(points)<-c("character", "value")

#sum
data3<-merge(data2, points, by="character")
sum(as.numeric(data3$value))


####PART 2
setwd("Advent_code/3/")
data<-read.csv("ac_3_1", header = FALSE)
summary<-data.frame(matrix(ncol = 1, nrow = 100))
for (i in seq(1,nrow(data), by = 3)){
  a<-unlist(strsplit(data$V1[i], split = ""))
  b<-unlist(strsplit(data$V1[i+1], split = ""))
  c<-unlist(strsplit(data$V1[i+2], split = ""))
  data$character[c(i,i+1,i+2)]<-intersect(intersect(a, b),c)
}

data3<-merge(data, points, by="character")
sum(as.numeric(data3$value))/3
