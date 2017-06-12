
dataRaw<-read.csv("activity.csv", header = T)
dataRaw$date<-as.Date(dataRaw$date, format = "%Y-%m-%d")
for(i in 1:length(dataRaw$steps)){
  if (is.na(dataRaw[i,1])) {
      dataRaw[i,1]<-activity.pattern[(i-1)*5,2] 
     }
}
sum(is.na(dataRaw$steps))


data<-merge(dataRaw,activity.pattern, by="interval")
for(i in 1:length(data$steps)){
  if (is.na(data[i,2])) {
    print(data[i,2])
    data[i,2]<-data[i,4] 
  }
}
