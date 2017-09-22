#enter data
name1=c(......)

#I will use preloaded data from R 
library(datasets)
speed=cars$speed


#count home many pairs
n=length(speed)
#speed2=speed*speed

#fit the data
hist(speed)
datamean=mean(speed)

#plot and print
print(datamean)

#run a bootastrap
nrepl=100000
alpha=.05

meanboot=numeric(nrepl)

for(i in 1:nrepl){
  #create a bootstrap sample
  bootid=sample(n,n,replace=TRUE)
  meanboot[i]=mean(speed[bootid])
}


#quartz()
hist(meanboot)

meanboot=sort(meanboot)
print(meanboot[round(nrepl*c(alpha/2,.5,1-alpha/2))])



