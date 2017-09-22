#enter data
name1=c(......)
name2=c(......)

#I will use preloaded data from R 
library(datasets)
speed=cars$speed
distance=cars$dist


#count home many pairs
n=length(speed)
#speed2=speed*speed

#fit the data
fitline=lm(distance~speed)

#plot and print
plot(speed,distance)
abline(fitline)
print(fitline)

#run a bootastrap
nrepl=10000
alpha=.05

aboot=numeric(nrepl)
bboot=numeric(nrepl)

for(i in 1:nrepl){
  #create a bootstrap sample
  bootid=sample(n,n,replace=TRUE)
  bootline=lm(distance[bootid]~speed[bootid])
  aboot[i]=bootline$coefficient[[2]]
  bboot[i]=bootline$coefficient[[1]]
}

#windows()
hist(aboot)

#windows()
hist(bboot)

saboot=sort(aboot)
sbboot=sort(bboot)
print(saboot[round(nrepl*c(alpha/2,.5,1-alpha/2))])
print(sbboot[round(nrepl*c(alpha/2,.5,1-alpha/2))])






