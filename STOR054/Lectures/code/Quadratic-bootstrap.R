#enter data
name1=c(......)
name2=c(......)

#I will use preloaded data from R 
library(datasets)
speed=cars$speed
distance=cars$dist


#count home many pairs
n=length(speed)
speed2=speed*speed

#fit the data
fitline=lm(distance~speed+speed2)

quadratic=fitline$coefficients[1]+speed*fitline$coefficients[2]+speed2*fitline$coefficients[3]

plot(speed,distance)
lines(speed,quadratic)
#plot and print
print(fitline)

#run a bootastrap
nrepl=10000
alpha=.05

a1boot=numeric(nrepl)
a2boot=numeric(nrepl)
bboot=numeric(nrepl)

for(i in 1:nrepl){
  #create a bootstrap sample
  bootid=sample(n,n,replace=TRUE)
  bootline=lm(distance[bootid]~speed[bootid]+speed2[bootid])
  a1boot[i]=bootline$coefficient[[2]]
  a2boot[i]=bootline$coefficient[[3]]
  bboot[i]=bootline$coefficient[[1]]
}

windows()
hist(a1boot)

windows()
hist(a2boot)

windows()
hist(bboot)

sa1boot=sort(a1boot)
sa2boot=sort(a2boot)
sbboot=sort(bboot)
print(sa1boot[round(nrepl*c(alpha/2,.5,1-alpha/2))])
print(sa2boot[round(nrepl*c(alpha/2,.5,1-alpha/2))])
print(sbboot[round(nrepl*c(alpha/2,.5,1-alpha/2))])






