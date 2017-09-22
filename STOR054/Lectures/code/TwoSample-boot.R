nrepl=100000

data1=c(1,4,3,2,5,3,2,4)
data2=c(4,5,3,4,5,6,7,4,8)

#data1=rnorm(35) #fake data - example of no difference
#data2=rnorm(20)

statistics=mean(data1)-mean(data2) #other statistics possible
n1=length(data1)
n2=length(data2)

statisticsboot=numeric(nrepl)
for (i in 1:nrepl){
  pooldata=sample(c(data1,data2),n1+n2)
  data1boot=pooldata[1:n1]
  data2boot=pooldata[(n1+1):(n1+n2)]
  
  statisticsboot[i]=mean(data1boot)-mean(data2boot) #same as above
}


hist(statisticsboot)
points(statistics,0)

print(mean(statisticsboot<statistics))