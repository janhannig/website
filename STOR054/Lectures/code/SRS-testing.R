nrepl=10000

nmodels=100-1
minp=0
maxp=1

alpha=.05

Npeople=225778000
nsample=603
observedyes=314


pyes=minp+(maxp-minp)*(1:nmodels)/(nmodels+1)
Kyes=round(Npeople*pyes)

pvalue=numeric(nmodels)

for (ki in 1:nmodels){
  set.seed(1231)
  nsampleyes=numeric(nrepl)
  
  for (i in 1:nrepl){
    balls=sample(Npeople,nsample)
    nsampleyes[i]=sum(balls<=Kyes[ki])
  }
  #quartz()
  hist(nsampleyes,main=paste("True proportion = ",Kyes[ki]/Npeople))

  pvalue[ki]=mean(nsampleyes<=observedyes)

}

#quartz()
plot(Kyes/Npeople,pvalue)
lines(Kyes/Npeople,rep(.025,nmodels))
lines(Kyes/Npeople,rep(.975,nmodels))

Kyes[which(pvalue<=1-alpha/2 & pvalue >= alpha/2)]/Npeople

