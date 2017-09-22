nrepl=100000

alpha=.05

Kfish=20
nfish=30
kfish=5

MinFish=50
MaxFish=400
nmodels=350

Nfish=round(MinFish+(MaxFish-MinFish)*(0:nmodels)/(nmodels))

pvalues=numeric(nmodels+1)

for (ki in 1:(nmodels+1)){
  set.seed(1231)
  
  ksamplefish=numeric(nrepl)
  
  for (i in 1:nrepl){
    balls=sample(Nfish[ki],nfish)
    ksamplefish[i]=sum(balls<=Kfish)
  }
  #quartz()
  #hist(ksamplefish,main=paste("True number of fish = ",Nfish[ki]))
  
  pvalues[ki]=mean(ksamplefish<=kfish)
  
}

quartz()
plot(Nfish,pvalues)
lines(Nfish,rep(.025,nmodels+1))
lines(Nfish,rep(.975,nmodels+1))
      

Nfish[which(pvalues<=1-alpha/2 & pvalues >= alpha/2)]
