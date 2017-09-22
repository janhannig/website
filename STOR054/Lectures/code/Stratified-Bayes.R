nmodels=1000-1
minp=0
maxp=1

alpha=.05

Npeople=c(42343562,59280986,104447452)
nsample=c(220,284,510)
observedyes=c(201,31,282)

pyes=minp+(maxp-minp)*(1:nmodels)/(nmodels+1)
Kyes=round(Npeople%*%t(pyes))

one=t(rep(1,nmodels))

prior=rep(1,3)%*%t(rep(1/nmodels,nmodels))

posterior=dhyper(observedyes,Kyes,(Npeople%*%one)-Kyes,nsample)*(prior)
posterior=posterior/(rowSums(posterior)%*%one)

BigKyes=round(sum(Npeople)*pyes)
margposterior=rep(0,nmodels)

for (i in 1:nmodels){
  for (j in 1:nmodels){
    for (k in 1:nmodels){
      totalK=Kyes[1,i]+Kyes[2,j]+Kyes[3,k]
      indexK=which.min(BigKyes<=totalK)
      margposterior[indexK]=margposterior[indexK]+posterior[1,i]*posterior[2,j]*posterior[3,k]
    }
  }
}

plot(pyes[pyes>=.44 & pyes<=0.58],margposterior[pyes>=.44 & pyes<=0.58])


cmargposterior=cumsum(margposterior)
BigKyes[which(cmargposterior<=1-alpha/2 & cmargposterior >= alpha/2)]/sum(Npeople)
