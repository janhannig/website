nmodels=1000-1
minp=0
maxp=1

alpha=.05

Npeople=225778000
nsample=603
observedyes=314

pyes=minp+(maxp-minp)*(1:nmodels)/(nmodels+1)
Kyes=round(Npeople*pyes)

prior=rep(1/nmodels,nmodels)
posterior=dhyper(observedyes,Kyes,Npeople-Kyes,nsample)*prior
posterior=posterior/sum(posterior)

#quartz()
plot(pyes,posterior)


csposterior=cumsum(posterior)
Kyes[which(csposterior<=1-alpha/2 & csposterior >= alpha/2)]/Npeople

