alpha=.05

Kfish=20
nfish=30
kfish=5

MinFish=50
MaxFish=1000

Nfish=MinFish:MaxFish
nmodels=MaxFish-MinFish+1

#prior=rep(1/nmodels,nmodels)
prior=dgeom(Nfish,kfish/(nfish*Kfish))
#prior=dgeom(Nfish,1/100)
posterior=dhyper(kfish,Kfish,Nfish-Kfish,nfish)*prior
posterior=posterior/sum(posterior)

quartz()

plot(Nfish,posterior)


csposterior=cumsum(posterior)
Nfish[which(csposterior<=1-alpha/2 & csposterior >= alpha/2)]


