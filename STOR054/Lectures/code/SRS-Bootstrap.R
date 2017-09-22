nrepl=100000

alpha=.05

Npeople=225778000
nsample=603
observedyes=314

pyes=observedyes/nsample
Kyes=round(Npeople*pyes)

nsampleyes=numeric(nrepl)
  
for (i in 1:nrepl){
  balls=sample(Npeople,nsample)
  nsampleyes[i]=sum(balls<Kyes)
}

bootsampleyes=sort(nsampleyes/nsample)

#quartz()
hist(bootsampleyes)
  
print(bootsampleyes[round(nrepl*c(alpha/2,.5,1-alpha/2))])
