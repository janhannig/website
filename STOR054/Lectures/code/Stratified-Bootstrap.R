nrepl=100000

alpha=.05

Npeople=c(46392000,64950000,114436000)
nsample=c(111,266,326)
observedyes=c(101,30,183)

pyes=observedyes/nsample
Kyes=round(Npeople*pyes)

pyescombined=sum(Npeople*observedyes/nsample)/sum(Npeople)


nsampleyes1=numeric(nrepl)
nsampleyes2=numeric(nrepl)
nsampleyes3=numeric(nrepl)

for (i in 1:nrepl){
  balls1=sample(Npeople[1],nsample[1])
  nsampleyes1[i]=sum(balls1<=Kyes[1])
  balls2=sample(Npeople[2],nsample[2])
  nsampleyes2[i]=sum(balls2<=Kyes[2])
  balls3=sample(Npeople[3],nsample[3])
  nsampleyes3[i]=sum(balls3<=Kyes[3])
}

pbootcombined=((Npeople[1]*nsampleyes1/nsample[1])+(Npeople[2]*nsampleyes2/nsample[2])+(Npeople[3]*nsampleyes3/nsample[3]))/sum(Npeople)

bootsampleyes=sort(pbootcombined)

#quartz()
hist(bootsampleyes)

print(bootsampleyes[round(nrepl*c(alpha/2,.5,1-alpha/2))])

