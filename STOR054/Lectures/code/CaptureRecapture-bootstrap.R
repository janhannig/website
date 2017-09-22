nrepl=100000

alpha=.05

Kfish=20
nfish=30
kfish=5


Nfish=round(Kfish*nfish/kfish)


ksamplefish=numeric(nrepl)
  
for (i in 1:nrepl){
  balls=sample(Nfish,nfish)
  ksamplefish[i]=sum(balls<=Kfish)
}

Nbootfish=sort(round(Kfish*nfish/ksamplefish))

quartz()
hist(Nbootfish)

print(Nbootfish[round(nrepl*c(alpha/2,.5,1-alpha/2))])