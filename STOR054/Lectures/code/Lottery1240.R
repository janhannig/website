ndrawings=1240
m=6
M=49

numbercount=rep(0,M)
runninggap=rep(0,M)
maxgap=rep(0,M)

for (i in 1:ndrawings){
  numbersdrawn=sort(sample(M,m))
  numbercount[numbersdrawn]=numbercount[numbersdrawn]+1
  runninggap=runninggap+1
  runninggap[numbersdrawn]=0
  maxgap=pmax(maxgap,runninggap)
}

##################


nrepl=1000
luckycount=rep(0,nrepl)
unluckycount=rep(0,nrepl)
biggap=rep(0,nrepl)

for (j in 1:nrepl){
#draw
  numbercount=rep(0,M)
  runninggap=rep(0,M)
  maxgap=rep(0,M)
  
  for (i in 1:ndrawings){
    numbersdrawn=sort(sample(M,m))
    numbercount[numbersdrawn]=numbercount[numbersdrawn]+1
    runninggap=runninggap+1
    runninggap[numbersdrawn]=0
    maxgap=pmax(maxgap,runninggap)
  }
  
  unluckycount[j]=min(numbercount)
  luckycount[j]=max(numbercount)
  biggap[j]=max(maxgap)
  
}

hist(unluckycount)
hist(luckycount)
hist(biggap)