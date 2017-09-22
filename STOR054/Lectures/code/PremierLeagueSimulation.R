nteams=20
probability=c(.48,.26,.26)

teampoint=rep(0,nteams)

for (i in 1:(nteams-1)){
  for (j in (i+1):nteams){
    #game one i home j away
    result=rmultinom(1,1,probability)
    teampoint[i]=teampoint[i]+3*result[1]+1*result[2]
    teampoint[j]=teampoint[j]+3*result[3]+1*result[2]
    #game two j home i away
    result=rmultinom(1,1,probability)
    teampoint[i]=teampoint[i]+3*result[3]+1*result[2]
    teampoint[j]=teampoint[j]+3*result[1]+1*result[2]
  }
}


###########
nrepl=1000
maxpoints=rep(0,nrepl)
minpoints=rep(0,nrepl)
varpoints=rep(0,nrepl)

for (k in 1:nrepl){
  
  teampoint=rep(0,nteams)
  
  for (i in 1:(nteams-1)){
    for (j in (i+1):nteams){
      #game one i home j away
      result=rmultinom(1,1,probability)
      teampoint[i]=teampoint[i]+3*result[1]+1*result[2]
      teampoint[j]=teampoint[j]+3*result[3]+1*result[2]
      #game two j home i away
      result=rmultinom(1,1,probability)
      teampoint[i]=teampoint[i]+3*result[3]+1*result[2]
      teampoint[j]=teampoint[j]+3*result[1]+1*result[2]
    }
  }
  
  maxpoints[k]=max(teampoint)
  minpoints[k]=min(teampoint)
  varpoints[k]=var(teampoint)
  
}

hist(maxpoints)
hist(minpoints)
hist(varpoints)
