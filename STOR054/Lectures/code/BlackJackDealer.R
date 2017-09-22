set.seed(123)
nrepl=10^5

ndecks=6

#create a deck 
#1-stands for ace - will be counted with care!
deck=rep(c(1,2,3,4,5,6,7,8,9,10,10,10,10),4)

#write a function returning the point total for cards
pointtotal=function(cards){
  #count A in the hand
  naces=sum(cards==1)
  
  #total the cards countin A=1
  minpoint=sum(cards)
  
  if (naces>0  & minpoint+10<=21){
    #counting one ace as 11
    finalpoint=minpoint+10
  }
  else{
    #counting all aces as 1 or no aces in hand
    finalpoint=minpoint
  }
  return(finalpoint)
}


#prepare the cards - put decks togather add additional cards
allcards=rep(deck,ndecks)

#number of cards in all decks
ncards=length(allcards)

#preparecounters
count17=count18=count19=count20=count21=countBJ=countBust=0
countplayed=0

for (i in 1:nrepl){
  shuffledcards=sample(allcards,ncards)
  
  #deal 2 cards
  ndealerscards=2
  dealerscards=shuffledcards[1:ndealerscards]
  #total the points
  pointdealer=pointtotal(dealerscards)
  #Will the dealer hit?
  hit=(pointdealer<17)
  
  #repeats what is in {...} while hit=TRUE
  while (hit){
    #deal additional card
    ndealerscards=ndealerscards+1
    dealerscards=shuffledcards[1:ndealerscards]
    #total point
    pointdealer=pointtotal(dealerscards)
    #Do I hit again?
    hit=(pointdealer<17)
  }
  
  if (pointdealer==17){
    count17=count17+1
  }
  if (pointdealer==18){
    count18=count18+1
  }
  
  if (pointdealer==19){
    count19=count19+1
  }
  if (pointdealer==20){
    count20=count20+1
  }
  if (pointdealer==21){
    if (ndealerscards==2){
      countBJ=countBJ+1
    }else{
      count21=count21+1
    }
  }
  if (pointdealer>21){
    countBust=countBust+1
  }
  
}

finalresults=c(count17,count18,count19,count20,count21,countBJ,countBust)/nrepl
labelresults=c("17","18","19","20","21","BJ","bust")

print(finalresults)
print(2*4*16/(51*52))


plot(finalresults,type="h",ylab="probability",xaxt="n")
axis(1,at=1:7,labels=labelresults)

nocounthard17results=finalresults
