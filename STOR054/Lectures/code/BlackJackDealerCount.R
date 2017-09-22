nrepl=10^5

ndecks=6
mincounttoplay=17


#create a deck 
#1-stands for ace - will be counted with care!
deck=rep(c(1,2,3,4,5,6,7,8,9,10,10,10,10),4)

#write a function returning the point total for cards
pointtotal=function(cards){
  naces=sum(cards==1)
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

deckcount=function(shufffledcards){
  countvalues=(shuffledcards<7)-(shuffledcards>9)-2*(shuffledcards==1)
  #fixed the fact that A=1 counts as -1 not +1
  deckcount=c(0,cumsum(countvalues))
  return(deckcount)
}

#prepare the cards - put decks togather add additional cards
allcards=rep(deck,ndecks)

#counts number of cards
ncards=length(allcards)

#preparecounters
count17=count18=count19=count20=count21=countBJ=countBust=0
countplayed=0

for (i in 1:nrepl){
  shuffledcards=sample(allcards,ncards)
  
  #get the deck count
  cardcount=deckcount(shuffledcards)  
  
  #find the position of the first time the count goes above mincount
  #startplaying after that
  startplay=which.max(c(cardcount>=mincounttoplay,TRUE))
  
  #only play if there is at least one deck left
  if (startplay<=ncards*(ndecks-1)/(ndecks)){
    countplayed=countplayed+1
    
    ndealerscards=2
    dealerscards=shuffledcards[startplay:(startplay+ndealerscards-1)]
    pointdealer=pointtotal(dealerscards)
    
    while (pointdealer<17){
      ndealerscards=ndealerscards+1
      dealerscards=shuffledcards[startplay:(startplay+ndealerscards-1)]
      pointdealer=pointtotal(dealerscards)
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
}

finalresults=c(count17,count18,count19,count20,count21,countBJ,countBust)/countplayed
labelresults=c("17","18","19","20","21","BJ","bust")

print(finalresults)
print(countplayed/nrepl)
print(2*4*16/(51*52))


plot(finalresults,type="h",ylab="probability",xaxt="n")
axis(1,at=1:7,labels=labelresults)

counthard17results=finalresults
