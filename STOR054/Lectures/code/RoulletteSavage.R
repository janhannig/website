nrepl <- 10000

startwealth <- 100
goal <- 500

#prepare placeholders
finalwealth <- numeric(nrepl)
lengthofplay <- numeric(nrepl)

for(i in 1:nrepl){
  wealth=startwealth
  play=0
  
  while (wealth>0 & wealth<goal){
    #strategy contains two pieces of information 
    #   how many numbers are you splitting your bet among 
    #      a single number= 1, red = 18, dozen = 12
    #   how much (total) are you betting 
    #      could be a fixed amount or a function of your wealth
    #      never more than your wealth
    
    betnumber <- 1 #how many numbers is the bet spread over? 
    wage=min(10,wealth) #how much are you betting
    
    
    roullette <- sample(0:36,1) #spin the wheel
    
    wealth <- wealth-wage+wage*36/betnumber*(roullette<betnumber) #settle the winnings
    
    play <- play+1  #count number of times you played
  }
  
  finalwealth[i] <- wealth #where did you end up 
  lengthofplay[i] <- play  #how many times you played to get to the end 
}

#answer
print(c(mean(finalwealth>0),median(lengthofplay)))

#some more fun information
#print(c(mean(finalwealth/startwealth)-1,-1/37))
#boxplot(lengthofplay)


#other bets to try
#wage=min((goal-wealth)*(betnumber/(36-betnumber)),wealth) #bet exactly the amount needed to get to the goal
