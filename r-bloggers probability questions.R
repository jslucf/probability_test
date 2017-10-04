#Jason Laso
#9-27-17
#R-bloggers Probability Questions Part 2
# https://www.r-bloggers.com/answer-probability-questions-with-simulation-part-2/
############################################################################################

library(dplyr)
 
                                                ###### Exercise 1
#If you take cards numbered from 1-10 and shuffle them, and lay them down in order, what is the probability that at least one card 
# matches its position. For example card 3 comes down third?

#setup 10 cards
cards = 1:10

#setup shuffled vector for simulation
shuffled=c()

set.seed(1)

#setup results vector for loop
results=c()

for(i in 1:10000){
  
  #shuffle the 10 cards
  shuffled = sample(cards, 10, replace=F)
  
  #Check if any of the shuffled cards match its natural position, and then append to results vector as 1 if yes, 0 if no
  results[i] = ifelse(any(shuffled == cards), 1, 0)
}

#Percentage of iterations that had at least 1 card match its natural position
sum(results)/10000                   # = 62.9%

#######################################################################################################################

                                                ########### problem2

#Consider an election 3 candidates and 35 voters and who casts his ballot randomly for one of the candidates. What is the probability of 
# a tie for the first position?

#setup the candidates
candidates = 1:3

#num of voters
voters = 35

#iterations of simulation
iter = 5000

#counter of tie-votes for simulation
ties=0

set.seed(1)

for(i in 1:iter){
  
  #Take random votes of the candidates by the voters
  election = sample(candidates, voters, replace=T) %>%
    #summarize them in a table, convert to DF, and then sort in descending order of frequency of votes in the sample
  table() %>% as.data.frame() %>% arrange(desc(Freq))
  
  #check to see if the top 2 candidates tied in votes. raise the counter by 1 if true, and then return to the top of simulation
  if(election$Freq[1] == election$Freq[2]){
    ties = ties+1
  }


}
# P(tied vote) = num of ties in simulation / number of total iterations in simulation
ties / iter

###Answer: ~12.72% chance of a tie

#################################################################################

                                                  ###### problem3
#If you were to randomly find a playing card on the floor every day, how many days would it take on
#average to find a full standard deck?

#setup deck of cards
deck = 1:52

#while loop condition to check if full deck was found
full.deck=0

#counter for simulation
counter = 0

#results vector of days needed to find all cards in each sim
days=c()

set.seed(1)

#simulate while loop 1000 times
for(i in 1:1000){
  
  #Keep going while deck is not full
  while(full.deck == 0){
    
    #Sample one random card, append to collection vector of this simulation, sort in ascending order, and then remove duplicates
    collection = unique(sort(append(sample(deck,1), collection)))
    
    #raise the counter by 1 for each card picked up
    counter = counter+1
    
    #check to see if any card in the collection does not match the same card in the standard deck. If any of them return true, then loop continues
    if(any(collection != deck) == T){
      
      full.deck = 0
      
    } else{
        # if all cards in collection match standard deck, then end the loop
        full.deck = 1
    }
    
  }
  #append the counter number to days vector
  days[i] = counter
  
  #reset everything in the loop
  counter=0
  collection=c()
  full.deck=0
}

summary(days)
hist(days)

###Answer: About 235 days on average

##########################################################################################

                                                ###Exercise 4

#Throw two dice. What is the probability the difference between them is 3, 4, or 5 instead of 0, 1, or 2?

#setup dice
dice=1:6

#setup vector to store rolls of dice in simulation
dice_roll=c()

#setup counter object for simulation
counter = 0

#set num of simulations
iter = 10000

set.seed(1)

for(i in 1:iter){
  #roll dice twice
  dice_roll = sample(dice,2,replace=T)
  
  #take absolute diff of the 2 rolls
  result = abs(dice_roll[1] - dice_roll[2])
  
  #tick counter by 1 if difference of two die are 3:5
  if(result %in% 3:5){
    counter = counter + 1
  }
}
#P(diff is 3:5) = num of positive simulations / total simulations
counter/iter    # = 33.6%

##########################################################################################

#Exercise 5
#What is the expected number of distinct birthdays in a group of 400 people? Assume 365 days and 
#that all are equally likely.

#setup calendar
cal.days=1:365

#setup a vector to hold results of sim
simulation=c()

set.seed(1)
#simulate 5000 times
for(n in 1:5000){
  
  #take sample of 400 birthdays from calendar
  bdays = sample(cal.days, 400, replace=T)
  
  #get the number of unique birthdays from sample and append to simulation vector
  simulation[n] = length(unique(bdays))
  
}
#check the results
simulation

#histogram of simulation
hist(simulation, breaks = min(simulation):max(simulation))

#summary stats
summary(simulation)


#### Answer: ~243 unique birthdays ###

#########################################################################################################


                                              ## Exercise 6
#What is the probability that a five-card hand in standard deck of cards has exactly three aces?

#setup deck of cards by rank (13 x 4)
deck = rep(1:13,4)

#establish a rank for aces in the numeric deck
aces = 1

#num of sims to run
iter=10000

#setup counter object
counter = 0

set.seed(0)

for(i in 1:iter){
  #pick a random 5 card hand and sort in ascending order
  hand = sort(sample(deck, 5, replace=F))
  
  #check if the hand has exactly 3 aces
  if( ( (hand[1] == hand[2]) == hand[3]) == aces){
    counter = counter +1
  }
}

#P(hand has exactly 3 aces) = successes/simulations
counter/iter    # = ~0.19%

#######################################################################################

                                      #Exercise 7
#Randomly select three distinct integers a, b, c from the set {1, 2, 3, 4, 5, 6, 7}.
#What is the probability that a + b > c?

#establish the set
set = 1:7

#number of iterations
iter=10000
set.seed(1)

#setup the counter
counter = 0

for(i in 1:iter){
  #pull 3 distinct random numbers from the set
  result = sample(set, 3, replace = F)
  
  #tick the counter if a+ b > c
  if((result[1] + result[2]) > result[3]){
    counter = counter+1
    
  }
}

#P(a+b > c)
counter/iter #= .791

####################################################################

                                      #Exercise 8
#Given that a throw of three dice show three different faces what is the probability if 
#the sum of all the dice is 8.

#setup dice
dice=1:6

iter=10000
set.seed(1)

#Setup counter to count number of qualified iterations where faces of all dice are different
counter=0

#Results counter for times where three dice = 8
eight=0

for(i in 1:iter){
  
  #Roll 3 distinct dice
  dice_roll = sample(dice, 3, replace=T)
  
  
  if((dice_roll[1] == dice_roll[2]) | 
     (dice_roll[2] == dice_roll[3]) | 
     (dice_roll[1] == dice_roll[3])){
    
    #If any of the dice are the same, then roll the loop back  
    
  } else{
    #Otherwise count the qualified iteration
    counter= counter+1
    
    if(sum(dice_roll[1:3]) == 8){
      #and tick result vector up by 1 if the 3 dice = 8
      eight = eight + 1
    }
  }
  
}

eight/counter #=.093


