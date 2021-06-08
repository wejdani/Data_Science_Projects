## _______ Wejdan Al-Ahmadi _______##

## --NOTE: I have used some of the functions from the previous assignment to complete this one, go to line 66 for todays assignment -- ##

## -------------------  Data import -------------------##

setwd("C:/Users/wejda/Desktop")

deck_Data <- read.csv("deck.csv", header = TRUE)

View(deck_Data)


## -------------------  Week 4 - Day 17 - Assignment (1)  -------------------##


## -------------------  Task (1) - Shuffle Function -------------------##

Shuffle <- function(deck){
  shuffled_Deck <- deck[sample(nrow(deck), size = 52),]
  return(shuffled_Deck)
}




## ------------------- Optional Assignment -------------------##

Deal <- function(num_Players, num_Cards){
  
  #check if we have enough cards to distribute among players
  
  if((num_Players*num_Cards)>52){
    
    print("sorry not enough cards")
    return()
    
  }
  
  #define the player matrix based on the function input
  Players <- matrix(1:num_Players, nrow = num_Players, ncol = 1)
  
  
  # first we shuffle the deck
  shuffled_Deck <- Shuffle(deck_Data)
  
  #then we assign each player a sample of cards, here I used the size as the number of cards multiplied by the number of players 
  #because I want the total amount of random cards to pull and then assign them to the players

    players_Deal <- shuffled_Deck[sample(nrow(shuffled_Deck), size = num_Cards*num_Players),]
    
  #then I combine both matrices by the column  
    players_hand <- cbind(players_Deal,Players)
    


  return(players_hand)
}





#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

## -------------------  Week 4 - Day 18 - Assignment (2)  -------------------##


## -------------------  Task (1) - 2-Player Winner Function -------------------##


# this function takes a dataframe of 2 players and tells us which one wins based on the score (calculated by sum of values)
two_Player_Winner<- function(players_DF){
  
  #First we define both players by extracting their data from the dataframe using subset
  Player1 <- subset(players_DF, Players == 1 )
  Player2 <- subset(players_DF, Players == 2 )
  
  #Then we sum the third column containing the values to find the total score for each player
  Player1_Score <- sum(Player1[,3])
  Player2_Score <- sum(Player2[,3])
  
  #now we first compare if Player 1 has a higher score than player 2
  #if this is the case we print out the statement along with the player scores
  
  if(Player1_Score>Player2_Score){
    print(paste0("Player 1 has more points than Player 2. Player 1: ", Player1_Score, " Player 2: ", Player2_Score))
  }
  
  #then we check to see if the second player has the higher score and then print the following statement
  else if(Player1_Score<Player2_Score){
    print(paste0("Player 2 has more points than Player 1. Player 1: ", Player1_Score, " Player 2: ", Player2_Score))
  }
  
  #lastly we check if both players have the same score which results in a tie
  else{
    print(paste0("Both Players have the same value of cards, its a tie! Player 1: ", Player1_Score, " Player 2: ", Player2_Score))
    
  }

}


# ---- Calling the functions for Task (1) ----# 

#first we use our function from the previous assignment to deal the cards
two_Player_Game <- Deal(2,10)
#then we check who won using the function for Task (1)
two_Player_Winner(two_Player_Game)


## ----------------------  Task (2) - N-Player Winner Function ----------------------##

#this method takes a dataframe of all players and determines the winner by the highest score and prints out all players scores
N_Player_Winner <- function(players_DF){
  
  #first lets find out the number of players, we do this by splitting the dataframe into a list based on the Players column
  split_players_list = split(players_DF, with(players_DF, interaction(Players)), drop = TRUE)

  #this variable count is used to start the loop from player 1
  count <- 1
  
  #this variable num_of_players takes the length of the splitted player list above as the number of players
  num_of_players <- length(split_players_list)
  
  #we initialize a new list for the scores which is calculated by the sum of the value column
  scores_list <- list()
  
  #we loop for the amount of players we have starting from 1 until the num of players calculated earlier
  while(count <= num_of_players ){
    
    #we extract the sum of the value column from the splitted player list and add it to the scores list we initialized before the loop
    scores_list[[count]] <- sum(split_players_list[[count]]$value)
    
    #we increase the counter
    count <- count +1
  }
  
  #this part is for combining the player number with their designated score, we created two vectors
  Player_number <- 1:num_of_players
  Player_Score <- unlist(scores_list)
  
  #and then we combined the above two vectors in a dataframe for the scorelist
  score_Board_DF <- data.frame(Player_number, Player_Score)
  
  
  #View(score_List_DF[order(-score_List_DF$Player_Value_Sum),])
  
  #now we order the score list in descending order, by using the '-' with the order method based on the player score
  ordered <- score_Board_DF[order(-Player_Score),]
  
  
  #lastly we print the entire score board and then print the first one as the winner since we ordered it in descending order
  print(ordered[,])
  print(paste("As we can see the winner is player number ", ordered[1,1], "with a high score of = ", ordered[1,2]))

}

# ---- Calling the functions for Task (2) ----# 

#first we deal the cards
five_players <- Deal(5,3)

#then we call the function
N_Player_Winner(five_players)




