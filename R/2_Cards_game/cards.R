## _______ Wejdan Al-Ahmadi _______##

## -------------------  Data import -------------------##

setwd("C:/Users/wejda/Desktop")

deck_Data <- read.csv("deck.csv", header = TRUE)

View(deck_Data)


## -------------------  Assignment -------------------##


## -------------------  Function 1 -------------------##

Shuffle <- function(deck){
  shuffled_Deck <- deck[sample(nrow(deck), size = 52),]
  return(shuffled_Deck)
}

## -------------------  Function 2 -------------------##

# This function is for dealing 5 cards to 3 players
Deal_5_Cards <- function(num_Players){
  #check if we have enough cards to distribute among players
  if((num_Players*5)>52){
    print("sorry not enough cards")
    return()
  }
  #we make a matrix for the 3 players
  Players <- matrix(1:num_Players, nrow = num_Players, ncol = 1)
  # first we shuffle the deck
  shuffled_Deck <- Shuffle(deck_Data)
  
  #then we assign each player a sample of 5 cards
  
  players_Deal <- shuffled_Deck[sample(nrow(shuffled_Deck), size = num_Players*5),]
  player_hand <- cbind(players_Deal,Players)
  
  
  return(player_hand)
}

# Call the deal function and view it in order 
# ---Case 1 : too many players---#

Players_Deal <- Deal_5_Cards(11)
View(Players_Deal[order(Players_Deal$Players),])

# ---Case 2 : proper amount of cards---#

Players_Deal <- Deal_5_Cards(5)
View(Players_Deal[order(Players_Deal$Players),])




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

# Call the deal function and view it in order 
# ---Case 1 : too many cards---#
Players_Deal2 <- Deal(5,11)
View(Players_Deal2[order(Players_Deal2$Players),])


# ---Case 2 : proper amount of cards---#
Players_Deal2 <- Deal(5,3)
View(Players_Deal2[order(Players_Deal2$Players),])
