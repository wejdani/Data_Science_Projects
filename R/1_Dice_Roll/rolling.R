
# -- Wejdan Al-Ahmadi --# 

# -- Week 4 Assignment 1 --#


# ----- FUNCTIONS ----- #

# This function simulates rolling a dice with number of sides equal to the parameter passed as "num_sides" and the number of rolls passed as "num_rolls"
new.roll_Sum<- function(num_sides, num_rolls){
  
  sum <- 0
  dice_vec <- 1:num_sides
  
  for(index in 1:num_rolls){
    index <-sample(dice_vec, size =1,replace = FALSE, prob = NULL)
    print(index)
    sum <- sum +index
    
  }
  
  print(paste('The sum of rolled dice are',sum))  
}

# Calls for the roll sum function: 

new.roll_Sum(10,6)

new.roll_Sum(10,10)

new.roll_Sum(20,6)

new.roll_Sum(20,10)


# This function counts the number of rolls whose values exceed either 6 or 16 based on the number of sides to the dice passed in the parameter

new.roll_Count<- function(num_sides, num_rolls){
  
  count <- 0
  dice_vec <- 1:num_sides
  index_compare <-0
  for(index in 1:num_rolls){
    
    index <-sample(dice_vec, size =1,replace = FALSE, prob = NULL)
    print(index)
    
    if(num_sides ==10 ){
      index_compare <- 6
      if(index >6)
        {count <- count +1}
    }
    else{
      index_compare <- 16
      if(index>16){
        count <- count +1
      }
    }
  }
  
  print(paste("Number of dice rolls above",index_compare ,"are",count))
  
}

# Calls for the roll count function:

new.roll_Count(20,6)

new.roll_Count(10,6)

new.roll_Count(10,10)

new.roll_Count(20,10)



