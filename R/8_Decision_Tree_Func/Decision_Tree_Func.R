## _______ Wejdan Al-Ahmadi _______##

# Function that takes the options and create a dataframe

#calculate the probability for each of the options

#print out a statement saying which options is preferred and the value it had

#options are: 

# 1. Outside - Sunny
# 2. Porch - Sunny 
# 3. Inside - Rainy
# 4. Inside - Sunny 
# 5. Porch - Rainy
# 6. Outside - Rainy

library(tidyverse)

#first we make the dataframe with all the options and values
location <- c("Outside", "Porch", "Inside", "Inside", "Porch", "Outside")
weather <- c("Sunny", "Sunny", "Rainy","Sunny" ,"Rainy", "Rainy")
weight <- c(100, 90, 50, 40, 20, 0)
options <- data.frame(location, weather, weight)

#this function takes the data frame as input and returns the most desirable options based on probability

Best_Party_Location <- function(options_DF){
  
  #first we arrange based on the location
  options_DF <- options_DF %>% 
    arrange(location) %>% 
    select(location, weather, weight)
  
  #we define these variables to help us in our calculation
  prob_inside <- 0
  prob_outside <- 0
  prob_porch <- 0
  prob_sun<-0
  prob_rain <-0
  
  #we loop through all the rows
  for(i in 1:nrow(options_DF)){
    
    #we check whether the location is inside
    if(options_DF[i,1] =="Inside"){
      
      #if its inside we check if its rainy or sunny and calculate the probability based on their specified weight in the dataframe
     if(options_DF[i,2]=="Rainy"){
       prob_rain <- 0.6*options_DF[i,3]
     }
      if(options_DF[i,2] =="Sunny"){
        prob_sun <- 0.4*options_DF[i,3]
      }
      
      #we add up for the total probability for inside
      prob_inside <- prob_rain + prob_sun
    }
    
    #we check whether the location is outside
    if(options_DF[i,1] =="Outside"){
      
      #if its outside we check if its rainy or sunny and calculate the probability based on their specified weight in the dataframe
      if(options_DF[i,2]=="Rainy"){
        prob_rain <- 0.6*options_DF[i,3]
      }
      if(options_DF[i,2] =="Sunny"){
        prob_sun <- 0.4*options_DF[i,3]
      }
      
      #we add up for the total probability for outside
      prob_outside <- prob_rain + prob_sun
    }
    
    #we check whether the location is porch
    if(options_DF[i,1] =="Porch"){
      
      #if its porch we check if its rainy or sunny and calculate the probability based on their specified weight in the dataframe
      if(options_DF[i,2]=="Rainy"){
        prob_rain <- 0.6*options_DF[i,3]
      }
      if(options_DF[i,2] =="Sunny"){
        prob_sun <- 0.4*options_DF[i,3]
      }
      
      #we add up for the total probability for porch
      prob_porch <- prob_rain + prob_sun
    }
    
    
  }
  
  #we combine all our probabilities that we calculated into one dataframe with their location
  options <- c(prob_outside,prob_porch,prob_inside)
  loc <- c("out", "porch", "inside")
  df<- data.frame(options, loc)
  
  #we find the max probability and extract that row
  maxval<-df[which.max(df$options),]

  #we return a print statement of the best option with its value  
  return(paste("the best option is", maxval$loc,"With value of", maxval$options))
}

#calling the method
Best_Party_Location(options)







