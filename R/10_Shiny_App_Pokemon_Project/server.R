## _______ Wejdan Al-Ahmadi _______##


#Please DOWNLOAD the datasets from the links below: 
#https://www.kaggle.com/rounakbanik/pokemon
#https://www.kaggle.com/kvpratama/pokemon-images-dataset

#I tried to upload them but they were just too big even for github since its 721 jpg images. I am sorry for any inconvenience 

#install.packages("imager")
library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(imager)
library(raster)  
library(magick)

#first I get the data
setwd("C:/Users/wejda/Desktop/DS_Bootcamp/Week 6/Day 29/Week6_ShinyAppHomework/Data")

pokemon_data <- read.csv("pokemon.csv", header = TRUE)
pokemon_data<- pokemon_data[-c(722:801),] #because I dont have images for Gen 7 pokemon I dropped the rows



  
  function(input, output, session){
    
    #this first plot is for the pokemons stats
    output$dispPlot <- renderPlot({
      
      
      #I get the pokemon name from the drop down menu
      pokemon_name <- input$Pokemon

      #I select the stats I want
      stat_names <- colnames(pokemon_data %>% dplyr::select( name,attack, defense, hp,sp_attack, sp_defense,speed))
      
      #I extract them from the main dataset
      stats_df <- pokemon_data[stat_names]
      
      #this part is to make the bar chart work, I flip the table 
      df <- melt(as.data.table(stats_df), id.vars = "name", variable.name = "Stats",
                 value.name="Num")
      
      #I then arrange by pokemon name
      df<-df %>% arrange(df, name)
      
      #I then take the data for the pokemon selected
      sub <- subset(df, name == pokemon_name)
      
      #I create a bar plot for that specific pokemon selected's stats
      stat_plot<- ggplot(sub, aes(x = Stats , y= Num, fill = Stats)) +
        geom_bar(position="dodge", stat = "identity") + xlab(paste(sub$name,"Stats"))
      
      
      stat_plot
      
    
    })
    
    #this part is for displaying the pokemons image
    output$dispImg <- renderPlot({
      
      #I get the name of the pokemon selected from the drop down menu 
      pokemon_name <- input$Pokemon
      
      #I get the index of the row based on the pokemon selected for the image
      number <- rownames(pokemon_data[pokemon_data$name == pokemon_name,])
      
      #I then access the images by making a 3 part variable for the path, because I want the image to dynamically change based on the choice
      path <- "C:/Users/wejda/Desktop/DS_Bootcamp/Week 6/Day 29/Week6_ShinyAppHomework/Data/pokemon_jpg/"
      ext <-".jpg" #the extension 
     # full_path <- paste0(path, number,ext) #I combine all 3 variables to make the path
     # myJPG <- stack(full_path) #I plot my image
     # img_plot<-plotRGB(myJPG)
      
      #img_plot
   
      image_url <- "https://raw.githubusercontent.com/wejdani/Week6_Day29_Homework/master/Data/pokemon_jpg/"
      pic <- image_read(paste0(image_url,number,ext))
      plot(pic)
      
      
      
    })
    
    #this is for extra data description of that specific pokemon
    output$dispTable <- renderTable ({
      
      #get the pokemon selected
      pokemon_name <- input$Pokemon
      
      #extract the data columns we want and put it in a dataframe
      description <- colnames(pokemon_data %>% dplyr::select(pokedex_number,generation,name,type1, type2, weight_kg))
      desc_df <- pokemon_data[description]
      
      #subset that dataframe using the input pokemon selected
      sub <- subset(desc_df, name==pokemon_name)
      sub
   
    
      
      
    })
  }
  




