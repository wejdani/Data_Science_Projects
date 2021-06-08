#install.packages("shiny")
library(shiny)


setwd("C:/Users/wejda/Desktop/DS_Bootcamp/Week 6/Day 29/Week6_ShinyAppHomework/Data")

pokemon_data <- read.csv("pokemon.csv", header = TRUE)
pokemon_data<- pokemon_data[-c(722:801),] #because I dont have images for Gen 7 pokemon I dropped the rows
setwd("C:/Users/wejda/Desktop/DS_Bootcamp/Week 6/Day 29/Week6_ShinyAppHomework")


#I used fluid because I was running into issues trying to plot multiple plots, especially one being an image, it didnt work with many of the 
#plot grouping functions such as: facet_wrap, grid, grid.arrange, grid.draw

#I divided my shiny app by 4 to include all the data I want

fluidPage(
  titlePanel("Pokedex"),
  
  fluidRow(
    column(12,
      
          sidebarPanel(
            selectInput("Pokemon", "Please Select a Pokemon",
                        choices= pokemon_data$name))
      
    ),
    column(3,
           imageOutput("dispPlot"),
           
    ),
    column(4,
        
           imageOutput("dispImg")
    ),
    column(12,
         
           tableOutput("dispTable")
    )
  )
)