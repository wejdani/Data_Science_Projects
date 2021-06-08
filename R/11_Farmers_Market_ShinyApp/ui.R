#install.packages("shinyfullscreen")
library(shinyfullscreen)
library(shiny)
library(shinydashboard)

title <- tags$a(href='https://piazza.com/class/kmh8s70dofm4s3?cid=221',
                tags$img(src='https://static.wikia.nocookie.net/lego/images/2/23/PPG_logo.png', height = '100', width = '200'), target="_blank")
# 
# farmers_table <- read.csv(url('https://raw.githubusercontent.com/wejdani/Week6_WeekendProject/master/Farmers_Data.csv'))
# farmers_data <- read.csv(url("https://raw.githubusercontent.com/wejdani/Week6_WeekendProject/master/farmers_clean.csv"))
# farmers <- read.csv(url("https://raw.githubusercontent.com/wejdani/Week6_WeekendProject/master/farmers.csv"))


#setwd("C:/Users/wejda/Desktop/DS_Bootcamp/Week 6/Day 30")

fluidPage(
  dashboardPage(
  
    
    dashboardHeader(title = title, titleWidth = 300,
                    tags$li(class = "dropdown",
                            tags$style(".main-header {max-height: 100px}"),
                            tags$style('.skin-blue .main-header .logo { background-color: white; }'),
                            tags$style('.skin-blue .main-header .logo:hover { background-color: #F0F8FF;}'),
                            tags$style('.main-header .logo {height: 100px;
                             font-family: "Georgia", Times, "Times New Roman", serif;
                             font-weight: bold;
                             font-size: 24px;}'),
                            tags$style(".sidebar-toggle {height: 100px; padding-top: 35px !important;}"),
                            tags$style(".navbar {min-height:100px !important}")
                    ) ),
    
    
    dashboardSidebar( 
      width = 300, # Adjust the sidebar
      tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
      sidebarMenu(
        menuItem("Data", tabName = "Data", icon = icon("book-open")),
        menuItem(" Wejdan Al-Ahmadi", icon = icon("themeisle "), tabName = "Wejdan"),
        menuItem(" Sara Aldubaie", icon = icon("github-alt "), tabName = "Sara")
      )     ),
    
    
    dashboardBody(
      
      box( width = 1000, 
        
       
        tabItems(
          
          
        tabItem(tabName = "Data",
                h2("This is our cleaned Farmers Market Data:"), 
                div(DT::dataTableOutput("Data"), style = "font-size:80%"),
        ),
        
        tabItem(tabName = "Wejdan",
                h2("Starting and Ending Times for all the markets:"),
                
                plotOutput("TimePlot", height=600),
                
                box ( width = 1000,   ( selectInput("Theme", h3("Please Select a theme"),
                                      choices= c("Basic", "Light", "Dark"))),
                     
                     (sliderInput("slider2", label = h3("Time Range"), min = 0, 
                                     max = 23, value = c(7,12))),
                     
                     (radioButtons("radio", label = h3("Plot direction:"),
                                      choices = list("Horizontal" = 1, "Vertical" = 2), 
                                      selected = 2)),
                     (radioButtons("radio2", label = h3("Plot Colors:"),
                                      choices = list("Scheme 1" = 1, "Scheme 2" = 2, "Scheme 3" = 3), 
                                      selected = 1)))
                
                
        ),
        
        tabItem(tabName = "Sara",
                h2("Percentage Of Farmers Using Social Media"),
                # select box 
                selectInput("select", label = h3("Select Social Media platform "), 
                            choices = list("Website" = "website", 
                                           "Facebook" = "facebook", 
                                           "Twitter" = "twitter",
                                           "Youtube" = "youtube", 
                                           "Other" = "other"), 
                            selected = 1),
                radioButtons("color_radio", label = h3("Choose Color"),
                             choices = list("Blue" = 1, "Purple" = 2, "Orange" = 3), 
                             selected = 1),
                
                plotOutput("output_plot"), 
                
                
                
        )
      ))
      
    )
      
    )
  
  )
  
