## _______ Wejdan Al-Ahmadi _______##
## _______   Sara Aldubaie  _______##
#library installs: 
#install.packages("shinydashboard")
#library calls

library(magick)
library(tidyverse)
library(ggplot2)
library(chron)
library(hrbrthemes)
library(DT)
#global variables


# farmers_table <- read.csv(url('https://raw.githubusercontent.com/wejdani/Week6_WeekendProject/master/Farmers_Data.csv'))
# farmers_data <- read.csv(url("https://raw.githubusercontent.com/wejdani/Week6_WeekendProject/master/farmers_clean.csv"))
# farmers <- read.csv(url("https://raw.githubusercontent.com/wejdani/Week6_WeekendProject/master/farmers.csv"))


function(input, output) {
  
  farmers_table <- read.csv('Farmers_Data.csv')
  farmers_data <- read.csv("farmers_clean.csv")
  farmers <- read.csv("farmers.csv")

  
  output$Data <- DT::renderDataTable({
    
    
    DT::datatable(farmers_table, escape=FALSE, 
                  options = list(
                    pageLength = 3,scrollX = TRUE,
                    columnDefs = list(list( targets = c(21), width = '1000px'))))
  })
  
  
  
  output$TimePlot <- renderPlot({
    
    timestamps <- c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00"
                    , "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00")
    timeNumbers <- c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
    stamps_to_nums <- data.frame(timeNumbers,timestamps)
    
    
    colorTheme<-input$radio2
    
    if(colorTheme == 1){
      color_1 ="#69b3a2"
      color_2 = "#a969b3"
    }
    if(colorTheme == 2){
      color_1 ="#BC8F8F"
      color_2 = "#6495ED"
    }
    if(colorTheme == 3){
      color_1 ="#FA8072"
      color_2 = "#3CB371"
    }
    
    
    sliderIN<- input$slider2
    
    horz_or_vert <- input$radio
    
    startINP <- sliderIN[1]
    endINP <- sliderIN[2]
    
    
    
    Time_Stamp_From <-  stamps_to_nums[which(stamps_to_nums$timeNumbers == startINP), 2]
    Time_Stamp_End <-  stamps_to_nums[which(stamps_to_nums$timeNumbers == endINP), 2]
    
    
    
    
    timeCount<- data.frame(table(farmers_data$First_Day_Time_From))
    timeCount2<-data.frame(table(farmers_data$First_Day_Time_To))
    
    colnames(timeCount) <- c("Time", "Frequency1")
    colnames(timeCount2) <- c("Time", "Frequency2")
    
    
    timeCount$Time <- format(strptime(timeCount$Time, "%H:%M"), "%H:%M")
    timeCount2$Time <- format(strptime(timeCount2$Time, "%H:%M"), "%H:%M")
    
    
    Time_DF<-merge(x = timeCount, y = timeCount2, by = "Time", all = TRUE)
    
    Time_DF <- Time_DF %>% drop_na()
    
    
    plot_1 <- Time_DF %>% filter( Time>=Time_Stamp_From &  Time<=Time_Stamp_End) %>% 
      ggplot(aes(x=Time)) +               
      geom_line(aes(y=Frequency1, group=1, colour="Starting Time"), colour=color_1) +
      geom_area(aes(y=Frequency1, group=1, colour="Starting Time"), fill=color_1, alpha=0.5)+
      geom_line(aes(y=Frequency2, group=1, colour= "Ending Time"), colour=color_2) +
      geom_area(aes(y=Frequency2, group=1, colour= "Ending Time"), fill=color_2, alpha=0.5)+
      scale_color_manual(values = c(
        'Starting Time' = color_1,
        'Ending Time' = color_2) ,guide = guide_legend(override.aes=aes(fill=NA))) +
      labs(color = "Time")+ ylab("Frequency")
    
    
    chosen_theme<-input$Theme
    
    if(chosen_theme=="Basic"){
      if(horz_or_vert ==1){plot_1 + coord_flip()}
      else{plot_1}
    }
    else if(chosen_theme =="Light"){
      if(horz_or_vert ==1){plot_1+theme_ipsum() + coord_flip()}
      else{plot_1+theme_ipsum()}
    }
    else if(chosen_theme =="Dark"){
      if(horz_or_vert ==1){plot_1+theme_ft_rc() + coord_flip()}
      else{plot_1+theme_ft_rc()}
    }
    
  })
  
  # remove duplicated rows 
  farmers_dub <- farmers %>% distinct() 
  # convert empty rows to NA
  farmers_dub <- farmers_dub %>% mutate_all(na_if,"") 
  farmers_dub <- farmers_dub %>% mutate_all(na_if,"-")
  
  # counting number of farmers in each social media 
  num_website <- length(na.omit(farmers_dub$Website))
  num_facebook <- length(na.omit(farmers_dub$Facebook))
  num_twitter <- length(na.omit(farmers_dub$Twitter))
  num_youtube <- length(na.omit(farmers_dub$Youtube))
  num_other <- length(na.omit(farmers_dub$OtherMedia))
  
  # counting number of NA's in each social media 
  
  num_website_NA <- sum(is.na(farmers_dub$Website))
  num_facebook_NA <- sum(is.na(farmers_dub$Facebook))
  num_twitter_NA <- sum(is.na(farmers_dub$Twitter))
  num_youtube_NA <- sum(is.na(farmers_dub$Youtube))
  num_other_NA <- sum(is.na(farmers_dub$OtherMedia))
  
  
  # calculating  percentage 
  
  website_pct <- round((num_website/8739)*100, digits = 2)
  facebook_pct <- round((num_facebook/8739)*100, digits = 2)
  twitter_pct <- round((num_twitter/8739)*100, digits = 2)
  youtube_pct <- round((num_youtube/8739)*100, digits = 2)
  other_pct <- round((num_other/8739)*100, digits = 2)
  
  #--------- Doughnut chart ----------#
  
  # dataframe for each social media  
  website_df <- data.frame(
    class =  c("Website", "NA"),
    pct = c(website_pct, (100-website_pct))
  )
  website_df <- website_df %>%
    arrange(desc(class)) %>%
    mutate(lab.ypos = cumsum(pct) - 0.5*pct)
  
  facebook_df <- data.frame(
    class =  c("Facebook", "NA"),
    pct = c(facebook_pct, (100-facebook_pct))
  )
  facebook_df <- facebook_df %>%
    arrange(desc(class)) %>%
    mutate(lab.ypos = cumsum(pct) - 0.5*pct)
  twitter_df <- data.frame(
    class =  c("Twitter", "NA"),
    pct = c(twitter_pct, (100-twitter_pct))
  )
  twitter_df <- twitter_df %>%
    arrange(desc(class)) %>%
    mutate(lab.ypos = cumsum(pct) - 0.5*pct)
  youtube_df <- data.frame(
    class =  c("Youtube", "NA"),
    pct = c(youtube_pct, (100-youtube_pct))
  )
  youtube_df <- youtube_df %>%
    arrange(desc(class)) %>%
    mutate(lab.ypos = cumsum(pct) - 0.5*pct)
  other_df <- data.frame(
    class =  c("Other", "NA"),
    pct = c(other_pct, (100-other_pct))
  )
  other_df <- other_df %>%
    arrange(desc(class)) %>%
    mutate(lab.ypos = cumsum(pct) - 0.5*pct)
  
  
  
  
  output$output_plot <- renderPlot({ 
    
    if (input$color_radio == 1) color_num = 1 
    if (input$color_radio == 2) color_num = 11 
    if (input$color_radio == 3) color_num = 7 
    
    if (input$select == "website"){
      
      # plot
      ggplot(website_df , aes(x = 2, y = pct, fill = class)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0)+
        geom_text(aes(y = lab.ypos, label = pct), color = "white")+
        scale_fill_brewer(palette=color_num) +
        theme_void()+
        xlim(0.5, 2.5)
    }else if(input$select == "facebook"){
      ggplot(facebook_df , aes(x = 2, y = pct, fill = class)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0)+
        geom_text(aes(y = lab.ypos, label = pct), color = "white")+
        scale_fill_brewer(palette=color_num) +
        theme_void()+
        xlim(0.5, 2.5)
      
    }else if(input$select == "twitter"){
      ggplot(twitter_df , aes(x = 2, y = pct, fill = class)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0)+
        geom_text(aes(y = lab.ypos, label = pct), color = "white")+
        scale_fill_brewer(palette=color_num) +
        theme_void()+
        xlim(0.5, 2.5)
      
    }else if(input$select == "youtube"){
      ggplot(youtube_df , aes(x = 2, y = pct, fill = class)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0)+
        geom_text(aes(y = lab.ypos, label = pct), color = "white")+
        scale_fill_brewer(palette=color_num) +
        theme_void()+
        xlim(0.5, 2.5)
      
    }else if(input$select == "other"){
      ggplot(other_df , aes(x = 2, y = pct, fill = class)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0)+
        geom_text(aes(y = lab.ypos, label = pct), color = "white")+
        scale_fill_brewer(palette=color_num) +
        theme_void()+
        xlim(0.5, 2.5)
    }
    
  })
  
}