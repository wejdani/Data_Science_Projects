## _______ Wejdan Al-Ahmadi _______##
## _______   Sara Aldubaie  _______##
## _______   Norah Alharthi  _______##

## -------------------  Libraries -------------------##
install.packages("BBmisc")
install.packages("gam")
install.packages("ggmap")
install.packages("maptools")
install.packages("zipcodeR")
library(tidyverse)
library(dplyr)
library(rlist)
library(stringr)
library(BBmisc)
library(lubridate)
library(tools)
library(tibble)
library(gam)
library(ggmap)
library(maps)
library(sp)
library(maptools)
library(zipcodeR)
## -------------------  Data import -------------------##
#setwd("C:/Users/wejda/Desktop/DS_Bootcamp/Week 5/Day 25/dirtydata")
farmers <- read.csv("./data/farmers.csv", header = TRUE,fileEncoding="UTF-8-BOM")
# -------------------- Issues with the dataset -------------------- #

# 1- Duplicate row entries  
# 2- Missing information including: 
#     - social media links 
#     - address(street, city, county, state [check with pipeline] and zip) 
#     - season date and time
#     - location
#     - produce Y and N values
#     - time stamp 'to' values missing example: Wed: 7:00 AM-; 
# 3- Data inconsistency
#     - produce Y and N values [check with pipeleine and remove from comment if not found]
#     - social media links (not in form of URL)
#     - city and state spelling not consistent (some are capital some are not)
#     - zip code should always be 5 digits (if less add 0 prefix)
#     - season date, some are in mm-dd-yyyy and some are in written format (july to june) 
#     - season time: (inconsistent quantity of time values/repeated time stamps in each row cell,
#     pm/am case sensitivity is inconsistent, some am/pm have '-' after it, extra space between 
#     day and ':' in some rows, day case sensitivity inconsistent (sun and Sun) 
#     - The seasons dates are in the wrong season columns the proper seasons are defined as : 
#       spring (March, April, May), 
#       summer (June, July, August), 
#       autumn (September, October, November) 
#       and winter (December, January, February). 
# 4- columns that need separation : 
#     - seasons start and end, 
#     - separate days in timestamp into 2 columns (from and to)    
#     - timestamp start day time from, start day time to, end day time from, end day time to
# 5- column name as a value : produce provided by each market, make it a list instead and add in one row
# 6- column names: 
#     - change season1 -> spring 
#     - x and y to lat and lon
# 7- formatting: inconsistent case sensitivity in table headers/column name


# -------------------- Initial cleanup  -------------------- #

#set all the blank and '-' cells to NA 
farmers <- farmers %>% mutate_all(na_if,"")
farmers <- farmers %>% mutate_all(na_if,"-")

#the first 2 rows are duplicated horizontally, (so duplicated in the same row doubling the number of columns)

#first we get the 2 extra rows
dup_rows <- subset(farmers, select =c(X:X.57))
dup_rows <- dup_rows[1:2,]

#we remove the extra columns
farmers_clean = subset(farmers, select = -c(updateTime:X.58))

#we rename the dup_rows columns so we can bind them together
colnames(dup_rows) <- colnames(farmers_clean)

#the zip data type is incompatible in dup_rows its a numeric and in farmers_clean its a character, so we convert it
farmers_clean<-farmers_clean %>% mutate(zip= as.numeric(zip))

#lastly we join all the rows together
farmers_clean<-bind_rows(farmers_clean, dup_rows)

## -------------------   Duplicate row entries  -------------------##

num_unique_values <- count(farmers %>% distinct())
num_unique_values
# We have 8739 out of 8761 unique values   

num_duplicated <- count(farmers)- num_unique_values
num_duplicated
#so this means that we have 22 duplicates 
#to view duplicate rows:
duplicate_rows <- farmers[duplicated(farmers),]


#to remove duplicate rows:
farmers_clean <- farmers_clean %>% distinct()
summary(farmers_clean) # Now we have 8739 rows which the number of unique rows, so it worked.


## -------------------  Drop the 'location' column  -------------------##

farmers_clean$Location <- NULL
view(farmers_clean)

## -------------------  Fix the case letter consistency in the table headers  -------------------##

#for this we need to use the 'tools' library 
names(farmers_clean) <- toTitleCase(names(farmers_clean))
View(farmers_clean)

## -------------------  Fix zip code to be 5 digits  -------------------##
# I think this one is more complicated than what we thought, because each state zip start with a certain number, we can't just add zeros

#convert zipcode to number 

farmers_clean$Zip <- as.numeric(farmers_clean$Zip)
farmers_distinct_zip <- sprintf("%05d", farmers_clean$Zip)
farmers_distinct_zip 

farmers_clean$Zip <- farmers_distinct_zip


## -------------------  Change x to longitude and y to latitude  -------------------##

names(farmers_clean)[names(farmers_clean) == "x"] <- "Longitude"
names(farmers_clean)[names(farmers_clean) == "y"] <- "Latitude"

View(farmers_clean) 


# --------------------  column name as values  -------------------- #
farmers_clean[name][is.na(farmers_clean[name])]<-"N"


#-------------------------------------------------#
#This method takes a sub dataframe of the real data, an empty temp list and an empty dataframe
#it loops through all the rows and if its a yes it adds the column name to the list and then adds it to the empty dataframe and cbinds it with the original data
column_to_rows_values <- function(data_DF, list_of_values, list_of_values_DF){
for(i in 1:nrow(data_DF)){
  for(j in 1:ncol(data_DF)){
    if(data_DF[i,j]=="Y"){
      list.append(list_of_values, colnames(data_DF[j]))
      list_of_values[[j]] <- colnames(data_DF[j])
    }
    list_of_values<-list_of_values %>% discard(is.null)
  }
  list_of_values_DF<-rbind(list_of_values_DF, p=toString(list_of_values))
}

return(farmers_clean <- cbind(farmers_clean, list_of_values_DF))
}
#-------------------------------------------------#
#First we do the Payment methods

#we get the names of the columns
Payment_name<- colnames(farmers_clean %>% select(Credit:SNAP))

#we extract the data from the original
Payment_DF <- farmers_clean[Payment_name]

#create empty temp variables
list_of_payment <- list()
Payment_Methods <- data.frame()

#call the method
farmers_clean<- column_to_rows_values(Payment_DF,list_of_payment, Payment_Methods)

#fix the column name
colnames(farmers_clean)[which(names(farmers_clean) == "X.Credit..WIC..SFMNP.")] <- "Payment_Methods"

#Now we do the Produce list

#we get the names of the columns
Produce_name<- colnames(farmers_clean %>% select(Organic:WildHarvested))

#we extract the data from the original
Produce_DF <- farmers_clean[Produce_name]

#create empty temp variables
list_of_produce <- list()
Produce_List <- data.frame()

#call the method
farmers_clean<-column_to_rows_values(Produce_DF,list_of_produce, Produce_List)

#fix the column name
colnames(farmers_clean)[which(names(farmers_clean) == "X.Organic..Bakedgoods..Cheese..Crafts..Flowers..Eggs..Herbs..Vegetables..Honey..Jams..Maple..Meat..Poultry..Prepared..Soap..Trees..Coffee..Beans..Fruits..Mushrooms..PetFood.")] <- "Produce_List"




#-------------------------------------------------#

#now we drop the extra columns we dont need
farmers_clean = subset(farmers_clean, select = -c(Credit:WildHarvested))
farmers_clean <- farmers_clean %>% mutate_all(na_if,"")


#we first remove all the spaces
farmers_clean$Season1Time<- str_replace_all(farmers_clean$Season1Time,  fixed(" "), "")



#this function loops through the times and seperates them by ; based on the first and last element
sep_time <- function(time_DF){
  split_time<-strsplit(farmers_clean$Season1Time, ";")
  time_DF <- data.frame()
  for(i in 1:length(split_time)){
    time_DF[i,1] <-split_time[[i]][1]   #first element
  }
  return(time_DF)
}

#we create empty dataframes and seperate the first and last days in each one: 

start <- data.frame()
start <- sep_time(start)
colnames(start)[which(names(start) == "V1")] <- "First_Day_Time"

end <- data.frame()
end <- sep_time(end)
colnames(end)[which(names(end) == "V1")] <- "Last_Day_Time"

#we merge all the dataframes together
farmers_clean <- cbind(farmers_clean, start, end)

#we seperate the day from the time
farmers_clean <- farmers_clean %>% separate(First_Day_Time, c("First_Day", "First_Day_Time"), sep=4)

farmers_clean <- farmers_clean %>% separate(Last_Day_Time, c("Last_Day", "Last_Day_Time"), sep=4)
#farmers_clean <- farmers_clean %>% separate(First_Day_Time, c("First_Day_Time", "AM/PM"), sep=-2)


#Fix the formatting
farmers_clean$First_Day_Time <-toupper(farmers_clean$First_Day_Time)
farmers_clean$Last_Day_Time <-toupper(farmers_clean$Last_Day_Time)

farmers_clean$First_Day <-toupper(farmers_clean$First_Day)
farmers_clean$Last_Day <-toupper(farmers_clean$Last_Day)

farmers_clean$First_Day <- substring(farmers_clean$First_Day,1,nchar(farmers_clean$First_Day)-1)
farmers_clean$Last_Day <- substring(farmers_clean$Last_Day,1,nchar(farmers_clean$Last_Day)-1)




# --------------------------------- fixing date columns------------------------------------- #

# separating the column by to 
farmers_clean <- farmers_clean %>% 
  separate(Season1Date, into = c("Season1Start", "Season1End"), sep = " to ")
farmers_clean <- farmers_clean %>% 
  separate(Season2Date, into = c("Season2Start", "Season2End"), sep = " to ")
farmers_clean <- farmers_clean %>% 
  separate(Season3Date, into = c("Season3Start", "Season3End"), sep = " to ")
farmers_clean <- farmers_clean %>% 
  separate(Season4Date, into = c("Season4Start", "Season4End"), sep = " to ")

# converting month written in text format into date 

change_to_date <- function(x) {
  #Values that have only characters in them like 'June', 'August' etc
  inds <- grep('^[A-Za-z]+$', x)
  #Add date value
  x[inds] <- paste(x[inds], 1)
  #Change to date class
  lubridate::parse_date_time(x, c('%m-%d-%Y', '%B %d'))
}

# apply previous function to multiple columns using across 
farmers_clean  <- farmers_clean %>% mutate(across(
  c(Season1Start, Season1End,
    Season2Start, Season2End,
    Season3Start, Season3End,
    Season4Start, Season4End), 
  change_to_date))

# fix the formatting of date yyyy-mm-dd

farmers_clean$Season1Start <- format(farmers_clean$Season1Start, "%Y-%m-%d")
farmers_clean$Season1End <- format(farmers_clean$Season1End, "%Y-%m-%d")
farmers_clean$Season2Start <- format(farmers_clean$Season2Start, "%Y-%m-%d")
farmers_clean$Season2End <- format(farmers_clean$Season2End, "%Y-%m-%d")
farmers_clean$Season3Start <- format(farmers_clean$Season3Start, "%Y-%m-%d")
farmers_clean$Season3End <- format(farmers_clean$Season3End, "%Y-%m-%d")
farmers_clean$Season4Start <- format(farmers_clean$Season4Start, "%Y-%m-%d")




# add column for spring start and end 

farmers_clean <- add_column(farmers_clean, SpringStart = NA, .after = "Season1Start")
farmers_clean <- add_column(farmers_clean, SpringEnd = NA, .after = "SpringStart")

# add column for summer start and end 

farmers_clean <- add_column(farmers_clean, SummerStart = NA, .after = "SpringEnd")
farmers_clean <- add_column(farmers_clean, SummerEnd = NA, .after = "SummerStart")

# add column for fall start and end 

farmers_clean <- add_column(farmers_clean, FallStart = NA, .after = "SummerEnd")
farmers_clean <- add_column(farmers_clean, FallEnd = NA, .after = "FallStart")

# add column for winter start and end 

farmers_clean <- add_column(farmers_clean, WinterStart = NA, .after = "FallEnd")
farmers_clean <- add_column(farmers_clean, WinterEnd = NA, .after = "WinterStart")

# vector with month number depending on the season 

Spring   <- c(3, 4, 5)
Summer   <- c(6, 7, 8)
Fall  <- c(9, 10, 11)
Winter   <- c(12, 1, 2)

# convert date value into its rightful season  (start)
date_to_season_s <- function(x){
  
  count = 1 # represent row number 
  while(count <= nrow(farmers_clean) ) { # less than or equal rows lenghth in table 
    
    if (!is.na(x[count]) & "%in%"(month(x[count]), Spring)) { # check of month belong to spring vector 
      farmers_clean$SpringStart[count] <<- x[count]
      
    }else if (!is.na(x[count]) &  "%in%"(month(x[count]), Summer)) { # check of month belong to summer vector  
      farmers_clean$SummerStart[count] <<- x[count]
      
    }else if (!is.na(x[count]) &  "%in%"(month(x[count]), Fall)) { # check of month belong to fall vector 
      farmers_clean$FallStart[count] <<- x[count]
      
    }else if (!is.na(x[count]) &  "%in%"(month(x[count]), Winter)) { # check of month belong to winter vector 
      farmers_clean$WinterStart[count] <<- x[count]
    }
    count = count +1
    
  } }

date_to_season_s(farmers_clean$Season1Start)
date_to_season_s(farmers_clean$Season2Start)
date_to_season_s(farmers_clean$Season3Start)
date_to_season_s(farmers_clean$Season4Start)


# replace date value into its rightful season  (End)


date_to_season_e <- function(x){
  
  count = 1  # represent row number
  while(count <= nrow(farmers_clean) ) { # less than or equal rows lenghth in table 
    
    if (!is.na(x[count]) & "%in%"(month(x[count]), Spring)) { # check of month belong to spring vector 
      farmers_clean$SpringEnd[count] <<- x[count]
      
    }else if (!is.na(x[count]) &  "%in%"(month(x[count]), Summer)) { # check of month belong to summer vector 
      farmers_clean$SummerEnd[count] <<- x[count]
      
    }else if (!is.na(x[count]) &  "%in%"(month(x[count]), Fall)) { # check of month belong to fall vector 
      farmers_clean$FallEnd[count] <<- x[count]
      
    }else if (!is.na(x[count]) &  "%in%"(month(x[count]), Winter)) { # check of month belong to winter vector 
      farmers_clean$WinterEnd[count] <<- x[count]
    }
    count = count +1
    
  } }

date_to_season_e(farmers_clean$Season1End)
date_to_season_e(farmers_clean$Season2End)
date_to_season_e(farmers_clean$Season3End)
date_to_season_e(farmers_clean$Season4End)


#droping not needed columns

# vector includes the name of columns we need to drop
drops <- c("Season1End","Season1Start",
           "Season2End","Season2Start",
           "Season3End","Season3Start",
           "Season4End","Season4Start", 
           "Season1Time", "Season2Time",
           "Season3Time", "Season4Time")
# droping the columns
farmers_clean <- farmers_clean[ , !(names(farmers_clean) %in% drops)]


a <- c(percentage_null_in_Facebook,percentge_null_in_Website,percentage_null_in_Twitter ,percentage_null_in_Youtube,percentage_null_in_OtherMedia)
b<- c("Facebook", "Website", "Twitter", "Youtube", "Other")

mytable <- table(a)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, b,
    main="Pie Chart of % of missing Social Media")




#see percentage of missing value in social media to deal with it
percentage_null_in_Facebook <- round(mean(is.na(farmers_clean$Facebook)),2) # Facebook
percentge_null_in_Website <- round(mean(is.na(farmers_clean$Website)),2) # Website
percentage_null_in_Twitter <- round(mean(is.na(farmers_clean$Twitter)),2) # Twitter
percentage_null_in_Youtube <- round(mean(is.na(farmers_clean$Youtube)),2) # Youtube
percentage_null_in_OtherMedia <- round(mean(is.na(farmers_clean$OtherMedia)),2) # OtherMedia


#drop "Twitter, Youtube, OtherMedia" columns because they have missing values more than have of the dataset
farmers_clean <- farmers_clean %>%
  select(-Twitter,-Youtube,-OtherMedia)

#to check if there is missing Cities
null_city <- is.na(farmers_clean$city)
null_city
null_city_num <- sum(null_city)
null_city_num

#to check if there is missing Counties
null_county <- is.na(farmers_clean$County)
null_county
null_county_num <- sum(null_county)
null_county_num

#to check if there is missing States
null_State <- is.na(farmers_clean$State)
null_State
null_State_num <- sum(null_State)
null_State_num


#drop all rows with missing latitude(x) an longitude(y)
farmers_clean <- farmers_clean[complete.cases(farmers_clean[ , 18:19]),]

#check the previous code is worked properly
farmers2 <- farmers_clean %>%
  filter(is.na(Longitude) | is.na(Latitude)) %>% 
  select(Street, City, County, State, Longitude, Latitude)


#find the county based on latitude(X) and longitude(y)

startm <- Sys.time()
#I am doing the reverse geocoding using the function map.where from the maps package
#Here I used one maps of the package; county

New_County <- map.where(database="county", 
                        farmers_clean$Longitude, farmers_clean$Latitude)
endm <- Sys.time()

#make a new data from "farmers1" with "new county" column
farmers_clean <- farmers_clean %>%
  select(-County)

farmers_clean <- add_column(farmers_clean, New_County, .after = "City")

#upper first letter to all values in "City,County,State" 
farmers_clean <- farmers_clean %>% mutate(City = toupper(City))
farmers_clean <- farmers_clean %>% mutate(County = toupper(County))
farmers_clean <- farmers_clean %>% mutate(State = toupper(State))







#--------------------------- Plots ---------------------------# 
#this plot looks at the first days the market opens and the last days and compares them 
require(gridExtra)

plot1 <-farmers_clean %>%
  filter(!is.na(First_Day)) %>%
  ggplot(aes(x= First_Day, fill=First_Day)) +
  xlab("First Days of the Farmer's Market")+
  geom_bar() 
  

plot2 <-farmers_clean %>%
  filter(!is.na(Last_Day)) %>%
  ggplot(aes(x= Last_Day, fill=Last_Day)) +
  xlab("Last Days of the Farmer's Market")+
  geom_bar() 

grid.arrange(plot1, plot2, ncol=2)


farmers_clean %>%
  filter(!is.na(SpringStart,Produce_List)) %>%
  ggplot(aes(x= SpringStart, y=length(Produce_List))) +
  xlab("First Days of the Farmer's Market")+
  geom_point() 

#-------------------------------------------------------------#

#This plot shows all the markets based on states in pie form
mytable <- table(farmers_clean$State)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable,
    main="Number of Farmers Markets Based on State" )

#-------------------------------------------------------------#
#This plot shows all the markets based on states in bar form
ggplot(data = farmers_clean) +
  stat_count(mapping = aes(x = farmers_clean$State), fill ="#c16b6b") +
  labs(x = "State",
       y = "count",
       title  = "State with most markets") +
  coord_flip()
#-------------------------------------------------------------#









#*************************************************************************#
#------------------------------ failed attempts  -------------------------# 
#*************************************************************************#
#these are extra steps we tried to do to further clean the data but they did not work

#---------------------------- unifying time format----------------------#
time_name <- colnames(farmers_clean %>% select(First_Day:Last_Day_Time))

time_df <- farmers_clean[time_name]

time_df$First_Day_Time<-sub('7:00AM-','7:00AM-9:00PM',time_df$First_Day_Time)
time_df$First_Day_Time<-sub('-12:00PM','7:00AM-12:00PM',time_df$First_Day_Time)
time_df$First_Day_Time<-sub('3PM-6PM', '3:00PM-6:00PM',time_df$First_Day_Time)

time_df$First_Day_Time<- gsub('.{3}$', '', time_df$First_Day_Time)

library('stringi')

for(i in 1:nrow(time_df)){
  if(nchar(time_df$First_Day_Time) > 13){
    time_df$First_Day_Time <- stri_sub(time_df$First_Day_Time, 1, 13)
    }
}


for(i in 1:nrow(time_df)){
  if(nchar(time_df$First_Day_Time) > 13){
    time_df$First_Day_Time<-substring(time_df$First_Day_Time, 1, 13)
  }
  }

if(nchar(time_df$First_Day_Time[20]) > 13){
  print(substring(time_df$First_Day_Time[20], 1, 13))
  
}

warnings()


time_df$First_Day_Time <-sprintf("%s9:00PM", time_df$First_Day_Time)
time_df$First_Day_Time <-sprintf("9:00AM%s", time_df$First_Day_Time)
time_df$First_Day_Time <-sprintf("%s:00", time_df$First_Day_Time)
time_df$First_Day_Time<-sub('3PM-6PM', '3:00PM-6:00PM',time_df$First_Day_Time)

nchar(time_df$First_Day_Time[1])

for(i in 1:nrow(time_df)){
if(nchar(time_df$First_Day_Time) <13){
  time_df$First_Day_Time <-sprintf("%s9:00PM", time_df$First_Day_Time)
  time_df$First_Day_Time <-sprintf("9:00AM%s", time_df$First_Day_Time)
  time_df$First_Day_Time <-sprintf("%s:00", time_df$First_Day_Time)
  time_df$First_Day_Time<-sub('3PM-6PM', '3:00PM-6:00PM',time_df$First_Day_Time)
}}


farmers_clean$First_Day_Time<-sprintf("%s9:00PM", farmers_clean$First_Day_Time)

farmers_clean$First_Day_Time<-sprintf("9:00AM%s", farmers_clean$First_Day_Time)

farmers_clean$First_Day_Time<-sprintf("%s:00", farmers_clean$First_Day_Time)

farmers_clean$First_Day_Time<-gsub("3PM-6PM", "3:00PM-6:00PM", farmers_clean$First_Day_Time)


farmers_clean$Last_Day_Time<-sprintf("%s9:00PM", farmers_clean$Last_Day_Time)

farmers_clean$Last_Day_Time<-sprintf("9:00AM%s", farmers_clean$Last_Day_Time)

farmers_clean$Last_Day_Time<-sprintf("%s:00", farmers_clean$Last_Day_Time)

farmers_clean$Last_Day_Time<-gsub("3PM-6PM", "3:00PM-6:00PM", farmers_clean$Last_Day_Time)



## -------------------  Fix Facebook links  -------------------##

# %s is a place holder for strings 
# try using gsub 
# Add links usual format or the place holder thingy 

#I think I need i might need to use if conditions for this depending on what's already there
#if it has https..., then do nothing
#if it has www..... without https, then add it (concatenate)
#if it's only a name, then add the the link part

farmers_clean$Facebook

ht <- "https://"
ht_w <- "https://www."



for (link in farmers_clean$Facebook){
  if (link <- "http://%s" ){
    paste(link)
    
  }else if (link <- "www.%s"){
    link <- paste(ht,"www.%s" )
    
  }else if(link <- "%s" ){ #only user name
    link <- paste(ht_w, "%s")
    
  }#else if (link <- "@%s"){ #if it has @ in it 
  #link <- paste(ht_w, "%s") #we also need to remove @ #How? 
  # }
}





#####################################

check.Url(farmers_clean$Facebook[1], checkProtocol = TRUE)


##########################################

grepl(value, chars, fixed = TRUE)
##########################################

ht <- "https://"
ht_w <- "www."
user_name <-"%s"
user_name1 <- "@"

grepl(ht, link, fixed = TRUE)

###############

for (link in farmers_clean$Facebook){
  if (grepl(ht, link, fixed = TRUE)){
    paste(link)
    
  }else if (grepl(ht_w, link, fixed = TRUE)){
    link <- paste(ht,link)
    
  }else if(grepl(user_name1, link, fixed = TRUE) ){ #only user name
    link <- paste(ht,ht_w,link)
    
  }#else if (link <- "@%s"){ #if it has @ in it 
  #link <- paste(ht_w, "%s") #we also need to remove @ #How? 
  # }
}

# convert empty space into NA 
farmers_clean$Website[farmers_clean$Website == ""] <- NA

count = 1
while (count <= nrow(farmers_clean)) {
  if ( count < 10 ){
    
    paste(farmers_clean$Website[count])
    
  }
  
  count = count + 1
}


grepl(ht, farmers_clean$Website[count], fixed = TRUE)

nrow(farmers_clean)










