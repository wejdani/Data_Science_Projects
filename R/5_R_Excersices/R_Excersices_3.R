## _______ Wejdan Al-Ahmadi _______##

library(tidyverse)


# ------------------------------------------------------------------- 12.2.1 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Using prose, describe how the variables and observations are organised in each of the sample tables.

#ANSWER: 

table1
#In this table the category is the country and the year for each row and the changing values are the case and population populated into seperate columns

table2
#this table seperates the cases and population into its own row in regards to the same country/year 

table3
#this table has the case over the population stored as a string for each country year pair

table4a
#this table is the first part of the dataset and has each country as a row and the values of the cases under their year as a column

table4b
#this table is the second part of the dataset and has each country as a row and the values of the population under their year as a column

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Compute the rate for table2, and table4a + table4b. You will need to perform four operations:

#1) Extract the number of TB cases per country per year.
#2) Extract the matching population per country per year.
#3) Divide cases by population, and multiply by 10000.
#4) Store back in the appropriate place.

#Which representation is easiest to work with? Which is hardest? Why?

#ANSWER: 

table2

#to extract the number of cases per country per year we filter 
table2_cases <- filter(table2, type=="cases") %>%
  rename(cases = count)
table2_cases

#to extract the number of population per country per year we filter 
table2_population <- filter(table2, type=="population")%>%
  rename(population = count)
table2_population


#now we create a new data frame and join both tables together and mutate to add another column with the cases per capita
table2_cases_per_capita<- data.frame(year=table2_cases$year, country = table2_cases$country, cases = table2_cases$cases, population = table2_population$population) %>%
  mutate(cases_per_capita = (cases/population)*10000)

table2_cases_per_capita

#now we store it back into table2 and arrange by country
bind_rows(table2, table2_cases_per_capita) %>%
  arrange(country, year, type, count)


table4a
table4b

table4c <-
  data.frame(
    country = table4a$country,
    `1999` = table4a[["1999"]] / table4b[["1999"]] * 10000,
    `2000` = table4a[["2000"]] / table4b[["2000"]] * 10000
  )
table4c

#I joined the two tables in one dataframe and did the calculations on the columns and stored it in table4c

#both tables were not easy to work with table2 had the values seperated in rows and table4 had the values seperated in two whole new tables,
#but I preferred table4 as it was more simpler to approach

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?

#ANSWER:


#first we need to filter to only include the cases rows
table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country)) +
  ylab("cases")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# ------------------------------------------------------------------- 12.3.3 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
#  Carefully consider the following example:
  
  stocks <- tibble(
    year   = c(2015, 2015, 2016, 2016),
    half  = c(   1,    2,     1,    2),
    return = c(1.88, 0.59, 0.92, 0.17)
  )
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

#(Hint: look at the variable types and think about column names.)

#pivot_longer() has a names_ptypes argument, e.g.  names_ptypes = list(year = double()). What does it do?
  
#ANSWER: 
#to understand this issue first we need to define and differentiate between pivot_long and pivot_wide

#pivot_longer() stacks multiple columns into one column, this can be an issue when you have multiple data types they all get converted into the type that would lose the least data

#pivot_wider() creates column names from values in column and the names will be treated as a charcter

stocks %>%
  pivot_wider(names_from = year, values_from = return) 
#if we take the pivot wider alone we can see that the table created a dataframe with the column name as years and the return is the value of that column

stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")
#however pivot longer unpivots the table and returns it to the tidy data frame

#but we can see that year now has the data type as character instead of numeric 
stocks %>%
  pivot_wider(names_from = year, values_from = return)%>%
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return",
               names_ptype = list(year = double()))

#to attempt to solve this the names_ptype wont work because it doesnt convert the year from character to numeric and it will result in an error

stocks %>%
  pivot_wider(names_from = year, values_from = return)%>%
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return",
               names_transform = list(year = as.numeric))

#so instead we can use names_transform to give us our wanted result
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#Why does this code fail?
  
  table4a %>% 
  pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")
#> Error: Can't subset columns that don't exist.
#> ✖ Locations 1999 and 2000 don't exist.
#> ℹ There are only 3 columns.

#ANSWER: 
#it fails because the column names 1999 and 2000 are numeric and not strings/characters, so tidyverse treats them as numbers and tries to get the 
#1999th column which does not exist, to solve this we can use backticks `` or convert the column names to strings
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  
#What would happen if you widen this table? Why? How could you add a new column to uniquely identify each value?
    
    people <- tribble(
      ~name,             ~names,  ~values,
      #-----------------|--------|------
      "Phillip Woods",   "age",       45,
      "Phillip Woods",   "height",   186,
      "Phillip Woods",   "age",       50,
      "Jessica Cordero", "age",       37,
      "Jessica Cordero", "height",   156
    )
    
people
#ANSWER: 
 people %>% pivot_wider(names_from = "name", values_from = "values")
#after applying the pivot_wider on the data set it gave us columns with lists of data for each the name and names, but this isnt a good way to uniquely 
 #identify rows
 
 people2 <- people %>%
   group_by(name, names) %>%
   mutate(pair_count = row_number())
 
 people2
 
 #to make a unique row that can add some value to our data set we need to group by the name, names pair and then mutate a row and count how many there are
 #here we can see we have two rows who have the name "Philip Woods" and the names value as age
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
 #Tidy the simple tibble below. Do you need to make it wider or longer? What are the variables?
   
   preg <- tribble(
     ~pregnant, ~male, ~female,
     "yes",     NA,    10,
     "no",      20,    12
   )
   preg
   
   #ANSWER: 
   #the variables in this dataset are: 
   # 1) gender of male or female 
   # 2) pregnancy status (in case of female either yes or no, in case of males always no)
   # 3) count the number of each category
   
   preg_tidy2 <- preg %>%
     pivot_longer(c(male, female), names_to = "gender", values_to = "count", values_drop_na = TRUE)
   preg_tidy2
   #first lets diffrentiate by gender and drop the NA because obviously males cannot get pregnant 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# ------------------------------------------------------------------- 12.4.3 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # What do the extra and fill arguments do in separate()? Experiment with the various options for the following two toy datasets.
   
   tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
     separate(x, c("one", "two", "three"))
   
   tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
     separate(x, c("one", "two", "three"))
   
   #ANSWER: 
   
   #the extra argument tells the seperate function what to do when we have too much data and the fill tells it what to do when we dont have enough 
   
   #extra demonstrations: 
   tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
     separate(x, c("one", "two", "three"), extra = "drop")
   #the extra = "drop" does the same as the default but it just doesnt produce a warning message in the console
   
   tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
     separate(x, c("one", "two", "three"), extra = "merge")
   
   #the extra = "merge" merges the extra data so you can see in the "three" column the second row the f,g are concatinated 
   
   #fill demonstrations: 
   tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
     separate(x, c("one", "two", "three"), fill = "right")
   #as we can see the "d,e" has one less value than the rest so the fill will add an NA in the place of the missing value 
   #and adding the fill = "right" will fill the missing data from the right
   
   tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
     separate(x, c("one", "two", "three"), fill = "left")
   
   #the fill = "left" produces a similar result to the previous except it fills from the left with NA
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
   # Both unite() and separate() have a remove argument. What does it do? Why would you set it to FALSE?
   
   #ANSWER: 
   ?unite()
   ?separate()
   
   #this argument removes the input column from the output data frame, you would set it to false if you want to keep it
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Compare and contrast separate() and extract(). Why are there three variations of separation (by position, by separator, and with groups), but only one unite?
   
   #ANSWER: 
   #because when we want to seperate either by using separate() or extract() we have one column that is split into numerous columns and theres many ways of splitting them 
   
   #separate examples: 
   # with separaters: 
   tibble(age_group = c("0_14", "15_25", "26_35", "36_45")) %>%
     separate(age_group, c("from", "to"), sep = "_")
   # with positions: 
   tibble(age_group = c("0014", "1525", "2635", "3645")) %>%
     separate(age_group, c("from", "to"), sep = c(2))
   
   #extract examples: 
   # with separaters: 
   tibble(plane_seats = c("A_1", "A_2", "A_3", "A_4")) %>%
     extract(plane_seats, c("Seat_Letter", "Seat_Number"), regex = "([A])_([1-4])")
   # with positions: 
   tibble(plane_seats = c("A1", "A2", "A3", "A4")) %>%
     extract(plane_seats, c("Seat_Letter", "Seat_Number"), regex = "([A])([1-4])")
   #something only extract can do: 
   tibble(plane_seats = c("AA1", "AB2", "A33", "A44")) %>%
     extract(plane_seats, c("Seat_Letter", "Seat_Number"), regex = "([A-B]+)([1-4]+)")
   
   
   #with unite() you have many columns that you want to integrate into one column but they can only be combined in one way 
   tibble(letter = c("A", "B", "C", "D"), number = c(1, 2, 3, 4)) %>%
     unite(plane_seats, letter, number, sep = "_")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# ------------------------------------------------------------------- 12.5.1 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
   #Compare and contrast the fill arguments to pivot_wider() and complete().
   
   #ANSWER: 
   # the fill argument in both functions replace NA values, they both accept lists to set values for each column
   # the fill in the pivot_wider() can also accept single values
   # they both replace implicit and explicit missing data
   
   #some examples using the dataset from the lecture: 
   stocks <- tibble(
     year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
     qtr    = c(   1,    2,    3,    4,    2,    3,    4),
     return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
   )
   
   #pivot_wider(): 
   stocks %>% 
     pivot_wider(names_from = year, values_from = return,
                 values_fill = 0)
   #complete(): 
   stocks %>% 
     complete(year, qtr, fill=list(return=0))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
   #What does the direction argument to fill() do?
   
   #ANSWER: 
   #it determines wether NA values should be replaced by the previous value ("down") *cant be NA* or the next value("up") * also cant be NA* 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# ------------------------------------------------------------------- 12.6.1 Exercises ------------------------------------------------------------------- #

   
   who1 <- who %>% 
     pivot_longer(
       cols = new_sp_m014:newrel_f65, 
       names_to = "key", 
       values_to = "cases", 
       values_drop_na = TRUE
     )
   who1
   # -------------------------------------------------------------------#
   who2 <- who1 %>% 
     mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
   who2
   # -------------------------------------------------------------------#
   who3 <- who2 %>% 
     separate(key, c("new", "type", "sexage"), sep = "_")
   who3
   # -------------------------------------------------------------------#  
   who4 <- who3 %>% 
     select(-new, -iso2, -iso3)
   # -------------------------------------------------------------------#
   who5 <- who4 %>% 
     separate(sexage, c("sex", "age"), sep = 1)
   who5
   # -------------------------------------------------------------------#
   who6 <- who5 %>% 
     separate(age, c("age_group_from", "age_group_to"), sep = -2)
   who6
   # -------------------------------------------------------------------#
   who %>%
     pivot_longer(
       cols = new_sp_m014:newrel_f65, 
       names_to = "key", 
       values_to = "cases", 
       values_drop_na = TRUE
     ) %>% 
     mutate(
       key = stringr::str_replace(key, "newrel", "new_rel")
     ) %>%
     separate(key, c("new", "var", "sexage")) %>% 
     select(-new, -iso2, -iso3) %>% 
     separate(sexage, c("sex", "age"), sep = 1)
   # -------------------------------------------------------------------#
   i
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
   #In this case study I set values_drop_na = TRUE just to make it easier to check that we had the correct values. Is this reasonable? 
   #Think about how missing values are represented in this dataset. Are there implicit missing values? What’s the difference between an NA and zero?
   
   #ANSWER: 
   #removing or keeping NA depends on the meaning of it within a dataset, so for example it could mean that there are no TB cases for that specific country
   #or it could mean that there is no record of wether or not there are any cases at all, in the first case its best to keep the NA and in the second 
   #it is best to remove it
   
   #how can we tell if our dataset's NA is for missing values or for representing a non case of TB for that record? lets first check the zeros
   who1 %>%
     filter(cases == 0) %>%
     nrow()
   #we can see that we have zeros, so these zeros are used to indicate the cases where they dont have TB, therefore we can deduce that NA is for missing records
   
   
   #now lets check if the country year pairs have any missing data
   pivot_longer(who, c(new_sp_m014:newrel_f65), names_to = "key", values_to = "cases") %>%
     group_by(country, year) %>%
     mutate(missing = sum(is.na(cases)) / n()) %>%
     filter(missing > 0, missing < 1)
   
   #we can see that each country/year contains some missing data
   
   #lets now check implicit missing data 
   
   num_rows <- nrow(who)
   
   num_com <- who %>% 
     complete(country, year) %>%
     nrow()
   
   num_com - num_rows
   
   #the result is 206 missing implicit data cases
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
   #What happens if you neglect the mutate() step? (mutate(names_from = stringr::str_replace(key, "newrel", "new_rel")))
   
   #ANSWER: 
   who1 %>%
     separate(key, c("new", "type", "sexage"), sep = "_")
   #we get a warning saying that missing pieces were filled with NA 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
   #I claimed that iso2 and iso3 were redundant with country. Confirm this claim.
   
   #ANSWER: 
   select(who3, country, iso2, iso3) %>%
     distinct() %>%
     group_by(country) %>%
     filter(n() > 1)
   
   #the way we can tell if its redundant or not is to check if each country has one unique abbreviation for both categories (iso2, iso3)
   #which after running this select we prove to be the case
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
   #For each country, year, and sex compute the total number of cases of TB. Make an informative visualisation of the data.
   
   #ANSWER: 
   
   who5 %>%
     group_by(country, year, sex) %>%
     filter(year > 1995) %>%
     summarise(cases = sum(cases)) %>%
     unite(country_sex, country, sex, remove = FALSE) %>%
     ggplot(aes(x = year, y = cases, group = country_sex, colour = sex)) +
     geom_point()
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
   
   
   
