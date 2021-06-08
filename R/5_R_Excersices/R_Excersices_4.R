## _______ Wejdan Al-Ahmadi _______##

install.packages("maps")
library(tidyverse)
library(nycflights13)
library(maps)


# ------------------------------------------------------------------- 13.2.1  Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Imagine you wanted to draw (approximately) the route each plane flies from its origin to its destination. 
#What variables would you need? What tables would you need to combine?

#ANSWER: 
#to a draw a map of flights from the origin to its destination we need the longitude and the latitude for each the origin and the destination

#the two tables we need are the flights (it contains the origin and dest) and the airports (it contains the lon and lat)

#we need to inner join on these two tables twice, to get the lon and lat for each the origin and the dest


flights_latlon <- flights %>%
  inner_join(select(airports, origin = faa, lat_origin = lat, lon_origin = lon),
             by = "origin"
  ) %>%
  inner_join(select(airports, dest = faa, lat_dest = lat, lon_dest = lon),
             by = "dest"
  )

flights_latlon %>%
  slice(1:100) %>%
  ggplot(aes(
    x = lon_origin, xend = lon_dest,
    y = lat_origin, yend = lat_dest
  )) +
  borders("state") +
  geom_segment(arrow = arrow(length = unit(0.1, "cm"))) +
  coord_quickmap() +
  labs(y = "Latitude", x = "Longitude")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#I forgot to draw the relationship between weather and airports. What is the relationship and how should it appear in the diagram?

#ANSWER:
View(weather)
View(airports)

# airports column "faa" is a foriegn key of weathers column "origin"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#weather only contains information for the origin (NYC) airports. If it contained weather records for all airports in the USA, 
#what additional relation would it define with flights?

View(weather)
View(flights)

#if we included all weathers for all airports in the USA then the flights will have information about the weather for the origin and the destination
#we would add the destination in the weather table and use it with the composite foreign key for the flights

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#We know that some days of the year are "special", and fewer people than usual fly on them. How might you represent that data as a data frame? 
#What would be the primary keys of that table? How would it connect to the existing tables?


special_holidays <- tribble(
  ~year, ~month, ~day, ~holiday,
  2021, 01, 01, "New Years Day",
  2021, 02, 07, "Super Bowl Sunday",
  2021, 02, 12, "Chinese New Years",
  2021, 02, 14, "Valentines Day",
  2021, 03, 08, "International Women's Day",
  2021, 03, 17, "St. Patrick's Day",
  2021, 04, 12, "Ramadan",
  2021, 05, 05, "Cinco de Mayo",
  2021, 05, 09, "Mother's Day",
  2021, 07, 04, "Independence Day",
  2021, 09, 31, "Halloween",
  2021, 11, 29, "Thanksgiving Day",
  2021, 12, 25, "Christmas Day"
)

#the primary key would be the (year, month, day) and can be used with that to join with the other tables


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# ------------------------------------------------------------------- 13.3.1  Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Add a surrogate key to flights.

#ANSWER: 
flight2<- flights %>%
  mutate(flight_id = row_number(), .before = year)

View(flight2)

#I added a flight id as the row number and made it so its at the beginning of the table by specifying its column to be before "year"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Identify the keys in the following datasets

#Lahman::Batting
#babynames::babynames
#nasaweather::atmos
#fueleconomy::vehicles
#ggplot2::diamonds

#(You might need to install some packages and read some documentation.)

#ANSWER: 

#first we install the packages
install.packages("Lahman")
install.packages("babynames")
install.packages("nasaweather")
install.packages("fueleconomy")
install.packages("ggplot2")
# 1- Lahman::Batting

View(Lahman::Batting)

Lahman::Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1) %>%
  nrow()

# the documentation for this package is: https://lahman.r-forge.r-project.org/doc/Batting.html

#as we can see the player id and the year id isnt enough to uniquely identify as players can have different appearances within a season on different teams


# 2- babynames::babynames

View(babynames::babynames)

babynames::babynames %>%
  count(year, sex, name) %>%
  filter(n > 1) %>%
  nrow()

#the name and year isnt enough to uniquely identify the babies the sex is also needed because the names are counted for each sex so the different sex can have the same name

# 3- nasaweather::atmos

View(nasaweather::atmos)

nasaweather::atmos %>%
  count(lat, long, year, month) %>%
  filter(n > 1) %>%
  nrow()

#for this table the primary key is the lat, lon, year, month

# 4- fueleconomy::vehicles

View(fueleconomy::vehicles)

fueleconomy::vehicles %>%
  count(id) %>%
  filter(n > 1) %>%
  nrow()

#the id column is the primary key

# 5- ggplot2::diamonds

View(ggplot2::diamonds)

ggplot2::diamonds %>%
  distinct() %>%
  nrow()

nrow(ggplot2::diamonds)

#after checking the table there is no primary key, furthermore after checking the distinct rows and comparing it against the total rows, we can see we have dupes

#the best solution is to mutate a new column for the ID's and have the row number be its value

diamonds <- mutate(ggplot2::diamonds, id = row_number(), .before = carat)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Draw a diagram illustrating the connections between the Batting, People, and Salaries tables in the Lahman package. 
#Draw another diagram that shows the relationship between People, Managers, AwardsManagers.

#How would you characterise the relationship between the Batting, Pitching, and Fielding tables?

# Master : PK = playerID
# Batting : PK = playerID, yearID, stint. FK = playerID = Master$playerID (n-to-1)
# Salaries: PK = yearID, teamID, playerID. FK = playerID = Master$playerID (n-to-1)

#I created the schema and uploaded it to this URL: https://ibb.co/hFxgngy


# Master : PK = playerID
# Managers: PK = yearID, teamID, inseason. FK = playerID = Masters$playerID (n-to-1)
# AwardsManagers: PK = playerID, awardID, yearID. FK = playerID = Master$playerID (n-to-1)

#I created the schema and uploaded it to this URL: https://ibb.co/yFQHwN5

# Batting : PK = playerID, yearID, stint.
# Pitching: PK = playerID, yearID, stint. FK = playerID, yearID, stint = Batting$playerID, Batting$yearID, Batting$stint (n-to-1)
# Fielding: PK = playerID, yearID, stint, POS. FK = playerID, yearID, stint = Batting$playerID, Batting$yearID, Batting$stint (n-to-1)

#I created the schema and uploaded it to this URL: https://ibb.co/YDKN8wq

nrow(anti_join(Lahman::Pitching, Lahman::Batting,
               by = c("playerID", "yearID", "stint")
))

nrow(anti_join(Lahman::Fielding, Lahman::Batting,
               by = c("playerID", "yearID", "stint")
))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# ------------------------------------------------------------------- 13.4.6  Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. 
#Here's an easy way to draw a map of the United States:

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#(Don't worry if you don't understand what semi_join() does - you'll learn about it next.)
#You might want to use the size or colour of the points to display the average delay for each airport.

#ANSWER: 

avg_dest_delays <-  flights %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))

avg_dest_delays %>%
  ggplot(aes(lon, lat, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Add the location of the origin and destination (i.e. the lat and lon) to flights.

#ANSWER: 

airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa"),
    suffix = c("_origin", "_dest")
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Is there a relationship between the age of a plane and its delays?

#ANSWER: 

plane_data <- inner_join(flights,
                            select(planes, tailnum, plane_year = year),
                            by = "tailnum"
) %>%
  mutate(age = year - plane_year) %>%
  filter(!is.na(age))  %>%
  group_by(age) %>%
  summarise(
    dep_delay_mean = mean(dep_delay, na.rm = TRUE),
    dep_delay_sd = sd(dep_delay, na.rm = TRUE),
    arr_delay_mean = mean(arr_delay, na.rm = TRUE),
    arr_delay_sd = sd(arr_delay, na.rm = TRUE),
    n_arr_delay = sum(!is.na(arr_delay)),
    n_dep_delay = sum(!is.na(dep_delay))
  )

#first we join flighst and planes to get all the data we need and then we calculate the average and standard deviation for both delays as well as their sum

#after we extract all the data we need we plot the planes age with the average delay to see if there is any relation 

ggplot(plane_data, aes(x = age, y = dep_delay_mean)) +
  geom_point() + ylab("Departure Delay (minutes)") + xlab("Plane age (Years)")

ggplot(plane_data, aes(x = age, y = arr_delay_mean)) +
  geom_point() + ylab("Arrival Delay (minutes)") + xlab("Plane age (Years)")

#we can see that the delay times spike when the plane reaches 10 years and then it starts going back down, maybe this is due to older planes being out of service
# or not being used as much or older planes have better time management with all the maintanence they need

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#What weather conditions make it more likely to see a delay?

#ANSWER: 

w1 <- weather %>% mutate(id = row_number())

w2 <- flights %>% inner_join(w1) %>%
  group_by(id) %>% summarise(ave_delay = mean(arr_delay, na.rm = TRUE), n_flights = n()) %>%
  left_join(w1)
#first we join the two tables to extract the data we need for the plot

w3 <- w2 %>% gather(key = weather_var, value = weather_value, temp:visib) %>%
  select(id, n_flights, ave_delay, weather_var, weather_value )
#then we filter even further to just get the average delay with the weather types 

ggplot(w3) +
  geom_smooth(aes(x = weather_value, y = ave_delay)) +
  facet_wrap(~weather_var, scales = "free")
#lastly we make our plots to compare between all weather conditions, the most weather conditions that affect the delay are: precip, pressure, visib and wind_dir. 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.

#ANSWER: 

flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(y = lat, x = lon, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis()

#here is the link to the wiki page that fact checks what happened on that date: https://en.wikipedia.org/wiki/June_12%E2%80%9313,_2013_derecho_series

#but simply put there was a lot of storms in the south east region of USA, the plot above shows that the delays were largest in Tennessee, southeast and midwest
#and those are the locations of where the storms happened
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ------------------------------------------------------------------- 13.5.1  Exercises ------------------------------------------------------------------- #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#What does it mean for a flight to have a missing tailnum? What do the tail numbers that don't have a matching record in planes have in common? 
#(Hint: one variable explains ~90% of the problems.)

#ANSWER: 

flights %>%
  filter(is.na(tailnum), !is.na(arr_time)) %>%
  nrow()

#the flights who have a missing tailnum also dont have an arr_time, I concluded from this that these flights were cancelled. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Filter flights to only show flights with planes that have flown at least 100 flights.

#ANSWER: 

planes_100 <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n >= 100)
#first we find all the planes that have flown at least 100 flights 

flights %>%
  semi_join(planes_100, by = "tailnum")
#then join that with the flights table

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models.

#ANSWER: 
fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by = c("make", "model"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?

#ANSWER: 

worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)

#first we get the 48 hours with the worst delays

weather_most_delayed <- semi_join(weather, worst_hours, 
                                  by = c("origin", "year",
                                         "month", "day", "hour"))

#then join with weather 

ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) +
  geom_point()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? 
#What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?

#ANSWER: 

# 1) 
anti_join(flights, airports, by = c("dest" = "faa")) %>% 
  distinct(dest)
#this returns the flights that went to airports not listed in the FAA, since the FAA has domestic airports, these are probably foreign flights. 

# 2) 
anti_join(airports, flights, by = c("faa" = "dest"))

#this returns the US airports that planes didnt fly to as a destination. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 6 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#You might expect that there's an implicit relationship between plane and airline, because each plane is flown by a single airline. 
#Confirm or reject this hypothesis using the tools you've learned above.

#ANSWER: 

planes_carriers <-
  flights %>%
  filter(!is.na(tailnum)) %>%
  distinct(tailnum, carrier)
#first we find all distinct airlibe/plane pairs

planes_carriers %>%
  count(tailnum) %>%
  filter(n > 1) %>%
  nrow()
#we filter by the planes who flew more than one airline using the tailnum

carrier_transfer_table <- planes_carriers %>%
  group_by(tailnum) %>%
  filter(n() > 1) %>%
  left_join(airlines, by = "carrier") %>%
  arrange(tailnum, carrier)
#now we join with the airlines to get the name
carrier_transfer_table
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


