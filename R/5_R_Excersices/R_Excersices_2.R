## _______ Wejdan Al-Ahmadi _______##

library(tidyverse)


view(diamonds)


# ------------------------------------------------------------------- 7.3.4 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# 1. Explore the distribution of each of the x, y, and z variables in diamonds. What do you learn? Think about a diamond and how you might decide
# which dimension is the length, width, and depth.

#ANSWER: 

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=x), binwidth = 0.1)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=y), binwidth = 0.1)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=z), binwidth = 0.1)

# My observations: 
# 1) That x and y variables only reach a max of 3000, while z can reach up to 6000
# 2) There are outliers in the plots for y and z 

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=y), binwidth = 0.1)+
coord_cartesian(ylim = c(0, 50))
# after running this plot we can see the outliers in the plot for y are at: 0, ~30 and ~60

#so in order to view them more in depth I will make a pipeline filter for them

unusual <- diamonds %>% 
  filter(y < 1 | y > 30) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual

# as it was stated in chapter 7 we can see that diamonds cant have a width of zero so these values are errors, and the price of diamonds with 31.8 and 58.9 mm is very cheap so 
# this must be another error in the data

# To answer the question of which dimension is the length, width, and depth. First I need to look into the specifications for diamonds and see which values should be higher or lower and what makes sense
# since diamonds are usualy more wide than tall, the highest value would be the width and the one just below that I would consider the length, because the depth is the length/width 
# therefore it needs to be less than both

summarise(diamonds, mean(x > y), mean(y > x), mean(x > z), mean(y > z))

# here I summarized the average values of all 3 variables and compared them to each other to decide which should represent the width, length and depth
# based on these values the y should be the width and x should be the length and the depth should be z

#now to repeat the same steps to view the outliers for variable z

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=z), binwidth = 0.1)+
  coord_cartesian(ylim = c(0, 50))
# after running this plot we can see the outliers in the plot for z are at: 0 and just above 30

#so in order to view them more in depth I will make a pipeline filter for them

unusual <- diamonds %>% 
  filter(z < 1 | z > 30) %>% 
  select(price, x, y, z) %>%
  arrange(z)
unusual

# I can see some rows that have all zero values which means the diamond would be non-existent so this is probably a case of missing data
# I also notice that all the values of Z are zero, if the z represents the depth it might be an error for diamonds to have a depth of zero

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# 2. Explore the distribution of price. Do you discover anything unusual or surprising? (Hint: Carefully think about the binwidth and 
# make sure you try a wide range of values.)

#ANSWER: 

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=price), binwidth = 5)

# When i set the binwidth to 5 I can clearly see there is a gap in the first cell between 0 and 5000, I also observed that the higher the price the less diamonds there are

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=price), binwidth = 5)+
  coord_cartesian(xlim = c(0, 2000))
# now if I limit the x to be ranging between 0 and 2000 I can clearly see that at 1500 and a little bit before and after there is no data, this means 
# that at the price range from 1400 until maybe 1600 there are no diamonds at that price. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# 3. How many diamonds are 0.99 carat? How many are 1 carat? What do you think is the cause of the difference?
  
diamonds %>% filter(between(carat, .99, 1)) %>%
  group_by(carat) %>% summarize(count = n())

# there are so much more 1 carat than 0.99 carat by a substantial amount, I think this is because no one would really buy a 0.99 carat diamond
# 1 carat sounds better, so for marketing purposes they might round up the 0.99 or only produce more 1 carats as much as possible. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# 4. Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming in on a histogram. 
# What happens if you leave binwidth unset? What happens if you try and zoom so only half a bar shows?

#ANSWER: 
# I will demonstrate this using the carat to plot

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat), binwidth = .01) #this is the whole plot without any zooming in

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat), binwidth = .01) + #this is the plot zooming in on the carats that are from 0 to 1 on the x axis using the coord_cartesian
  coord_cartesian(xlim = c(0, 1))


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat), binwidth = .01) + #this is the plot zooming in on the carats that are from 0 to 1 on the x axis using the xlim
  xlim(c(0, 1))


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat), binwidth = .01) + #this is the plot zooming in on the carats thats price are from 0 to 500 on the y axis using the coord_cartesian
  coord_cartesian(ylim = c(0, 500))


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat), binwidth = .01) + #this is the plot zooming in on the carats thats price are from 0 to 500 on the y axis using the ylim
  ylim(c(0, 500))

#my final thoughts are that the coord_cartsesian shows from the range we set but also shows a little bit of data above the limit specified 
# where as the ylim or xlim completely cuts off all data after the range we specify

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat)) + #this is the plot zooming in on the carats that are from 0 to 1 on the x axis using the coord_cartesian
  coord_cartesian(xlim = c(0, 1))


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat)) + #this is the plot zooming in on the carats that are from 0 to 1 on the x axis using the xlim
  xlim(c(0, 1))


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat)) + #this is the plot zooming in on the carats thats price are from 0 to 500 on the y axis using the coord_cartesian
  coord_cartesian(ylim = c(0, 500))


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x=carat)) + #this is the plot zooming in on the carats thats price are from 0 to 500 on the y axis using the ylim
  ylim(c(0, 500))

#and if you remove the binwidth the bar's sizes all vary between the different plots and their sizes are based on the data and the limit 
# so for a more unified plotting I would put the binwidth back

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ------------------------------------------------------------------- 7.4.1 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 1. What happens to missing values in a histogram? What happens to missing values in a bar chart? Why is there a difference?
  
#ANSWER:
survey <- c("yes","no","yes","no","yes","no","yes","no",NA, NA, NA)
df <- data.frame(survey)

ggplot(data = df) + 
  geom_bar(mapping = aes(x = survey))

# NA values are included as a separate category in bar charts


numbers <- c(1,3,NA,NA,7,NA,NA)
df2 <- data.frame(numbers)


ggplot(data = df2) + 
  geom_histogram(mapping = aes(x = numbers))


# NA values are removed from histogram plots

# the reason it gets removed in histograms is because the x aesthetic variable needs to be continuous (numeric) and NA is not a number so it is dropped
# bars can plot categorical data so it considers NA as just another category


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 2. What does na.rm = TRUE do in mean() and sum()?
#ANSWER:
#it ignores/removes all the NA values in both functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# ------------------------------------------------------------------- 7.5.1.1 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 1. Use what you’ve learned to improve the visualisation of the departure times of cancelled vs. non-cancelled flights.

#ANSWER:
install.packages("nycflights13")
library(nycflights13)
flights %>% 
  mutate(cancelled = is.na(dep_time) | is.na(arr_time)) %>% 
  ggplot() +
  geom_boxplot(aes(x = cancelled, y = dep_time))
#instead of using freqplot a boxplot would give us better visualization. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 2. What variable in the diamonds dataset is most important for predicting the price of a diamond? How is that variable correlated with cut? 
# Why does the combination of those two relationships lead to lower quality diamonds being more expensive?

#ANSWER:

ggplot(diamonds) +
  geom_point(aes(x = carat, y = price), color = "red")
# I chose to use the carat as a variable to predict the price of the diamond based on what is most popularly known

ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()

#if we look at this boxplot between the carat and the cut we can see a negative relationship, the higher the carat the lower the cut it has 
# this might happen because smaller diamonds tend to require better cuts but arent always sold at a higher price 
#while a larger diamond can be profitable with a lower quality cut

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 3. Install the ggstance package, and create a horizontal boxplot. How does this compare to using coord_flip()?

#ANSWER:
install.packages("ggstance")
library(ggstance)

ggplot(diamonds) + geom_boxplot(aes(x = cut, y = carat)) + coord_flip()

ggplot(diamonds) + geom_boxploth(aes(x = carat, y = cut))

#there is no difference in both plots, I guess just two different ways to achieve the same thing the second way being more compact/simple
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 4. One problem with boxplots is that they were developed in an era of much smaller datasets and tend to display a prohibitively large number of “outlying values”. 
# One approach to remedy this problem is the letter value plot. Install the lvplot package, and try using geom_lv() to display the distribution of price vs cut. 
# What do you learn? How do you interpret the plots?

#ANSWER:
install.packages("lvplot")
library(lvplot)
ggplot(diamonds) + geom_lv(aes(x = cut, y = price))

#this displays more data and might be better for larger datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 5. Compare and contrast geom_violin() with a facetted geom_histogram(), or a coloured geom_freqpoly(). What are the pros and cons of each method?

#ANSWER:

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin() +
  coord_flip()

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

#freqpoly: 
# is better for distinguishing individual variables 
# is harder for comparing these variables with each other due to overlap

#violin: 
#its easy to tell the difference in the overall shape of the distributions 
# it cant tell us which cut is more frequent out of all the cuts of diamonds
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 6 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 6. If you have a small dataset, it’s sometimes useful to use geom_jitter() to see the relationship between a continuous and categorical variable. 
# The ggbeeswarm package provides a number of methods similar to geom_jitter(). List them and briefly describe what each one does.

#ANSWER:

# 1) geom_quasirandom() produces plots that are a mix of jitter and violin plots. There are several different methods that determine exactly how the random location of the points is generated.
# 2) geom_beeswarm() produces a plot similar to a violin plot, but by offsetting the points.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ------------------------------------------------------------------- 7.5.2.1 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 1. How could you rescale the count dataset above to more clearly show the distribution of cut within colour, or colour within cut?

#ANSWER:

diamonds %>% count(color, cut) %>% group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot() +
  geom_tile(mapping = aes(x = color, y = cut, fill = prop)) +
  labs(title = 'Distribution of cut within color')

# we calculate a new variable which is the proportion of each cut within a color 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 2. Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. What makes the plot difficult to read? 
# How could you improve it?

#ANSWER:

flights %>%
  group_by(month, dest) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")

# the destination on the y axis is not meaningful
# there are missing values in the empty white spaces

flights %>%
  group_by(month, dest) %>%                                 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%                                        
  filter(n() == 12) %>%                                     
  ungroup() %>%
  mutate(dest = reorder(dest, dep_delay)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 3. Why is it slightly better to use aes(x = color, y = cut) rather than aes(x = cut, y = color) in the example above?

#ANSWER:
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(y = color, x = cut)) +
  geom_tile(mapping = aes(fill = n))

#because it is easier to read when the larger or longer labels are on the y axis, its always easier to read horizontal labels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#





# ------------------------------------------------------------------- 7.5.3.1 Exercises ------------------------------------------------------------------- #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 1. Instead of summarising the conditional distribution with a boxplot, you could use a frequency polygon. 
# What do you need to consider when using cut_width() vs cut_number()? How does that impact a visualisation of the 2d distribution of carat and price?

#ANSWER:
ggplot(
  data = diamonds,
  mapping = aes(color = cut_number(carat, 5), x = price)
) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

#cut_width() and cut_number() both split a variable into groups. with cut_width() we choose the width and the number of bins is automatically calculated
# and with cut_number() we choose the number of bins and the width will automatically be calculated 
#as you can see above I chose cut_number and split the carats into 5 groups

ggplot(
  data = diamonds,
  mapping = aes(color = cut_width(carat, 1, boundary = 0), x = price)
) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

#when I used cut_width() there are rarely any diamonds with more than 1 carats which would result in a non informative plot and anything less than 1 
#would create too many groups and clutter up the plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 2. Visualise the distribution of carat, partitioned by price.

#ANSWER:
diamonds %>% ggplot() +
  geom_density(mapping = aes(x = carat,
                             color = cut_width(price, 5000, boundary = 0)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 3. How does the price distribution of very large diamonds compare to small diamonds? Is it as you expect, or does it surprise you?

#ANSWER:
diamonds %>% ggplot +
  geom_boxplot(mapping = aes(x = cut_number(carat, 10),
                             y = price)) +
  coord_flip()

# the price distribution of larger diamonds has a larger range of different prices compared to the smaller ones, this is interesting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 4. Combine two of the techniques you’ve learned to visualise the combined distribution of cut, carat, and price.

#ANSWER:
diamonds %>% ggplot() +
  geom_boxplot(mapping = aes(x = cut, y = price,
                             color = cut_number(carat, 5)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 5. Two dimensional plots reveal outliers that are not visible in one dimensional plots. For example, some points in the plot below have 
#an unusual combination of x and y values, which makes the points outliers even though their x and y values appear normal when examined separately.

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

# Why is a scatterplot a better display than a binned plot for this case?

#ANSWER:


ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = x, y = y), bins = 800) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

#this is the binned plot of the same plot above, with scatter plots the outliers are much more visible
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

