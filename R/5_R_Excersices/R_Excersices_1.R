## _______ Wejdan Al-Ahmadi _______##

installed.packages("tidyverse")
library(tidyverse)


#Excercise 3.2.4

  # 1. Run ggplot(data = mpg). What do you see?

ggplot(data = mpg) # I see an empty graph because there is no added layers to it
  
  # 2. How many rows are in mpg? How many columns?

nrow(mpg) #there are 234 rows
ncol(mpg) #there are 11 columns
    
  # 3. What does the drv variable describe? Read the help for ?mpg to find out.
  
?mpg  #the type of drive train, where f = front-wheel drive, r = rear wheel drive, 4 = 4wd

# after googling it further this variable describes the distribution of the engines power on the wheels 
# we usually have 4 types of drive trains: (All-Wheel Drive[AWD], Four-Wheel Drive[4WD], Front-Wheel Drive[FWD], and Rear-Wheel Drive [RWD])


  # 4. Make a scatterplot of hwy vs cyl.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl))
  
  #What happens if you make a scatterplot of class vs drv? Why is the plot not useful?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv)) 

#this plot is not useful because of the nature of the relationship between the class which is the type of the car and the drv which is the type of the drive train
#since vehicles cant have more than one type of drivetrain at a time this graph is useless because the relationship between its axis is one to one


#Excercise 3.3.1

  # 1. What’s gone wrong with this code? Why are the points not blue?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

#because the color is included with the aes in the mapping argument so it considers it an aesthetic and maps it as a variable and a value

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

#this is the correct way of achieving the coloring to be blue

  
  # 2. Which variables in mpg are categorical? Which variables are continuous? (Hint: type ?mpg to read the documentation for the dataset). How can you see this information when you run mpg?

?mpg

mpg

#if we run mpg in the console we can see <chr> and <int>,<dbl> above each variable the <chr> are catergorical because they are strings and 
#the <int> and <dbl> are continuous because they are numerical data


  # 3. Map a continuous variable to color, size, and shape. How do these aesthetics behave differently for categorical vs. continuous variables?

#for color: 

#continuous: 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cyl))
#this maps the same color on a gradient scale based on the numbers (from light to dark)

#categorical: 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = manufacturer))
#this maps the colors by giving each categorical variable a different color

#for shape: 

#continuous: 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = cyl))
#unlike size and color it really shows here that you cannot map continuous variables to shape because there is no logical way of ordering shapes 
#for colors and sizes the numbers can be used as a scale or a gradient but for shapes you cant decide which one goes where based on a number


#categorical: 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = manufacturer))
#the shape works well with categories however there is only six shapes so all the extra category of manufacturer get deleted, unless you specify them manually


#for size: 

#continuous: 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cyl))
#this maps each size based on the numerical value, so the smaller the number the smaller the size and vice versa for larger sizes

#categorical: 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = manufacturer))
#this gives each manufacturer category a size based on alphabetical order so if the string started with A it gets the smallest size

    
  # 4. What happens if you map the same variable to multiple aesthetics?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = cyl, color = cyl))
#it is just redundant and too much would make the plot messy and not very pleasing to the eye
    
  # 5. What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point)
?geom_point

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = manufacturer),stroke =1)
#Stroke is used to change the size of the border of your point, the bigger the number the thicker the border

  
  # 6. What happens if you map an aesthetic to something other than a variable name, like aes(colour = displ < 5)? Note, you’ll also need to specify x and y.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color=displ<5))
#as we can see in the plot this created a logical condition and reflected it on the color, so everything less than 5 is one color(blue here) and everything greater is 
#another color (pink here) in regards to the displ

#Excercise 3.5.1

# 1. What happens if you facet on a continuous variable?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+
facet_grid(.~ cyl)

# it divided the sub plots into categories based on the values of the continuous variable


# 2. What do the empty cells in plot with facet_grid(drv ~ cyl) mean? How do they relate to this plot?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))+
facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

mpg

#the empty cells represent the lack of data that matches up between drv and cyl, so if we look at cyl 4 it has no record with drv r therefore their cell is empty 
#another one would be cyl 5 and drv 4 or drv r there is no record of these two being correlated and this is reflected in the graph

# 3. What plots does the following code make? What does . do?
  
  ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

#the '.' notation is like an ignore, its used for if we want to facet on one variable, if we place it after the '~' it will remove all the column attributes and only leave the rows

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

#as mentioned previously this is an ignore that is used when we only want to facet against either a row or a column , and in the case of the '.' being before the '~' symbol then 
#it will plot them on columns without rows


# 4. Take the first faceted plot in this section:
  
  ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
  
#What are the advantages to using faceting instead of the colour aesthetic? What are the disadvantages? How might the balance change if you had a larger dataset?

#some of the advantages of faceting are: 
  # 1) it is more distint and easier to distinguish
  # 2) less is more, the less colors makes it easier to observe
  # 3) its easier to observe and derive insights for each class seperatly 
  # 4) the more classes you have the harder it becomes to plot them with colors because they start to blend into each other, this doesnt happen with facets
  
#some of the dis-advantages of faceting are: 
  # 1) comparing across all classes becomes more difficult because when they are all clustered in one plot it makes it easier to observe where all the points are across the same x and y grid
  # 2) with colored aesthetics you can easily tell the different classes at first glance if they arent too many
  # 3) the larger the data the harder it might be to derive insights from so many individual little plots instead of one big clustered one that compares all data points
  
  # However the balance lies in the amount of data we have the more classes we have the more colors we need so the facet might be better in distinguishing, 
  # but the more data we have the harder it might be to compare across all the data using facets
  
  
# 5. Read ?facet_wrap. What does nrow do? What does ncol do? What other options control the layout of the individual panels? Why doesn’t facet_grid() have nrow and ncol arguments?
  ?facet_wrap

  # the nrow defines the number of rows that facet is plotted on and the ncol is the number of columns that facet is plotted on 
  # we need these two arguments in the wrap more than the grid because the wrap only has one variable it plots against so we need to know how many to plot, 
  # while grids have 2 variables and the number of rows and cols are determined by the value
# other options to control the layout include: 
 
  #scales	
  #Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
    
  #shrink	
  #If TRUE, will shrink scales to fit output of statistics, not raw data. If FALSE, will be range of raw data before statistical summary.
  
  #switch	
  #By default, the labels are displayed on the top and right of the plot. If "x", the top labels will be displayed to the bottom. If "y", the right-hand side labels will be displayed to the left. Can also be set to "both".
  
  #drop	
  #If TRUE, the default, all factor levels not used in the data will automatically be dropped. If FALSE, all factor levels will be shown, regardless of whether or not they appear in the data.
  
  #dir	
  #Direction: either "h" for horizontal, the default, or "v", for vertical.
  
  
# 6. When using facet_grid() you should usually put the variable with more unique levels in the columns. Why?
  ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_grid(class ~ drv)

  ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_grid(drv ~ class)
  
  #if you run the two plots above you can see that the second one is more readable because the columns have more unique values
  
#Excercise 3.6.1
# 1. What geom would you use to draw a line chart? A boxplot? A histogram? An area chart?
  # a combination of geom_point and geom_line, I put the mappings in the ggplot to avoid duplication
  
  ggplot(data = mpg,mapping = aes(x = displ, y = hwy)) + 
    geom_point() + 
    geom_line()
  
# 2. Run this code in your head and predict what the output will look like. Then, run the code in R and check your predictions.
  
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
    geom_point() + 
    geom_smooth(se = FALSE)
#I thought the line would be continuous instead of 3 seperate lines, but I can see why its seperated by color but even than I expected it to be one line that changes colors gradually
  
# 3. What does show.legend = FALSE do? What happens if you remove it? Why do you think I used it earlier in the chapter?

  # show.legend = FALSE hides the legend box
  
  #this is the code in the chapter earlier
  ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy))
  
  ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
 
   ggplot(data = mpg) +
    geom_smooth(
      mapping = aes(x = displ, y = hwy, color = drv),
     ) #when I remove it it displays the legends
  
  ggplot(data = mpg) +
    geom_smooth(
      mapping = aes(x = displ, y = hwy, color = drv),
      show.legend = FALSE)
  #after running all three without the show.legend I noticed none of them have legends except the last one, so the legends were removed in the last plot for consistency
  
# 4. What does the se argument to geom_smooth() do?
  ?geom_smooth
 #se	:  Display confidence interval around smooth? (TRUE by default, see level to control.)
 #its a toggle to turn the gray area behind the lines on and off

# 5. Will these two graphs look different? Why/why not?
  
  ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_point() + 
    geom_smooth()
  
  ggplot() + 
    geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
  
  #no they will not look different, because in the first plot they will get the data and mappings from the ggplot by inheritence, basically its two different ways to achieve the same goal
  #the first might be preferred in case you want to change the geom type but keep the same data
  
# 6. Recreate the R code necessary to generate the following graphs.
   # 1) 
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point() +
    geom_smooth(se = FALSE)
  
  # 2)
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_smooth(mapping = aes(group = drv), se = FALSE) +
    geom_point()
  
  # 3)
  
  ggplot(mpg, aes(x = displ, y = hwy, colour = drv)) +
    geom_point() +
    geom_smooth(se = FALSE)
  
  # 4)
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(colour = drv)) +
    geom_smooth(se = FALSE)
  
  # 5)
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(colour = drv)) +
    geom_smooth(aes(linetype = drv), se = FALSE)
  
  # 6) 
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(size = 4, color = "white") +
    geom_point(aes(colour = drv))
  
  
#Excercise 3.7.1
# 1. What is the default geom associated with stat_summary()? How could you rewrite the previous plot to use that geom function instead of the stat function?
    ?stat_summary
  #the default geom for stat_summary() is geom = "pointrange"
  
  #the previous plot 
  ggplot(data = diamonds) +
    stat_summary(
      mapping = aes(x = cut, y = depth),
      fun.min = min,
      fun.max = max,
      fun = median
    )
  
  ggplot(data = diamonds) +
    geom_pointrange(
      mapping = aes(x = cut, y = depth),
      stat = "summary",
      fun.min = min,
      fun.max = max,
      fun = median)

  # we used stat = summary instead of stat = identity to use the summary
  
# 2. What does geom_col() do? How is it different to geom_bar()?
  ?geom_col()
    # geom_col() is a type of bar chart, it is used to represent the values in the data as the height of the bars
    # geom_bar() makes the height of the bar proportional to the number of cases for each group
    # geom_bar()'s default stat is count this counts the number of cases at each x position 
    # geom_col()'s default stat is identity which leaves data as it is

# 3. Most geoms and stats come in pairs that are almost always used in concert. Read through the documentation and make a list of all the pairs. What do they have in common?
     geoms <- c('geom_bar()',
                'geom_bin2d()',	
                'geom_boxplot()',
                'geom_contour_filled()',	
                'geom_contour()',
                'geom_count()',
                'geom_density_2d()',
                'geom_density()',
                'geom_dotplot()',
                'geom_function()',
                'geom_sf()',
                'geom_sf()',
                'geom_smooth()',	
                'geom_violin()',
                'geom_hex()',
                'geom_qq_line()',
                'geom_qq()',
                'geom_quantile()')
     
     stat <- c('stat_count()',
               'stat_bin_2d()',
               'stat_boxplot()',
               'stat_contour_filled()',
               'stat_contour()',
               'tat_sum()',
               'stat_density_2d()',
               'stat_density()',
               'stat_bindot()',
               'stat_function()',
               'stat_sf()',
               'stat_sf()',
               'stat_smooth()',
               'stat_ydensity()',
               'stat_bin_hex()',
               'stat_qq_line()',
               'stat_qq()',
               'stat_quantile()')
     pairs <- data.frame(geoms, stat)
     
     #the names are what they have in common this is used for easy documentation, for example: geom_bin2d() maps to stat_bin_2d()
     
# 4. What variables does stat_smooth() compute? What parameters control its behaviour?
     
     #https://www.rdocumentation.org/packages/ggplot2/versions/0.9.0/topics/stat_smooth
     
     #variables it computes: 
       #a: data.frame with additional columns
       #y: predicted value
       #ymin: lower pointwise confidence interval around the mean
       #ymax: upper pointwise confidence interval around the mean
       #se: standard error
    
     #parameters that control its behavior include: 
     
      #method : smoothing method (function) to use, eg. lm, glm, gam, loess, rlm. For datasets with n < 1000 default is loess. For datasets with 1000 or more observations defaults to gam, see 
      #formula : formula to use in smoothing function, eg. y ~ x, y ~ poly(x, 2), y ~ log(x)
      #se : display confidence interval around smooth? (TRUE by default, see level to control
      #fullrange : should the fit span the full range of the plot, or just the data
      #level : level of confidence interval to use (0.95 by default)
      #n : number of points to evaluate smoother at
      #na.rm : If FALSE (the default), removes missing values with a warning. If TRUE silently removes missing values.
     
     
     
# 5. In our proportion bar chart, we need to set group = 1. Why? In other words what is the problem with these two graphs?
    
    ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, y = after_stat(prop), group =1))
    
  ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop), group = 1))
  
  #when we remove the group = 1 all the bars heights are equal to 1, because geom_bar assumes that the groups are equal to the x values 

#Excercise 3.8.1.
  
# 1. What is the problem with this plot? How could you improve it?
  
  ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
    geom_point()
  
  # there is alot of overlap between cty and hwy so we are losing data visulization 
  
  ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
    geom_point(position = "jitter")
  
  # adding some jitter to the plot helps spread the data more randomly so we can see the points of condensation more clearly 
  
# 2. What parameters to geom_jitter() control the amount of jittering?
  ?geom_jitter
  
  # the width which controls the horizontal displacement and the height which controls the vertical displacement 
  
# 3. Compare and contrast geom_jitter() with geom_count().
  
  ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_jitter()
  
  #geom jitter adds randomized placements to the data, this may shift the x and y values to a certain degree but it aids in observations 
  
  ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_count()
  
  #geom count only changes the size of the data at points of condensation so the data remains more accurate
  
# 4. What’s the default position adjustment for geom_boxplot()? Create a visualisation of the mpg dataset that demonstrates it.
  
  ?geom_boxplot
#the default pisition is dodge2, this moves the geom horizontally to avoid overlap 
  ?position_dodge2
  
  ggplot(data = mpg, aes(x = displ, y = hwy, colour = class)) +
    geom_boxplot()
  