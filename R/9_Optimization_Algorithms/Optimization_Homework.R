## _______ Wejdan Al-Ahmadi _______##


# ====================================================================================================
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>LINEAR OPTIMIZATIOIN<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ====================================================================================================
install.packages("lpSolve")
install.packages("cowplot")
library(lpSolve)
library(tidyverse)
library(grid)
library(gridExtra)
library(cowplot)
#_____________________ Strawberry, Orange ______________________#

objective.in1 = c (13, 23)
const.mat1 = matrix( c(5,15,4,4,35,20), nrow = 3, byrow = TRUE)
const.dir1 = c("<=", "<=", "<=")
const.rhs1 = c(480, 160, 1190)

prob_1 = lp(direction='max', objective.in1, const.mat1, const.dir1, const.rhs1)

ans_1 = prob_1$solution
ans_1
val_1 = prob_1$objval
val_1

prob_1

colnames(const.mat1) <- c("Strawberry", "Orange")
Ingredients_1 <- c("CO2", "Water", "Flavor")

Max_Supply_1 <- const.rhs1
Supply_1 <- data.frame(Ingredients_1,Max_Supply_1)

Recipe_Per_Soda_B_1 <- cbind(const.mat1, Supply_1)
Recipe_Per_Soda_B_1


for_legend_1<-ggplot(Recipe_Per_Soda_B_1, aes(fill=Ingredients_1, y=Max_Supply_1,x=interaction(Strawberry))) + 
  geom_bar(position="dodge", stat="identity", width = 1)+scale_fill_discrete(name="Ingredients")

plot1<-ggplot(Recipe_Per_Soda_B_1, aes(fill=Ingredients_1, y=Max_Supply_1,x=interaction(Strawberry))) + 
  geom_bar(position="dodge", stat="identity", width = 1, show.legend = FALSE)+ylab("Supply")+xlab("Strawberry")

plot2<-ggplot(Recipe_Per_Soda_B_1, aes(fill=Ingredients_1, y=Max_Supply_1,x=interaction(Orange))) + 
  geom_bar(position="dodge", stat="identity", width = 1, show.legend = FALSE)+ylab("Supply")+xlab("Orange")




legend_1 <- cowplot::get_legend(for_legend_1)


Batch_1_Recipe_Plot<-grid.arrange(plot1, plot2,legend_1, ncol=3)


batch_amount_1 <- round(ans_1)
batch_amount_1
batch_name_1 <- c("Strawberry", "Orange")
Profit_1 <- batch_amount_1*objective.in1
Profit_1

profit_per_batch_1 <- data.frame(batch_amount_1,Profit_1,batch_name_1)
profit_per_batch_1



Profit_1_Plot <-ggplot(profit_per_batch_1, aes(x=interaction(batch_amount_1), y=Profit_1, fill=batch_name_1)) +
  geom_bar(stat="identity")+theme_minimal()+ylab("Profit")+ 
  xlab("Number of batches") +scale_fill_discrete(name="Batches")
Profit_1_Plot
#_______________________________________________________________#

#__________________ Strawberry, Orange, Grape __________________#
# Question 1: Make improvements to LP Brewery's manufacturing schedule

# objective function info the lecture example
# Max (13 X1 + 23 X2 + 30 X3)
# Constraints matrix ---> A coefficient matrix, RHS matrix
# 5 X1 + 15 X2 + 4 X3 <= 480   <-  CO2
# 4 X1 + 4 X2 + 10 X3 <= 160   <- Water
# 35 X1 + 20 X2 +15 X3<= 1190  <- Flavor
# 5 X1 + 10 X2 + 20 X3 <= 160  <- hours of labor
# A = [5,15,4,4,4,10,35,20,15,5,10,20]
# B = [480;160;1190;800]

# lp(direction='max', objective.in, const.mat, const.dir, const.rhs)

objective.in2 = c (13, 23, 30)
const.mat2 = matrix( c(5,15,4,4,4,10,35,20,15,5,10,20), nrow =4 , byrow = TRUE)
const.dir2 = c("<=", "<=", "<=","<=")
const.rhs2 = c(480, 160, 1190,800)

prob_2 = lp(direction='max', objective.in2, const.mat2, const.dir2, const.rhs2)

ans_2 = prob_2$solution
ans_2
val_2 = prob_2$objval
val_2
prob_2

colnames(const.mat2) <- c("Strawberry", "Orange", "Grape")
Ingredients_2 <- c("CO2", "Water", "Flavor", "Hours of Labor")

Max_Supply_2 <- const.rhs2
Supply_2 <- data.frame(Ingredients_2,Max_Supply_2)

Recipe_Per_Soda_B_2 <- cbind(const.mat2, Supply_2)
Recipe_Per_Soda_B_2






for_legend_2<-ggplot(Recipe_Per_Soda_B_2, aes(fill=Ingredients_2, y=Max_Supply_2,x=interaction(Strawberry))) + 
  geom_bar(position="dodge", stat="identity", width = 1)+scale_fill_discrete(name="Ingredients")

plot3<-ggplot(Recipe_Per_Soda_B_2, aes(fill=Ingredients_2, y=Max_Supply_2,x=interaction(Strawberry))) + 
  geom_bar(position="dodge", stat="identity", width = 1, show.legend = FALSE)+ylab("Supply")+xlab("Strawberry")

plot4<-ggplot(Recipe_Per_Soda_B_2, aes(fill=Ingredients_2, y=Max_Supply_2,x=interaction(Orange))) + 
  geom_bar(position="dodge", stat="identity", width = 1, show.legend = FALSE)+ylab("Supply")+xlab("Orange")

plot5<-ggplot(Recipe_Per_Soda_B_2, aes(fill=Ingredients_2, y=Max_Supply_2,x=interaction(Grape))) + 
  geom_bar(position="dodge", stat="identity", width = 1, show.legend = FALSE)+ylab("Supply")+xlab("Grape")


legend_2 <- cowplot::get_legend(for_legend_2)


Batch_2_Recipe_Plot<-grid.arrange(plot3, plot4, plot5,legend_2, ncol=4)

batch_amount_2 <- round(ans_2)
batch_amount_2
batch_name_2 <- c("Strawberry", "Orange", "Grape")
Profit_2 <- batch_amount_2*objective.in2
Profit_2

profit_per_batch_2 <- data.frame(batch_amount_2,Profit_2,batch_name_2)
profit_per_batch_2



Profit_2_Plot <-ggplot(profit_per_batch_2, aes(x=interaction(batch_amount_2), y=Profit_2, fill=batch_name_2)) +
  geom_bar(stat="identity")+theme_minimal()+ylab("Profit")+ 
  xlab("Number of batches") +scale_fill_discrete(name="Batches")



#_______________________________________________________________#



#____________________________________________________________________________________________________




# =============================================================================================
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MARKOV CHAINS<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# =============================================================================================
install.packages("markovchain")
library(markovchain)

# Question 2: Make improvements to Memoryless Bar's supply chain decisions

#I made it so they retain 60% and either increase by 40% to the next inventory space, and I made the transition from 0 and 1 by 0.5 for each inventory 2 and 3
prob <- matrix(c(0,0,0.5,0.5,
                 0,0,0.5,0.5,
                 0,0,0.6, 0.4,
                 0,0,0.4, 0.6),
               nrow = 4, byrow = TRUE)

soda_batches <- new("markovchain", states = c('0','1','2','3'),
                    transitionMatrix = prob, name= 'Inventory')
print(soda_batches)
plot(soda_batches)


#after 1 year the inventory is:

current_inventory <- soda_batches ^12
current_inventory
round((current_inventory[1:4]),2)
