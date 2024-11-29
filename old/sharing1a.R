##R as a calculator

1+2
(1-2*3)/4
5^2

#memory in R: you can go back again to edit the previous commands
##storing objects in R

#use <- or -> to assign an object
a<-12345
b<-54321
c<-a+b

#spaces don't matter, and there is no restriction on what you want to call it
my.calculation <- c/a+b

#but R is caps-sensitive
My.Calculation <- a/(c+b)

##R is more than just a calculator: has built in functions
sqrt(25)
exp(2) #also known as e^x
log(a, base=2) #default log(x) is log(x, base=exp(1)), also known as ln
log10(a) #log with base 10
sum(a, b, c)

#to find out how to use a function, use help
?log

#to find out which function to use to do something, use google!

##how R handles stats: data types
#vectors
c(a, b, c, 11, 22, 33) #c stands for concatenate, produces a column vector
y <- c(a, b, c, 11, 22, 33) #store vectors as objects

seq(1, 10, 0.1) #generates a sequence of numbers from 1 to 10 in intervals of 0.1
x1 <- seq(0.5, 3, 0.5)

rep(NA, 10) #generates a repeated sequence of "NA"s 10 times
x2 <- c(rep(1, 3), rep(2,3)) #concatenate 2 vectors one after another

#vectors make it easy to handle a string of calculations
y+x1
y*x2

#data usually looks more familiar in matrix form
eg.matrix <- cbind(y, x1, x2)

#square brackets to select rows, columns, and specific cells in the matrix

eg.matrix[1,]
eg.matrix[,2]
eg.matrix[5,3]

#but data is better handled in "data frames"
eg.data <- data.frame(eg.matrix)

eg.matrix$y #so that columns and rows can be given names and be selected by names
eg.matrix$x1
eg.matrix$x2 #but square brackets can still be used to select rows, cols and elements

##the most difficult thing in R: how to prepare data and load them in
#easiest way: prepare the matrix in excel, copy, and paste into notepad!
#next most important thing: change the directory! do it manually

#hint again: use google if you forget the function!
#use ?read.table for details of the function

read.table("mangrove.txt", header=TRUE, sep="\t")

#successful? store it into an object now!
mangrove <- read.table("mangrove.txt", header=TRUE, sep="\t")

#congrats! you've passed the most difficult part in R. seriously.
