#to draw random numbers from a distribution function,
#e.g., the Normal distribution

rnorm(10) #standard Normal distribution, i.e., mean=0, variance=1

rnorm(30)
rnorm(50)
rnorm(100)
rnorm(200)

par(mfrow=c(2,2))
hist(rnorm(30))
hist(rnorm(50))
hist(rnorm(100))
hist(rnorm(200))

#or a uniform distribution

runif(10) #default min=0, max=1

par(mfrow=c(2,2))
hist(runif(100))
hist(runif(100))
hist(runif(100))
hist(runif(100))

#what if you wanted to choose a random number from 1 to 10?

1:10

sample(1:10) #default is sampling WITHOUT replacement

sample(1:10, replace=TRUE) #sampling with replacement

#This allows me to choose a subset from a larger set
#with each element an equal probability of being selected

#Example: Sherry's seedling
#She wants to choose 24 seedlings of species A and 24 seedlings of species B
#for each round

A<-read.table(file="clipboard", sep="\t", header=TRUE)

B<-read.table(file="clipboard", sep="\t", header=TRUE)

round1<-data.frame(
	seedling=c(
		as.character(sample(A$Seedling.code, 24)),
		as.character(sample(B$Seedling.code, 24))
		),
	species=c(rep("A",24),rep("B",24))
	)

#note therefore that each time I run the code
#I would get a different set of seedlings selected

#when should I use sampling with replacement?
#e.g., when I am shuffling the order

round1[sample(1:nrow(round1)),]

#Sherry is laying out her 48 seedlings in a 12x4 grid

matrix(round1[sample(1:nrow(round1)),]$seedling, nrow=12, ncol=4)

#are "random" numbers in R really random?
#there are 2147483647*2+1 "seeds" in R
#each will generate a different result
#using the same seed will generate the same result

par(mfrow=c(2,2))
set.seed(11082015)
hist(runif(100))
set.seed(11082015)
hist(runif(100))
set.seed(11082015)
hist(runif(100))
set.seed(11082015)
hist(runif(100))

#you can assume there is no relationship
#between the output and the seed used
#you can think of seeds as pre-generated random numbers
#or "pseudo-random" numbers
#every time you execute a function requiring a random draw, R uses a seed
#in the past versions based on the date/time
#now based on the "process ID"

#if you really wanted to be anal about randomness
#go to random.org to draw an integer
#(the website is limited to between -1000000000 to +1000000000)
#and use this as the seed

#this also allows you to "control" the "random" output without bias

set.seed(-404935572)
a<-sample(A$Seedling.code, 24)

set.seed(992462935) #you have to set a seed for every separate random run
b<-sample(B$Seedling.code, 24)

round1<-data.frame(
	seedling=c(
		as.character(a),
		as.character(b)
		),
	species=c(rep("A",24),rep("B",24))
	)

set.seed(-631164436)
matrix(round1[sample(1:nrow(round1)),]$seedling, nrow=12, ncol=4)

#this way, you can set the seed before every
#NMDS run
#bootstrap run
#Monte Carlo run e.g., used in PERMANOVA, envfit(), etc.
#and get exactly the same results as you did before
#BUT: try at least a few seeds
#to make sure that you have not arrived a local minima by random chance

##END of sharing

##continuing on: Sherry's experimental design

#3 factors

#1. type of competition (3 levels: inter, intra, control)
#note: 2 replicates
#and 2 possible seedlings of the other species to pair with for interspecific

comp<-c("inter-1", "inter-2", "intra", "intra", "control","control")

#2. flooding regime (2 levels: flooded, not flooded)

flooding<-c("flooded","drained")

#3. type of soil (2 levels: wet type soil, dry type soil)

soil<-c("wet","dry")

#all possible combinations for EACH seedling species

expand.grid(comp,flooding,soil)

#shuffle the order of treatments...

set.seed(987527506)
A.combis<-expand.grid(comp,flooding,soil)[sample(1:24),]

#...and repeat again for the other species of seedlings

set.seed(-103012066)
B.combis<-expand.grid(comp,flooding,soil)[sample(1:24),]

rbind(A.combis, B.combis)

round1<-cbind(round1, rbind(A.combis, B.combis))

set.seed(-631164436)
matrix(round1[sample(1:nrow(round1)),]$seedling, nrow=12, ncol=4)