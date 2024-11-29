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
#now explore the data

names(mangrove)

#if you forgot what objects you have
ls()

#practice selecting rows and columns?
mangrove$height
mangrove$salinity
mangrove$species
mangrove$treatment
#etc.

#data frames store most text entries as "factors" and not strings
class(mangrove$survival)
class(mangrove$treatment)
class(mangrove$salinity)
class(mangrove$height)

#you can coerce some forms of data into another, just overwrite it
mangrove$salinity <- as.numeric(mangrove$salinity)

#how many elements are there in your objects?
length(mangrove) #using length() on a dataframe gives the number of columns
length(mangrove$survival) #number of rows in the column

#conditional subsets
mangrove$survival=="Y" #this is a logical vector
mangrove$survival[mangrove$survival=="Y"] #this selects elements according to the logical vector
mangrove$species[mangrove$survival=="Y"]

#so, how many survived?
length(mangrove$species[mangrove$survival=="Y"])

#mean and standard deviation
mean(mangrove$dry)
#missing values! use argument na.rm=TRUE
mean(mangrove$dry, na.rm=TRUE)
sd(mangrove$dry, na.rm=TRUE) #so, what's standard error?

#mean and standard deviation of a subset
mean(mangrove$dry[mangrove$species=="bgymnorhiza"], na.rm=TRUE)
sd(mangrove$dry[mangrove$species=="bgymnorhiza"], na.rm=TRUE)

#fortunately, there's an easier way to do all this
tapply(mangrove$dry, mangrove[,1:2], mean, na.rm=TRUE)
tapply(mangrove$dry, mangrove[,1:2], sd, na.rm=TRUE)
tapply(mangrove$dry, mangrove[,1:2], length)

#applying calculations by the matrix
all.n <- tapply(mangrove$survival, mangrove[,1:2], length)
survived.n <- tapply(mangrove$survival[mangrove$survival=="Y"], mangrove[mangrove$survival=="Y",1:2], length)
proportion <- survived.n/all.n

#plot this as a barplot
barplot(proportion, beside=TRUE)
barplot(t(proportion), beside=TRUE)
barplot(t(proportion), beside=TRUE, ylim=c(0,1))

##exploring your data graphically
#histograms: once again, google it if not sure
hist(mangrove$height)

#plot only a subset of the data, e.g. only Bruguiera gymnorhiza
mangrove$species=="bgymnorhiza"
mangrove$height[mangrove$species=="bgymnorhiza"] #select only those TRUE
hist(mangrove$height[mangrove$species=="bgymnorhiza"], main="B. gymnorhiza", xlab="Height class /cm")
hist(mangrove$height[mangrove$species=="bcylindrica"], main="B. cylindrica", xlab="Height class /cm")

#plot two graphs side by side
par(mfrow=c(2,1))
hist(mangrove$height[mangrove$species=="bgymnorhiza"], main="B. gymnorhiza", xlab="Height class /cm")
hist(mangrove$height[mangrove$species=="bcylindrica"], main="B. cylindrica", xlab="Height class /cm")

#box plots
par(mfrow=c(1,2), mar=c(4.5,4,1,1))
boxplot(fresh~treatment, data=mangrove, subset=mangrove$species=="bgymnorhiza", names=c("Treatment", "Control"), xlab="B. gymnorhiza", ylab="Fresh weight /g")
boxplot(fresh~treatment, data=mangrove, subset=mangrove$species=="bcylindrica", names=c("Treatment", "Control"), xlab="B. cylindrica", ylab="Fresh weight /g")

#you can present them as levels of factors
par(mfrow=c(2,1), mar=c(3.5,3.5,0.8,1), mgp=c(2,0.8,0))
boxplot(height~salinity+treatment, data=mangrove, subset=mangrove$species=="bgymnorhiza", cex.axis=0.8, xlab="B. gymnorhiza", ylab="Height /cm")
boxplot(height~salinity+treatment, data=mangrove, subset=mangrove$species=="bcylindrica", cex.axis=0.8, xlab="B. cylindrica", ylab="Height /cm")

#scatterplots
plot(mangrove$fresh~mangrove$height)

#plot only a subset of the data
mangrove[mangrove$species=="bgymnorhiza",] #select only the rows where $species is TRUE
plot(fresh~height, data=mangrove[mangrove$species=="bgymnorhiza",], ylab="Fresh weight /g", xlab="Height /cm", sub="B. gymnorhiza")
plot(fresh~height, data=mangrove[mangrove$species=="bcylindrica",], ylab="Fresh weight /g", xlab="Height /cm", sub="B. cylindrica")

#too ugly! we want them on the same plot, but using different symbols and colours
par(mfrow=c(1,1))
plot(fresh~height, data=mangrove[mangrove$species=="bgymnorhiza",], pch=1, col="blue", ylab="Fresh weight /g", xlab="Height /cm")
points(fresh~height, data=mangrove[mangrove$species=="bcylindrica",], pch=4, col="red")
#hmm, some points look squeezed up near the origin. Change the y- and x-axis limits.
plot(fresh~height, data=mangrove[mangrove$species=="bgymnorhiza",], pch=1, col="blue", ylab="Fresh weight /g", xlab="Height /cm", xlim=c(0,45), ylim=c(0,100))
points(fresh~height, data=mangrove[mangrove$species=="bcylindrica",], pch=4, col="red")

#plot only a subset of the data; many ways in R to do the same thing
plot(fresh~salinity, data=mangrove, subset=mangrove$species=="bgymnorhiza", ylab="Fresh weight /g", xlab="Salinity", pch=1, col="blue")
points(fresh~salinity, data=mangrove, subset=mangrove$species=="bcylindrica", pch=4, col="red")
#Similarly, some points are out of the plotting region. Change the y-axis limits.
plot(fresh~salinity, data=mangrove, subset=mangrove$species=="bgymnorhiza", ylab="Fresh weight /g", xlab="Salinity", pch=1, col="blue", ylim=c(0,100))
points(fresh~salinity, data=mangrove, subset=mangrove$species=="bcylindrica", pch=4, col="red")

#you can add legends using the legend() function.

##executing basic statistical tests:
#t tests
t.test(mangrove$fresh~mangrove$species)
#alternatively,
t.test(height~species, data=mangrove)

#but wait! Do the data satisfy the conditions for using t-tests?
#one of the conditions is normality, which we can investigate graphically using histograms

par(mfrow=c(2,2))
hist(mangrove$height[mangrove$species=="bgymnorhiza"], xlab="Height classes /cm", main="B. gymnorhiza")
hist(mangrove$height[mangrove$species=="bcylindrica"], xlab="Height classes /cm", main="B. cylindrica")
hist(mangrove$height[mangrove$treatment=="t"], xlab="Height classes /cm", main="Hormone Treatment")
hist(mangrove$height[mangrove$treatment=="u"], xlab="Height classes /cm", main="Control Treatment")

#let's assume you don't want to risk using parametric tests
#you can try a classical non-parametric test like Wilcoxon's ranked sums test (also known as Mann-Whitney's test)
wilcox.test(height~treatment, data=mangrove, subset=mangrove$species=="bgymnorhiza")
wilcox.test(height~treatment, data=mangrove, subset=mangrove$species=="bcylindrica")

#ANalysis Of VAriance for multiple comparisons
aov(mangrove$height~mangrove$salinity)

summary(aov(mangrove$height~mangrove$salinity)) #directly call a summary or

salinity.aov <- aov(mangrove$height~mangrove$salinity)
summary(salinity.aov) #save the model as an object then call a summary on the object

#can we do post-hoc tests like in SPSS and Minitab?
TukeyHSD(salinity.aov)
#sometimes functions require the data to be in a certain format
salinity.aov <- aov(mangrove$height~as.factor(mangrove$salinity))
TukeyHSD(salinity.aov)

#correlations
cor(mangrove$height, mangrove$fresh)
#oops! Doesn't work. Why? Missing numbers again.
cor(mangrove$height, mangrove$fresh, use="complete.obs")
cor.test(mangrove$height, mangrove$fresh, use="complete.obs")
#there is also kendall's and spearman's correlations for non-parametric
cor(mangrove$height[mangrove$species=="bgymnorhiza"], mangrove$fresh[mangrove$species=="bgymnorhiza"], use="complete.obs")
cor(mangrove$height[mangrove$species=="bcylindrica"], mangrove$fresh[mangrove$species=="bcylindrica"], use="complete.obs")

#simple linear regression
all.lm <- lm(fresh~height, data=mangrove)
summary(all.lm)

#these models contain lots of goodies inside
names(all.lm)

all.lm$coefficients #or simply "all.lm$coeff"

#draw a best fit line from this linear regression through the scatterplot
abline(all.lm$coeff, lty=2)

#you can fit separate lines for each species too.
gym.lm <- lm(fresh~height, data=mangrove, subset=mangrove$species=="bgymnorhiza")
cyl.lm <- lm(fresh~height, data=mangrove, subset=mangrove$species=="bcylindrica")

abline(gym.lm$coeff, col="blue")
abline(cyl.lm$coeff, col="red")

#bar charts are alot more troublesome in r, but will cover in detail in the next session

all.mean <- tapply(mangrove$dry, mangrove[,1:2], mean, na.rm=TRUE)
all.sd <- tapply(mangrove$dry, mangrove[,1:2], sd, na.rm=TRUE)
all.se <- all.sd/sqrt(survived.n)

bar <- barplot(t(all.mean), beside=TRUE, ylim=c(0,16), ylab="Dry weight /g")
arrows(bar, t(all.mean)-t(all.se), bar, t(all.mean)+t(all.se), angle=90, code=3)

#last note: scripts make life easier in R!