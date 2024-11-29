#Practice, practice, practice: load the data
mangrove <- read.table("mangrove.txt", header=TRUE, sep="\t")

##Simple linear regression

#let's asking a stupid question:
#is height of propagules a function of fresh weight?

all.mod <- lm(height~fresh, data=mangrove)
summary(all.mod)
#interpret the results:
#some 80% of variation in height is explained by fresh mass
#the slope of the best fit line is 0.44964,
#and this slope is significantly different from 0 (p-value<2x10^-16)
#the intercept is 12.37811 (usually significance is disregarded), etc.

#the fitted model contains many goodies!

names(all.mod)
all.mod$coefficients #alternatively, just "coeff" if the short form is unique

#use this to plot a straight line through the scatterplot to visualize the relationship
plot(height~fresh, data=mangrove)
abline(all.mod$coeff, col="red")

#you can fit separate lines for each species too.
plot(fresh~height, data=mangrove, subset=mangrove$species=="bgymnorhiza", ylab="Fresh weight /g", xlab="Salinity", pch=1, col="blue", ylim=c(0,100))
points(fresh~height, data=mangrove, subset=mangrove$species=="bcylindrica", pch=4, col="red")

gym.lm <- lm(fresh~height, data=mangrove, subset=mangrove$species=="bgymnorhiza")
cyl.lm <- lm(fresh~height, data=mangrove, subset=mangrove$species=="bcylindrica")

abline(gym.lm$coeff, col="blue")
abline(cyl.lm$coeff, col="red")