#Revision: let's load the mangrove data. Remember to change the directory!!
mangrove <- read.table("mangrove.txt", header=TRUE, sep="\t")

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

#conditional subsets
mangrove$survival=="Y" #this is a logical vector
mangrove$survival[mangrove$survival=="Y"] #this selects elements according to the logical vector
mangrove$species[mangrove$survival=="Y"]

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
plot(fresh~height, data=mangrove, subset=mangrove$species=="bgymnorhiza", ylab="Fresh weight /g", xlab="Salinity", pch=1, col="blue")
points(fresh~height, data=mangrove, subset=mangrove$species=="bcylindrica", pch=4, col="red")
#Similarly, some points are out of the plotting region. Change the y-axis limits.
plot(fresh~height, data=mangrove, subset=mangrove$species=="bgymnorhiza", ylab="Fresh weight /g", xlab="Salinity", pch=1, col="blue", ylim=c(0,100))
points(fresh~height, data=mangrove, subset=mangrove$species=="bcylindrica", pch=4, col="red")

#you can add legends using the legend() function.

#correlations
cor(mangrove$height, mangrove$fresh)
#oops! Doesn't work. Why? Missing numbers again.
cor(mangrove$height, mangrove$fresh, use="complete.obs")
cor.test(mangrove$height, mangrove$fresh, use="complete.obs")
#there is also kendall's and spearman's correlations for non-parametric
cor(mangrove$height[mangrove$species=="bgymnorhiza"], mangrove$fresh[mangrove$species=="bgymnorhiza"], use="complete.obs")
cor(mangrove$height[mangrove$species=="bcylindrica"], mangrove$fresh[mangrove$species=="bcylindrica"], use="complete.obs")

#last note: scripts make life easier in R!