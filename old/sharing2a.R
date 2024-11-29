##Multiple linear regression
#let's move on to the next data set
#sometimes, it is necesary to assign row names
#especially in ordination so that the data matrix is clean
#but row names must be unique
community <- read.table("community.txt", header=TRUE, row.names=1, sep="\t")
env <- read.table("env.txt", header=TRUE, row.names=1, sep="\t")

env
community

#simple things first: is there an easy way to calculate diversity indices in R?

library(vegan)
#the thing about the vegan package is that the community matrix has to be species as columns and plots as rows
shannon <- diversity(community, index="shannon") #Simpson's also available
specrich <- specnumber(community)

#you can then try building regression models of shannon's index and species richness, etc., with environmental variables

##Ordination: Non-parametric MultiDimensional Scaling
#warning: theory-wise, this can be deep water since we do not learn multivariate statistics in undergraduate courses

grass.mds <- metaMDS(community, distance="bray")
plot(grass.mds, type="t")
#do the plots show any obvious clusters in the ordination space?
grass.clust <- hclust(vegdist(community, method="bray"))
plot(grass.clust)

#Gradient Analysis: find out the correlations of environmental variables with the ordination axes
#fits the environmental variables as a convex hull over the ordination surface
grass.envfit <- envfit(grass.mds, env, permutations=1000) #calculates p-values via permuatations
grass.envfit #degree of correlation given by r2
#see how it looks visually
plot(grass.mds, display="sites", type="t")
plot(grass.envfit, add=TRUE, col="blue")
abline(h=0, lty=2)
abline(v=0, lty=2)
#investigate the actual gradient of change
ordisurf(grass.mds, env$forest, col="green", add=TRUE)
#see why it is called a "convex hull": not always flat surface
ordisurf(grass.mds, env$moisture, col="darkgreen", add=TRUE)

#(if there's time) Species accumulation curves and estimated species richness
grass.sac <- specaccum(community)
plot(grass.sac)
specpool(community)
#if you're familiar with the code, nicer graphs are usually possible
plot(grass.sac$richness~grass.sac$sites, type="n", ylim=c(0,19), xlim=c(0,12), xlab="No. of plots surveyed", ylab="Expected cumulative species richness")
lines(c(0,grass.sac$richness)~c(0,grass.sac$sites))
lines(grass.sac$sites[-12], grass.sac$richness[-12]+grass.sac$sd[-12], lty=2)
lines(grass.sac$sites[-12], grass.sac$richness[-12]-grass.sac$sd[-12], lty=2)