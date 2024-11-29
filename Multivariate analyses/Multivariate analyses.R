#######################################
# R Workshops - Multivariate analysis #
#          Dr. Chong Kwek Yan         #
#              3 Jan 2020             #
#######################################

# Part I: Principal Components Analysis (PCA)

#Load the lake information and physical parameters datasets

#As always, make sure you set the working directory accordingly first
lakeinfo<-read.csv("NLA2007_SampledLakeInformation_20091113.csv", header=TRUE)
env<-read.csv("NLA2007_WaterQuality_20091123.csv", header=TRUE)

lakeinfo<-lakeinfo[lakeinfo$VISIT_NO==1,] # Filter for first visit
env<-env[env$VISIT_NO==1&env$SAMPLE_CATEGORY=="P",] # Filter for first visit the primary sample taken

env<-env[,c(
        "SITE_ID",
        "PH_LAB", 
        "COND", 
        "TURB", 
        "DOC",
        "NTL_PPM",
        "PTL", 
        "CL_PPM", 
        "SO4_PPM", 
        "CA_PPM", 
        "MG_PPM",
        "NA_PPM",
        "K_PPM"
)] # Select these columns only
env<-data.frame(env, row.names=1) #make the first column (the lake ID) the row identifier so that the table only contains variables

## Intercorrelated predictors

#Total Nitrogen versus total Phosphorous
plot(NTL_PPM~PTL, data=env) # Doesn't look like there's any relationship?
plot(log(NTL_PPM)~log(PTL), data=env) # After transformation

NPmod<-lm(log(NTL_PPM)~log(PTL), data=env)

#Is there a way of reflecting both N and P as a single value?
#Let's plot a best fit line between log(N) and log(P)
#and rotate the plot until the best fit line is the horizontal axis

library(rgl)

with(env, plot3d(log(PTL), log(NTL_PPM), rep(0, length(PTL)), zlab="", type="p", size=10))
lines3d(
  x=log(range(env$PTL)),
  y=predict(NPmod, newdata=list(PTL=range(env$PTL))),
  z=rep(0, length(env$PTL)),
  col="red", lwd=5, add=TRUE
) # Try rotating the plot--carefully!

#Explore the case of intercorrelation between three variables
with(env, plot3d(log(NTL_PPM), log(PTL), log(K_PPM), type="s", radius=0.2, col="grey"))

#What if many variables are correlated?

windows()

pairs(env, upper.panel = NULL) # pairwise plot
env.log<-apply(env, 2, log10) # log-transform every column
pairs(env.log, upper.panel = NULL)

## Take a look at a subset the data

lakeinfo.sub<-lakeinfo[lakeinfo$STATE_NAME=="Idaho",] # Filter for lakes in the state of Idaho
env.sub<-env[rownames(env) %in% lakeinfo.sub$SITE_ID,] # Filter for the unique lake numbers of the subsetted dataset

## Conduct PCA

env.pca<-prcomp(env.sub, scale=TRUE) # scale=TRUE means the variables are divided by s.e.
                                      # should always be used

par(mfrow=c(1,2))
biplot(env.pca, choice=1:2)
biplot(env.pca, choice=3:2) # Each "serial number" represents a lake (in Idaho)
                            # There are a few lakes much further away from the rest of the lakes

#From univariate to multivariate normality

par(mfrow=c(4,4), mar=c(2,3,1,0.5), mgp=c(2,1,0))
for(i in 1:ncol(env.sub)) hist(env.sub[,i], main=names(env.sub)[i]) # This is just a less painful way of typing hist(...) for every variable

env.log.sub<-env.log[rownames(env) %in% lakeinfo.sub$SITE_ID,]

par(mfrow=c(4,4), mar=c(2,3,1,0.5), mgp=c(2,1,0))
for(i in 1:ncol(env.sub)) hist(env.log.sub[,i], main=names(env.log.sub)[i]) # Most histograms look symmetrically bell-shaped after log-transformation

env.log.pca<-prcomp(env.log.sub, scale=TRUE)

par(mfrow=c(2,2))
biplot(env.pca, choice=1:2)
biplot(env.pca, choice=3:2)
biplot(env.log.pca, choice=1:2) # These are the PCA with log-transformed variables
biplot(env.log.pca, choice=3:2) # The data points are now much more evenly spread out

env.log.pca$rotation # "Loadings" of each variable on each PCA axis

env.log.pca$x # "Scores" of each data point in PCA space, i.e., the coordinates in each PCA axis

## How many axes in the PCA are "important"?

plot(env.log.pca) # Screeplot

env.log.pca$sdev^2/sum(env.log.pca$sdev^2) # Variance explained by each PCA axis
                                            # Note that sum(env.log.pca$sdev^2) equals the number of variables

## Prettier biplot

par(mfrow=c(1,1), mar=c(4,4,2,2), mgp=c(2,1,0))
plot(env.log.pca$x[,1], env.log.pca$x[,2], xlab="PCA 1", ylab="PCA 2", pch=16, col="gray", cex=1.5)

mult=5 # length multiplier for the arrows

arrows(0, 0, env.log.pca$rotation[,1]*mult, env.log.pca$rotation[,2]*mult, length=0.1, col="blue")
identify(env.log.pca$rotation[,1]*mult, env.log.pca$rotation[,2]*mult, colnames(env.log), col="blue", cex=0.8)
axis(3, at=axTicks(1), labels = axTicks(1)/mult, col="blue")
axis(4, at=axTicks(2), labels = axTicks(2)/mult, col="blue")

## Prettier screeplot

varex<-env.log.pca$sdev^2/sum(env.log.pca$sdev^2)

plot(varex, ylab="Variance explained", xlab="PCA axis")
lines(1:length(varex), varex)

## How do you use a PCA? 1. Use the first (few) axis as the response

cbind(as.character(lakeinfo.sub$SITE_ID), rownames(env.sub)) # First check that rows match

t.test(env.log.pca$x[,1]~lakeinfo.sub$LAKE_ORIGIN) # Are man-made versus natural lakes different?
t.test(env.log.pca$x[,2]~lakeinfo.sub$LAKE_ORIGIN)

t.test(env.log.pca$x[,1]~lakeinfo.sub$URBAN) # Are urban versus non-urban lakes different?
t.test(env.log.pca$x[,2]~lakeinfo.sub$URBAN)

anova(aov(env.log.pca$x[,1]~LAKE_ORIGIN*URBAN, data=lakeinfo.sub)) # 2-way ANOVA

## Incorporating categorical variables into the PCA

env.log.sub2<-cbind(env.log.sub, 
      NATURAL=as.numeric(lakeinfo.sub$LAKE_ORIGIN)-1, # Make the categorical variables into 0's and 1's
      URBAN=as.numeric(lakeinfo.sub$URBAN)-1
      )

env.log.pca2<-prcomp(env.log.sub2, scale=TRUE)

natural.col<-ifelse(env.log.sub2[,"NATURAL"]==1, "green", "red") # natural lakes are plotted as green, man-made as red
urban.pch<-ifelse(env.log.sub2[,"URBAN"]==1, 17, 16) # urban lakes are plotted as triangles, non-urban as circles

par(mfrow=c(1,2), mar=c(4,4,2,2), mgp=c(2,1,0))

plot(env.log.pca$x[,1], env.log.pca$x[,2], xlab="PCA 1", ylab="PCA 2", pch=urban.pch, col=natural.col, cex=1.5)
arrows(0, 0, env.log.pca$rotation[,1]*mult, env.log.pca$rotation[,2]*mult, length=0.1, col="blue")
text(env.log.pca$rotation[,1]*mult, env.log.pca$rotation[,2]*mult, colnames(env.log), col="blue", cex=0.8)
axis(3, at=axTicks(1), labels = axTicks(1)/mult, col="blue")
axis(4, at=axTicks(2), labels = axTicks(2)/mult, col="blue")

plot(env.log.pca2$x[,1], env.log.pca2$x[,2], xlab="PCA 1", ylab="PCA 2", pch=urban.pch, col=natural.col, cex=1.5)
arrows(0, 0, env.log.pca2$rotation[,1]*mult, env.log.pca2$rotation[,2]*mult, length=0.1, col="blue")
text(env.log.pca2$rotation[,1]*mult, env.log.pca2$rotation[,2]*mult, colnames(env.log), col="blue", cex=0.8)
axis(3, at=axTicks(1), labels = axTicks(1)/mult, col="blue")
axis(4, at=axTicks(2), labels = axTicks(2)/mult, col="blue")

# Part II: Non-metric multidimensional Scaling (NMDS)

#Load the algae dataset

algae<-read.csv("NLA2007_Phytoplankton_SoftAlgaeCount_20091023.csv", header=TRUE)
algae<-algae[algae$VISIT_NO==1,]

algae$SITE_ID<-as.character(algae$SITE_ID)
algae$TAXANAME<-as.character(algae$TAXANAME)

algae.sub<-algae[algae$SITE_ID %in% lakeinfo.sub$SITE_ID,] # Let's just use a subset of the data first

algae.sub.com<-xtabs(ABUND~SITE_ID+TAXANAME, data=algae.sub) # Cross-tabulating to get a site-by-species table
algae.sub.com<-as.data.frame.matrix(algae.sub.com)

library(vegan) # This library is very useful for analysis of community data

algae.div<-diversity(algae.sub.com, index = "shannon") # Calculate Shannon Diversity with a snap of your fingers
                                                      # Try the argument index="Simpson"
                                                      # or the function specnumber(algae.sub.com)

## How do you use a PCA? 2. Use the first (few) axis as predictors

par(mfrow=c(4,4), mar=c(3,3,0.5,0.5), mgp=c(2,1,0))
for(i in 1:ncol(env.log.sub2)) plot(algae.div~env.log.sub2[,i], xlab=colnames(env.log.sub2)[i], ylab="Shannon Diversity", col=natural.col, pch=urban.pch)
plot(algae.div~env.log.pca2$x[,1], col=natural.col, pch=urban.pch, xlab="PCA 1", ylab="Shannon Diversity")
plot(algae.div~env.log.pca2$x[,2], col=natural.col, pch=urban.pch, xlab="PCA 2", ylab="Shannon Diversity")

summary(lm(algae.div~env.log.pca2$x[,1])) # Fit a simple regression model

#Let's try PCA on the algae communities themselves

algae.com<-xtabs(ABUND~SITE_ID+TAXANAME, data=algae) # Let's cross-tabulate for all lakes
algae.com<-as.data.frame.matrix(algae.com)

dim(algae.com) # Check the number of dimensions. Makes sense for PCA? Why didn't I use the subset?
algae.com[1:5,1:5] # Preview part of the site-by-species table

par(mfrow=c(2,3), mar=c(3,3,2,1)) # We're going to try a series of transformations before PCA

## Untransformed

algae.pca<-prcomp(algae.com, scale=TRUE)
plot(algae.pca$x[,1], algae.pca$x[,2], pch=16, col="gray", xlab="PCA 1",  ylab="PCA 2", main="Untransformed")

## Will log-transformation help?

algae.log.pca<-prcomp(log(algae.com+1), scale=TRUE) # Many zeroes in abundances, and can't take the logarithm of zero
plot(algae.log.pca$x[,1], algae.log.pca$x[,2], pch=16, col="gray", xlab="PCA 1",  ylab="PCA 2", main="Log")

## Will using proportional abundances help?

algae.prop.pca<-prcomp(decostand(algae.com, method="total"), scale=TRUE)
plot(algae.prop.pca$x[,1], algae.prop.pca$x[,2], pch=16, col="gray", xlab="PCA 1",  ylab="PCA 2", main="Proportion")

## Will a "Hellinger transformation" help?

algae.hell.pca<-prcomp(decostand(algae.com, method="hellinger"), scale=TRUE)
plot(algae.hell.pca$x[,1], algae.hell.pca$x[,2], pch=16, col="gray", xlab="PCA 1",  ylab="PCA 2", main="Hellinger (Proportion + square-root)")

## Will a "Wisconsin double-standardisation" help?

algae.wis.pca<-prcomp(wisconsin(algae.com), scale=TRUE)
plot(algae.wis.pca$x[,1], algae.wis.pca$x[,2], pch=16, col="gray", xlab="PCA 1",  ylab="PCA 2", main="Wisconsin")

## Will square-root transformation after Wisconsin double-standardisation help?

algae.wis2.pca<-prcomp(sqrt(wisconsin(algae.com)), scale=TRUE)
plot(algae.wis2.pca$x[,1], algae.wis2.pca$x[,2], pch=16, col="gray", xlab="PCA 1",  ylab="PCA 2", main="Wisconsin + square-root")


# Non-metric Multidimensional Scaling

algae.mds<-metaMDS(algae.sub.com, distance="bray", k=2) # "Compressed" into two dimensions

algae.mds$stress # "Distortion" from the compression to a much smaller number of dimensions

par(mfrow=c(1,2))
plot(algae.mds, type="n")
points(algae.mds, display = "sites", pch=urban.pch, col=natural.col)
plot(algae.mds, type="n")
text(algae.mds, display = "species", col="blue", cex=0.5)

## "Relieving stress"

algae.mds3<-metaMDS(algae.sub.com, distance="bray", k=3) # Increasing the number of dimensions to three

algae.mds3$stress # Stress improves by more than 5 percentage points

par(mfrow=c(2,2))
plot(algae.mds3, type="n", choices=c(1,2), ylim=c(-1,1.5))
points(algae.mds3, display = "sites", choices=c(1,2), pch=urban.pch, col=natural.col)
plot(algae.mds3, type="n", choices=c(3,2), ylim=c(-1,1.5))
points(algae.mds3, display = "sites", choices=c(3,2), pch=urban.pch, col=natural.col)
plot(algae.mds3, type="n", choices=c(1,2), ylim=c(-1,1.5))
text(algae.mds3, display = "species", choices=c(1,2), col="gray", cex=0.5)
plot(algae.mds3, type="n", choices=c(3,2), ylim=c(-1,1.5))
text(algae.mds3, display = "species", choices=c(3,2), col="gray", cex=0.5)

# Using NMDS in "indirect gradient analysis"

cbind(as.character(lakeinfo.sub$SITE_ID), rownames(env.sub), rownames(algae.sub.com)) # Check again that the rows match

(algae.envfit.log<-envfit(algae.mds3, env.log.sub2, permutations=999, choices=1:3)) # What if we used the two-dimensional NMDS?
                                                                # What if we used un-transformed environmental data?

mult=2 # Reduce the multiplier for the arrow otherwise it would overshoot the plotting region

par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(algae.mds3, type="n", choices=c(1,2))
points(algae.mds3, display = "sites", choices=c(1,2), pch=urban.pch, col=natural.col)
plot(algae.envfit.log, choices=c(1,2), add=TRUE, p.max = 0.05, arrow.mul=mult) # p.max is the argument that helps to filter out non-significant variables
plot(algae.mds3, type="n", choices=c(3,2))
points(algae.mds3, display = "sites", choices=c(3,2), pch=urban.pch, col=natural.col)
plot(algae.envfit.log, choices=c(3,2), add=TRUE, p.max = 0.05, arrow.mul=mult)
plot(algae.mds3, type="n", choices=c(1,2), ylim=c(-1,1.5))
text(algae.mds3, display = "species", choices=c(1,2), col="gray", cex=0.5)
plot(algae.envfit.log, choices=c(1,2), add=TRUE, p.max = 0.05, arrow.mul=mult)
plot(algae.mds3, type="n", choices=c(3,2), ylim=c(-1,1.5))
text(algae.mds3, display = "species", choices=c(3,2), col="gray", cex=0.5)
plot(algae.envfit.log, choices=c(3,2), add=TRUE, p.max = 0.05, arrow.mul=mult)
  # What do the results mean?

# Exercise!

#Using the grass.xls dataset,
#1. Carry out a PCA on the environmental parameters. Is there a point in doing so?
#2. Conduct NMDS and 'indirect gradient analysis' on the community data.

###########
#   END   #
###########