##Revision: Preparing and loading data

#always remember to change working directory first!
mangrove <- read.table("mangrove.txt", header=TRUE, sep="\t")

#sometimes, it is necesary to assign row names
#especially in ordination so that the data matrix is clean
#but rown names must be unique
community <- read.table("community.txt", header=TRUE, row.names=1, sep="\t")
env <- read.table("env.txt", header=TRUE, row.names=1, sep="\t")

#but usually not necessary, especially for regression
aroid <- read.table("aroid.txt", header=TRUE, sep="\t")

##Simple linear regression

#let's asking a stupid question:
#is height of propagules a function of fresh weight?

htfresh.mod <- lm(height~fresh, data=mangrove)
summary(htfresh.mod)
#interpret the results:
#some 80% of variation in height is explained by fresh mass
#the slope of the best fit line is 0.44964,
#and this slope is significantly different from 0 (p-value<2x10^-16)
#the intercept is 12.37811 (usually significance is disregarded), etc.

#the fitted model contains many goodies!

names(htfresh.mod)
htfresh.mod$coefficients #alternatively, just "coeff" if the short form is unique

#use this to plot a straight line through the scatterplot to visualize the relationship
plot(height~fresh, data=mangrove)
abline(htfresh.mod$coeff, col="red")

#the line doesn't look quite right
#this can be seen better via a residual plot
plot(htfresh.mod$residuals~htfresh.mod$fitted.values)
#ideally residuals should be randomly scattered above and below 0
abline(h=0, lty=2)

#a remedial measure for this can be to transform variables
#let's try logging the explanatory variable
plot(height~log(fresh), data=mangrove)
#another assumption of linear models is homogeneity of variance
#let's log the response variable as well
plot(log(height)~log(fresh), data=mangrove)
#this is potentially better. Fit a linear model to confirm
htfreshlgxy.mod <- lm(log(height)~log(fresh), data=mangrove)
abline(htfreshlgxy.mod$coeff, col="red")
#looks better, but there seems to be an outlier
#let's check the residual plot
plot(htfreshlgxy.mod$res~htfreshlgxy.mod$fitted)
abline(h=0, lty=2)
#identify the outlier
identify(htfreshlgxy.mod$fitted, htfreshlgxy.mod$res, labels=rownames(mangrove[complete.cases(mangrove),]))
mangrove[73,]
#re-run the analysis excluding case no. 73
plot(log(height)~log(fresh), data=mangrove[-73,])
htfreshlgxya.mod <- lm(log(height)~log(fresh), data=mangrove[-73,])
abline(htfreshlgxya.mod$coeff, col="red")

#we can even draw confidence intervals
fresh <- seq(0,100,0.1) #first create a series of x-values to predict from
predicted <- predict(htfreshlgxya.mod, newdata=data.frame(fresh), interval="confidence")
lines(predicted[,2]~log(fresh), col="red", lty=2)
lines(predicted[,3]~log(fresh), col="red", lty=2)

#axis are logged. Can we plot them in their original scales?
plot(height~fresh, data=mangrove)
lines(exp(predicted[,1])~fresh, col="red")
lines(exp(predicted[,2])~fresh, col="red", lty=2)
lines(exp(predicted[,3])~fresh, col="red", lty=2)
#find out where that outlier was
points(height~fresh, data=mangrove[73,], pch=16, col="blue")

#Categorical explanatory variables

plot(height~fresh, data=mangrove, type="n")
points(height~fresh, data=mangrove, subset=mangrove$species=="bgymnorhiza", pch=4, col="green")
points(height~fresh, data=mangrove, subset=mangrove$species=="bcylindrica", pch=1, col="blue")

#are the relationships between height and fresh weight different between the two species?
#we will be testing the log-log relationships, i.e.
plot(log(height)~log(fresh), data=mangrove, type="n", xlim=c(0,5))
points(log(height)~log(fresh), data=mangrove, subset=mangrove$species=="bgymnorhiza", pch=4, col="green")
points(log(height)~log(fresh), data=mangrove, subset=mangrove$species=="bcylindrica", pch=1, col="blue")
#don't forget that outlier!
points(log(height)~log(fresh), data=mangrove[73,], pch=16, col="blue")

htfreshsplglg.mod <- lm(log(height)~log(fresh)+species, data=mangrove[-73])
summary(htfreshsplglg.mod)

#plot the separate best fit lines
htfreshsplglg.mod$coeff #take a look at the coefficients matrix first
abline(htfreshsplglg.mod$coeff[1]+htfreshsplglg.mod$coeff[3],htfreshsplglg.mod$coeff[2], col="green", lty=2, lwd=2)
abline(htfreshsplglg.mod$coeff[1],htfreshsplglg.mod$coeff[2], col="blue", lty=2, lwd=2)

#is this the whole picture?
#add an interaction term between explanatory variables

htfreshsplglg.mod2 <- lm(log(height)~log(fresh)*species, data=mangrove[-73])
summary(htfreshsplglg.mod2)
#bordline significant lower order categorical term
#indicates that the two lines may (or may not) have the same intercept
#perhaps only slopes are different
htfreshsplglg.mod2$coeff
abline(htfreshsplglg.mod2$coeff[1]+htfreshsplglg.mod2$coeff[3], htfreshsplglg.mod2$coeff[4]+htfreshsplglg.mod2$coeff[2], lty=1, col="green", lwd=2)
abline(htfreshsplglg.mod2$coeff[1], htfreshsplglg.mod2$coeff[2], lty=1, col="blue", lwd=2)
abline(v=0) #ok, not the same intercept, but at the borders of a 95% C.I.

#two questions to try out at home:
#(1) How do you plot the best fit lines if the first order categorical terms was significant?
#(2) How do you plot on the original (unlogged) axis?

##Multiple linear regression
#let's move on to the next data set

env
community

#simple things first: is there an easy way to calculate diversity indices in R?

library(vegan)
#the thing about the vegan package is that the community matrix has to be species as columns and plots as rows
shannon <- diversity(community, index="shannon") #Simpson's also available
specrich <- specnumber(community)

#let's try to regress shannon's index against evironmental variables

#exploratory plots are always the first thing to do
pairs(env)
pairs(cbind(env,shannon))
#are there any obvious relationships?
pairs(cbind(env,shannon), lower.panel=panel.smooth) #the red lines are loess smoothers

#always check for multi-collinearity
#simple way: correlation tests

cor(env$clay, env$silt); cor(env$silt,env$sand); cor(env$sand, env$clay)
#all Pearson's correlation coefficients are very high
#check for significance, using Spearman's
cor.test(env$clay, env$silt, method="spearman")
cor.test(env$silt, env$sand, method="spearman")
cor.test(env$sand, env$clay, method="spearman")
#these three were not independent anyway: add up to 100%

#avoiding collinearity: combine variables
soil.clust <- hclust(vegdist(env[6:8], method="jaccard"))
plot(soil.clust)

#let's classify plots into 2 groups based on combined soil texture
rect.hclust(soil.clust, k=2, border="red")
#save this as a categorical variable
env$soil <- cutree(soil.clust, k=2)
#remove the original three variables
env <- env[-6:-8]

env

pairs(cbind(env,shannon), lower.panel=panel.smooth)
#looks like there is a bimodal relationship with forest
#quadratic terms are added with a identity matrix "I"
div.mod <- lm(shannon~pH+moisture+incline+forest+I(forest^2)+area+soil, data=env)
summary(div.mod)
#does this mean that all variables are not significant?

#simplest form of model selection: drop variables with least significant p-values
div.mod2 <- update(div.mod, ~.-soil)
summary(div.mod2) #area is now borderline significant! Drop another variable.
div.mod3 <- update(div.mod2, ~.-moisture)
summary(div.mod3)
#we can't drop forest without violating the hierarchy of the terms
#drop next least significant first order term
div.mod4 <- update(div.mod3, ~.-pH)
summary(div.mod4)
#still can't drop forest as forest^2 is now borderline significant. On the other hand, area is no longer.
div.mod5 <- update(div.mod4, ~.-incline)
summary(div.mod5)
#finally, drop last non-forest term
div.mod6 <- update(div.mod5, ~.-area)
summary(div.mod6)

#plot best fit quadratic line with confidence intervals
forest <- seq(min(env$forest), max(env$forest))
div.mod6.pred <- predict(div.mod6, newdata=data.frame(forest), interval="confidence")
plot(shannon~forest, data=env, ylim=c(0,2))
lines(div.mod6.pred[1], col="red")
lines(div.mod6.pred[,1], col="red")
lines(div.mod6.pred[,2], col="red", lty=2)
lines(div.mod6.pred[,3], col="red", lty=2)

#p-values can be a dodgy criteria for variable selection
#now common to use Akaike's Information Criterion
#e.g.
drop1(div.mod) #soil is also the first to be dropped, and so on...
#how about automating the backward stepwise procedure?
step(div.mod, direction="backward")
#different final model selected, because model hierarchy was violated in the first step!
#don't recommend it
#forward stepwise also possible
div1.mod <- lm(shannon~+1, data=env)
step(div1.mod, direction="forward", scope=shannon~pH+moisture+incline+forest+area+soil)
#again, different model
#possible to search through interaction terms
step(div1.mod, direction="forward", scope=shannon~pH*moisture*incline*forest*area*soil)
#but final model may be difficult to interpret
#automated method in R does not take into account quadratic terms
#interaction terms should only be included if there is ecological theory to interpret it
#many possible starting points affect the final model chosen
#yet assumes only one optimal final model
#all in all, stepwise procedures especially with large numbers of variables are difficult to carry out and justify

#question: can we regress on species counts as a response variable?

##Ordination: Non-parametric MultiDimensional Scaling
#warning: theory-wise, this can be deep water since we do not learn multivariate statistics in undergraduate courses

library(MASS)

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

##Revision: one-way ANalysis Of VAriance
#let's return to the original purpose of the mangrove data set

gymt.aov <- aov(dry~treatment, data=mangrove, subset=species=="bgymnorhiza")
cylt.aov <- aov(dry~treatment, data=mangrove, subset=species=="bcylindrica")
summary(gymt.aov); summary(cylt.aov)
#treatment with hormones did not have a significant effect on dry weight of both species

##two-way ANOVA
#controlling for salinity...
gymtsal.aov <- aov(dry~treatment+salinity, data=mangrove, subset=species=="bgymnorhiza")
cyltsal.aov <- aov(dry~treatment+salinity, data=mangrove, subset=species=="bcylindrica")
summary(gymtsal.aov); summary(cyltsal.aov)
#treatment was still not significant (neither was salinity)

#interaction effects...
gymtxsal.aov <- aov(log(dry)~treatment*salinity, data=mangrove, subset=species=="bgymnorhiza")
cyltxsal.aov <- aov(log(dry)~treatment*salinity, data=mangrove, subset=species=="bcylindrica")
summary(gymtxsal.aov); summary(cyltxsal.aov)
#interaction term was borderline significant with B. gymnorhiza

#let's see how the means of each treatment group look on graphs
gym.survived <- tapply(mangrove$survival[mangrove$survival=="Y"&mangrove$species=="bgymnorhiza"], mangrove[mangrove$survival=="Y"&mangrove$species=="bgymnorhiza",names(mangrove)=="treatment"|names(mangrove)=="salinity"], length)
gymdry.mean <- tapply(mangrove$dry[mangrove$species=="bgymnorhiza"], mangrove[mangrove$species=="bgymnorhiza",names(mangrove)=="treatment"|names(mangrove)=="salinity"], mean, na.rm=TRUE)
gymdry.sd <- tapply(mangrove$dry[mangrove$species=="bgymnorhiza"], mangrove[mangrove$species=="bgymnorhiza",names(mangrove)=="treatment"|names(mangrove)=="salinity"], sd, na.rm=TRUE)
gymdry.se <- gymdry.sd/sqrt(gym.survived)

gym.bar <- barplot(gymdry.mean, beside=TRUE, ylim=c(0,25), ylab="Dry weight /g", xlab="Salinity /%", legend=c("treatment","control"), main="Bruguiera gymnorhiza", axis.lty=1)
arrows(gym.bar, gymdry.mean-gymdry.se, gym.bar, gymdry.mean+gymdry.se, angle=90, code=3, length=0.1)

cyl.survived <- tapply(mangrove$survival[mangrove$survival=="Y"&mangrove$species=="bcylindrica"], mangrove[mangrove$survival=="Y"&mangrove$species=="bcylindrica",names(mangrove)=="treatment"|names(mangrove)=="salinity"], length)
cyldry.mean <- tapply(mangrove$dry[mangrove$species=="bcylindrica"], mangrove[mangrove$species=="bcylindrica",names(mangrove)=="treatment"|names(mangrove)=="salinity"], mean, na.rm=TRUE)
cyldry.sd <- tapply(mangrove$dry[mangrove$species=="bcylindrica"], mangrove[mangrove$species=="bcylindrica",names(mangrove)=="treatment"|names(mangrove)=="salinity"], sd, na.rm=TRUE)
cyldry.se <- cyldry.sd/sqrt(cyl.survived)

cyl.bar <- barplot(cyldry.mean, beside=TRUE, ylim=c(0,5), ylab="Dry weight /g", xlab="Salinity /%", legend=c("treatment", "control"), main="Bruguiera cylindrica", axis.lty=1)
arrows(cyl.bar, cyldry.mean-cyldry.se, cyl.bar, cyldry.mean+cyldry.se, angle=90, code=3, length=0.1)

#(if time permits): let's try ANOVA on survival rates in the aroid dataset

dseguine.aov <- aov(Survival~Light*Water, data=aroid, subset=Species=="Dieffenbachia seguine")
dcamilla.aov <- aov(Survival~Light*Water, data=aroid, subset=Species=="Dieffenbachia Camilla")
eaureum.aov <- aov(Survival~Light*Water, data=aroid, subset=Species=="Epipremnum aureum")
spodo.aov <- aov(Survival~Light*Water, data=aroid, subset=Species=="Syngonium podophyllum")

summary(dseguine.aov); summary(dcamilla.aov); summary(eaureum.aov); summary(spodo.aov)

#question: What's wrong with this analysis?
#clue: look at the graphs
dseguine.mean <- tapply(aroid$Survival[aroid$Species=="Dieffenbachia seguine"], aroid[aroid$Species=="Dieffenbachia seguine",names(aroid)=="Light"|names(aroid)=="Water"], mean)
dcamilla.mean <- tapply(aroid$Survival[aroid$Species=="Dieffenbachia Camilla"], aroid[aroid$Species=="Dieffenbachia Camilla",names(aroid)=="Light"|names(aroid)=="Water"], mean)
eaureum.mean <- tapply(aroid$Survival[aroid$Species=="Epipremnum aureum"], aroid[aroid$Species=="Epipremnum aureum",names(aroid)=="Light"|names(aroid)=="Water"], mean)
spodo.mean <- tapply(aroid$Survival[aroid$Species=="Syngonium podophyllum"], aroid[aroid$Species=="Syngonium podophyllum",names(aroid)=="Light"|names(aroid)=="Water"], mean)

dseguine.sd <- tapply(aroid$Survival[aroid$Species=="Dieffenbachia seguine"], aroid[aroid$Species=="Dieffenbachia seguine",names(aroid)=="Light"|names(aroid)=="Water"], sd)
dcamilla.sd <- tapply(aroid$Survival[aroid$Species=="Dieffenbachia Camilla"], aroid[aroid$Species=="Dieffenbachia Camilla",names(aroid)=="Light"|names(aroid)=="Water"], sd)
eaureum.sd <- tapply(aroid$Survival[aroid$Species=="Epipremnum aureum"], aroid[aroid$Species=="Epipremnum aureum",names(aroid)=="Light"|names(aroid)=="Water"], sd)
spodo.sd <- tapply(aroid$Survival[aroid$Species=="Syngonium podophyllum"], aroid[aroid$Species=="Syngonium podophyllum",names(aroid)=="Light"|names(aroid)=="Water"], sd)

dseguine.n <- tapply(aroid$Survival[aroid$Species=="Dieffenbachia seguine"], aroid[aroid$Species=="Dieffenbachia seguine",names(aroid)=="Light"|names(aroid)=="Water"], length)
dcamilla.n <- tapply(aroid$Survival[aroid$Species=="Dieffenbachia Camilla"], aroid[aroid$Species=="Dieffenbachia Camilla",names(aroid)=="Light"|names(aroid)=="Water"], length)
eaureum.n <- tapply(aroid$Survival[aroid$Species=="Epipremnum aureum"], aroid[aroid$Species=="Epipremnum aureum",names(aroid)=="Light"|names(aroid)=="Water"], length)
spodo.n <- tapply(aroid$Survival[aroid$Species=="Syngonium podophyllum"], aroid[aroid$Species=="Syngonium podophyllum",names(aroid)=="Light"|names(aroid)=="Water"], length)

dseguine.se <- dseguine.sd/sqrt(dseguine.n)
dcamilla.se <- dcamilla.sd/sqrt(dcamilla.n)
eaureum.se <- eaureum.sd/sqrt(eaureum.n)
spodo.se <- spodo.sd/sqrt(spodo.n)

par(mfrow=c(2,2))
bar.dseguine <- barplot(t(dseguine.mean), beside=TRUE, ylim=c(0,1), main="Dieffenbachia seguine", las=3)
arrows(bar.dseguine, t(dseguine.mean)+t(dseguine.se), bar.dseguine, t(dseguine.mean)-t(dseguine.se), code=3, length=0.05, angle=90)

bar.dcamilla <- barplot(t(dcamilla.mean), beside=TRUE, ylim=c(0,1), main="Dieffenbachia 'Camilla'", las=3)
arrows(bar.dcamilla, t(dcamilla.mean)+t(dcamilla.se), bar.dcamilla, t(dcamilla.mean)-t(dcamilla.se), code=3, length=0.05, angle=90)

bar.eaureum <- barplot(t(eaureum.mean), beside=TRUE, ylim=c(0,1), main="Epipremnum aureum", las=3)
arrows(bar.eaureum, t(eaureum.mean)+t(eaureum.se), bar.eaureum, t(eaureum.mean)-t(eaureum.se), code=3, length=0.05, angle=90)

bar.spodo <- barplot(t(spodo.mean), beside=TRUE, ylim=c(0,1), main="Syngonium podophyllum", las=3)
arrows(bar.spodo, t(spodo.mean)+t(spodo.se), bar.spodo, t(spodo.mean)-t(spodo.se), code=3, length=0.05, angle=90)