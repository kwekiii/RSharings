#############################################
#                                           #
#               Session 1                   #
#                 Review                    #
#                    &                      #
# From Simple to Multiple Linear Regression #
#                                           #
#############################################

##Review: Reading data

#Ideally, the first step should be to change the working directory (WD).
#Then you can read data.
#Remember to save them as objects, otherwise R just reads, vomits, and forgets.

mangrove<-read.csv("mangrove.csv", header=TRUE)

#Here's a cool alternative to loading data in R.
#Sometimes you need to read a file from a place different from your WD.
#If you have to change WD and change back, it can be a bit troublesome,
#so try this:

read.csv(file.choose(), header=TRUE)

#Remember to insert "object name"<- in front if you want to save it.
#But don't get lazy and use this all the time. Changing WD is still #1.

#No. 2 rule: always construct a clean script for your analyses!
#By "clean" I mean a tidy script well annotated
#so someone else can easily figure out where to pick up where you left off
#when you just send the fella your script and data files.

##Simple linear regression
#Concept of allometry:
#propagule dry weight as a simple function of propagule length

g.mod1<-lm(dry~height, data=mangrove, subset=species=="bgymnorhiza")
c.mod1<-lm(dry~height, data=mangrove, subset=species=="bcylindrica")

##Results
summary(g.mod1)
summary(c.mod1)

#Outputs from functions in R hold many hidden treasures.
#Leveling up from R literacy to R proficiency
#depends on your ability to hack into these treasures.

names(g.mod1)
names(c.mod1)

g.mod1$coef
c.mod1$coef

#Three important assumptions in linear regression models:
#1: Linear relationship between X (explanatory/independent) and Y (response/dependent)
#2: Residuals follow a normal distribution
#3: Variance is constant ("homoscedasticity")

##Model diagnostics 1: X-Y plots with best fit line

plot(dry~height, data=mangrove, subset=species=="bgymnorhiza")
abline(g.mod1$coef)
plot(dry~height, data=mangrove, subset=species=="bcylindrica")
abline(c.mod1$coef)

#Tip: to make an array of plots,
par(mfrow=c(2,1))
plot(dry~height, data=mangrove, subset=species=="bgymnorhiza")
abline(g.mod1$coef)
plot(dry~height, data=mangrove, subset=species=="bcylindrica")
abline(c.mod1$coef)
#par() is a function where you can specify many basic graphical parameters
#mfrow=c(r,c) where r is the number of rows and c is the number of columns

#Do you think the relationship is best described by a straight line?

##Model diagnostics 2: Residual histograms

g.mod1$residuals
c.mod1$res #residuals are the "errors", i.e. how far apart are the data points from the predicted/expected/"best fit" value

hist(g.mod1$res);hist(c.mod1$res) #histograms of the residuals
#Do they look bell-shaped, symmetric about zero?

##Model diagnostics 3: Residual plots

g.mod1$fitted
c.mod1$fitted

plot(g.mod1$res~g.mod1$fitted)
abline(h=0)

plot(c.mod1$res~c.mod1$fitted)
abline(h=0)

#Are the spread of points
#consistent (i.e., homoscedastic);
#even above and below (i.e., symmetric);
#show no ascending, descending, or curving trends (i.e., linear!)?
#When sample size is small and there are few points,
#it's hard to judge.

##Transformations

#When variance (i.e., the spread of points) increases with fitted values,
#a common solution is log-transformation

mangrove$dry.log<-log(mangrove$dry)

summary(g.mod2<-lm(dry.log~height, data=mangrove, subset=species=="bgymnorhiza"))
#kill 2 birds with one stone: save the model while producing a print-out

#after a round of transformations, you should check the diagnostics plots again

par(mfrow=c(2,3))

plot(dry~height, data=mangrove, subset=species=="bgymnorhiza")
abline(g.mod1$coef)
hist(g.mod1$res)
abline(v=0, col="red")
plot(g.mod1$res~g.mod1$fitted)
abline(h=0)

plot(dry.log~height, data=mangrove, subset=species=="bgymnorhiza")
abline(g.mod2$coef)
hist(g.mod2$res)
abline(v=0, col="red")
plot(g.mod2$res~g.mod2$fitted)
abline(h=0)

#Do you think improvement has been made towards model assumptions?

#The problem with transformations is that the Y axis is now distorted.
#I.e., it is the transformed Y' that has a linear relationship with X.
#The original Y still has a non-linear relationship with X.

height.new<-seq(
	min(mangrove$height[mangrove$species=="bgymnorhiza"], na.rm=TRUE),
	max(mangrove$height[mangrove$species=="bgymnorhiza"], na.rm=TRUE),
	length.out=100
	)

g.mod2.pred<-predict(g.mod2, newdata=data.frame(height=height.new),
	interval="confidence",
	level=0.95
	)

#to reset par() settings to default, simply close the graphing window

plot(dry~height, data=mangrove, subset=species=="bgymnorhiza")
lines(exp(g.mod2.pred[,1])~height.new, col="red", lty=1)
lines(exp(g.mod2.pred[,2])~height.new, col="red", lty=2) #upper and
lines(exp(g.mod2.pred[,3])~height.new, col="red", lty=2) #lower 95% C.I.

##Quadratic regression

#Let's look at another aspect of the mangrove dataset:
#How does salinity affect mangrove propagule growth?

plot(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="u")

#Common question: "Is there a trend??"
#What happens if you fit a straight line model through this data?

summary(g.mod3<-lm(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="u"))

salinity.new<-seq(0,100,length.out=100)

g.mod3.pred<-predict(g.mod3, newdata=data.frame(salinity=salinity.new),
	interval="confidence",
	level=0.95
	)

lines(g.mod3.pred[,1]~salinity.new, col="red")
lines(g.mod3.pred[,2]~salinity.new, col="red", lty=2)
lines(g.mod3.pred[,3]~salinity.new, col="red", lty=2)

#But in the first place the assumption of linearity is wrong.
#From an inspection of the scatterplot, the trend is not even monotonic.
#A simple way to fit data that show a U-shaped trend
#is to assume a quadratic relationship, Y = ax^2 + bx + c

summary(g.mod4<-lm(dry~I(salinity^2)+salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="u")) #I is an "identity matrix"
#Note that the first order term is still within the model.

#Note the improvement in the R^2 value.

g.mod4.pred<-predict(g.mod4, newdata=data.frame(salinity=salinity.new),
	interval="confidence",
	level=0.95
	)

plot(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="u")
lines(g.mod4.pred[,1]~salinity.new, col="red")
lines(g.mod4.pred[,2]~salinity.new, col="red", lty=2)
lines(g.mod4.pred[,3]~salinity.new, col="red", lty=2)


#You should check the model diagnostics.
#A quick way:
plot(g.mod4)
#What do you think?
##Additional model diagnostics: Q-Q plots, outliers, and influential points

##Multiple regression
#We can ask the research question:
#What is the effect of hormone treatment on Bruguiera gymnorrhiza propagules,
#after controlling for the effect of salinity?

g.mod5<-lm(dry~I(salinity^2)+salinity+treatment, data=mangrove,
	subset=species=="bgymnorhiza")

g.mod5.pred.u<-predict(g.mod5, newdata=
		data.frame(salinity=salinity.new,
			treatment="u")
	)
g.mod5.pred.t<-predict(g.mod5, newdata=
		data.frame(salinity=salinity.new,
			treatment="t")
	)

plot(dry~salinity, data=mangrove, subset=species=="bgymnorhiza",
	type="n")
points(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="u",
	col="blue", pch=1)
points(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="t",
	col="red", pch=4)
lines(g.mod5.pred.u~salinity.new, col="blue")
lines(g.mod5.pred.t~salinity.new, col="red")

#This is a version of an ANCOVA,
#where you assume no interaction between predictors.
#What if the research question was to be rephrased as:
#Did the hormone shift the salinity tolerance of the propagules?

g.mod6<-lm(dry~I(salinity^2)+salinity+treatment+I(salinity^2):treatment+salinity:treatment, data=mangrove,
	subset=species=="bgymnorhiza")

g.mod6.pred.u<-predict(g.mod6, newdata=
		data.frame(salinity=salinity.new,
			treatment="u")
	)
g.mod6.pred.t<-predict(g.mod6, newdata=
		data.frame(salinity=salinity.new,
			treatment="t")
	)

plot(dry~salinity, data=mangrove, subset=species=="bgymnorhiza",
	type="n")
points(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="u",
	col="blue", pch=1)
points(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="t",
	col="red", pch=4)
lines(g.mod6.pred.u~salinity.new, col="blue")
lines(g.mod6.pred.t~salinity.new, col="red")

#So did it really shift?
summary(g.mod5); summary(g.mod6) #hint

#Homework: check out the diagnostic plots for the last two models.
#Are transformations needed?