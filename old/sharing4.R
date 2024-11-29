################################
#                              #
#          Session 2           #
#       Model Selection        #
#             in               #
#  Multiple Linear Regression  #
#                              #
################################

##Review: Reading data and fitting a multiple linear regression with interactions

mangrove<-read.csv("mangrove.csv", header=TRUE)

gmod.full<-lm(dry~I(salinity^2)+salinity+treatment
	+I(salinity^2):treatment+salinity:treatment,
	data=mangrove, subset=species=="bgymnorhiza")
#let's call this a "full" model
#because all possible variables and interactions have been included

summary(gmod.full)

#On the other hand, let's look at a "null" model
#where there are no variables.

gmod.null<-lm(dry~1, data=mangrove, subset=species=="bgymnorhiza")

summary(gmod.null)

#What is the best fit line in a null model?

plot(dry~salinity, data=mangrove, subset=species=="bgymnorhiza",
	type="n")
points(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="u",
	col="blue", pch=1)
points(dry~salinity, data=mangrove,
	subset=species=="bgymnorhiza"&treatment=="t",
	col="red", pch=4)
legend("topright", pch=c(1,4), col=c("blue","red"),
	legend=c("Control","Treatment"))
abline(gmod.null, lty=2)

#What is the best fit line in a full model?

salinity.new<-seq(0,100,length.out=100)
gmod.full.pred.u<-predict(gmod.full, newdata=
		data.frame(salinity=salinity.new,
			treatment="u")
	)
gmod.full.pred.t<-predict(gmod.full, newdata=
		data.frame(salinity=salinity.new,
			treatment="t")
	)

lines(gmod.full.pred.u~salinity.new, col="blue")
lines(gmod.full.pred.t~salinity.new, col="red")

#Is the full model really better than the null model in describing the data?
#Let's look at residual "noise" after fitting these two models.
#Noise is essentially variance
#and variance the sum of squared errors
#and errors are the residuals!

sum(gmod.full$res^2); sum(gmod.null$res^2)
#Which had more "noise"?

#How much better is the full model than the null model?
#Let's look at the percent reduction in "noise":
(sum(gmod.null$res^2)-sum(gmod.full$res^2))/sum(gmod.null$res^2)

#Compare this with the R^2 value of the full model summary:
summary(gmod.full)

#OK so the full model explains this much more variation than the null model
#but is this amount of explained variation statistical significant, or
#did it arise from simply random variation?
#To do this, we carry out an F-test
#because the ratio of variances follows an F distribution. 

anova(gmod.full, gmod.null) #Effectively,
#the null model is the null hypothesis and
#the full model is the alternative hypothesis.

#Compare this again with the F statistic in the summary printout:
summary(gmod.full)

#But surely there are possibilities in between the full and null models?
#On one extreme end we can fit 99 variables to n=100 as a really full model
#but this is meaningless, partly because
#you will have to collect so much data every time to find out something.
#So we look for a PARSIMONIOUS model,
#a model that gives some predictive power
#but does not need everything under the sun to work.
#Remember: every variable added comes with the usual suite of assumptions
#of linearity, normality, homoscedasticity, etc.
#So a parsimonous model is the minimum number of assumptions
#for the same level of predictive power.
#This is the whole idea behind model selection:
#to select "the best" or the most parsimonious model.

##Stepwise (or Hierarchical) model selection
##using p-values
##and stepping "backwards" from the full model!

#Let's drop one of those interaction terms
gmod1<-lm(dry~I(salinity^2)+salinity+treatment
	+salinity:treatment,
	data=mangrove, subset=species=="bgymnorhiza")

gmod1.pred.u<-predict(gmod1, newdata=
		data.frame(salinity=salinity.new,
			treatment="u")
	)
gmod1.pred.t<-predict(gmod1, newdata=
		data.frame(salinity=salinity.new,
			treatment="t")
	)

lines(gmod1.pred.u~salinity.new, col="blue", lty=2)
lines(gmod1.pred.t~salinity.new, col="red", lty=2)
#Do you think the model changed very much after dropping that interaction term?

anova(gmod.full, gmod1)

#There is an easier way to drop terms

summary(gmod1<-update(gmod.full, ~.-I(salinity^2):treatment))

#Note: we always drop higher order terms
#before dropping their component (or "nested") first order terms.
#I.e., three-way interactions are dropped before two-way interactions
#and two-way interactions are dropping before their first order terms, and
#quadratic terms must be dropped before removing the first order term.
#This is called the "hierarchy" of the variables.

#So what term can be dropped next?

summary(gmod2a<-update(gmod1, ~.-I(salinity^2)))
summary(gmod2b<-update(gmod1, ~.-salinity:treatment))

anova(gmod1,gmod2a);anova(gmod1,gmod2b)

summary(gmod3a<-update(gmod2b, ~.-I(salinity^2)))
summary(gmod3b<-update(gmod2b, ~.-treatment))

anova(gmod3a,gmod2b);anova(gmod3b,gmod2b)

summary(gmod4<-update(gmod3b, ~.-I(salinity^2)))

anova(gmod4,gmod3b)

#gmod3b is often called the "minimum adequate model"

#If you follow the summary table, you will find that the non-significant
#variables in the t-tests are the ones dropped.
#This is the lazy way to do the above.
#Sometimes more than one variable is non-significant and can be dropped
#(without violating variable hierarchy).
#The practice is to drop the "least significant" variable first.

##Model selection using information theoretic criteria

#A key problem of using p-values to drop or add variables
#is the dependence of p-values on power (and consequently sample size).
#A huge dataset usually ends up retaining many variables.
#The selection criteria of models should depend on
#the information contained in these models.
#Such criteria have been developed from "information theory",
#therefore they are called information-theoretic criteria.
#An exampe is Akaike's Information Criterion, or AIC.

AIC(gmod.full)
AIC(gmod1)
#AIC(gmod2a);AIC(gmod2b)
#AIC(gmod3a);AIC(gmod3b)
#AIC(gmod4)

#A DECREASE in AIC by 2 points is considered a "substantial improvement".
#If backward stepwise selection is conducted using AIC,
#which model do you think you would arrive at?

##Forwards stepwise model selection
#starts with the null model

#using p-values
anova(gmod4,gmod.null)
anova(gmod5<-update(gmod.null,~.+treatment),gmod.null)

#using AIC

AIC(gmod.null)
AIC(gmod4);AIC(gmod5)

#Normally we would stop here! Because adding neither variable was significant
#or substantially improved the null model.
#So the direction of "stepping" and the starting model matters.

#You can automate all of the above using the step() function.
?step

##Multimodel inference
#We now have all possible models:
AIC<-c(
	AIC(gmod.full),
	AIC(gmod1),
	AIC(gmod2a),
	AIC(gmod2b),
	AIC(gmod3a),
	AIC(gmod3b),
	AIC(gmod4),
	AIC(gmod5),
	AIC(gmod.null)
	)

#AIC uncorrected is too generous and insufficiently penalizes for small datasets
#We almost always use AIC "corrected for small sample sizes"
#but we need to extract the number of parameters in each model, k.

length(gmod.full$coef) #The full model has six parameters, including the intercept.
length(gmod.null$coef) #The null model only has the intercept parameter

#Imagine typing that formula one by one and saving it.
#The best thing of programming languages is the ability to "loop"
#i.e., repeat a tedious task for different objects automatically.

gmods<-list(gmod.full, gmod1, gmod2a, gmod2b, gmod3a, gmod3b, gmod4, gmod5, gmod.null)
k<-rep(NA,length(gmods))
formula<-rep(NA, length(gmods))

for (i in 1:length(gmods)){
	k[i]<-length(gmods[[i]]$coef)
	AIC[i]<-AIC(gmods[[i]])
	formula[i]<-as.character(gmods[[i]]$call[2]) #we can even extract the formula of the model
	}
(AICc<-AIC+2*k*(k+1)/(length(gmod.full$res)-k-1))

#So which is the best model according AICc?
formula[which.min(AICc)]

#How to all models perform relative to this best model?
dAICc<-AICc-min(AICc)

#What is the likelihood that each model is ranked best relative to the best model?
(rL<-exp(-0.5*dAICc)) #The best model is set at 1; all else relative to it.

#If the best model is definitely within this set of candidate models,
#the probability of each model being the best model is:
(w<-rL/sum(rL))

data.frame(formula,k,AICc,dAICc,rL,w)[order(w, decreasing=TRUE),]
#sorted in decreasing probability of being the best model

#This is called ranking models using AIC weights.