#################
#               #
#   Session 3   #
#  Generalized  #
#     Linear    #
#     Models    #
#               #
#################

#load data
mangrove<-read.csv("mangrove.csv", header=TRUE)

##Review: Three important assumptions in linear regression models:
#1: Linear relationship between X (explanatory/independent) and Y (response/dependent)
#2: Residuals follow a normal distribution
#3: Variance is constant ("homoscedasticity")

#But in real data these assumptions are rarely met.

##Binary responses and Logistic Regression

#Some of the mangrove propagules did not survive the hormone or salinity treatments

bg<-mangrove[mangrove$species=="bgymnorhiza",]
bc<-mangrove[mangrove$species=="bcylindrica",]

table(bg$survival, bg$treatment)
table(bg$survival, bg$salinity)

table(bc$survival, bc$treatment)
table(bc$survival, bc$salinity)

#let's tabulate the number suvived
bg$survival1<-ifelse(bg$survival=="Y",1,0)
bc$survival1<-ifelse(bc$survival=="Y",1,0)

xtabs(survival1~salinity+treatment, data=bg)/xtabs(~salinity+treatment, data=bg)
xtabs(survival1~salinity+treatment, data=bc)/xtabs(~salinity+treatment, data=bc)
#xtabs() stands for "cross(i.e., 'X')-tabulate"
#and can be used to create contingency tables

bc.tab1<-xtabs(survival1~salinity+treatment, data=bc)

chisq.test(bc.tab1)
#Chi-squared tests are fussy (see your nearest stats textbook

#Fisher's exact tests are difficult to compute by hand
#but easy in R.
fisher.test(bc.tab1)

#But both are tests of independence.
#They tell you nothing except whether the distribution of responses
#in the contingency table will arise due to random chance,
#which is not very helpful.

#For example, you might want to know the relationship between
#the probability of survival and the salinity.
#The relationship might not be monotonic (as seen previously)
#and you might even want to test interactions with hormone treatment.
#These are all impossible with contingency tables alone.

#At the same time, fitting a linear regression is obviously silly:

bc.lm1<-lm(survival1~salinity, data=bc)

bc1<-as.data.frame(xtabs(~survival1+salinity, data=bc), stringsAsFactors=FALSE)

plot(survival1~salinity, data=bc1, cex=Freq/2.5)
abline(bc.lm1, lty=2)

#or even a quadratic regression
bc.lm2<-lm(survival1~salinity+I(salinity^2), data=bc)

salinity.new<-seq(0,100,length.out=100)
bc.lm2.pred<-predict(bc.lm2, newdata=data.frame(salinity=salinity.new))

lines(bc.lm2.pred~salinity.new, lty=2)

#much of the problem is because the response is only 0 or 1
#even when the response is a percentage or proportion,
#it is still bound by 0 and, or 0 and 1

#Logistic regression is essentially a transformation
#where binomial 0s and 1s are expressed as ratios of "odds".
#Ratios are still bound by 0,
#so the odds ratio is log-transformed,
#so that it varies from negative infinity to positive infinity.
#This linking transformation is called the "logit" link.

summary(bc.lr1<-glm(survival1~salinity,
	family=binomial(link="logit"), data=bc))
summary(bc.lr2<-glm(survival1~salinity+I(salinity^2),
	family=binomial(link="logit"), data=bc))

bc.lr2.pred<-predict(bc.lr2, newdata=data.frame(salinity=salinity.new),
	type="response")

lines(bc.lr2.pred$fit~salinity.new)

#What is the significance of the quadratic term relative to the first-order term?

anova(bc.lr2,bc.lr1, test="Chisq") #note the difference with the summary output

#We can set up a "full" and a "null" model according to our previous framework.

summary(bc.lr.full<-glm(survival1~salinity+I(salinity^2)+treatment
	+salinity:treatment+I(salinity^2):treatment,
	family=binomial(link="logit"), data=bc))
bc.lr.null<-glm(survival1~1,family=binomial(link="logit"), data=bc)

AIC(bc.lr.full);AIC(bc.lr1);AIC(bc.lr2);AIC(bc.lr.null)

##Count data and Poisson regression

#Aside from yes/no responses,
#counts are also very common in ecological data, e.g.,
#counts of species, seedlings, etc.
#Counts have similar problems as binary data:
#they are discrete and they are bound by zero.

palm<-read.csv("palm.csv", header=TRUE)
plot(seedling~litter, data=palm)

#Can't we just log-transform count data?

log(palm$seedling)

#log-transformation can be a quick but acceptable way under several conditions:
#The range is large so discrete data becomes approximately continuous, and
#There are no zeros, or the smallest number besides zero is close to zero.

#Still, if you are able to use R, it should be easy to just employ better
#alternatives, e.g., fitting regression lines with Poisson errors
#instead of Gaussian (i.e., "normal") errors.

palm.po1<-glm(seedling~litter, data=palm, family=poisson(link="log"))
palm.po.null<-glm(seedling~1, data=palm, family=poisson(link="log"))

summary(palm.po1)

#If you're lucky, you're ready to go ahead with model selection.
#However, Poisson regression has a very big assumption:
#that variance=mean(=lambda)
#When the range is large,
#or there are many zeros and the next smallest number besides zero is large,
#the assumption does not hold.

##Over/under-dispersion
#When variance>>mean or variance<<mean,
#we say that the variance is overdispersed or underdispersed.

litter.new<-seq(min(palm$litter),max(palm$litter), length.out=200)
palm.po1.pred<-predict(palm.po1,newdata=data.frame(litter=litter.new),
	type="response", se.fit=TRUE)
lines(palm.po1.pred$fit~litter.new)
lines(palm.po1.pred$fit+1.96*palm.po1.pred$se.fit~litter.new, lty=2)
lines(palm.po1.pred$fit-1.96*palm.po1.pred$se.fit~litter.new, lty=2)
#The 95% confidence band obviously looks too narrow.

#The dispersion parameter can be calculated by:
(phi<-(palm.po1$null.deviance-palm.po1$deviance)/palm.po1$df.residual)

##Negative binomial regression
#One alternative is to instead assume
#variance=phi*mean

library(MASS)

summary(palm.nb1<-glm.nb(seedling~litter, data=palm))
palm.nb.null<-glm.nb(seedling~1, data=palm)

anova(palm.nb1,palm.nb.null, test="Chisq")

##Quasi-likelihood correction

#AIC is calculated as -2*logLik+2*k
#AICc is calcualted as AIC+2*k*(k+1)/(n-k-1)
#Quasi-likelihood penalized AIC is calculated as -2*logLik/phi+2*k
#QAICc is calculated as QAIC+2*k*(k+1)/(n-k-1)

-2*logLik(palm.po1)[1]/phi+2*length(palm.po1$coef)

#Despite all these alternatives to modelling count data,
#data with many zeros, i.e. zero-inflated, tend to be problematic.
#Zero-inflated regression and hurdle models can deal with these,
#but will not be covered here.