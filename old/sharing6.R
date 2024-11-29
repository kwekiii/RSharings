########################
#                      #
#      Session 3       #
#  Generalized Linear  #
#     Mixed-effects    #
#        Models        #
#                      #
########################

#load the palms data

palm<-read.csv("palm.csv", header=TRUE)

#Aside from the three assumptions mentioned previously
#that apply to linear regression,
#there are more assumptions that apply more generally to other models:
#4: Samples are independent

pairs(palm[,c(4,9,10,19)], pch=as.numeric(palm$fragment),
	col=as.numeric(palm$fragment))

#As can be seen, plots in the same forest fragment tend to be more similar
#to each other in characteristics, e.g., seedling counts, litter depth, etc.
#Because sampling occurs in space, and things closer together
#are always more similar than things further apart,
#independence would require samples to be always far apart.
#This is not always practical. Imagine restricting yourself to one plot
#per forest fragment. You will need a lot of forest fragments
#for enough statistical power.
#In practice, we usually have two or more levels of hierarchy in our sampling.
#E.g., we randomly choose a sample of forest fragments,
#then we randomly distribute sampling plots within each fragment.
#This is called "sub-sampling" plots within the sample of fragments.
#It is operationally more efficient, but you can no longer assume independence.

litter.mean<-tapply(palm$litter, palm$fragment, mean)
seedling.mean<-tapply(palm$seedling, palm$fragment, mean)

par(mfrow=c(1,2))
plot(seedling~litter, data=palm,
	pch=as.numeric(palm$fragment), col=as.numeric(palm$fragment),
	ylim=c(0,350), xlim=c(1,8))
plot(litter.mean, seedling.mean,
	pch=1:length(levels(palm$fragment)), col=1:length(levels(palm$fragment)),
	ylim=c(0,350), xlim=c(1,8), cex=table(palm$fragment)/2+0.5)

#Sometimes, we call these sub-samples "pseudoreplicates",
#because they are not true replicates,
#which must be independent.
#If you average all the values for each fragment,
#It would satisfy the assumption of independence better,
#but you would have much worse power.

#A compromise would be to assume that
#the trend between plots within each fragment
#follow the same TREND as between the fragments
#but the POSITION fluctuates according to a random distribution.

#The fragment is therefore called a RANDOM EFFECT.
#The trends being investigated are called the FIXED EFFECT.
#Usual (generalized) linear models only have fixed effects.
#When there are both random effects and fixed effects, it is called a
#MIXED EFFECTS MODEL.

library(lme4)

lmer(seedling~litter+(1|fragment), data=palm)

#To check the residuals, fit the random effect as a fixed effect
#in a normal regression model.

palm.lm1<-lm(seedling~litter+fragment, data=palm)
par(mfrow=c(2,2))
plot(palm.lm1)

#With heteroscedasticity, you know you have to transform...

palm.lm2<-lm(log(seedling+0.5)~litter+fragment, data=palm)
par(mfrow=c(2,2))
plot(palm.lm2)

#...or fit an appropriate family!
#These kinds of models are Generalized Linear Mixed-effects Models (GLMMs).

palm.glmm1<-glmer(seedling~litter+(1|fragment), family=poisson, data=palm)
palm.glmm.null<-glmer(seedling~1+(1|fragment), family=poisson, data=palm)
	#this is called a random effects-only model

#check for overdispersion
#include +1 to k for each random effect.

(c_litter<-(deviance(palm.glmm.null)-deviance(palm.glmm1))/
	(length(residuals(palm.glmm1))-length(fixef(palm.glmm1))-1))

#The model is overdispersed,
#but negative binomial mixed-effects models are still being developed
#so we can either penalize the AIC measures with the dispersion parameter,
#or another trick is to fit an observation-level random effect.

palm.glmm2<-glmer(seedling~litter+(1|fragment)+(1|transect),
	family=poisson, data=palm) #dispersion is effectively assumed to be 1
palm.glmm.null2<-glmer(seedling~1+(1|fragment)+(1|transect),
	family=poisson, data=palm) #the new null model

anova(palm.glmm2, palm.glmm.null2)
	#note that the use of Chi-squared test is automatic.

#Everything else, e.g. model selection, is the same...