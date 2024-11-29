#how many elements are there in your objects?
length(mangrove) #using length() on a dataframe gives the number of columns
length(mangrove$survival) #number of rows in the column

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

#bar charts are alot more troublesome in r, but will cover in detail in the next session

all.mean <- tapply(mangrove$dry, mangrove[,1:2], mean, na.rm=TRUE)
all.sd <- tapply(mangrove$dry, mangrove[,1:2], sd, na.rm=TRUE)
all.se <- all.sd/sqrt(survived.n)

bar <- barplot(t(all.mean), beside=TRUE, ylim=c(0,16), ylab="Dry weight /g")
arrows(bar, t(all.mean)-t(all.se), bar, t(all.mean)+t(all.se), angle=90, code=3)


