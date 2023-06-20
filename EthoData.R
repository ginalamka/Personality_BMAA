## Ethovision Personality Data
## Gina Lamka
#started in 2019, reworked and completed 2023

#kelsey's code and data are here: https://data.mendeley.com/datasets/4hczg93xkp/2/files/458214a3-f6b5-4ac3-81a8-0c3f460d2b0e

#note: Changes in Raw EthoData from 2019
  #typo for Eric, age 21, paterinty said 31 when should be 314
  #removed FishA, FishB, FishC, FishD, FishE, FishF -- these were test fish and the methods were not 100% solid at the time
  #typo for clutch 3-18G treatment at age 14, said 25 treatment when should be 5 (ID: Gumby, Harriet, IceCube, Manta, Newberry, Ollie, PInkie, Quan, Rally)
  #typo for Isomer, age 161, clutch said 5-5A but should be 5-3A

#WILL NEED TO REMOVE: look into fish with * by their names!
#QC - check into Lupin, MantisShrimp, Nikki
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:/Users/ginab/Box/Old Computer/Grad School/BALL STATE/Thesis/2023_Etho,Embryo,Spinning/Personality_BMAA") #set working directory
etho=read.table("EthoData_updated.csv",header=TRUE, sep=",")  #see changes in "updated" dataset above

#change the indv tested on day 15 to day 14
for(i in 1:nrow(etho)){
  if(etho[i,3]==15){
    etho[i,1]=14
  }
}   

#set Treatment as a factor
etho$Treatment = as.factor(etho$Treatment)

#for now, keep age as a number since age might have an interaction effect with treatment?

#setwd("C:/Users/ginab/Box/Old Computer/Grad School/BALL STATE/Thesis/2023_Etho,Embryo,Spinning/Personality_BMAA") #set working directory
#etho=read.table("pca_cutethodata.csv",header=TRUE, sep=",")
#etho=read.table("pca_supercutethodata.csv",header=TRUE, sep=",")
str(etho)

#FOR CUT ETHO DATA ONLY--change the indv tested on day 15 to day 14
#for(i in 1:nrow(etho)){
#  if(etho[i,1]==15){
#    etho[i,1]=14
#  }
#} #age = as.numeric(etho[,3])  


####Step 1: Check correlations among variables
{
C <- cor(etho) #gives correl table - look for those > 0.5
cor.test(etho$CumDurZ2, etho$CumDur.Z2)
cor.test(etho$LatencyZ2, etho$LatencyZ2360)
cor.test(etho$MeanMeander, etho$TotMeander)
cor.test(etho$Mean.Activ, etho$Tot.Act)
cor.test(etho$Mean.Activ, etho$MeanMeander)
cor.test(etho$MeanVel, etho$MeanAngVel)  #strangely, these are not related

library(corrplot)
corrplot(C, method = "number")

#grab variables of interest
age <- etho$Age   #highly related to arena size
pat <- etho$Paternity
treat <- etho$Treatment
dist <- etho$TotDist
dur <- etho$CumDur.Z2   #CumDur.Z2 is the PROPORTION, CumDurZ2 is the numeric across 360 seconds. note one or two not exact matches?
lat <- etho$LatencyZ2360   #contains '-' when never entered Z2. consider if this should be NA or max assay time (360 sec)
mea <- etho$MeanMeander  #not sure what this really is lol ; #note, has pos and neg values
angv <- etho$MeanAngVel  #note, has pos and neg values
frq <- etho$FreqZAlter
actv <- etho$Mean.Mobility
andr <- etho$VarTurnAngle

data = matrix(nrow=nrow(etho), ncol=8)
data <- cbind(age, pat, treat, dist, dur, lat, mea, angv, actv, andr)  #frq, actv
D <- cor(data)
corrplot(D, method = "number")
  #age, distance, activity, and freq zone alteration all related -- choose ONE
}
##Suggested Variables
 #Dependent Var
  #Mean Mobility     = Activity
  #CumDur.Z2         = Boldness
  #MeanAngVel        = Exploration  (+/- values)
 #Independent Var
  #Treatment
 #Covariates
  #Age - general changes over time; longitudinal, not a category (may be random var if multiple measures per age)
 #Random Variables
  #Paternity/Clutch
  #ID

#Angular Velocity - relative turn angle / time difference ; degrees per second; can be + or - 
  #measures the speed of change in direction of movement - used to assess turn bias or circular tendency and abnormalities of behavior
  #NOTE - turn angle is sensitive to small movements of body points. if distance moved is very small, turn angle can get high, unrealistic values (additionally, consecutive turns can have high values)
  #to combat this ^^, let's scale distance moved

#Mobility - percentage of pixel change between current sample and previous sample *detected in the subject only* ; ranges from 0 - 100%
  #NOTE - mobility depends on size of subject only, not on arena size. small fish results in small number of pixels that change, therefore small movements results in high change
  #used to detect activity

#Cumulative duration in Z2 was activated only once crossed the threshold of Z2; same with freq zone alteration

{#removed dep variables that were considered:
  #Latency,  etho$LatencyZ2               (boldness)
  #Total Distance,  etho$TotDist          (activity)
  #Mean Meander,  etho$MeanMeander        (exploration)     +/- values
  #Mean Activity, etho$meanactiv          (activity)

#Meander - the change in direction of movement of a subject relative to the distance moved by the subject. 
  #Gives an indiciation on how convoluted the subject's trajectory is.
  #relative meander - change in direction is signed (clockwise = negative, counterclockwise = positive); RM = relative turn angle/distance moved
  #absolute meander - change is unsigned. AKA absolute value of relative meander ; AM = |relative turn angle/distance moved|
  #NOTE - when distance moved is very small (young ages), meander can have high, unrealistic values

#Activity - percentage of changed pixels in *entire arena* between current and previous sample (independent of the subject)
  #actv = n changed pixels / total n pixels * 100 ; [if all pixels are same, no activity]
  #if animal is moving and increases in velocity, activity/mobility will increase, cuz pixels are increasingly different as it moves faster
  #mean activity averages across number of samples (i.e., time)
  #NOTE - activity depends on size of subject relative to arena, therefore, if animal is small in arena, activity will be small
}

###Step 2: PCA ANALYSIS
{#https://www.statology.org/principal-components-analysis-in-r/
  #used to determine the variables of interest
  #first, look for variables in the top PCs. For those that are related, choose one.
  #then determine which are not related and figure out if they can be interpreted as a personality behavioral trait

#calc principal components
pca <- prcomp(data, scale=TRUE)  #use etho for all variables

#need to reverse the signs
pca$rotation <- -1*pca$rotation

#display principal components
pca$rotation

#need to reverse signs of scores
pca$x <- -1*pca$x

#create biplot to visualize
biplot(pca, scale=0)

#calc total variance explained by each princ component
pca$sdev^2/sum(pca$sdev^2)

#do PCAs for each age
for(i in unique(etho$Age)){
  temp = etho[etho$Age==i,,drop=FALSE]
  pca_i <- prcomp(temp[,3:29], scale=TRUE)
  pca_i$rotation <- -1*pca_i$rotation
  print(i)
  print(pca_i$rotation)
  pca_i$x <- -1*pca_i$x
  biplot(pca_i, scale=0, main=i)
}
}

###Step 3: Find relatedness among treatments
#many fathers represented in multiple treatments, and multiple clutches from mutliple fathers
{
for(p in unique(etho$Treatment)){
  temp = etho[etho$Treatment == p,,drop=FALSE]
  print(paste("treatment",p))
  print(paste(unique(temp$Paternity)))
  print(paste(unique(temp$Clutch)))
}
for(f in unique(etho$Paternity)){
  hold = etho[etho$Paternity == f,,drop=FALSE]
  print(paste(unique(hold$Clutch), unique(hold$Treatment)))
}

#Father 318 in ALL 3 treatments
#Father 31&33 in 0 and 5
#Father 51&55 in 0 and 25
#Father 313&37 in 5 and 25

#0 has 2 clutches with Father 316 (3-16A & 3-16B) and 2 clutches with Father 318 (3-18A & 3-18I) and 2 clutches with Father 47 (4-7A & 4-7B) and 2 clutches with Father 53 (5-3A & 5-3B).
#5 has 2 clutches with Father 39 (3-9A & 3-9B) and 2 clutches with Father 318 (3-18E & 3-18G)
#25 has 2 clutches with Father 37 (3-7B & 3-7F) and 3 clutches with Father 318 (3-18C & 3-18F)
}

###Step 4: determine distribution of data and how to best analyze
  #want to find the best model using LRT to decide which to put into rptR
library(rcompanion)
library(car)
library(DescTools)
library(FSA)
library(dplyr)
library(lme4)
library(stargazer)
library(lmerTest)
library(lmtest)
library(rptR)

##VarTurnAngle
hist(log(etho$VarTurnAngle))
hist(etho$VarTurnAngle)
plotNormalHistogram(log(etho$VarTurnAngle))
etho$scaledVarTurnAngle <- scale(etho$VarTurnAngle, center=T, scale=T)
plotNormalHistogram(scale(etho$VarTurnAngle, center=T, scale=T))
qqnorm(etho$VarTurnAngle)
qqline(etho$VarTurnAngle)
qqnorm(etho$scaledVarTurnAngle)
qqline(etho$scaledVarTurnAngle)  
qqnorm(log(etho$VarTurnAngle))
qqline(log(etho$VarTurnAngle))

v.1 <- lmer(log(etho$VarTurnAngle) ~ Treatment + Age + Mean.Mobility + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
v.2 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
anova(v.1, v.2) #scaled is better than logged, though they are basically the same

v1 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(v1)  #doesnt look like any random effects will be important

v7 <- lmer(scaledVarTurnAngle ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(v7)  #no variance due to random effects
anova(v7, v1) #v1 is better -- include mean.mobility

v2 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility + (1|FishName) + (1|Clutch), data = etho)
summary(v2)   #doesnt look like any random effects will be important
anova(v2, v1) #close but v1 is better

v3 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility + (1|FishName), data = etho)
summary(v3)
anova(v3, v1) #v1 is better

v4 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho) 
summary(v4)  #doesnt look like any random effects will be important
anova(v4, v1) #v4 is better

v5 <- lm(scaledVarTurnAngle ~ Treatment + Age + Treatment*Age + Mean.Mobility, data = etho)
summary(v5)
anova(v4, v5) #v4 is better

v6 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility +(1|Clutch), data = etho)
summary(v6)   
anova(v6, v4) #v4 is better

v7 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility + Treatment*Age+ (1|FishName) + (1|Clutch), data = etho) 
summary(v7)  #doesnt look like any random effects will be important
anova(v7, v4) #close but v4 is better

v8 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility + Treatment*Age+ (1|FishName), data = etho) 
summary(v8)  #doesnt look like any random effects will be important
anova(v4, v8) #v4 is better

v9 <- lmer(scaledVarTurnAngle ~ Treatment + Age + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho) 
summary(v9)  #doesnt look like any random effects will be important
anova(v4, v9) #v4 is better


rptV = rpt(scaledVarTurnAngle ~ Treatment + Age + Mean.Mobility + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho, grname = "FishName", datatype = "Gaussian", nboot = 1000, npermut = 100)
summary(rptV) #is singular
plot(rptV)
plot(rptV, type="permut")
#R = 0.0434 , p = 0.043 
#fit is singular, but random effects all seem to account for a significant amount of variance, since full model fits better


#Latency
  #I foresee issues with Latency like in CumDur -- more a success/fail type thing
  #dividing Latency by 360 s (total trial time) turns it into a proportion
hist(etho$LatencyZ2360)
qqnorm(etho$LatencyZ2360)
qqline(etho$LatencyZ2360)

hist(logitTransform(etho$LatencyZ2360))  #this def seems to be the best for distribution but range suggests need for scaling
hist(asinTransform(etho$LatencyZ2360))
hist(log((etho$LatencyZ2360+1)))
hist(etho$LatencyZ2360)

rptL = rpt(LatencyZ2360 ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho, grname = "FishName", datatype = "Poisson", nboot = 1000, npermut = 100)
summary(rptV) 
plot(rptV)
plot(rptV, type="permut")



##Mean.Mobility
  #mobility = activity as the response var, Treatment and Age are Fixed Effects, VarTurnAngle is a covariate, and Clutch, Paternity, and FishID are random effects
hist(etho$Mean.Mobility)
qqnorm(etho$Mean.Mobility)
qqline(etho$Mean.Mobility)

m1 <- lmer(Mean.Mobility ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(m1)

m2 <- lmer(Mean.Mobility ~ Treatment + Age + scale(VarTurnAngle, center=T, scale=T)+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(m2)
anova(m2, m1)  #m2 is better

m3 <- lmer(Mean.Mobility ~ Treatment + Age + scale(VarTurnAngle, center=T, scale=T)+ (1|FishName) + (1|Clutch) , data = etho)
summary(m3)
anova(m3, m2)  #m2 is better

m4 <- lmer(Mean.Mobility ~ Treatment + Age + scale(VarTurnAngle, center=T, scale=T)+ (1|FishName), data = etho) 
summary(m4)
anova(m4, m2)  #m2 is better

m5 <- lm(Mean.Mobility ~ Treatment + Age + scale(VarTurnAngle, center=T, scale=T), data = etho)
summary(m5)
anova(m2, m5)  #m2 is better

m6 <- lmer(Mean.Mobility ~ Treatment + Age + scale(VarTurnAngle, center=T, scale=T) + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho) #scale is off here #lmer(Mean.Mobility ~ Treatment + Age + Treatment*Age + (1|FishName) + (1|Clutch), data = etho)
anova(m2, m6)  #these are the same, use the more simple model, m2

m7 <- lmer(Mean.Mobility ~ Treatment + Age + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
anova(m6, m7)  #m6 is better than m7, but m2 is still best

m8 <- lm(Mean.Mobility ~ Treatment + Age + Treatment*Age + scale(VarTurnAngle, center=T, scale=T) , data = etho)
anova(m8, m5) #these are the same, so use without interaction effect

m9 <- lmer(Mean.Mobility ~ Treatment + Age + scale(VarTurnAngle, center=T, scale=T) + (1|Clutch), data = etho)
anova(m9, m2) #m2 is still best

anova(m1, m2, m3, m4, m5, m6, m7, m8, m9) 

summary(m2)
#because the std.dev overlaps the variance, i think that means that clutch is unnecessary, so try without it

m0 <- lmer(Mean.Mobility ~ Treatment + Age + scale(VarTurnAngle, center=T, scale=T)+ (1|FishName) + (1|Paternity), data = etho)
anova(m2, m0)  #these are the same, so go with the more simple model

etho$varangle <- scale(etho$VarTurnAngle, center=T, scale=T)
rptM = rpt(Mean.Mobility ~ Treatment + Age + (1|FishName) + (1|Paternity), data = etho, grname = "FishName", datatype = "Gaussian", nboot = 1000, npermut = 500)
summary(rptM)
#R = .0916 , p < .001
#fit is singular, but random effects both seem to account for significant amount of variance, since full model fits better

theta <- getME(m0, "theta")
diag.element <- getME(m2, "lower")==0
which(theta[diag.element]<1e-5) #looks like all of these are important


##CumDur.Z2
  #this is a PROPORTION, so will be a binomial family
  #find best data transformation
# @Gina - I'm not 100% about this, but I think we should not transform proportion data if we want to use the binomial family 
# once we transform the proportion data with log or arcsine, or potentially even scale it, it loses the binomial distribution qualities so we would not be able to use proportion/binomial models

logitTransform <- function(p) { log(p/(1-p)) }
asinTransform <- function(p) { asin(sqrt(p)) }
hist(etho$CumDur.Z2/100)
hist(logitTransform((etho$CumDur.Z2+1)/100))
plotNormalHistogram(etho$CumDur.Z2/100)
plotNormalHistogram(log((etho$CumDur.Z2+1)/100))
plotNormalHistogram(logitTransform((etho$CumDur.Z2+1)/100))
plotNormalHistogram(asinTransform(etho$CumDur.Z2/100))

qqnorm(logitTransform((etho$CumDur.Z2+1)/100)) 
qqline(logitTransform((etho$CumDur.Z2+1)/100))
qqnorm(asinTransform((etho$CumDur.Z2+1)/100)) 
qqline(asinTransform((etho$CumDur.Z2+1)/100))
qqnorm(log((etho$CumDur.Z2+1)/100)) 
qqline(log((etho$CumDur.Z2+1)/100))

rangeScale <- function(x) { (x-min(x)) / (max(x)-min(x)) }
it <- rangeScale(logitTransform((etho$CumDur.Z2+1)/100))
it2 <- rangeScale(asinTransform((etho$CumDur.Z2+1)/100))
it3 <- rangeScale(log((etho$CumDur.Z2+1)/100))
plot((etho$CumDur.Z2+1)/100, it, col="red")
points((etho$CumDur.Z2+1)/100, it2, col="blue")
points((etho$CumDur.Z2+1)/100, it3, col="yellow")
#according to http://strata.uga.edu/8370/rtips/proportions.html#:~:text=1%20Logit%20transformation%20The%20logit%20transformation%20is%20the,effects%20...%204%20Recommendations%20...%205%20References%20
  #the transformation with the largest curve is the best BUT when put in the model (below), looks like arcsine is the better fit for the model

d.1 <- lmer(rangeScale(logitTransform((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
# @Gina - I get the warning about these 3 models being singular. I found where Ben Bolker addresses this here: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#singular-models-random-effect-variances-estimated-as-zero-or-correlations-estimated-as---1
# It does have to do with overfitted models and/or the random effects accounting for no or low variance. He recommends this code for checking for small values in the variance-covariance matrix:
theta <- getME(d.1, "theta")
diag.element <- getME(d.1, "lower")==0
which(theta[diag.element]<1e-5) # so maybe we drop Paternity

d.2 <- lmer(rangeScale(asinTransform((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
theta <- getME(d.2, "theta")
diag.element <- getME(d.2, "lower")==0
which(theta[diag.element]<1e-5)

d.3 <- lmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
theta <- getME(d.3, "theta")
diag.element <- getME(d.3, "lower")==0
which(theta[diag.element]<1e-5)

anova(d.1, d.2, d.3)  #this looks like asin transformation is best, go with that for now
# @Gina - I'm not sure we can use the likelihood ratio test to evaluate the best model among options that all have the same variables 
# I think it is meant for comparing models to see which variables to include/exclude (nested models).
# Instead, I usually see people using the diagnostic plots and the code to check the model assumptions to determine which dependent variable transformation produces the best fit model.
  #NOTE if you do rangeScale(logit, asin, log), log looks best, if you dont scale it, asin transformation is better

D.2 <- glmer(rangeScale(asinTransform((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho, family = "binomial")
anova(d.2, D.2)  #this says that glmer with binomial dist fits much better than lmer

D.1 <- glmer(rangeScale(logitTransform((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho, family = "binomial")
D.3 <- glmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho, family = "binomial")
anova(D.1, D.2, D.3)  #D.2 is best glmer model

anova(d.3, D.2)  #this suggests that a log of CumDur fit to a linear model with a Gaussian dist is better than a glm with a Binomial dist

d1 <- lmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(d1)  #is singular - paternity accounts for zero variance, clutch and fishname also seem unhelpful
anova(d1)

d2 <- lmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName) + (1|Clutch), data = etho)
summary(d2)
anova(d1, d2) #they are the same, so use fewer params, aka d2

d3 <- lmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName), data = etho)
summary(d3)
anova(d2, d3) #they are the same, so use fewer params, aka d3

d4 <- lmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho) #scales are off on interaction effect
summary(d4)  #is singular cuz random effects account for no variance
anova(d4, d3) #they are the same, so use fewer params, aka d3

d5 <- lm(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + Treatment*Age, data = etho)
summary(d5)
anova(d3, d5) #they are the same, so use fewer params, aka d3

d6 <- lm(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age , data = etho)
anova(d3, d5, d6)  #nearly the same, simpler model is d6

d7 <- lmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + Treatment*Age + (1|FishName), data = etho)
anova(d7, d6)  #d6 is better
anova(d3, d6)  #without the random effects is better
  #I think that means that there isnt expected to be repeatability here
  #BUT need a random effect for rpt
d13 <- lmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|Clutch), data = etho) 
d23 <- lmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|Paternity), data = etho)
anova(d3, d13, d23) #these are all the same, but paternity is the slightly better fit

rangeScale <- function(x) { (x-min(x)) / (max(x)-min(x)) }
etho$cumdur <- rangeScale(log((etho$CumDur.Z2+1)/100))
etho$CUMDUR <- rangeScale(asinTransform((etho$CumDur.Z2+1)/100))
rptD1 = rpt(cumdur ~ Treatment + Age + (1|Paternity), data = etho, grname = "Paternity", datatype = "Gaussian", nboot = 100, npermut = 100)
summary(rptD1) #paternity can be removed therefore probs not a good model
rptD2 = rpt(cumdur ~ Treatment + Age + (1|FishName), data = etho, grname = "FishName", datatype = "Gaussian", nboot = 100, npermut = 100)
summary(rptD2) 
  #R = .106 , p < 0.001
rptD3 = rpt(CUMDUR ~ Treatment + Age + (1|FishName), data = etho, grname = "FishName", datatype = "Proportion", nboot = 100, npermut = 100)
summary(rptD3)  #failed
rptD4 = rptBinary(CUMDUR ~ Treatment + Age + (1|FishName), data = etho, grname = "FishName", nboot = 100, npermut = 100)
summary(rptD4)  #failed
rptD5 = rptBinary(cumdur ~ Treatment + Age + (1|FishName), data = etho, grname = "FishName", nboot = 100, npermut = 100)
summary(rptD5)  #output looks like shit

#NOTE - rptD wont work if datatype = "Proportion" for logged duration or arcsine duraton, so fit it to Gaussian
#paternity seems to fit better than FishName in lmer, but in rpt, FishName seems more important

etho2$scale.age = scale(etho$Age, scale = T, center = T)
hold <- matrix(360, nrow=nrow(etho), ncol=1)
etho2 <- cbind(etho,hold)
etho$cumdur <- etho$CumDur.Z2/100
etho2$cumdur = round(etho2$CumDurZ2)
rptD6 = rpt(cbind(cumdur,hold) ~ Treatment + scale.age + (1|FishName), data = etho2, grname = "FishName", datatype = "Proportion", nboot = 100, npermut = 100)
summary(rptD6)  
plot(rptD6, type="permut")


#check assumptions for distribution (binomial)
library(DHARMa)
simulationOutput1 <- simulateResiduals(fittedModel = glmer((etho$CumDur.Z2/100) ~ Treatment + Age + (1|FishName), family=binomial, data=etho), n=250) #250 simulations, but if want higher precision change n>1000; transform because trials does not fit a poisson distribution.
  #warning: In eval(family$initialize) : non-integer #successes in a binomial glm! == online looks like this warning can be ignored
plot(simulationOutput1$scaledResiduals) #Expect a flat distribution of the overall residuals, and uniformity in y direction if plotted against any predictor
  #looks somewhat uniform but with variance
testDispersion(simulationOutput1) #if under- or over-dispersed, then p-value<0.05, but then check the dispersion parameter and try to determine what in the model could be the cause and address it there, also check for zero inflation.
  #p < 0.001
testZeroInflation(simulationOutput1) #compare expected vs observed zeros, not zero-inflated if p>0.05.
  #p < 0.001
testUniformity(simulationOutput1) #check for heteroscedasticity ("a systematic dependency of the dispersion / variance on another variable in the model" Hartig, https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html), which is indicated if dots aren't on the red line and p<0.05.
  #p < 0.001
plot(simulationOutput1)

simulationOutput2 <- simulateResiduals(fittedModel = glmer(rangeScale(asinTransform((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName), family=binomial, data=etho), n=250) #250 simulations, but if want higher precision change n>1000; transform because trials does not fit a poisson distribution.
  #is singular and has similar warning to above
plot(simulationOutput2$scaledResiduals)
  #looks similar to plot above
testDispersion(simulationOutput2)
  #p = 0.432
testZeroInflation(simulationOutput2)
  #p < 0.001
testUniformity(simulationOutput2)
  #p < 0.001
plot(simulationOutput2)

simulationOutput3 <- simulateResiduals(fittedModel = glmer(((etho$CumDur.Z2+1)/100) ~ Treatment + Age + (1|FishName), family=binomial, data=etho), n=250) #250 simulations, but if want higher precision change n>1000; transform because trials does not fit a poisson distribution.
#is singular and warning "non-integer #successes in a binomial glm!"
plot(simulationOutput3$scaledResiduals)
#residuals are everywhere
testDispersion(simulationOutput3)
#p = 0.112
testZeroInflation(simulationOutput3)
#p < 0.001
testUniformity(simulationOutput3)
#p < 0.001
plot(simulationOutput3)

simulationOutput4 <- simulateResiduals(fittedModel = glmer(rangeScale(log((etho$CumDur.Z2+1)/100)) ~ Treatment + Age + (1|FishName), family=binomial, data=etho), n=250) #250 simulations, but if want higher precision change n>1000; transform because trials does not fit a poisson distribution.
#is singular and has similar warning to above
plot(simulationOutput4$scaledResiduals)
#looks similar to plot above
testDispersion(simulationOutput4)
#p < 0.001
testZeroInflation(simulationOutput4)
#p < 0.001
testUniformity(simulationOutput4)
#p < 0.001
plot(simulationOutput4)

#I *think* that since all of these seem significantly different from expected (zero inflation, non uniformity, dispersion), that suggests that the model can't be binomial and should try to fit as Gaussian
# @Gina - 

########
shapiro.test(residuals(log(etho$CumDur.Z2)))
shapiro.test(residuals(etho["Mean.Mobility"]))
#look into what to do with angles cuz it is bound by 180 in both directions

plotNormalHistogram(log(etho$Mean.Mobility))
shapiro.test(residuals(lm(log(Mean.Mobility)~Age,etho)))  #by age or by treatment
bartlett.test(Mean.Mobility~Age,etho)

boxplot(log(Mean.Mobility)~Age,etho,main="total distance")
fligner.test(Mean.Mobility~Treatment,etho) 
kruskal.test(Mean.Mobility~Treatment,etho)
dunnTest(Mean.Mobility~Treatment,etho,method="bh")

###Spaghetti Plots
library(CorrMixed)
Spaghetti.Plot(Dataset=etho, Outcome=TotDist, Id=FishName, Time=Age,
               xlim=c(0,190),ylim=c(0,max(etho$TotDist)))

Spaghetti.Plot(Dataset=etho, Outcome=LatencyZ2360, Id=FishName, Time=Age,
               xlim=c(0,190),ylim=c(0,360))

Spaghetti.Plot(Dataset=etho, Outcome=MeanAngVel, Id=FishName, Time=Age,
               xlim=c(0,190),ylim=c(0,max(etho$MeanAngVel)))

Spaghetti.Plot(Dataset=etho, Outcome=CumDur.Z2, Id=FishName, Time=Age,
               xlim=c(0,190),ylim=c(0,100))

Spaghetti.Plot(Dataset=etho, Outcome=MeanMeander, Id=FishName, Time=Age,
               Col = gt.cols[etho$Treatment], xlim=c(0,190),ylim=c(min(etho$MeanMeander),max(etho$MeanMeander)))


####Survival Model
#https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html
#https://rpkgs.datanovia.com/survminer/reference/ggsurvtable.html
setwd("C:/Users/ginab/Box/Old Computer/Grad School/BALL STATE/Thesis/Etho") #set working directory in the biometry folder on my computer)
etho=read.table("EthoData.csv",header=TRUE, sep=",")

#adjust data for survival analysis
for(i in 1:nrow(etho)){
  if(etho[i,3]==15){
    etho[i,3]=14
  }
} 
surv <- matrix(1,nrow=nrow(etho))
colnames(surv) <- c("Survive")
etho=cbind(etho,surv)
etho$Survive = ifelse(etho$LatencyZ2360<360,1,0)

library(coxme)
library(survminer)
library(ggplot2)
library(survival)

run0 <- survfit(Surv(LatencyZ2360,Survive)~Treatment+(1|Paternity), data =etho)
summary(run0)
ggsurvtable(run0, data=etho)

run1 <- coxme(Surv(LatencyZ2360,Survive)~Treatment+(1|FishName)+(Age), data =etho)
summary(run1)

run2 <- coxme(Surv(CumDurZ2,Survive)~Treatment+(1|FishName), data =etho)
summary(run2)

plot1 <- ggsurvtable(data=etho, run1)

plot1 <- ggsurvplot(survfit(run1), data=etho)




library(rcompanion)
library(car)
library(DescTools)
library(FSA)
library(dplyr)
library(lme4)
library(stargazer)
library(lmerTest)
library(gt)
library(ghibli)
library(scales)

head(etho)
dad = as.character(etho[,1])
clutch = as.character(etho[,2])
age = as.numeric(etho[,3])  #may want this to be character? consider later
id = as.character(etho[,4])
treat = as.character(etho[,7])

table(treat)
#treat
#0  25   5 
#477 419 320 
table(id)
table(age)
#age
#14  21  49  77 105 133 161 189 
#196 174 139 146 142 142 140 137 

#change the indv tested on day 15 to day 14
for(i in 1:nrow(etho)){
  if(etho[i,3]==15){
    etho[i,3]=14
  }
} #age = as.numeric(etho[,3])

#change latency to enter zone 2 "-" into NAs
for(i in 1:nrow(etho)){
  if((etho[i,19])=="-"){
    etho[i,19]=360
  }
}
#for(i in 1:nrow(etho)){
#  if(etho[i,19]==360){
#    etho[i,19]=0
#  }
#}
for(i in 1:nrow(etho)){     #15=MeanTimeZ2  
  if(etho[i,15]=="-"){
    etho[i,15]=0
  }
}
etho[,15]=as.numeric(etho[,15])


library(colorspace)
library(scales)
#gt.cols <- qualitative_hcl(6, "Dark2") #ghibli_palette('PonyoMedium')#[4]
gt.cols <- c("#FFC5D0", "#E2D4A8", "#A8E1BF", "#A4DDEF", "#E4CBF9", "#5F96C2", "#9189C7", "orchid2")

####Averages plot
col=1
plot(-100, -100 , xlab="age", ylab="total distance", xlim=c(0, max(age)), ylim=c(0, (max(etho[,8])/2.5)))
for(t in unique(treat)){
  dat = etho[etho[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,8], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[col], lwd = 8)
  col=col+1
}

plot(-100, -100 , xlab="age", ylab="latency zone2 - na =360", xlim=c(0, max(age)), ylim=c(0, 360))
for(t in unique(treat)){
  dat = etho[etho[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,20], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[1:3], lwd = 8)
}

plot(-100, -100 , xlab="age", ylab="mean.activity", xlim=c(0, max(age)), ylim=c(0, 1))
for(t in unique(treat)){
  dat = etho[etho[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,30], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[1:3], lwd = 8)
}

plot(-100, -100 , xlab="age", ylab="total.activity", xlim=c(0, max(age)), ylim=c(0, 6000))
for(t in unique(treat)){
  dat = etho[etho[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,31], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[1:3], lwd = 8)
}

plot(-100, -100 , xlab="age", ylab="mean.mobility", xlim=c(0, max(age)), ylim=c(0, 20))
for(t in unique(treat)){
  dat = etho[etho[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,32], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[3], lwd = 8)
}


############
##### Generic plotting code
#1=Paternity, 2=Clutch, 3=Age, 4=FishName, 5=Trial, 6=Date, 7=Treatment, 8=TotDist, 9=MeanVel, 10=MeanTimeZ1, 11=FreqZ1, 12=CumDurZ1, 13=LatencyZ1, 
#14=CumDur.Z1, 15=MeanTimeZ2, 16=MeanTimeZ2Zero, 17=FreqZ2, 18=CumDurZ2, 19=LatencyZ2, 20=LatencyZ2360, 21=CumDur.Z2, 22=MeanMeander, 23=TotMeander, 
#24=FreqZAlter, 25=FreqTransZ1.Z2, 26=FreqTransZ2.Z1, 27=MeanTurnAngle, 28=VarTurnAngle, 29=MeanAngVel, 30=Mean.Activ, 31=Tot.Act, 32=Mean.Mobility

{
var = 8
varname = "Mean % Active"
range(etho[,var])
#if(anyNA(etho[,var]==TRUE)){
#  hold<- na.omit(etho)
#  etho <- hold
#}


ymin <- round(min(etho[,var]), digits = 2)#-.1
ymax <-(round(max(etho[,var]), digits = 2))#+.1
ln.alph <- 0.5
pt.alph <- 1.25
diff <- 0.15
xmin <- 0
xmax <- 200
offsets <- c(-0.5, 0, 0.5, 0.1) #c(-0.2, -0.1, 0, 0.1, 0.2)
orig.xs <- c(14, 21, 49, 77, 105, 133, 161, 189) #years of interest 
text.size <- 1.75
pt.cex <- 1.25
lwd <- 4

## make plot
plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
     xaxt = 'n', main = varname, xlab = 'Age', ylab = varname,
     cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
#axis(2, at = c(-0.1, 0, 0.1, 0.2), cex.axis = text.size)
axis(1, at = c(14, 21, 49, 77, 105, 133, 161, 189), labels = c('', '21', '49','77','105', '133', '161','189'), cex.axis = text.size)
#abline(h = 0, lty = 2)

col <- 1
for(c in unique(etho[,7])){                #iterate over treatment
  print(c)
  temp <- etho[etho[,7] == c,, drop=FALSE] #separate by treatment
  
  y1<-temp[temp[,3] == orig.xs[1],,]       #grab ages
  y2<-temp[temp[,3] == orig.xs[2],,]
  y3<-temp[temp[,3] == orig.xs[3],,]
  y4<-temp[temp[,3] == orig.xs[4],,]
  y5<-temp[temp[,3] == orig.xs[5],,]
  y6<-temp[temp[,3] == orig.xs[6],,]
  y7<-temp[temp[,3] == orig.xs[7],,]
  y8<-temp[temp[,3] == orig.xs[8],,]
  
  xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
  #columns <- c(18, 19, 20, 21)
  lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd)
  points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
  arrows(x0 = xs, y0 = c(mean(y1[,var])-sd(y1[,var]), mean(y2[,var])-sd(y2[,var]), mean(y3[,var])-sd(y3[,var]), mean(y4[,var])-sd(y4[,var]), mean(y5[,var])-sd(y5[,var]), mean(y6[,var])-sd(y6[,var]), mean(y7[,var])-sd(y7[,var]), mean(y8[,var])-sd(y8[,var])), 
         y1 = c(mean(y1[,var])+sd(y1[,var]), mean(y2[,var])+sd(y2[,var]), mean(y3[,var])+sd(y3[,var]), mean(y4[,var])+sd(y4[,var]), mean(y5[,var])+sd(y5[,var]), mean(y6[,var])+sd(y6[,var]), mean(y7[,var])+sd(y7[,var]), mean(y8[,var])+sd(y8[,var])), 
         lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
  #  for(l in unique(orig.xs)){
  #    column <- columns[l]
  #    ## 95% CIs (inappropriate for large sample sizes)
  #    # arrows(x0 = xs[l], x1 = xs[l], y0 = (mean(temp[,column], na.rm = TRUE) - (sd(temp[,column], na.rm = TRUE)/10*1.96)),
  #    #        y1 = (mean(temp[,column], na.rm = TRUE) + (sd(temp[,column], na.rm = TRUE)/10*1.96)),
  #    #        lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
  #  arrows(x0 = OTime[,1], y0 = (OTime[,2])-(sd(OTime[,7])), y1 = (OTime[,2])+(sd(OTime[,7])), lwd = .5, col = "black", code=3, angle=90, length=0.1)
  
  #    
  #    arrows(x0 = xs[l], x1 = xs[l], y0 = quantile(temp[,column], probs = c(0.025,0.975))[1],   #will need to do this for each year of interest
  #           y1 = quantile(temp[,column], probs = c(0.025,0.975))[2],
  #           lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
  #  }    
  col <- col+1
}
}
legend('topleft', legend = c('Control', '5 ug/L','25 ug/L'), col = gt.cols, pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = FALSE, x.intersp = 0.2)



#####
#find indv that have been measured all 8 times
#looks like there are 225 indv, 107 lived whole time
lived = NULL
for(w in unique(etho[,4])){
  if(length(which(etho$FishName==w))==8){
    lived = rbind(lived, w)
  }
}

all = etho[which(etho[,4]%in%lived),,drop=FALSE]

plot(-100, -100 , xlab="age", ylab="total distance", xlim=c(0, max(age)), ylim=c(0, (max(ad[,8])/2.5)))
for(t in unique(treat)){
  dat = ad[ad[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,8], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[1:3], lwd = 8)
}
for(t in unique(treat)){
  dat = ad[ad[,7]==t,,drop=FALSE]
  points(dat[,3],dat[,8],col="black")
}

plot(-100, -100 , xlab="age", ylab="latency zone2 - na =360", xlim=c(0, max(age)), ylim=c(0, 360))
for(t in unique(treat)){
  dat = ad[ad[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,20], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[1:3], lwd = 8)
}

plot(-100, -100 , xlab="age", ylab="mean.activity", xlim=c(0, max(age)), ylim=c(0, 1))
for(t in unique(treat)){
  dat = ad[ad[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,30], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[1:3], lwd = 8)
}

plot(-100, -100 , xlab="age", ylab="total.activity", xlim=c(0, max(age)), ylim=c(0, 6000))
for(t in unique(treat)){
  dat = ad[ad[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,31], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[1:3], lwd = 8)
}

plot(-100, -100 , xlab="age", ylab="mean.mobility", xlim=c(0, max(age)), ylim=c(0, 20))
for(t in unique(treat)){
  dat = ad[ad[,7]==t,,drop=FALSE]
  avg = aggregate(x = dat[,32], by = list(dat[,3]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[3], lwd = 8)
}


sm = all[all[,3]<100,,drop=FALSE]
ad = all[all[,3]>100,,drop=FALSE]
ALL = all


######################
#analysis

plotNormalHistogram(all$TotDist)
shapiro.test(residuals(lm(TotDist~Treatment,all)))
bartlett.test(TotDist~Treatment,all)

boxplot(TotDist~Treatment,all,main="total distance")
fligner.test(TotDist~Treatment,all) 
kruskal.test(TotDist~Treatment,all)
dunnTest(TotDist~Treatment,all,method="bh")


c6 <- lmer((TotDist)~Treatment+Age+(1|FishName), data = all) 
summary(c6)
anova(c6)

c1 <- lmer(LatencyZ2360~Treatment+(1|FishName)+(Age), data = all) 
summary(c1)
anova(c1)

c1 <- lmer(Tot.Act~Treatment+(1|FishName)+(Age), data = all) 
summary(c1)
anova(c1)



#######################
#Repeatabilities
#######################
#https://cran.r-project.org/web/packages/rptR/vignettes/rptR.html
#https://medium.com/@muhammadibrahim_54071/why-and-which-data-transformation-should-i-use-cfb9e31923cf
library(rptR)

rep1 <- rpt(Mean.Activ~Treatment+Age+(1|FishName)+(1|Clutch),
            grname = c("FishName","Clutch","Fixed"),
            data=ad, datatype="Gaussian",nboot=1000, npermut=0) #,ratio=FALSE for variance rather than repeatability
print(rep1)

rep2 <- rpt(LatencyZ2360~Treatment+Age+(1|FishName)+(1|Paternity),
            grname = c("FishName","Paternity", "Fixed"),
            data=ad, datatype="Gaussian",nboot=1000, npermut=0)
print(rep2)
plot(rep2, grname="FishName")

rep3 <- rpt(TotDist~Treatment+Age+(1|FishName)+(1|Clutch),
            grname = c("FishName","Clutch","Fixed"),
            data=ad, datatype="Gaussian",nboot=1000, npermut=0)
print(rep3)
plot(rep3, grname="Fixed")

##What I *think* this output means is that
  #7% repeatability has to do with indv
  #4% repeatability has to do with father
  #43% repeatability is due to fixed effects (age and treatment)



#######################
#OLD CODE BELOW -- 2019
#######################

#this code adapted from https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
#also based on "EthoCode" R file previously created

summary(Etho)
colnames(Etho) #check available variables
str(Etho) #check variable class

plot(Etho$TotDist~Etho$Treatment)
plot(Etho$TotDist~Etho$Age)

plot(Etho$MeanVel~Etho$Treatment)
plot(Etho$MeanVel~Etho$Age)

library(ggplot2) #recall ggplot package
g<-ggplot(Etho, aes (Age,MeanVel))  #create a graph called "g" of Etho data, treatment=x, total dist=y
g #recall graph (same as line 23)
summary(g) #get summary data of graph
print(g) #recall graph (same as line 21)

# ggplot adapted from https://www.youtube.com/watch?v=1fCP1465qFs "Scatter plot in r using ggplot // ggplot2 // part 3 // ask rahul" 
g<-ggplot(Etho, aes(Age,MeanVel))+geom_point() #supposed to put points on the graph
g #recall graph to put points on it
ggplot(Etho, aes(Age,MeanVel))+geom_point(color="steelblue") #changes the color of the points
ggplot(Etho, aes(Age,MeanVel))+geom_point(color="steelblue",size=2) #changes size of the points
ggplot(Etho, aes(Age,MeanVel))+geom_point(color="steelblue",size=2,alpha=1/2) #makes points translucent/lighter

ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color=Treatment)) #changes color of points based on the category of Treatment

ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color=Treatment))+labs(title = "plot of Age vs Mean Vel") #adds main title
ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color=Treatment))+labs(title = "plot of Age vs Mean Vel")+labs(x="Age")+labs(y="Mean Velocity") #adds x and y labels

ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color=Treatment))+theme_classic() #changes background to a theme when type "theme_" and different options appear

ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color=Treatment))+theme_classic(base_family="Calibri")+labs(title = "plot of Age vs Mean Vel")+labs(x="Age")+labs(y="Mean Velocity")

ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color="steelblue"))+geom_smooth() #this should put a smooth trendline through the data. the shaded area is the standard error/95% confidence level
ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color="steelblue"))+geom_smooth(method="ln") #this should put in a linear trendline/regression line
ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color=Treatment))+geom_smooth(method="ln",color = Treatment,size=2,linetype=2,se=FALSE) #this changes color of the lines to the Treatment (3 different lines), the size of the line, changes the type of the line, and takes away the standard error
ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color="steelblue"))+geom_smooth(method="ln")+facet_grid(Treatment~.) #vertically divide the plots by Treatment
ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color="steelblue"))+geom_smooth(method="ln")+facet_grid(.~Treatment) #horizontally divide the plots by Treatment 

ggplot(Etho, aes(Age,MeanVel))+geom_point(aes(color=Treatment))+geom_smooth(method="ln",color=Treatment)+facet_grid(.~Treatment)+theme_classic()+labs(title="Scatterplot of Age and Mean Velocity")+labs(x="Age")+labs(y="Mean Velocity") #put it alllllll together


