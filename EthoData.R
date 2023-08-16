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
#consider if those with only one/two readings should be removed?

#changes in embryo data: 
  #indv with # for an ID are given unique numbers rather than all being 1 or 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:/Users/ginab/Box/Old Computer/Grad School/BALL STATE/Thesis/2023_Etho,Embryo,Spinning/Personality_BMAA") #set working directory
etho=read.table("EthoData_updated.csv",header=TRUE, sep=",")  #see changes in "updated" dataset above

#change the indv tested on day 15 to day 14
for(i in 1:nrow(etho)){
  if(etho[i,3]==15){
    etho[i,3]=14
  }
}   

#set Treatment as a factor
etho$Treatment = as.factor(etho$Treatment)

#put arena size in the dataframe (mm diameter)
arena = matrix(nrow = nrow(etho), ncol = 1)
for(j in 1:nrow(etho)){
  if(etho[j,3]<=49){
    arena[j,1] = 58
  }else if(etho[j,3]==77){
    arena[j,1] = 89
  }else{
    arena[j,1] = 138
  }
}

#put arena area in the dataframe (mm^2)
area = matrix(nrow = nrow(etho), ncol = 1)
for(l in 1:nrow(etho)){
  area[l,1] = 3.14 * (arena[l,1]/2)^2
}

etho = cbind(etho,arena, area)

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
library(DHARMa)

{
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
#since var turn angle isnt being used anymore, no need to include as covariate
hist(etho$Mean.Mobility)
qqnorm(etho$Mean.Mobility)
qqline(etho$Mean.Mobility)

m1 <- lmer(Mean.Mobility ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(m1)

m2 <- lmer(Mean.Mobility ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(m2)
anova(m2, m1)  #m2 is better

m3 <- lmer(Mean.Mobility ~ Treatment + Age + (1|FishName) + (1|Clutch) , data = etho)
summary(m3)
anova(m3, m2)  #same, so go with the simpler model, m3

m4 <- lmer(Mean.Mobility ~ Treatment + Age + (1|FishName), data = etho) 
summary(m4)
anova(m4, m2)  #m2 is better

m5 <- lm(Mean.Mobility ~ Treatment + Age, data = etho)
summary(m5)
anova(m2, m5)  #m2 is better

m6 <- lmer(Mean.Mobility ~ Treatment + Age + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho) #scale is off here #lmer(Mean.Mobility ~ Treatment + Age + Treatment*Age + (1|FishName) + (1|Clutch), data = etho)
anova(m2, m6)  #these are the same, use the more simple model, m2

m7 <- lmer(Mean.Mobility ~ Treatment + Age + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
anova(m6, m7)  #m6 is better than m7, but m2 is still best

m8 <- lm(Mean.Mobility ~ Treatment + Age + Treatment*Age + scale(VarTurnAngle, center=T, scale=T) , data = etho)
anova(m8, m5) #these are the same, so use without interaction effect

m9 <- lmer(Mean.Mobility ~ Treatment + Age + (1|Clutch), data = etho)
anova(m9, m2) #m2 is still best

anova(m1, m2, m3, m4, m5, m6, m7, m8, m9) 

summary(m2)
#because the std.dev overlaps the variance, i think that means that clutch is unnecessary, so try without it

m0 <- lmer(Mean.Mobility ~ Treatment + Age + scale(VarTurnAngle, center=T, scale=T)+ (1|FishName) + (1|Paternity), data = etho)
anova(m2, m0)  #these are the same, so go with the more simple model

etho$varangle <- scale(etho$VarTurnAngle, center=T, scale=T)
rptM = rpt(Mean.Mobility ~ Treatment + Age + (1|FishName) + (1|Clutch), data = etho, grname = "FishName", datatype = "Gaussian", nboot = 1000, npermut = 500)
summary(rptM)
#R = .0916 , p < .001
#fit is singular, but random effects both seem to account for significant amount of variance, since full model fits better

theta <- getME(m3, "theta")
diag.element <- getME(m3, "lower")==0
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


#######
#Checking other approaches after meeting with Jonathan 
logitTransform <- function(p) { log(p/(1-p)) }
asinTransform <- function(p) { asin(sqrt(p)) }
hist(logitTransform((etho$CumDur.Z2+1)/100))

plot(etho$TotDist, etho$arena)
plot(etho$TotDist, etho$area)
#note, arena = diameter

etho$rat = etho$TotDist/etho$arena
hist(etho$rat)
hist(log(etho$rat+1))
hist(asinTransform(rat))

etho$rat2 = etho$TotDist/etho$area
hist(rat2)
hist(log(rat2+1))
hist(asinTransform(rat2))

library(DHARMa)
library(lme4)
etho$scale.age = scale(etho$Age, scale = T, center = T)
etho$FishName = as.factor(etho$FishName)
etho$Treatment = as.factor(etho$Treatment)
etho$scale.rat = scale(etho$rat, scale = T, center = T)+1
etho$scale.rat2 = scale(etho$rat2, scale = T, center = T)+1
hist(etho$TotDist)
etho$scale.totdis = scale(etho$TotDist, scale = T, center = T)+1

#Poisson 
simulationOutput <- simulateResiduals(fittedModel = glmer(round(TotDist) ~ Treatment + scale.age + Treatment*scale.age + (1|FishName) + (1|Clutch) + (1|Paternity) + offset(log(arena)), family = poisson, data=etho), n=500) 
#Negative binomial
simulationOutput <- simulateResiduals(fittedModel = glmer.nb(round(TotDist) ~ Treatment + scale.age + Treatment*scale.age + (1|FishName) + (1|Clutch) + (1|Paternity) + offset(log(arena)), data=etho), n=500)
#Gaussian
simulationOutput <- simulateResiduals(fittedModel = lmer(log(TotDist+1) ~ Treatment + scale.age + Treatment*scale.age + (1|FishName) + (1|Clutch) + (1|Paternity), data=etho), n=500)

#I tried rat, rat2, scale.rat, and scale.rat2 in all three families. those that worked were gaus(rat), pois(rat2), negbi(scale.rat2), gaus(rat2) -- none really looked good
#also checked out TotDist and scale.totdis and neither of those look great either

plot(simulationOutput$scaledResiduals) #Expect a flat distribution of the overall residuals, and uniformity in y direction if plotted against any predictor. 
testDispersion(simulationOutput) #if under- or over-dispersed, then p-value<0.05, but then check the dispersion parameter and try to determine what in the model could be the cause and address it there, also check for zero inflation.
#p.rgaus= 0.66, p.2rpois = 0.28, p.scaler2negbi = 0.92, p.r2gaus = 0.74         
#p.totdis.pois < 0.01 , p.totdis.negbi < 0.01, p.totdis.gaus = 0.74
testZeroInflation(simulationOutput) #compare expected vs observed zeros, not zero-inflated if p>0.05.
#p.rgaus= 1, p.2rpois < 0.01, p.scaler2negbi = 0.28, p.r2gaus = 1      
#p.totdis.pois = 1, p.totdis.negbi < 0.01, p.totdis.gaus = 1
testUniformity(simulationOutput) #check for heteroscedasticity ("a systematic dependency of the dispersion / variance on another variable in the model" Hartig, https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html), which is indicated if dots aren't on the red line and p<0.05.
#p.rgaus< 0.01, p.2rpois < 0.01, p.scaler2negbi = 0.04, p.r2gaus < 0.01     
#p.totdis.pois < 0.01, p.totdis.negbi < 0.01, p.totdis.gaus < 0.01
plot(simulationOutput)

#not convinced this is better
  #*maybeee* log(totdis+1) would be ok but still not fully convinced



t1 <- glmer(round(TotDist) ~ Treatment + scale.age + (1|FishName) + (1|Clutch) + (1|Paternity) + offset(log(arena)), data = etho, family = "poisson") #arena as offset
summary(t1) 
anova(t1)

t2 <- lmer(log(TotDist+1) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity) + log(area), data = etho) #area as offset
summary(t2) 
anova(t1,t2)

t3 <- lmer(log(TotDist+1) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity) , data = etho) 
summary(t3) #is singular
anova(t1,t2, t3) #t1 is better, t1 and t2 are same

t4 <- lmer(log(TotDist+1) ~ Treatment + Age + (1|FishName) + (1|Clutch) + log(arena), data = etho) #arena as offset
summary(t4) 
anova(t4, t1) #same, go with simpler t4

t5 <- lmer(log(TotDist+1) ~ Treatment + Age + (1|FishName)  + log(arena), data = etho) #arena as offset
summary(t5) 
anova(t5, t4) #t4 better

t6 <- lmer(log(TotDist+1) ~ Treatment + Age + (1|Clutch)  + log(arena), data = etho) #arena as offset
summary(t6) 
anova(t6, t4) #t4 better

t7 <- lmer(log(TotDist+1) ~ Treatment + Age + (1|FishName) + (1|Clutch) + log(area), data = etho) #arena as offset
summary(t7) 
anova(t4, t7) #same, go with t4

rptT = rpt(log(TotDist+1) ~ Treatment + Age + (1|FishName) + (1|Clutch) + log(area), data = etho, grname = "FishName", datatype = "Gaussian", nboot = 1000, npermut = 100)
summary(rptT) #is singular
plot(rptT)
plot(rptT, type="permut")
#R = 0.0481 , p = 0.004 
#fit is singular, but random effects all seem to account for a significant amount of variance, since full model fits better

#we are going to proceed without the total distance (6/28/23)
}

#####################################################
#####################################################

###Step 5: Determine if we want to include embryo data here
#At day 5 post lay, a subset of embryos were removed from the tiles they were laid on and put in an open petri dish under a compound microscope. 
#For 60s trials, embryos were recorded and burst activity, burst count, and activity/inactivity were calculated using Danioscope software.
#embryos were exposed for 5 days to 5 treatments of BMAA: control, 5, 25, 125, 625 ng/uL and then were raised for additional assays.

setwd("C:/Users/ginab/Box/Old Computer/Grad School/BALL STATE/Thesis/2023_Etho,Embryo,Spinning/Personality_BMAA") #set working directory
bby=read.table("BMAA_EmbryoData_updated.csv",header=TRUE, sep=",")  #BMAA_EmbryoData_Raw2023
#458 datapoints total.. 245 indvs , 213 from another experiment (note other experiment has additional treatments!)
#up to this point, indv are treated all the same, except in daniscope (to get these values) indvs were recorded individually while non indvs were recorded as a group (should not change results)
#therefore, should probably include all embryos from control, 5, 25 treatments

library(DHARMa)
library(lme4)

#FOR FULL DATASET
#remove babies that are in the highest treatments
rm = bby[bby[,3]=="125"|bby[,3]=="625",,drop=F]
'%NOTin%' <- Negate(`%in%`)
duced = bby[which(bby[,14]%NOTin%rm[,14]),,drop=F]
table(duced[,3])
bby=duced

#FOR REDUCED DATASET
#remove group indvs
not = bby[bby[,13]=="n",,drop=F]  #32 in control, 59 in 5, 37 in 25
indv = bby[bby[,13]=="y",,drop=F]
bby = indv

head(bby)
str(bby)

#set Treatment, Indv, Video File, and ID as a factor
bby$TreatmentFactor = as.factor(bby$TreatmentFactor)
bby$indv = as.factor(bby$indv)
bby$ID = as.factor(bby$ID)
bby$Video.File= as.factor(bby$Video.File)

hist(bby$Mean_Burst_Activity_...)  #this is the % of trial time that embryo was active (proportion!)
hist(bby$Mean_Burst_Count)
plot(bby$Mean_Burst_Count.Minute~bby$Mean_Burst_Count)  #both are the same, since each trial was 60s
hist(bby$Mean_Burst_Duration_.s.)  #hella zero inflated
plot(bby$Mean_Inactivity_...~bby$Mean_Burst_Activity_...)  #both are the same, but inversely related since high activity means low inactivity
plot(bby$Mean_Inactivity_Duration_.s.~bby$Mean_Burst_Duration_.s.)  #these are not the same! consider the differences of each
hist(bby$Mean_Inactivity_Duration_.s.) #how are there values >60 ?? look into this!
hist(bby$Mean_Total_Burst_Duration_.s.)
plot(bby$Mean_Total_Burst_Duration_.s.~bby$Mean_Burst_Activity_...)  #these are the same

plot(bby$Mean_Burst_Activity_...~bby$TreatmentFactor)
plot(bby$Mean_Burst_Count~bby$TreatmentFactor)
plot(bby$Mean_Burst_Duration_.s.~bby$TreatmentFactor)

plot(bby$Mean_Burst_Activity_...~bby$indv)
plot(bby$Mean_Burst_Count~bby$indv)
plot(bby$Mean_Burst_Duration_.s.~bby$indv)

#there doesnt seem to be any super interesting patterns for the influence of treatment on activity, burst count, or burst duration

#check to see if there are differences in the group vs indv.
library(ggplot2)
ggplot(bby, aes(x = indv, y = Mean_Burst_Count)) +
  facet_wrap(~TreatmentFactor) +
  geom_boxplot() +
  theme_classic()
ggplot(bby, aes(x = indv, y = Mean_Burst_Duration_.s.)) +
  facet_wrap(~TreatmentFactor) +
  geom_boxplot() +
  theme_classic()
ggplot(bby, aes(x = indv, y = Mean_Burst_Activity_...)) +
  facet_wrap(~TreatmentFactor) +
  geom_boxplot() +
  theme_classic()
#in all three variables, there are some large outliers - consider these.
#in all three variables, indvs videoed in the group have a *slightly* higher average than those recorded as indv - potentially due to higher outliers pulling the means up
  #potential argument for conspecific/ group effects ?

sm = bby[bby$Mean_Burst_Activity_...<=15,,drop=F]
plot(sm$Mean_Burst_Activity_...~sm$TreatmentFactor)

sm = bby[bby$Mean_Burst_Count<=6,,drop=F]
plot(sm$Mean_Burst_Count~sm$TreatmentFactor)

sm = bby[bby$Mean_Burst_Duration_.s.<=10,,drop=F]
plot(sm$Mean_Burst_Duration_.s.~sm$indv)

#if I remove outliers, the means of all three variables are the same across treatments..
  #however, the variance may increase as treatment increases in Burst Count and Activity 
  #interestingly, the variance may decrease as treatment increases in Burst Duration  
  #this *ehh somewhat* follows what we expected, since BMAA can cause convulsions, is neuroexcitatory, and reduces locomotor performance ...Butttt might be a stretch

corbby = bby[,4:10]
C <- cor(corbby) #gives correl table - look for those > 0.5
library(corrplot)
corrplot(C, method = "number")
#obvi, some are highly correlated, others are not

#thoughts on variables to choose.. 
  #to be consistent with etho data, a duration (i.e., burst duration* or burst activity) and burst count would be best
  #response variables: Mean_Burst_Count and Mean_Burst_Duration_.s.
  #fixed effect: TreatmentFactor, indv (aka if assay was indv or in a group)
  #random effects: clutch, parentage (aka Paternity), video.file (if recorded indv or in group) -- note not ID since only one recording of each

#FULL Model : count/duration ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage) + (1|Video.File)

#look at dif variables and look for treatment patterns, if any -- check -- there arent any interesting patterns
#figure out if indv vs group should be included -- check -- looks like it could go either way really - depends on the conclusions we want to draw           
#then look at if behaviors relate to the three personality traits
#then fit models to the data




#~~~~~~~~~~~
###start with burst count
#Poisson 
simulationOutput <- simulateResiduals(fittedModel = glmer(Mean_Burst_Count ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage) + (1|Video.File), family = poisson, data=bby), n=500) 
#Negative binomial
simulationOutput <- simulateResiduals(fittedModel = glmer.nb(Mean_Burst_Count ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage) + (1|Video.File), data=bby), n=500) 
#Gaussian
simulationOutput <- simulateResiduals(fittedModel = lmer(Mean_Burst_Count ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage) + (1|Video.File), data=bby), n=500) #is singular


plot(simulationOutput$scaledResiduals) #Expect a flat distribution of the overall residuals, and uniformity in y direction if plotted against any predictor. 
testDispersion(simulationOutput) #if under- or over-dispersed, then p-value<0.05, but then check the dispersion parameter and try to determine what in the model could be the cause and address it there, also check for zero inflation.
#p.p = 0.34 , p.nb = 0.38 , p.g = 0.96 (full dataset)
#p.p = 0.048 , p.nb = 0.74 , p.g = 0.96 -- reduced dataset

testZeroInflation(simulationOutput) #compare expected vs observed zeros, not zero-inflated if p>0.05.
#p.p = 0.94 , p.nb = 0.43 , p.g < 0.001 (full dataset)
#p.p = 0.15 , p.nb = 0.76 , p.g < 0.001 - reduced dataset
testUniformity(simulationOutput) #check for heteroscedasticity ("a systematic dependency of the dispersion / variance on another variable in the model" Hartig, https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html), which is indicated if dots aren't on the red line and p<0.05.
#p.p = 0.032 , p.nb = 0.028 , p.g < 0.001 -- looks weirddd (full dataset)
#p.p = 0.05 , p.nb = 0.31 , p.g < 0.001 - reduced dataset

plot(simulationOutput)
#either poisson or neg binomial would work. going with poisson for now -- note Poisson works for total embryo and reduced (only indvs) datasets!

t1 <- glmer(Mean_Burst_Count ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage) + (1|Video.File), data = bby, family = "poisson") 

t2 <- glmer(Mean_Burst_Count ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage), data = bby, family = "poisson")
anova(t1,t2) #full model (t1) is better

t3 <- glmer(Mean_Burst_Count ~ TreatmentFactor  + (1|Clutch) + (1|Parentage) + (1|Video.File), data = bby, family = "poisson") #failed to converge
anova(t1, t3)

t4 <- glmer(Mean_Burst_Count ~ TreatmentFactor + indv + (1|Clutch)  + (1|Video.File), data = bby, family = "poisson") 
anova(t1, t4) #similar but t1 is better even tho it has another variable

t5 <- glmer(Mean_Burst_Count ~ TreatmentFactor + indv  + (1|Parentage) + (1|Video.File), data = bby, family = "poisson") 
anova(t1,t5) #same, t5 is more simple

t6 <- glmer(Mean_Burst_Count ~ TreatmentFactor + indv + (1|Video.File), data = bby, family = "poisson") 
t7 <- glmer(Mean_Burst_Count ~ TreatmentFactor + indv + (1|Parentage), data = bby, family = "poisson")
anova(t7,t5) #t5 is better than t6 & t7

t8 <- glmer(Mean_Burst_Count ~ TreatmentFactor  + (1|Parentage) + (1|Video.File), data = bby, family = "poisson") 
anova(t5, t8) #t5 is better

t9 <- glmer(Mean_Burst_Count ~ indv  + (1|Parentage) + (1|Video.File), data = bby, family = "poisson") 
anova(t9,t5) #same, but treatment matters so keep it
anova(t1, t4,t5) #same 


#seems like being in a group matters -- ggplot and facet by this. also see how patterns hold up when removing group indv completly
#maybe group vs indv best in another paper and just indv good for personality?

#RE discussion with Avril: fixed will be best if we want to evaluate the differences in indv vs group, random will be best if we just want to control for it

summary(t2) #no significant effect of treatment on embryo burst count
boxplot(bby$Mean_Burst_Count~bby$TreatmentFactor)

#check another way -- nonparametric tests since not normal
plotNormalHistogram(bby$Mean_Burst_Count)
shapiro.test(residuals(lm(Mean_Burst_Count~TreatmentFactor,bby)))
bartlett.test(Mean_Burst_Count~TreatmentFactor,bby)

fligner.test(Mean_Burst_Count~TreatmentFactor,bby) 
kruskal.test(Mean_Burst_Count~TreatmentFactor,bby)
dunnTest(Mean_Burst_Count~TreatmentFactor,bby,method="bh")
#when not controlling for clutch or parentage, no signif differences in treatment

#~~~~~~~~~~~
#then burst duration
#Poisson 
simulationOutput <- simulateResiduals(fittedModel = glmer(round(Mean_Burst_Duration_.s.) ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage) + (1|Video.File), family = poisson, data=bby), n=500) 
#Negative binomial
simulationOutput <- simulateResiduals(fittedModel = glmer.nb(round(Mean_Burst_Duration_.s.) ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage) + (1|Video.File), data=bby), n=500) #is singular
#Gaussian
simulationOutput <- simulateResiduals(fittedModel = lmer(Mean_Burst_Duration_.s. ~ TreatmentFactor + indv + (1|Clutch) + (1|Parentage) + (1|Video.File), data=bby), n=500) #is singular


plot(simulationOutput$scaledResiduals) #Expect a flat distribution of the overall residuals, and uniformity in y direction if plotted against any predictor. 
testDispersion(simulationOutput) #if under- or over-dispersed, then p-value<0.05, but then check the dispersion parameter and try to determine what in the model could be the cause and address it there, also check for zero inflation.
#reduced data: p.p = 0.028 , p.nb = 0.11 , p.g = 0.84
#full data: p.p = 0.02 , p.nb = 0.02 , p.g = 0.84
testZeroInflation(simulationOutput) #compare expected vs observed zeros, not zero-inflated if p>0.05.
#reduced data: p.p = 0.38 , p.nb = 0.29 , p.g < 0.001
#full data: p.p = 0.18 , p.nb = 0.004 , p.g < 0.001
testUniformity(simulationOutput) #check for heteroscedasticity ("a systematic dependency of the dispersion / variance on another variable in the model" Hartig, https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html), which is indicated if dots aren't on the red line and p<0.05.
#reduced data: p.p < 0.001 , p.nb = 0.01 , p.g < 0.001
#full data: p.p = 0.10 , p.nb < 0.001 , p.g < 0.001
plot(simulationOutput)
#either poisson or neg binomial would work for reduced data. going with poisson for now -- note Poisson works for total embryo and reduced (only indvs) datasets!

t1 <- glmer(round(Mean_Burst_Duration_.s.) ~ TreatmentFactor + (1|ID) + (1|Clutch) + (1|Parentage), data = bby, family = "poisson") 

t2 <- glmer(round(Mean_Burst_Duration_.s.) ~ TreatmentFactor + (1|ID) + (1|Clutch), data = bby, family = "poisson")
anova(t1,t2) #same, use simpler model t2

t3 <- glmer(round(Mean_Burst_Duration_.s.) ~ TreatmentFactor + (1|ID), data = bby, family = "poisson")
anova(t2, t3) #t2 is better

t4 <- glmer(round(Mean_Burst_Duration_.s.) ~ TreatmentFactor + (1|ID) + (1|Parentage), data = bby, family = "poisson") 
anova(t2, t4) #t2 is better

t5 <- glmer(round(Mean_Burst_Duration_.s.) ~ TreatmentFactor + (1|Clutch) + (1|Parentage), data = bby, family = "poisson") 
anova(t2,t5) #t2 is better

#note for reduced dataset, t1 is better than t2 and t4 is best of all

summary(t2) #for full data: treatments 125, 5, 0 significantly different in embryo burst duration
summary(t4) #for reduced data: intercept is signif??
boxplot(bby$Mean_Burst_Duration_.s.~bby$TreatmentFactor)

#check another way -- nonparametric tests since not normal
plotNormalHistogram(bby$Mean_Burst_Duration_.s.)
shapiro.test(residuals(lm(Mean_Burst_Duration_.s.~TreatmentFactor,bby)))
bartlett.test(Mean_Burst_Duration_.s.~TreatmentFactor,bby)

fligner.test(Mean_Burst_Duration_.s.~TreatmentFactor,bby) 
kruskal.test(Mean_Burst_Duration_.s.~TreatmentFactor,bby)
dunnTest(Mean_Burst_Duration_.s.~TreatmentFactor,bby,method="bh")
#when not controlling for clutch or parentage, no signif differences in treatment


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


