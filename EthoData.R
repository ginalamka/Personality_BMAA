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

setwd("C:/Users/ginab/Box/Old Computer/Grad School/BALL STATE/Thesis/2023_Etho,Embryo,Spinning/Personality_BMAA") #set working directory
etho=read.table("pca_cutethodata.csv",header=TRUE, sep=",")
etho=read.table("pca_supercutethodata.csv",header=TRUE, sep=",")
str(etho)

#FOR CUT ETHO DATA ONLY--change the indv tested on day 15 to day 14
for(i in 1:nrow(etho)){
  if(etho[i,1]==15){
    etho[i,1]=14
  }
} #age = as.numeric(etho[,3])  


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

data = matrix(nrow=nrow(etho), ncol=8)
data <- cbind(age, pat, treat, dist, dur, lat, mea, angv, actv)  #frq, actv
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
library(lmtest)
library(rptR)

#MeanAngVel
hist(log(abs(etho$MeanAngVel)))
hist(log(abs(scale(etho$MeanAngVel))))
qqnorm(log(abs(etho$MeanAngVel)))
qqline(log(abs(etho$MeanAngVel)))
qqnorm(log(abs(scale(etho$MeanAngVel))))
qqline(log(abs(scale(etho$MeanAngVel))))  #doesnt seem like scaling helps

v1 <- lmer(log(abs(etho$MeanAngVel)) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(v1)  #is singular - no variance due to Paternity
anova(v1)

v2 <- lmer(log(abs(etho$MeanAngVel)) ~ Treatment + Age + (1|FishName) + (1|Clutch), data = etho)
summary(v2)   #is singular - no variance due to indv ID
anova(v2)

v3 <- lmer(log(abs(etho$MeanAngVel)) ~ Treatment + Age + (1|FishName), data = etho)
summary(v3)
anova(v3)

v4 <- lmer(log(abs(etho$MeanAngVel)) ~ Treatment + Age + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho) #scale is off here
summary(v4)  #is singular - no variance due to indv ID or Paternity
anova(v4)

v5 <- lm(log(abs(etho$MeanAngVel)) ~ Treatment + Age + Treatment*Age, data = etho)
summary(v5)
anova(v5)

v6 <- lmer(log(abs(etho$MeanAngVel)) ~ Treatment + Age + (1|Clutch), data = etho)
summary(v6)   
anova(v6)


anova(v1, v2, v3, v4, v5, v6, test="LRT") #I think this is saying v3 is the best?
lrtest(v1, v2, v3, v4, v5, v6)            #but I think this is suggesting differently?

logangvel = log(abs(etho$MeanAngVel))
rptV = rpt(logangvel ~ Treatment + Age + (1|FishName), data = etho, grname = c("FishName", "Residual", "Fixed"), datatype = "Gaussian", nboot = 1000, npermut = 100)
summary(rptV)
#R =  , p 


#Mean.Mobility
hist(etho$Mean.Mobility)
qqnorm(etho$Mean.Mobility)
qqline(etho$Mean.Mobility)

m1 <- lmer(Mean.Mobility ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(m1)
anova(m1)

m2 <- lmer(Mean.Mobility ~ Treatment + Age + (1|FishName) + (1|Clutch), data = etho)
summary(m2)
anova(m2)

m3 <- lmer(Mean.Mobility ~ Treatment + Age + (1|FishName), data = etho)
summary(m3)
anova(m3)

m4 <- lmer(Mean.Mobility ~ Treatment + Age + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho) #scale is off here
summary(m4)
anova(m4)

m5 <- lm(Mean.Mobility ~ Treatment + Age + Treatment*Age, data = etho)
summary(m5)
anova(m5)

anova(m1, m2, m3, m4, m5, test="LRT") #I think this is saying m3 is the best?
lrtest(m1, m2, m3, m4, m5)            #but I think this is suggesting differently?

rptM = rpt(Mean.Mobility ~ Treatment + Age + (1|FishName), data = etho, grname = c("FishName", "Residual", "Fixed"), datatype = "Gaussian", nboot = 1000, npermut = 100)
summary(rptM)
#R = .17 , p < .05


#CumDur.Z2
hist(log(etho$CumDur.Z2+1))
plotNormalHistogram(etho$CumDur.Z2)
plotNormalHistogram(log(etho$CumDur.Z2+1))

qqnorm(log(etho$CumDur.Z2+1)) 
qqline(log(etho$CumDur.Z2+1))

d1 <- lmer(log(etho$CumDur.Z2+1) ~ Treatment + Age + (1|FishName) + (1|Clutch) + (1|Paternity), data = etho)
summary(d1)  #is singular - paternity accounts for zero variance
anova(d1)

d2 <- lmer(log(etho$CumDur.Z2+1) ~ Treatment + Age + (1|FishName) + (1|Clutch), data = etho)
summary(d2)
anova(d2)

d3 <- lmer(log(etho$CumDur.Z2+1) ~ Treatment + Age + (1|FishName), data = etho)
summary(d3)
anova(d3)

d4 <- lmer(log(etho$CumDur.Z2+1) ~ Treatment + Age + Treatment*Age+ (1|FishName) + (1|Clutch) + (1|Paternity), data = etho) #scales are off on interaction effect
summary(d4)  #is singular cuz paternity accounts for no variance
anova(d4)

d5 <- lm(log(etho$CumDur.Z2+1) ~ Treatment + Age + Treatment*Age, data = etho)
summary(d5)
anova(d5)

anova(d1, d2, d3, d4, d5, test="LRT")
lrtest(d1, d2, d3, d4, d5)

rptD = rpt((log(etho$CumDur.Z2+1)) ~ Treatment + Age + (1|FishName), data = etho, grname = c("FishName", "Residual", "Fixed"), datatype = "Gaussian", nboot = 1000, npermut = 100)
rptD = rpt(d3, data = etho, grname = c("FishName", "Residual", "Fixed"), datatype = "Gaussian", nboot = 1000, npermut = 100)
summary(rptD)
#R =  , p =


###### Code to check assumptions
# library(DHARMa)
# library(lme4)
# simulationOutput <- simulateResiduals(fittedModel = glmer() ~ Treatment + scale(Age, center = T, scale = T) + (1|FishName) + (1|Clutch), family=binomial, data=etho), n=250) #250 simulations, but if want higher precision change n>1000; Log transform because trials does not fit a poisson distribution.
# plot(simulationOutput$scaledResiduals) #Expect a flat distribution of the overall residuals, and uniformity in y direction if plotted against any predictor. Looks randomly scattered
# testDispersion(simulationOutput) #if under- or over-dispersed, then p-value<0.05, but then check the dispersion parameter and try to determine what in the model could be the cause and address it there, also check for zero inflation.
# #p=
# testZeroInflation(simulationOutput) #compare expected vs observed zeros, not zero-inflated if p>0.05.
# #p=
# testUniformity(simulationOutput) #check for heteroscedasticity ("a systematic dependency of the dispersion / variance on another variable in the model" Hartig, https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html), which is indicated if dots aren't on the red line and p<0.05.
# #p=
# plot(simulationOutput)

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


