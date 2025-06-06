library(tidyverse)
library(lme4)
library(lmtest)
library(psych)
library(sjPlot)
library(sjstats)

#READING IN DATA
Mooddf <- read.csv("The_Effect_of_Alcohol_on_Mood_when_Drinking_with_a_Platonic_Friend_Data.csv", TRUE, "," )
Mooddf <-as_tibble(Mooddf) 
class(Mooddf)
head(Mooddf)
summary(Mooddf)

#ASSESSING DESCRIPTIVES
Age<-Mooddf$Age
describe(Age)

DaysAlc <-Mooddf$DaysAlc 
describe(DaysAlc)

AveDrinkOcc <-Mooddf$AveDrinkOcc 
describe(AveDrinkOcc)

COF1_YearsofFriendship <-Mooddf$ COF1_YearsofFriendship
describe(COF1_YearsofFriendship)

COF3_DrinkFreqMonth <-Mooddf$ COF3_DrinkFreqMonth
describe(COF3_DrinkFreqMonth)

COF2_Closeness <-Mooddf$COF2_Closeness
describe( COF2_Closeness)

aggregate(BAC2 ~SubDrink, data = Mooddf, FUN= "mean")
aggregate(BAC2 ~SubDrink, data = Mooddf, FUN= "sd")


aggregate(BAC3 ~SubDrink, data = Mooddf, FUN= "mean")
aggregate(BAC3 ~SubDrink, data = Mooddf, FUN= "sd")

aggregate(BAC4 ~SubDrink, data = Mooddf, FUN= "mean")
aggregate(BAC4 ~SubDrink, data = Mooddf, FUN= "sd")

aggregate(NoOutEndoz ~SubDrink, data = Mooddf, FUN= "median")

PANAS_Pos <- Mooddf$PANAS_Pos
describe(PANAS_Pos)

PANAS_Neg <- Mooddf$PANAS_Neg
describe(PANAS_Neg)

#PANAS_Neg skew and kurtosis are bad, so test transformation

NegRecipRtPANAS_Neg <- Mooddf$NegRecipRtPANAS_Neg
describe(NegRecipRtPANAS_Neg)

#Transformed PANAS_Neg has good skew and kurtosis

EightMMPos <- Mooddf$EightMMPos
describe(EightMMPos)

EightMMNeg <- Mooddf$EightMMNeg
describe(EightMMNeg)


#ASSESSING FOR DIFFERENCES IN BASELINE (PRE-DRINK) MOOD BY CONDITION AND SESSION

#PANAS_Pos with session
PANAS_Possess <- lmer(PANAS_Pos ~  Session + (1|SubID:DyadID), data = Mooddf,REML=F,
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))

summary(PANAS_Possess)

#PANAS_Pos with session removed
PANAS_PossessNull <- lmer(PANAS_Pos~  (1|SubID:DyadID)  , data = Mooddf,REML=F,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(PANAS_PossessNull)

#likelihood ratio test
lrtest(PANAS_Possess, PANAS_PossessNull)

#PANAS_Pos significantly differs by session
aggregate(PANAS_Pos ~Session, data = Mooddf, FUN= "mean")
aggregate(PANAS_Pos ~Session, data = Mooddf, FUN= "sd")

#PANAS_Pos with drink
PANAS_Posdrink <- lmer(PANAS_Pos ~  SubDrink + (1|SubID:DyadID), data = Mooddf,REML=F,
                      control=lmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=2e5)))

summary(PANAS_Posdrink)

#PanaPANAS_Posspos with drink removed
PANAS_PosdrinkNull <- lmer(PANAS_Pos~  (1|SubID:DyadID)  , data = Mooddf,REML=F,
                          control=lmerControl(optimizer="bobyqa", 
                                              optCtrl=list(maxfun=2e5)))
summary(PANAS_PosdrinkNull)

#likelihood ratio test
lrtest(PANAS_Posdrink, PANAS_PosdrinkNull)
#PANAS_Pos does NOT significantly differ by drink condition

#PANAS_Neg with session
NegRecipRtPANAS_Negsess <- lmer(NegRecipRtPANAS_Neg ~  Session + (1|SubID:DyadID), data = Mooddf,REML=F,
                      control=lmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=2e5)))

summary(NegRecipRtPANAS_Negsess)

#PANAS_Neg with session removed
NegRecipRtPANAS_NegsessNull <- lmer(NegRecipRtPANAS_Neg~  (1|SubID:DyadID), data = Mooddf,REML=F,
                          control=lmerControl(optimizer="bobyqa", 
                                              optCtrl=list(maxfun=2e5)))
summary(NegRecipRtPANAS_NegsessNull)
#likelihood ratio test
lrtest(NegRecipRtPANAS_Negsess, NegRecipRtPANAS_NegsessNull)

#PANAS_Neg does NOT significantly differ by session

#PANAS_Neg with drink
NegRecipRtPANAS_Negdrink <- lmer(NegRecipRtPANAS_Neg ~  SubDrink + (1|SubID:DyadID), data = Mooddf,REML=F,
                       control=lmerControl(optimizer="bobyqa", 
                                           optCtrl=list(maxfun=2e5)))

summary(NegRecipRtPANAS_Negdrink)

#PANAS_Neg with drink removed
NegRecipRtPANAS_NegdrinkNull <- lmer(NegRecipRtPANAS_Neg~  (1|SubID:DyadID), data = Mooddf,REML=F,
                           control=lmerControl(optimizer="bobyqa", 
                                               optCtrl=list(maxfun=2e5)))
summary(NegRecipRtPANAS_NegdrinkNull)
#likelihood ratio test
lrtest(NegRecipRtPANAS_Negdrink, NegRecipRtPANAS_NegdrinkNull)

#PANAS_Neg does NOT significantly differ by drink condition

#TESTING WHETHER Session COVARIATE SIGNIFICANTLY ENHANCES MODEL FIT FOR PRIMARY AIMS

#TESTING Session WITH POST-DRINK POSITIVE MOOD

#EightMMPos with session
EightMMPosSess <- lmer(EightMMPos ~ SubDrink + Session + (1|SubID:DyadID), data = Mooddf,REML=F,
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))

summary(EightMMPosSess)

#EightMMPos with session removed
EightMMPosSessNull <- lmer(EightMMPos ~ SubDrink + (1|SubID:DyadID), data = Mooddf,REML=F,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(EightMMPosSessNull)
#likelihood ratio test
lrtest(EightMMPosSess, EightMMPosSessNull)
#Session significantly enhances post-drink positive mood model fit


#FULL PRIMARY AIM MODEL TESTING EightMMPos
EightMMPosFull <- lmer(EightMMPos ~ SubDrink + Session + (1|SubID:DyadID), data = Mooddf,REML=F,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))

summary(EightMMPosFull)

# PRIMARY AIM MODEL TESTING EightMMPos WITH DRINK REMOVED
EightMMPosFullNull <- lmer(EightMMPos ~  Session + (1|SubID:DyadID), data=na.omit(Mooddf[ , all.vars(formula(EightMMPosFull))]),REML=F,
                             control=lmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
summary(EightMMPosFullNull)

#likelihood ratio test
lrtest(EightMMPosFull, EightMMPosFullNull)
#DRINK SIGNIFICANTLY ENHANCES FIT OF EightMMPos MODEL

# extract coefficients
EightMMPosFullcoefs <- data.frame(coef(summary(EightMMPosFull)))
# use normal distribution to approximate p-value
EightMMPosFullcoefs$p.z <- 2 * (1 - pnorm(abs(EightMMPosFullcoefs$t.value)))
EightMMPosFullcoefs

effectsize::standardize_parameters(EightMMPosFull) 

aggregate(EightMMPos ~SubDrink, data = Mooddf, FUN= "mean")
aggregate(EightMMPos  ~SubDrink, data = Mooddf, FUN= "sd")


#TESTING Session AS A COVARIATE WITH POST DRINK NEGATIVE MOOD 

#EightMMNeg with session
EightMMNegSess <- lmer(EightMMNeg ~ SubDrink + Session + (1|SubID:DyadID), data = Mooddf,REML=F,
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))

summary(EightMMNegSess)

#EightMMNeg with session removed
EightMMNegSessNull <- lmer(EightMMNeg ~ SubDrink + (1|SubID:DyadID), data = Mooddf,REML=F,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(EightMMNegSessNull)
#likelihood ratio test
lrtest(EightMMNegSess, EightMMNegSessNull)
#Session does NOT significantly enhance post-drink negative mood model fit

#FULL PRIMARY AIM MODEL TESTING EightMMNeg
EightMMNegFull <- lmer(EightMMNeg ~ SubDrink + (1|SubID:DyadID), data = Mooddf,REML=F,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))

summary(EightMMNegFull)

#PRIMARY AIM MODEL TESTING EightMMNeg WITH DRINK REMOVED
EightMMNegFullNull <- lmer(EightMMNeg ~   (1|SubID:DyadID), data=na.omit(Mooddf[ , all.vars(formula(EightMMNegFull))]),REML=F,
                             control=lmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
summary(EightMMNegFullNull)

#likelihood ratio test
lrtest(EightMMNegFull, EightMMNegFullNull)
#DRINK DOES NOT SIGNIFICANTLY ENHANCE FIT OF EightMMNeg MODEL

# extract coefficients
EightMMNegFullcoefs <- data.frame(coef(summary(EightMMNegFull)))
# use normal distribution to approximate p-value
EightMMNegFullcoefs$p.z <- 2 * (1 - pnorm(abs(EightMMNegFullcoefs$t.value)))
EightMMNegFullcoefs


effectsize::standardize_parameters(EightMMNegFull)

aggregate(EightMMNeg ~SubDrink, data = Mooddf, FUN= "mean")
aggregate(EightMMNeg  ~SubDrink, data = Mooddf, FUN= "sd")