library(tidyverse)
library(lme4)
library(lmtest)
library(psych)
library(sjstats)

#reading in data
DissDesign <- read.csv("BeerGogglesorLiquidCourageStudyDesignEffectiveness.csv", TRUE, "," )
DissDesign <-as_tibble(DissDesign) 
class(DissDesign)
head(DissDesign)
summary(DissDesign)

#STUDY DESIGN EFFECTIVENESS CHECKING
aggregate(SIS_2  ~ SubDrink, data = DissDesign, FUN= "mean")
aggregate(SIS_2  ~ SubDrink, data = DissDesign, FUN= "sd")

SIS_2byDrink <- lmer(SIS_2 ~ SubDrink + (1|SubID:DyadID), data= DissDesign, 
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(SIS_2byDrink)

#Drink removed 
SIS_2byDrinkNULL <- lmer(SIS_2 ~ (1|SubID:DyadID), data=DissDesign, 
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(SIS_2byDrinkNULL)

#likelihood ratio test
lrtest(SIS_2byDrink, SIS_2byDrinkNULL)

aggregate(PANAS_Pos  ~ SubDrink, data = DissDesign, FUN= "mean")
aggregate(PANAS_Pos  ~ SubDrink, data = DissDesign, FUN= "sd")

PANAS_PosbyDrink <- lmer(PANAS_Pos ~ SubDrink + (1|SubID:DyadID), data= DissDesign, 
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))

summary(PANAS_PosbyDrink)

#Drink removed 
PANAS_PosbyDrinkNULL <- lmer(PANAS_Pos ~ (1|SubID:DyadID), data=DissDesign, 
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(PANAS_PosbyDrinkNULL)

#likelihood ratio test
lrtest(PANAS_PosbyDrink, PANAS_PosbyDrinkNULL)


aggregate(PANAS_Neg ~ SubDrink, data = DissDesign, FUN= "mean")
aggregate(PANAS_Neg  ~ SubDrink, data = DissDesign, FUN= "sd")

#NegRecipRtPANAS_Neg is the pre-drink negative mood variable used in significance testing  (a negative reciprocal root transformation was applied to facilitate acceptable skew and kurtosis)
NegRecipRtPANAS_NegbyDrink <- lmer(NegRecipRtPANAS_Neg ~ SubDrink + (1|SubID:DyadID), data= DissDesign, 
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))

summary(NegRecipRtPANAS_NegbyDrink)

#Drink removed 
NegRecipRtPANAS_NegbyDrinkNULL <- lmer(NegRecipRtPANAS_Neg ~ (1|SubID:DyadID), data=DissDesign, 
                             control=lmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
summary(NegRecipRtPANAS_NegbyDrinkNULL)

#likelihood ratio test
lrtest(NegRecipRtPANAS_NegbyDrink, NegRecipRtPANAS_NegbyDrinkNULL)

aggregate(PANAS_Pos  ~ Session, data = DissDesign, FUN= "mean")
aggregate(PANAS_Pos  ~ Session, data = DissDesign, FUN= "sd")

PANAS_PosbySession <- lmer(PANAS_Pos ~ Session + (1|SubID:DyadID), data= DissDesign, 
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))

summary(PANAS_PosbySession)

#Session removed 
PANAS_PosbySessionNULL <- lmer(PANAS_Pos ~ (1|SubID:DyadID), data=DissDesign, 
                             control=lmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
summary(PANAS_PosbySessionNULL)

#likelihood ratio test
lrtest(PANAS_PosbySession, PANAS_PosbySessionNULL)


aggregate(PANAS_Neg ~ Session, data = DissDesign, FUN= "mean")
aggregate(PANAS_Neg  ~ Session, data = DissDesign, FUN= "sd")

NegRecipRtPANAS_NegbySession <- lmer(NegRecipRtPANAS_Neg ~ Session + (1|SubID:DyadID), data= DissDesign, 
                                   control=lmerControl(optimizer="bobyqa", 
                                                       optCtrl=list(maxfun=2e5)))

summary(NegRecipRtPANAS_NegbySession)

#Drink removed 
NegRecipRtPANAS_NegbySessionNULL <- lmer(NegRecipRtPANAS_Neg ~ (1|SubID:DyadID), data=DissDesign, 
                                       control=lmerControl(optimizer="bobyqa", 
                                                           optCtrl=list(maxfun=2e5)))
summary(NegRecipRtPANAS_NegbySessionNULL)

#likelihood ratio test
lrtest(NegRecipRtPANAS_NegbySession, NegRecipRtPANAS_NegbySessionNULL)

#PPA RATING ANALYSES
#reading in data
Diss <- read.csv("BeerGogglesorLiquidCourageFullData.csv", TRUE, "," )
Diss <-as_tibble(Diss) 
class(Diss)
head(Diss)
summary(Diss)
#PERCEIVER DRINK EFFECT ON PPA RATING
#Testing for session order as a covariate for effect of perceiver drink on PPA rating

Session <- lmer(PPA ~ SubDrink +Session + (1|SubID:DyadID) + (1|TargetID), data= Diss, 
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(Session)

#Session removed 
SessionNULL <- lmer(PPA ~ SubDrink + (1|SubID:DyadID) + (1|TargetID), data=Diss, 
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(SessionNULL)

#likelihood ratio test
lrtest(Session, SessionNULL)

#Session not significant so will not retain in full model


#Testing pre-drink positive mood as a covariate for effect of perceiver drink on PPA rating
Aim1aPP <- lmer(PPA ~ SubDrink + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))
summary(Aim1aPP)

#PANAS_Pos removed
Aim1aPPNull <- lmer(PPA ~ SubDrink + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim1aPP))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim1aPPNull)

#likelihood ratio test
lrtest(Aim1aPP, Aim1aPPNull)
#Pre-drink positive mood not significant so will not include in full model


#Testing pre-drink negative mood as a covariate for effect of perceiver drink on PPA rating
Aim1aPN <- lmer(PPA ~ SubDrink + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(Aim1aPN)

#Panas_neg removed
Aim1aPNNull <- lmer(PPA ~ SubDrink + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim1aPN))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim1aPNNull)

#likelihood ratio test
lrtest(Aim1aPN, Aim1aPNNull)

#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing effect of perceiver drink on PPA rating (PANAS_neg as a covariate)
FullAim1aPN <- lmer(PPA ~ SubDrink + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(FullAim1aPN)

#Drink removed
FullAim1aPNNull <- lmer(PPA ~ NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(FullAim1aPN))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(FullAim1aPNNull)

#likelihood ratio test
lrtest(FullAim1aPN, FullAim1aPNNull)
# extract coefficients
FullAim1aPNcoefs <- data.frame(coef(summary(FullAim1aPN)))
# use normal distribution to approximate p-value
FullAim1aPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim1aPNcoefs$t.value)))
FullAim1aPNcoefs

effectsize::standardize_parameters(FullAim1aPN)
#Not significant

#TARGET DRINK EFFECT ON PPA RATING
#Testing session as a covariate for effect of perceiver drink on PPA rating
Aim1bsess <- lmer(PPA ~ TargetAlc + Session + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                  control=lmerControl(optimizer="bobyqa", 
                                      optCtrl=list(maxfun=2e5)))

summary(Aim1bsess)

#Session removed
Aim1bsessNull <- lmer(PPA ~ TargetAlc + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                      control=lmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=2e5)))
summary(Aim1bsessNull)
#likelihood ratio test
lrtest(Aim1bsess, Aim1bsessNull)
#Session not significant so will not retain in full model

#Testing pre-drink positive mood as a covariate for effect of target drink on PPA rating
Aim1bPP <- lmer(PPA ~ TargetAlc + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))
summary(Aim1bPP)

#PANAS_pos removed
Aim1bPPNull <- lmer(PPA ~ TargetAlc + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim1bPP))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim1bPPNull)

#likelihood ratio test
lrtest(Aim1bPP, Aim1bPPNull)
#Pre-drink positive mood not significant so will not include in full model


#Testing pre-drink negative mood as a covariate for effect of target drink on PPA rating
Aim1bPN <- lmer(PPA ~ TargetAlc + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(Aim1bPN)

#PANAS_neg removed
Aim1bPNNull <- lmer(PPA ~ TargetAlc + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim1bPN))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim1bPNNull)

#likelihood ratio test
lrtest(Aim1bPN, Aim1bPNNull)
#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing effect of target drink on PPA rating (PANAS_neg as a covariate)
FullAim1bPN <- lmer(PPA ~ TargetAlc + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))

summary(FullAim1bPN)

#Drink Removed
FullAim1bPNNull <- lmer(PPA ~ NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim1bPN))]),
                        control=lmerControl(optimizer="bobyqa", 
                                            optCtrl=list(maxfun=2e5)))
summary(FullAim1bPNNull)

#likelihood ratio test
lrtest(FullAim1bPN, FullAim1bPNNull)
# extract coefficients
FullAim1bPNcoefs <- data.frame(coef(summary(FullAim1bPN)))
# use normal distribution to approximate p-value
FullAim1bPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim1bPNcoefs$t.value)))
FullAim1bPNcoefs

effectsize::standardize_parameters(FullAim1bPN)
#Not significant

#INTERACTION BETWEEN PERCEIVER DRINK AND TARGET DRINK ON PPA RATING
#Testing session as a covariate for interaction effect of target and perceiver drink on PPA rating
Aim1csess <- lmer(PPA ~ SubDrink + TargetAlc +SubDrink*TargetAlc + Session + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                  control=lmerControl(optimizer="bobyqa", 
                                      optCtrl=list(maxfun=2e5)))

summary(Aim1csess)

#Session removed
Aim1csessNull <- lmer(PPA ~ SubDrink + TargetAlc +SubDrink*TargetAlc+ (1|SubID:DyadID) + (1|TargetID), data = Diss,
                      control=lmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=2e5)))
summary(Aim1csessNull)
#likelihood ratio test
lrtest(Aim1csess, Aim1csessNull)
#Session not significant so will not retain in full model


#Testing pre-drink positive mood as a covariate for interaction effect of target and perceiver drink on PPA rating
Aim1cPP <- lmer(PPA ~ SubDrink + TargetAlc +SubDrink*TargetAlc+ PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))
summary(Aim1cPP)

#PANAS_pos removed
Aim1cPPNull <- lmer(PPA ~ SubDrink + TargetAlc +SubDrink*TargetAlc+(1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim1cPP))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim1cPPNull)

#likelihood ratio test
lrtest(Aim1cPP, Aim1cPPNull)
#Pre-drink positive mood not significant so will not include in full model


#Testing pre-drink negative mood as a covariate for interaction effect of target and perceiver drink on PPA rating
Aim1cPN <- lmer(PPA ~ SubDrink + TargetAlc +SubDrink*TargetAlc + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(Aim1cPN)

#PANAS_neg removed
Aim1cPNNull <- lmer(PPA ~ SubDrink + TargetAlc +SubDrink*TargetAlc+ (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim1cPN))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim1cPNNull)

#likelihood ratio test
lrtest(Aim1cPN, Aim1cPNNull)
#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing interaction effect of target drink and perceiver drink on PPA rating (PANAS_neg as a covariate)
FullAim1cPN <- lmer(PPA ~ SubDrink + TargetAlc+SubDrink*TargetAlc+ NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))
summary(FullAim1cPN)

#Interaction effect removed
FullAim1cPNNull <- lmer(PPA ~ SubDrink + TargetAlc+ NegRecipRtPANAS_Neg +(1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(FullAim1cPN))]),
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(FullAim1cPNNull)

#likelihood ratio test
lrtest(FullAim1cPN, FullAim1cPNNull)
# extract coefficients
FullAim1cPNcoefs <- data.frame(coef(summary(FullAim1cPN)))
# use normal distribution to approximate p-value
FullAim1cPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim1cPNcoefs$t.value)))
FullAim1cPNcoefs

effectsize::standardize_parameters(FullAim1cPN)

#Not significant


#INTERACTION BETWEEN PERCEIVER DRINK AND ORIENTATION MATCH ON PPA RATING
#Testing session as a covariate for interaction effect of perceiver drink and orientation match on PPA rating
Aim2asess <- lmer(PPA ~ SubDrink + OrMatch + SubDrink*OrMatch + Session + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                  control=lmerControl(optimizer="bobyqa", 
                                      optCtrl=list(maxfun=2e5)))

summary(Aim2asess)

#Session Removed
Aim2asessNull <- lmer(PPA ~ SubDrink + OrMatch + SubDrink*OrMatch + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                      control=lmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=2e5)))
summary(Aim2asessNull)
#likelihood ratio test
lrtest(Aim2asess, Aim2asessNull)
#Session not significant so will not retain in full model

#Testing pre-drink positive mood as a covariate for interaction effect of perceiver drink and orientation match on PPA rating
Aim2aPP <- lmer(PPA ~ SubDrink + OrMatch + SubDrink*OrMatch + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))
summary(Aim2aPP)

#PANAS_pos removed
Aim2aPPNull <- lmer(PPA ~SubDrink + OrMatch + SubDrink*OrMatch + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim2aPP))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim2aPPNull)

#likelihood ratio test
lrtest(Aim2aPP, Aim2aPPNull)
#PANAS_pos not significant so will not retain in full model

#Testing pre-drink negative mood as a covariate for interaction effect of perceiver drink and orientation match on PPA rating
Aim2aPN <- lmer(PPA ~ SubDrink + OrMatch + SubDrink*OrMatch + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(Aim2aPN)

#PANAS_neg removed
Aim2aPNNull <- lmer(PPA ~ SubDrink + OrMatch + SubDrink*OrMatch + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim2aPN))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim2aPNNull)

#likelihood ratio test
lrtest(Aim2aPN, Aim2aPNNull)
#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing interaction effect of perceiver drink and orientation match on PPA rating (PANAS_neg as a covariate)
FullAim2aPN <- lmer(PPA ~ SubDrink + OrMatch + SubDrink*OrMatch + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(FullAim2aPN)

#Interaction effect removed
FullAim2aPNNull <- lmer(PPA ~ SubDrink + OrMatch + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(FullAim2aPN))]),
                             control=lmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
summary(FullAim2aPNNull)

#likelihood ratio test
lrtest(FullAim2aPN, FullAim2aPNNull)
# extract coefficients
FullAim2aPNcoefs <- data.frame(coef(summary(FullAim2aPN)))
# use normal distribution to approximate p-value
FullAim2aPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim2aPNcoefs$t.value)))
FullAim2aPNcoefs
effectsize::standardize_parameters(FullAim2aPN)
#Interaction not significant

#INTERACTION BETWEEN TARGET DRINK AND ORIENTATION MATCH ON PPA RATING
#Testing session as a covariate for interaction effect of target drink and orientation match on PPA rating
Aim2bsess <- lmer(PPA ~ TargetAlc + OrMatch + TargetAlc*OrMatch + Session + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                  control=lmerControl(optimizer="bobyqa", 
                                      optCtrl=list(maxfun=2e5)))

summary(Aim2bsess)

#Session removed
Aim2bsessNull <- lmer(PPA ~ TargetAlc + OrMatch + TargetAlc*OrMatch + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                      control=lmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=2e5)))
summary(Aim2bsessNull)
#likelihood ratio test
lrtest(Aim2bsess, Aim2bsessNull)
#Session not significant so will not retain in full model

#Testing pre-drink positive mood as a covariate for interaction effect of target drink and orientation match on PPA rating
Aim2bPP <- lmer(PPA ~ TargetAlc + OrMatch + TargetAlc*OrMatch + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))
summary(Aim2bPP)

#PANAS_pos removed
Aim2bPPNull <- lmer(PPA ~TargetAlc + OrMatch + TargetAlc*OrMatch + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim2bPP))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim2bPPNull)

#likelihood ratio test
lrtest(Aim2bPP, Aim2bPPNull)
#PANAS_pos not significant so will not retain in full model

#Testing pre-drink negative mood as a covariate for interaction effect of target drink and orientation match on PPA rating
Aim2bPN <- lmer(PPA ~ TargetAlc + OrMatch + TargetAlc*OrMatch + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(Aim2bPN)

#PANAS_neg removed
Aim2bPNNull <- lmer(PPA ~ TargetAlc + OrMatch + TargetAlc*OrMatch + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim2bPN))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim2bPNNull)

#likelihood ratio test
lrtest(Aim2bPN, Aim2bPNNull)

#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing interaction effect of target drink and orientation match on PPA rating (PANAS_neg as a covariate)
FullAim2bPN <- lmer(PPA ~ TargetAlc + OrMatch + TargetAlc*OrMatch + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(FullAim2bPN)

#Interaction effect removed
FullAim2bPNNull <- lmer(PPA ~ TargetAlc + OrMatch + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(FullAim2bPN))]),
                             control=lmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
summary(FullAim2bPNNull)

#likelihood ratio test
lrtest(FullAim2bPN, FullAim2bPNNull)
# extract coefficients
FullAim2bPNcoefs <- data.frame(coef(summary(FullAim2bPN)))
# use normal distribution to approximate p-value
FullAim2bPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim2bPNcoefs$t.value)))
FullAim2bPNcoefs
effectsize::standardize_parameters(FullAim2bPN)
#Interaction not significant



#INTERACTION BETWEEN PERCEIVER DRINK AND SEXUAL EXPECTANCIES ON PPA RATING
#Note orientation match only data is needed
#reading in data
OrMatchDiss <- read.csv("BeerGogglesorLiquidCourageOrMatchOnly.csv", TRUE, "," )
OrMatchOrMatchDiss <-as_tibble(OrMatchDiss) 
class(OrMatchDiss)
head(OrMatchDiss)
summary(OrMatchDiss)

#Testing session as a covariate for interaction effect of perceiver drink and sexual expectancies on PPA rating
Aim3sess <- lmer(PPA ~ SubDrink + SEDQ_Desire + SubDrink*SEDQ_Desire + Session + (1|SubID:DyadID) + (1|TargetID), data = OrMatchDiss,
                 control=lmerControl(optimizer="bobyqa", 
                                     optCtrl=list(maxfun=2e5)))

summary(Aim3sess)

#Session removed
Aim3sessNull <- lmer(PPA ~ SubDrink + SEDQ_Desire + SubDrink*SEDQ_Desire + (1|SubID:DyadID) + (1|TargetID), data = OrMatchDiss,
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))
summary(Aim3sessNull)
#likelihood ratio test
lrtest(Aim3sess, Aim3sessNull)
#Session not significant so will not retain in full model

#Testing pre-drink positive mood as a covariate for interaction effect of perceiver drink and sexual expectancies on PPA rating
Aim3PP <- lmer(PPA ~ SubDrink + SEDQ_Desire + SubDrink*SEDQ_Desire + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = OrMatchDiss,
               control=lmerControl(optimizer="bobyqa", 
                                   optCtrl=list(maxfun=2e5)))
summary(Aim3PP)

#PANAS_pos removed
Aim3PPNull <- lmer(PPA ~SubDrink + SEDQ_Desire + SubDrink*SEDQ_Desire + (1|SubID:DyadID) + (1|TargetID), data=na.omit(OrMatchDiss[ , all.vars(formula(Aim3PP))]),
                   control=lmerControl(optimizer="bobyqa", 
                                       optCtrl=list(maxfun=2e5)))
summary(Aim3PPNull)

#likelihood ratio test
lrtest(Aim3PP, Aim3PPNull)
#Significant effect of pre-drink positive mood so will include in full model

#Testing pre-drink negative mood as a covariate for interaction effect of perceiver drink and target motion on PPA rating
Aim3PN <- lmer(PPA ~ SubDrink + SEDQ_Desire + SubDrink*SEDQ_Desire + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = OrMatchDiss,
               control=lmerControl(optimizer="bobyqa", 
                                   optCtrl=list(maxfun=2e5)))

summary(Aim3PN)

#PANAS_neg removed
Aim3PNNull <- lmer(PPA ~ SubDrink + SEDQ_Desire + SubDrink*SEDQ_Desire + (1|SubID:DyadID) + (1|TargetID), data=na.omit(OrMatchDiss[ , all.vars(formula(Aim3PN))]),
                   control=lmerControl(optimizer="bobyqa", 
                                       optCtrl=list(maxfun=2e5)))
summary(Aim3PNNull)

#likelihood ratio test
lrtest(Aim3PN, Aim3PNNull)
#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing interaction effect of perceiver drink and sexual expectancies on PPA rating (PANAS_pos and PANAS_neg as covariates)
FullAim3PNPP <- lmer(PPA ~ SubDrink + SEDQ_Desire + SubDrink*SEDQ_Desire + NegRecipRtPANAS_Neg + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = OrMatchDiss,
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))

summary(FullAim3PNPP)

#Interaction effect removed
FullAim3PNPPNull <- lmer(PPA ~ SubDrink + SEDQ_Desire +  NegRecipRtPANAS_Neg + PANAS_Pos + (1|SubID:DyadID) + PANAS_Pos+ (1|TargetID), data=na.omit(OrMatchDiss[ , all.vars(formula(FullAim3PNPP))]),
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(FullAim3PNPPNull)

#likelihood ratio test
lrtest(FullAim3PNPP, FullAim3PNPPNull)
# extract coefficients
FullAim3PNPPcoefs <- data.frame(coef(summary(FullAim3PNPP)))
# use normal distribution to approximate p-value
FullAim3PNPPcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim3PNPPcoefs$t.value)))
FullAim3PNPPcoefs
effectsize::standardize_parameters(FullAim3PNPP)
#Interaction not significant



#INTERACTION BETWEEN PERCEIVER DRINK AND TARGET EXPRESSION ON PPA RATING
#Testing session as a covariate for interaction effect of perceiver drink and target expression on PPA rating
Aim4asess <- lmer(PPA ~ SubDrink + TargetSmile + SubDrink*TargetSmile + Session + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                  control=lmerControl(optimizer="bobyqa", 
                                      optCtrl=list(maxfun=2e5)))

summary(Aim4asess)

#Session removed
Aim4asessNull <- lmer(PPA ~ SubDrink + TargetSmile + SubDrink*TargetSmile + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                      control=lmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=2e5)))
summary(Aim4asessNull)
#likelihood ratio test
lrtest(Aim4asess, Aim4asessNull)
#Session not significant so will not retain in full model

#Testing pre-drink positive mood as a covariate for interaction effect of perceiver drink and target expression on PPA rating
Aim4aPP <- lmer(PPA ~ SubDrink + TargetSmile + SubDrink*TargetSmile + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))
summary(Aim4aPP)

#PANAS_pos removed
Aim4aPPNull <- lmer(PPA ~SubDrink + TargetSmile + SubDrink*TargetSmile + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim4aPP))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim4aPPNull)

#likelihood ratio test
lrtest(Aim4aPP, Aim4aPPNull)
#PANAS_pos not significant so will not retain in full model

#Testing pre-drink negative mood as a covariate for interaction effect of perceiver drink and target expression on PPA rating
Aim4aPN <- lmer(PPA ~ SubDrink + TargetSmile + SubDrink*TargetSmile + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(Aim4aPN)

#PANAS_neg removed
Aim4aPNNull <- lmer(PPA ~ SubDrink + TargetSmile + SubDrink*TargetSmile + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim4aPN))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim4aPNNull)

#likelihood ratio test
lrtest(Aim4aPN, Aim4aPNNull)
#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing interaction effect of perceiver drink and target expression on PPA rating (PANAS_neg as a covariate)
FullAim4aPN <- lmer(PPA ~ SubDrink + TargetSmile + SubDrink*TargetSmile + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))

summary(FullAim4aPN)

#Interaction effect removed
FullAim4aPNNull <- lmer(PPA ~ SubDrink + TargetSmile +  NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(FullAim4aPN))]),
                        control=lmerControl(optimizer="bobyqa", 
                                            optCtrl=list(maxfun=2e5)))
summary(FullAim4aPNNull)

#likelihood ratio test
lrtest(FullAim4aPN, FullAim4aPNNull)
# extract coefficients
FullAim4aPNcoefs <- data.frame(coef(summary(FullAim4aPN)))
# use normal distribution to approximate p-value
FullAim4aPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim4aPNcoefs$t.value)))
FullAim4aPNcoefs
effectsize::standardize_parameters(FullAim4aPN)
#Interaction not significant

#INTERACTION BETWEEN PERCEIVER DRINK AND TARGET MOTION ON PPA RATING
#Testing session as a covariate for interaction effect of perceiver drink and target motion on PPA rating
Aim4bsess <- lmer(PPA ~ SubDrink + TargetDyn + SubDrink*TargetDyn + Session + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                  control=lmerControl(optimizer="bobyqa", 
                                      optCtrl=list(maxfun=2e5)))

summary(Aim4bsess)

#Session removed
Aim4bsessNull <- lmer(PPA ~ SubDrink + TargetDyn + SubDrink*TargetDyn + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                      control=lmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=2e5)))
summary(Aim4bsessNull)
#likelihood ratio test
lrtest(Aim4bsess, Aim4bsessNull)
#Session not significant so will not retain in full model

#Testing pre-drink positive mood as a covariate for interaction effect of perceiver drink and target motion on PPA rating
Aim4bPP <- lmer(PPA ~ SubDrink + TargetDyn + SubDrink*TargetDyn + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))
summary(Aim4bPP)

#PANAS_pos removed
Aim4bPPNull <- lmer(PPA ~SubDrink + TargetDyn + SubDrink*TargetDyn + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim4bPP))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim4bPPNull)

#likelihood ratio test
lrtest(Aim4bPP, Aim4bPPNull)
#PANAS_pos not significant so will not retain in full model

#Testing pre-drink negative mood as a covariate for interaction effect of perceiver drink and target motion on PPA rating
Aim4bPN <- lmer(PPA ~ SubDrink + TargetDyn + SubDrink*TargetDyn + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                control=lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e5)))

summary(Aim4bPN)

#PANAS_neg removed
Aim4bPNNull <- lmer(PPA ~ SubDrink + TargetDyn + SubDrink*TargetDyn + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(Aim4bPN))]),
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))
summary(Aim4bPNNull)

#likelihood ratio test
lrtest(Aim4bPN, Aim4bPNNull)
#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing interaction effect of perceiver drink and target motion on PPA rating (PANAS_neg as a covariate)
FullAim4bPN <- lmer(PPA ~ SubDrink + TargetDyn + SubDrink*TargetDyn + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                    control=lmerControl(optimizer="bobyqa", 
                                        optCtrl=list(maxfun=2e5)))

summary(FullAim4bPN)

#Interaction effect removed
FullAim4bPNNull <- lmer(PPA ~ SubDrink + TargetDyn +  NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(FullAim4bPN))]),
                        control=lmerControl(optimizer="bobyqa", 
                                            optCtrl=list(maxfun=2e5)))
summary(FullAim4bPNNull)

#likelihood ratio test
lrtest(FullAim4bPN, FullAim4bPNNull)
# extract coefficients
FullAim4bPNcoefs <- data.frame(coef(summary(FullAim4bPN)))
# use normal distribution to approximate p-value
FullAim4bPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim4bPNcoefs$t.value)))
FullAim4bPNcoefs
effectsize::standardize_parameters(FullAim4bPN)
#Interaction not significant


#EFFECT OF POST-DRINK POSITIVE MOOD ON PPA RATING
#Note: Return to full dataset
#Testing session as a covariate for the effect of post-drink positive mood on PPA rating
Aim5bPossess <- lmer(PPA ~ EightMMPos + Session + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))

summary(Aim5bPossess)

#Session removed
Aim5bPossessNull <- lmer(PPA ~ EightMMPos + (1|SubID:DyadID) + (1|TargetID)  , data = Diss,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(Aim5bPossessNull)
#likelihood ratio test
lrtest(Aim5bPossess, Aim5bPossessNull)
#Session not significant so will not retain in full model

#Testing pre-drink positive mood as a covariate for the effect of post-drink positive mood on PPA rating
Aim5bPosPP <- lmer(PPA ~ EightMMPos + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID)  , data = Diss,
                   control=lmerControl(optimizer="bobyqa", 
                                       optCtrl=list(maxfun=2e5)))
summary(Aim5bPosPP)

#PANAS_pos removed
Aim5bPosPPNull <- lmer(PPA ~ EightMMPos + (1|SubID:DyadID) + (1|TargetID)  , data=na.omit(Diss[ , all.vars(formula(Aim5bPosPP))]),
                       control=lmerControl(optimizer="bobyqa", 
                                           optCtrl=list(maxfun=2e5)))
summary(Aim5bPosPPNull)

#likelihood ratio test
lrtest(Aim5bPosPP, Aim5bPosPPNull)
#PANAS_pos not significant so will not retain in full model

#Testing pre-drink negative mood as a covariate for interaction effect of perceiver drink and target motion on PPA rating
Aim5bPosPN <- lmer(PPA ~ EightMMPos + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                   control=lmerControl(optimizer="bobyqa", 
                                       optCtrl=list(maxfun=2e5)))

summary(Aim5bPosPN)

#PANAS_neg removed
Aim5bPosPNNull <- lmer(PPA ~ EightMMPos + (1|SubID:DyadID) + (1|TargetID)  , data=na.omit(Diss[ , all.vars(formula(Aim5bPosPN))]),
                       control=lmerControl(optimizer="bobyqa", 
                                           optCtrl=list(maxfun=2e5)))
summary(Aim5bPosPNNull)

#likelihood ratio test
lrtest(Aim5bPosPN, Aim5bPosPNNull)
#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing the effect of post-drink positive mood on PPA rating (PANAS_neg as a covariate)
FullAim5bPosPN <- lmer(PPA ~ EightMMPos + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID)  , data = Diss,
                            control=lmerControl(optimizer="bobyqa", 
                                                optCtrl=list(maxfun=2e5)))
summary(FullAim5bPosPN)

#EightMMPos removed
FullAim5bPosPNNull <- lmer(PPA ~  NegRecipRtPANAS_Neg  + (1|SubID:DyadID) + (1|TargetID), data=na.omit(Diss[ , all.vars(formula(FullAim5bPosPN))]),
                                control=lmerControl(optimizer="bobyqa", 
                                                    optCtrl=list(maxfun=2e5)))
summary(FullAim5bPosPNNull)

#likelihood ratio test
lrtest(FullAim5bPosPN,  FullAim5bPosPNNull)
# extract coefficients
FullAim5bPosPNcoefs <- data.frame(coef(summary(FullAim5bPosPN)))
# use normal distribution to approximate p-value
FullAim5bPosPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim5bPosPNcoefs$t.value)))
FullAim5bPosPNcoefs
effectsize::standardize_parameters(FullAim5bPosPN)
#Post-drink positive mood significantly enhanced model fit but  was not significant as an individual predictor 

##EFFECT OF POST-DRINK NEGATIVE MOOD ON PPA RATING
#Testing session as a covariate for the effect of post-drink positive mood on PPA rating
Aim5bNegsess <- lmer(PPA ~ EightMMNeg + Session + (1|SubID:DyadID) + (1|TargetID), data = Diss,
                     control=lmerControl(optimizer="bobyqa", 
                                         optCtrl=list(maxfun=2e5)))

summary(Aim5bNegsess)

#Session removed
Aim5bNegsessNull <- lmer(PPA ~ EightMMNeg + (1|SubID:DyadID) + (1|TargetID)  , data = Diss,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))
summary(Aim5bNegsessNull)
#likelihood ratio test
lrtest(Aim5bNegsess, Aim5bNegsessNull)
#Session not significant so will not retain in full model

#Testing pre-drink positive mood as a covariate for the effect of post-drink negative mood on PPA rating
Aim5bNegPP <- lmer(PPA ~ EightMMNeg + PANAS_Pos + (1|SubID:DyadID) + (1|TargetID)  , data = Diss,
                   control=lmerControl(optimizer="bobyqa", 
                                       optCtrl=list(maxfun=2e5)))
summary(Aim5bNegPP)

#PANAS_pos removed
Aim5bNegPPNull <- lmer(PPA ~ EightMMNeg + (1|SubID:DyadID) + (1|TargetID)  , data=na.omit(Diss[ , all.vars(formula(Aim5bNegPP))]),
                       control=lmerControl(optimizer="bobyqa", 
                                           optCtrl=list(maxfun=2e5)))
summary(Aim5bNegPPNull)

#likelihood ratio test
lrtest(Aim5bNegPP, Aim5bNegPPNull)
#PANAS_pos not significant so will not retain in full model

#Testing pre-drink negative mood as a covariate for the effect of post-drink negative mood on PPA rating
Aim5bNegPN <- lmer(PPA ~ EightMMNeg + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID)  , data = Diss,
                   control=lmerControl(optimizer="bobyqa", 
                                       optCtrl=list(maxfun=2e5)))

summary(Aim5bNegPN)

#PANAS_neg removed
Aim5bNegPNNull <- lmer(PPA ~ EightMMNeg + (1|SubID:DyadID) + (1|TargetID)  , data=na.omit(Diss[ , all.vars(formula(Aim5bNegPN))]),
                       control=lmerControl(optimizer="bobyqa", 
                                           optCtrl=list(maxfun=2e5)))
summary(Aim5bNegPNNull)

#likelihood ratio test
lrtest(Aim5bNegPN, Aim5bNegPNNull)
#Significant effect of pre-drink negative mood so will include in full model

#FULL MODEL: Testing the effect of post-drink negative mood on PPA rating (PANAS_neg as a covariate)
FullAim5bNegPN <- lmer(PPA ~ EightMMNeg + NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID)  , data = Diss,
                         control=lmerControl(optimizer="bobyqa", 
                                             optCtrl=list(maxfun=2e5)))


summary(FullAim5bNegPN)
#EightMMNeg removed
FullAim5bNegPNNull <- lmer(PPA ~   NegRecipRtPANAS_Neg + (1|SubID:DyadID) + (1|TargetID) , data=na.omit(Diss[ , all.vars(formula(FullAim5bNegPN))]),
                             control=lmerControl(optimizer="bobyqa", 
                                                 optCtrl=list(maxfun=2e5)))
summary(FullAim5bNegPNNull)

#likelihood ratio test
lrtest(FullAim5bNegPN, FullAim5bNegPNNull)
# extract coefficients
FullAim5bNegPNcoefs <- data.frame(coef(summary(FullAim5bNegPN)))
# use normal distribution to approximate p-value
FullAim5bNegPNcoefs$p.z <- 2 * (1 - pnorm(abs(FullAim5bNegPNcoefs$t.value)))
FullAim5bNegPNcoefs
effectsize::standardize_parameters(FullAim5bNegPN)
#post-drink negative mood was positively associated with PPA but post-drink negative mood did not significantly enhance model fit

