library(tidyverse)
library(lmtest)
library(psych)
library(dplyr)
library(emmeans)
library(ordinal)

#reading in data
SelectQ <- read.csv("BeerGogglesorLiquidCourageSelectToInteractData.csv", header=T, stringsAsFactors=T)
SelectQ  <-as_tibble(SelectQ ) 
class(SelectQ )
head(SelectQ )
summary(SelectQ )

PANAS_Pos = SelectQ$PANAS_Pos

NegRecipRtPANAS_Neg = SelectQ$NegRecipRtPANAS_Neg

SelectTopFourCount = SelectQ$SelectTopFourCount

aggregate(SelectTopFourCount  ~ SubDrink, data = SelectQ, FUN= "mean")
aggregate(SelectTopFourCount  ~ SubDrink, data = SelectQ, FUN= "sd")

DyadID = SelectQ$DyadID

SubID = SelectQ$SubID

SubDrink = SelectQ$SubDrink

Session = SelectQ$Session

class(SelectQ$SelectTopFourCount)

SelectQ$SelectTopFourCount <- factor(SelectQ$SelectTopFourCount, ordered=TRUE)


#testing session order as a covariate
SelectTopFourCount_Model_Sess <- clmm(SelectTopFourCount ~  SubDrink + Session+ (1|SubID:DyadID), data = SelectQ)	
summary(SelectTopFourCount_Model_Sess)


SelectTopFourCount_Model <- clmm(SelectTopFourCount ~ SubDrink + (1|SubID:DyadID), data = SelectQ)	
summary(SelectTopFourCount_Model)



anova(SelectTopFourCount_Model_Sess,SelectTopFourCount_Model)
#session order not sig

#testing pre-drink negative mood as a covariate
SelectTopFourCount_Model_Neg <- clmm(SelectTopFourCount ~  SubDrink + NegRecipRtPANAS_Neg+ (1|SubID:DyadID), data = SelectQ)	
summary(SelectTopFourCount_Model_Neg)


SelectTopFourCount_Model <- clmm(SelectTopFourCount ~ SubDrink + (1|SubID:DyadID), data=na.omit(SelectQ[ , all.vars(formula(SelectTopFourCount_Model_Neg))]))	
summary(SelectTopFourCount_Model)



anova(SelectTopFourCount_Model_Neg,SelectTopFourCount_Model)
#pre-drink negative mood not sig

#testing pre-drink positive mood as a covariate
SelectTopFourCount_Model_Pos <- clmm(SelectTopFourCount ~  SubDrink +PANAS_Pos+ (1|SubID:DyadID), data = SelectQ)
summary(SelectTopFourCount_Model_Pos)


SelectTopFourCount_Model <- clmm(SelectTopFourCount ~ SubDrink + (1|SubID:DyadID), data=na.omit(SelectQ[ , all.vars(formula(SelectTopFourCount_Model_Pos))]))
summary(SelectTopFourCount_Model)



anova(SelectTopFourCount_Model_Pos,SelectTopFourCount_Model)
#predrink pos mood not sig


#official test of drink on selecttopfour
SelectTopFourCount_Model <- clmm(SelectTopFourCount ~ SubDrink +  (1|SubID:DyadID), data = SelectQ)	
summary(SelectTopFourCount_Model)


SelectTopFourCount_Model_Null <- clmm(SelectTopFourCount ~ 1+ (1|SubID:DyadID), data = SelectQ)	
summary(SelectTopFourCount_Model_Null)


anova(SelectTopFourCount_Model,SelectTopFourCount_Model_Null)

confint(SelectTopFourCount_Model)



