## ----echo=TRUE----------------------------------------------------------------

library(dplyr)
library(lme4)
library(ggplot2)
library(emmeans)


setwd("C:\\Users\\Tijn\\Documents\\PhD\\Experiment_2_Interference_anaphora_resolution\\Analysis\\analysis")


collectedDF <- read.csv("allACTFiles.txt", sep=" ")

collectedDF$item <- as.factor(collectedDF$item)

collectedDF <- subset(collectedDF, item != 014 & item != 024 & subjectnr != 004,  drop=TRUE)

collectedDF$item <- as.factor(collectedDF$item)


collectedDF <- subset(collectedDF, !(subjectnr == 004 & (item == 3 | item == 4 | item == 6 | item == 7)))
collectedDF <- subset(collectedDF, !(subjectnr == 038 & item == 14))
collectedDF <- subset(collectedDF, !(subjectnr == 040 & item == 19))
collectedDF <- subset(collectedDF, !(subjectnr == 013 & item == 25))
collectedDF <- subset(collectedDF, !(subjectnr == 043 & (item == 3 | item == 6 | item == 7)))
collectedDF <- subset(collectedDF, !(subjectnr == 020 & item == 30))

collectedDF$cond <- as.character(collectedDF$cond)

collectedDF <- collectedDF %>% group_by(cond) %>% mutate(quan = strsplit(cond[1], "_")[[1]][1], subj = strsplit(cond[1], "_")[[1]][2], obj = strsplit(cond[1], "_")[[1]][3]) %>% ungroup()


collectedDF <- collectedDF[,-c(4)]


collectedDF$quan <- as.factor(collectedDF$quan)
collectedDF$subj <- as.factor(collectedDF$subj)
collectedDF$obj <- as.factor(collectedDF$obj)

NOTREFQ="GEEN"


## ----echo=TRUE----------------------------------------------------------------

region1 <- subset(collectedDF, code == 1)



## ----echo=TRUE----------------------------------------------------------------


#FFDUR

#NS
#modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region1, ffdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region1, ffdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



#modelsimplerand11 <- lmer(log(ffdur) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=subset(region1, ffdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand11), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################


#none converged




## ----echo=TRUE----------------------------------------------------------------

#GDUR

#NS
#modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region1, gdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region1, gdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TGDUR

#NS
#modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region1, tgdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region1, tgdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged

################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

#RPDUR

#NS
#modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region1, rpdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region1, rpdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR

modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region1, totfixdur != 0))
print(summary(modelsimple), corr=FALSE)

emmip(modelsimple, subj ~ obj ~ quan)


#TEST WITHOUT EEN_MATCH_MATCH (NO RESOLUTION POSSIBLE DUE TO AMBIGUITY)

collectedDF2 <- subset(collectedDF, !(quan == "EEN" & subj == "MATCH" & obj == "MATCH"))
region1a <- subset(collectedDF2, code == 1)

modelsimple1 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region1a, totfixdur != 0))
print(summary(modelsimple1), corr=FALSE)

emmip(modelsimple1, subj ~ obj ~ quan)




#didn't converge
#modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region1, totfixdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################




modelsimplerand11 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=subset(region1, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand11), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RR


region1$rrdur <- region1$totfixdur - region1$gdur
region1$rr <- region1$rrdur > 0

#didn't converge
#modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region1, family=binomial)
#print(modelsimple)

#NS
#modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region1, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#none converged





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region1$rrdur <- region1$totfixdur - region1$gdur


modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region1, rrdur != 0))
print(summary(modelsimple), corr=FALSE)


emmip(modelsimple, subj~quan )
emmip(modelsimple, subj~obj )
emmip(modelsimple, subj~obj~quan )

#NS
#modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region1, rrdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#not present in Region 1



## ----echo=TRUE----------------------------------------------------------------

region2 <- subset(collectedDF, code == 2)



## ----echo=TRUE----------------------------------------------------------------


#FFDUR

#NS
#modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region2, ffdur != 0))
#print(summary(modelsimple), corr=FALSE)

#didn't converge
#modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region2, ffdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#GDUR

#close to significance
modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region2, gdur != 0))
print(summary(modelsimple), corr=FALSE)

#didn't converge
#modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region2, gdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TGDUR

modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region2, tgdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region2, tgdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand9 <- lmer(log(tgdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region2, tgdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand9), corr=FALSE)


emmip(modelsimplerand9, subj~obj~quan)

################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RPDUR

modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region2, rpdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region2, rpdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


modelsimplerand9 <- lmer(log(rpdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region2, rpdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand9), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR

#close to significance
modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region2, totfixdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region2, totfixdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#RR


region2$rrdur <- region2$totfixdur - region2$gdur
region2$rr <- region2$rrdur > 0

#NS
#modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region2, family=binomial)
#print(modelsimple)

#NS
#modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region2, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#none converged





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region2$rrdur <- region2$totfixdur - region2$gdur


#NS
#modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region2, rrdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region2, rrdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

temp.region2 <- subset(region2, gbck != 0)
temp.region2$gbck <- abs(temp.region2$gbck - 2)

#didn't converge
#modelsimple <- glmer(gbck ~ quan * subj * obj + (1|subjectnr) + (1|item), data=temp.region2, family=binomial)
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- glmer(gbck ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(temp.region2, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)




#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged



## ----echo=TRUE----------------------------------------------------------------

region3 <- subset(collectedDF, code == 3)




## ----echo=TRUE----------------------------------------------------------------


#FFDUR

#NS
#modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region3, ffdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region3, ffdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#GDUR

modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region3, gdur != 0))
print(summary(modelsimple), corr=FALSE)
emmip(modelsimple, subj~obj~quan)

modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region3, gdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)
emmip(modelsubset, subj~obj)

#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------
#TGDUR

modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region3, tgdur != 0))
print(summary(modelsimple), corr=FALSE)


modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region3, tgdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset, subj~obj)




#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand11 <- lmer(log(tgdur) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=subset(region3, tgdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand11), corr=FALSE)

emmip(modelsimplerand11, subj~obj~quan)


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#RPDUR

modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region3, rpdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region3, rpdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand11 <- lmer(log(rpdur) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=subset(region3, rpdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand11), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged







## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR

modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region3, totfixdur != 0))
print(summary(modelsimple), corr=FALSE)
emmip(modelsimple, subj~obj~quan)

modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region3, totfixdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset,obj~subj)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand9 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region3, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand9), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#RR


region3$rrdur <- region3$totfixdur - region3$gdur
region3$rr <- region3$rrdur > 0

modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region3, family=binomial)
print(modelsimple)

#NS
#modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region3, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################




modelsimplerand9 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=region3, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand9), corr=FALSE)

#NS, better random fit
modelsimplerand10 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=region3, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region3$rrdur <- region3$totfixdur - region3$gdur


modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region3, rrdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region3, rrdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

temp.region3 <- subset(region3, gbck != 0)
temp.region3$gbck <- abs(temp.region3$gbck - 2)

#didn't converge
#modelsimple <- glmer(gbck ~ quan * subj * obj + (1|subjectnr) + (1|item), data=temp.region3, family=binomial)
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- glmer(gbck ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(temp.region3, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged



## ----echo=TRUE----------------------------------------------------------------

region5 <- subset(collectedDF, code == 5)




## ----echo=TRUE----------------------------------------------------------------


#FFDUR

modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region5, ffdur != 0))
print(summary(modelsimple), corr=FALSE)

emmip(modelsimple, subj~obj~quan)

#close to significance
modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region5, ffdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################


#none converged





## ----echo=TRUE----------------------------------------------------------------

#GDUR

#NS
#modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region5, gdur != 0))
#print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region5, gdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset, subj~obj)

#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################



modelsubsetrand2 <- lmer(log(gdur) ~ subj * obj  + (1 + obj | subjectnr) + (1 + obj | item), data=subset(region5, gdur != 0 & quan == NOTREFQ), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsubsetrand2), corr=FALSE)


emmip(modelsubsetrand2, subj~obj)




## ----echo=TRUE----------------------------------------------------------------
#TGDUR

#NS
#modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region5, tgdur != 0))
#print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region5, tgdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset, subj~obj)

#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand10 <- lmer(log(tgdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region5, tgdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand10), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################



modelsubsetrand3 <- lmer(log(tgdur) ~ subj * obj  + (1 + subj | subjectnr) + (1 + subj | item), data=subset(region5, tgdur != 0 & quan == NOTREFQ), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsubsetrand3), corr=FALSE)





## ----echo=TRUE----------------------------------------------------------------

#RPDUR

#NS
#modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region5, rpdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region5, rpdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################


#NS
#modelsubsetrand3 <- lmer(log(rpdur) ~ subj * obj  + (1 + subj | subjectnr) + (1 + subj | item), data=subset(region5, rpdur != 0 & quan == NOTREFQ), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsubsetrand3), corr=FALSE)






## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR


#NS
#modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region5, totfixdur != 0))
#print(summary(modelsimple), corr=FALSE)

#close to significance
modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region5, totfixdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand9 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region5, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand9), corr=FALSE)

#NS
#modelsimplerand10 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region5, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand10), corr=FALSE)




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RR


region5$rrdur <- region5$totfixdur - region5$gdur
region5$rr <- region5$rrdur > 0

#NS
#modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region5, family=binomial)
#print(modelsimple)

#didn't converge
#modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region5, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand11 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=region5, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand11), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region5$rrdur <- region5$totfixdur - region5$gdur


#close to significance
modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region5, rrdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region5, rrdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

temp.region5 <- subset(region5, gbck != 0)
temp.region5$gbck <- abs(temp.region5$gbck - 2) 

#didn't converge, very large eigenvalue
#modelsimple <- glmer(gbck ~ quan * subj * obj + (1|subjectnr) + (1|item), data=temp.region5, family=binomial)
#print(summary(modelsimple), corr=FALSE)

#close to significance (subjMIS 0.08)
modelsubset <- glmer(gbck ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(temp.region5, quan == NOTREFQ), family=binomial)
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

region6 <- subset(collectedDF, code == 6)



## ----echo=TRUE----------------------------------------------------------------


#FFDUR

#NS
#modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region6, ffdur != 0))
#print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region6, ffdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset, subj~obj)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand9 <- lmer(log(ffdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region6, ffdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand9), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#GDUR

modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region6, gdur != 0))
print(summary(modelsimple), corr=FALSE)

emmip(modelsimple, subj~obj~quan)

modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region6, gdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset, subj~obj)

#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TGDUR

modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region6, tgdur != 0))
print(summary(modelsimple), corr=FALSE)

emmip(modelsimple, subj~obj~quan)

modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region6, tgdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset, subj~obj)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RPDUR

#NS
#modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region6, rpdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region6, rpdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR

modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region6, totfixdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region6, totfixdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset, subj~obj)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand9 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region6, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand9), corr=FALSE)




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#RR


region6$rrdur <- region6$totfixdur - region6$gdur
region6$rr <- region6$rrdur > 0

#didn't converge
#modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region6, family=binomial)
#print(modelsimple)

#NS
#modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region6, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



#9 better random fit
modelsimplerand9 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=region6, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand9), corr=FALSE)


modelsimplerand10 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=region6, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region6$rrdur <- region6$totfixdur - region6$gdur


#NS
#modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region6, rrdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region6, rrdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand9 <- lmer(log(rrdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region6, rrdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand9), corr=FALSE)




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################


#NS
#modelsubsetrand3 <- lmer(log(rrdur) ~ subj * obj  + (1 + subj | subjectnr) + (1 + subj | item), data=subset(region6, rrdur != 0 & quan == NOTREFQ), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsubsetrand3), corr=FALSE)






## ----echo=TRUE----------------------------------------------------------------

temp.region6 <- subset(region6, gbck != 0)
temp.region6$gbck <- abs(temp.region6$gbck - 2)

#didn't converge, very large eigenvalue
#modelsimple <- glmer(gbck ~ quan * subj * obj + (1|subjectnr) + (1|item), data=temp.region6, family=binomial)
#print(summary(modelsimple), corr=FALSE)

#objMIS 0.0554 (close to significance)
modelsubset <- glmer(gbck ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(temp.region6, quan == NOTREFQ), family=binomial)
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged



## ----echo=TRUE----------------------------------------------------------------

region7 <- subset(collectedDF, code == 7)




## ----echo=TRUE----------------------------------------------------------------


#FFDUR

#NS
#modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region7, ffdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region7, ffdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#GDUR

#NS
#modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region7, gdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region7, gdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------
#TGDUR

#NS
#modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region7, tgdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region7, tgdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

#RPDUR

#close to significance
modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region7, rpdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region7, rpdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)

emmip(modelsubset, subj~obj)

#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand10 <- lmer(log(rpdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region7, rpdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR

modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region7, totfixdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region7, totfixdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

#RR


region7$rrdur <- region7$totfixdur - region7$gdur
region7$rr <- region7$rrdur > 0

#didn't converge
#modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region7, family=binomial)
#print(modelsimple)

modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region7, quan == NOTREFQ), family=binomial)
print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand10 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=region7, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand10), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region7$rrdur <- region7$totfixdur - region7$gdur


#close to significance
modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region7, rrdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region7, rrdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

temp.region7 <- subset(region7, gbck != 0)
temp.region7$gbck <- abs(temp.region7$gbck - 2)

#didn't converge
#modelsimple <- glmer(gbck ~ quan * subj * obj + (1|subjectnr) + (1|item), data=temp.region7, family=binomial)
#print(summary(modelsimple), corr=FALSE)

#didn't converge
#modelsubset <- glmer(gbck ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(temp.region7, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)

#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

region8 <- subset(collectedDF, code == 8)




## ----echo=TRUE----------------------------------------------------------------


#FFDUR

modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region8, ffdur != 0))
print(summary(modelsimple), corr=FALSE)

#close to significance
modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region8, ffdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#close to significance
modelsimplerand10 <- lmer(log(ffdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region8, ffdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################


#close to significance
modelsubsetrand3 <- lmer(log(ffdur) ~ subj * obj  + (1 + subj | subjectnr) + (1 + subj | item), data=subset(region8, ffdur != 0 & quan == NOTREFQ), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsubsetrand3), corr=FALSE)





## ----echo=TRUE----------------------------------------------------------------

#GDUR

modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region8, gdur != 0))
print(summary(modelsimple), corr=FALSE)


#NS
#modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region8, gdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#both comparable random fit (not good)
modelsimplerand10 <- lmer(log(gdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region8, gdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)


modelsimplerand11 <- lmer(log(gdur) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=subset(region8, gdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand11), corr=FALSE)

emmip(modelsimplerand11, subj~obj~quan)

################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#NS
#modelsubsetrand2 <- lmer(log(gdur) ~ subj * obj  + (1 + obj | subjectnr) + (1 + obj | item), data=subset(region8, gdur != 0 & quan == NOTREFQ), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsubsetrand2), corr=FALSE)

#NS
#modelsubsetrand3 <- lmer(log(gdur) ~ subj * obj  + (1 + subj | subjectnr) + (1 + subj | item), data=subset(region8, gdur != 0 & quan == NOTREFQ), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsubsetrand3), corr=FALSE)





## ----echo=TRUE----------------------------------------------------------------
#TGDUR

modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region8, tgdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region8, tgdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand10 <- lmer(log(tgdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region8, tgdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RPDUR

modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region8, rpdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region8, rpdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged






## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR

modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region8, totfixdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region8, totfixdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


modelsimplerand11 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=subset(region8, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand11), corr=FALSE)

emmip(modelsimplerand11, subj~obj~quan)


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################



modelsubsetrand2 <- lmer(log(totfixdur) ~ subj * obj  + (1 + obj | subjectnr) + (1 + obj | item), data=subset(region8, totfixdur != 0 & quan == NOTREFQ), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsubsetrand2), corr=FALSE)





## ----echo=TRUE----------------------------------------------------------------

#RR


region8$rrdur <- region8$totfixdur - region8$gdur
region8$rr <- region8$rrdur > 0

#NS
#modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region8, family=binomial)
#print(modelsimple)

#NS
#modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region8, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand11 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=region8, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand11), corr=FALSE)





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region8$rrdur <- region8$totfixdur - region8$gdur


modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region8, rrdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region8, rrdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

temp.region8 <- subset(region8, gbck != 0)
temp.region8$gbck <- abs(temp.region8$gbck - 2)

#didn't converge
#modelsimple <- glmer(gbck ~ quan * subj * obj + (1|subjectnr) + (1|item), data=temp.region8, family=binomial)
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- glmer(gbck ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(temp.region8, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)

#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

region9 <- subset(collectedDF, code == 9)




## ----echo=TRUE----------------------------------------------------------------


#FFDUR

modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region9, ffdur != 0))
print(summary(modelsimple), corr=FALSE)

#didn't converge
#modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region9, ffdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand9 <- lmer(log(ffdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region9, ffdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand9), corr=FALSE)


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

#GDUR

modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region9, gdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region9, gdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TGDUR

modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region9, tgdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region9, tgdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#both equal random fit, 10 slightly better
modelsimplerand10 <- lmer(log(tgdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region9, tgdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)


modelsimplerand11 <- lmer(log(tgdur) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=subset(region9, tgdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand11), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RPDUR

modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region9, rpdur != 0))
print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region9, rpdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################



modelsimplerand9 <- lmer(log(rpdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region9, rpdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand9), corr=FALSE)




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR

modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region9, totfixdur != 0))
print(summary(modelsimple), corr=FALSE)


#NS
#modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region9, totfixdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#comparable, 10 slightly better
modelsimplerand10 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region9, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)


modelsimplerand11 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + obj  | subjectnr) + (1 + obj | item), data=subset(region9, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand11), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RR


region9$rrdur <- region9$totfixdur - region9$gdur
region9$rr <- region9$rrdur > 0

#NS
#modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region9, family=binomial)
#print(summary(modelsimple))

#NS
modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region9, quan == NOTREFQ), family=binomial)
print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#none converged





################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region9$rrdur <- region9$totfixdur - region9$gdur


#NS
#modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region9, rrdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region9, rrdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------



temp.region9 <- subset(region9, gbck != 0)
temp.region9$gbck <- abs(temp.region9$gbck - 2) 

#didn't converge
#modelsimple <- glmer(gbck ~ quan * subj * obj + (1|subjectnr) + (1|item), data=temp.region9, family=binomial)
#print(summary(modelsimple), corr=FALSE)


#NS
#modelsubset <- glmer(gbck ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(temp.region9, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################




#NS
#modelsubset1 <- glmer(as.factor(gbck) ~ subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(temp.region9, quan==NOTREFQ), family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsubset1), corr=FALSE)






## ----echo=TRUE----------------------------------------------------------------

collectedDF1 <- read.csv("allACTFiles_vp.txt", sep=" ")
collectedDF1$item <- as.factor(collectedDF1$item)
collectedDF1 <- subset(collectedDF1, item != 014 & item != 024 & subjectnr != 004,  drop=TRUE)
collectedDF1$item <- as.factor(collectedDF1$item)
collectedDF1 <- subset(collectedDF1, !(subjectnr == 004 & (item == 3 | item == 4 | item == 6 | item == 7)))
collectedDF1 <- subset(collectedDF1, !(subjectnr == 038 & item == 14))
collectedDF1 <- subset(collectedDF1, !(subjectnr == 040 & item == 19))
collectedDF1 <- subset(collectedDF1, !(subjectnr == 013 & item == 25))
collectedDF1 <- subset(collectedDF1, !(subjectnr == 043 & (item == 3 | item == 6 | item == 7)))
collectedDF1 <- subset(collectedDF1, !(subjectnr == 020 & item == 30))
collectedDF1$cond <- as.character(collectedDF1$cond)
collectedDF1 <- collectedDF1 %>% group_by(cond) %>% mutate(quan = strsplit(cond[1], "_")[[1]][1], subj = strsplit(cond[1], "_")[[1]][2], obj = strsplit(cond[1], "_")[[1]][3]) %>% ungroup()
collectedDF1 <- collectedDF1[,-c(4)]
collectedDF1$quan <- as.factor(collectedDF1$quan)
collectedDF1$subj <- as.factor(collectedDF1$subj)
collectedDF1$obj <- as.factor(collectedDF1$obj)

NOTREFQ="GEEN"

levels(as.factor(collectedDF1$code))



## ----echo=TRUE----------------------------------------------------------------

region67 <- subset(collectedDF1, code == 6)



## ----echo=TRUE----------------------------------------------------------------


#FFDUR

#NS
#modelsimple <- lmer(log(ffdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region67, ffdur != 0))
#print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(ffdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region67, ffdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand9 <- lmer(log(ffdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region67, ffdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand9), corr=FALSE)




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

#GDUR

modelsimple <- lmer(log(gdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region67, gdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(gdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region67, gdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#NS
#modelsimplerand10 <- lmer(log(gdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region67, gdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand10), corr=FALSE)




################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none significant




## ----echo=TRUE----------------------------------------------------------------
#TGDUR

modelsimple <- lmer(log(tgdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region67, tgdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(tgdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region67, tgdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged

################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged





## ----echo=TRUE----------------------------------------------------------------

#RPDUR

#NS
#modelsimple <- lmer(log(rpdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region67, rpdur != 0))
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- lmer(log(rpdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region67, rpdur != 0 & quan == NOTREFQ))
#print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand9 <- lmer(log(rpdur) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=subset(region67, rpdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand9), corr=FALSE)

#NS
#modelsimplerand10 <- lmer(log(rpdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region67, rpdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand10), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged



## ----echo=TRUE----------------------------------------------------------------
#TOTFIXDUR

modelsimple <- lmer(log(totfixdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region67, totfixdur != 0))
print(summary(modelsimple), corr=FALSE)

modelsubset <- lmer(log(totfixdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region67, totfixdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


modelsimplerand10 <- lmer(log(totfixdur) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=subset(region67, totfixdur != 0), control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
print(summary(modelsimplerand10), corr=FALSE)

emmip(modelsimplerand10, subj~obj~quan)


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

#RR


region67$rrdur <- region67$totfixdur - region67$gdur
region67$rr <- region67$rrdur > 0

#NS
#modelsimple <- glmer(as.factor(rr) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=region67, family=binomial)
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- glmer(as.factor(rr) ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(region67, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#NS
#modelsimplerand8 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + quan + subj   | subjectnr) + (1 + quan + subj  | item), data=region67, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand8), corr=FALSE)

#NS
#modelsimplerand9 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + quan   | subjectnr) + (1 + quan  | item), data=region67, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand9), corr=FALSE)

#NS
#modelsimplerand10 <- glmer(as.factor(rr) ~ quan * subj * obj  + (1 + subj  | subjectnr) + (1 + subj | item), data=region67, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e5)))
#print(summary(modelsimplerand10), corr=FALSE)



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

#RRDUR


region67$rrdur <- region67$totfixdur - region67$gdur


modelsimple <- lmer(log(rrdur) ~ quan * subj * obj  + (1|subjectnr) + (1|item), data=subset(region67, rrdur != 0))
print(summary(modelsimple), corr=FALSE)

emmip(modelsimple,  obj ~ quan)


modelsubset <- lmer(log(rrdur) ~ subj * obj + (1|subjectnr) + (1|item), data=subset(region67, rrdur != 0 & quan == NOTREFQ))
print(summary(modelsubset), corr=FALSE)


#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################

#none converged


################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




## ----echo=TRUE----------------------------------------------------------------

temp.region67 <- subset(region67, gbck != 0)
temp.region67$gbck <- abs(region67$gbck - 2)

#didn't converge
#modelsimple <- glmer(gbck ~ quan * subj * obj + (1|subjectnr) + (1|item), data=temp.region67, family=binomial)
#print(summary(modelsimple), corr=FALSE)

#NS
#modelsubset <- glmer(gbck ~ subj * obj  + (1|subjectnr) + (1|item), data=subset(temp.region67, quan == NOTREFQ), family=binomial)
#print(summary(modelsubset), corr=FALSE)



#################################
#SIMPLE MODELS (+RANDOM EFFECTS)#
#################################


#none converged



################################
#MODELSUBSETS (+RANDOM EFFECTS)#
################################

#none converged




