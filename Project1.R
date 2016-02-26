## R Project ##
library(plm)
library(QMSS)
library(VGAM)

d=read.csv(file.choose())

#### Independent variables ####
## Income
table(d$rrincome)
summay(d$rrincome)

### OLS with sex, age and income ### 
monlm<-lm(rgivchrty~age+rsex+rrincome, data=d)

vollm<-lm(rvolchrty~age+rsex+rrincome, data=d)

### 1. Ordinal Logit ####

Lmonlm<-vglm(rgivchrty~age+rsex+rrincome, data=d, family = propodds)
coef.child <- coef(summary(Lmonlm))
coef.child <- data.frame(coef.child)
coef.child[, "p.value"] <- 2*(1 - pnorm(abs(coef.child[, "z.value"])))
coef.child$odds.ratio <- exp(coef.child[, "Estimate"])
coef.child

Lvollm<-vglm(rvolchrty~age+rsex+rrincome, data=d, family = propodds)
coef.child1 <- coef(summary(Lvollm))
coef.child1 <- data.frame(coef.child1)
coef.child1[, "p.value"] <- 2*(1 - pnorm(abs(coef.child1[, "z.value"])))
coef.child1$odds.ratio <- exp(coef.child1[, "Estimate"])
coef.child1

### 2. Add control variable Educ and ###
ELmonlm<-vglm(rgivchrty~age+rsex+rrincome+educ, data=d, family = propodds)
coef.child1 <- coef(summary(ELmonlm))
coef.child1 <- data.frame(coef.child1)
coef.child1[, "p.value"] <- 2*(1 - pnorm(abs(coef.child1[, "z.value"])))
coef.child1$odds.ratio <- exp(coef.child1[, "Estimate"])
coef.child1

ELvollm<-vglm(rvolchrty~age+rsex+rrincome+educ, data=d, family = propodds)
coef.child1 <- coef(summary(ELvollm))
coef.child1 <- data.frame(coef.child1)
coef.child1[, "p.value"] <- 2*(1 - pnorm(abs(coef.child1[, "z.value"])))
coef.child1$odds.ratio <- exp(coef.child1[, "Estimate"])
coef.child1

### 3. Interaction: Sex*income ###
d$sexInc <- d$rsex * log(d$realinc)
ELmonlm<-vglm(rgivchrty~age+rsex+rrincome+educ+sexInc, data=d, family = propodds)

ELvollm<-vglm(rvolchrty~age+rsex+rrincome+educ+sexInc, data=d, family = propodds)



