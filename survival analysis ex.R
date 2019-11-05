#SURVIVAL ANALYSIS(unemployement data sets)
#install.packages("survival")
#install.packages("survminer")
library(survminer)
library(survival)
survival_unemp <- read.csv(file.choose())
View(survival_unemp)
str(survival_unemp)
attach(survival_unemp)
time <- spell
event <- event
group <- ui

summary(time)
summary(event)
summary(group)
table(group)
#kaplan-meier non parametic analysis
kmsurvival <- survfit(Surv(time,event)~1)
#only one individual
summary(kmsurvival)
plot(kmsurvival,xlab = "time",ylab = "survival probability")
ggsurvplot(kmsurvival,data = survival_unemp,risk.table = TRUE)
#kaplan-meier non parametric analysis by group
kmsurvival1 <- survfit(Surv(time,event)~group)
summary(kmsurvival1)
plot(kmsurvival1,xlab = "time",ylab = "survival probability")
ggsurvplot(kmsurvival1,data = survival_unemp,risk.table = TRUE)
