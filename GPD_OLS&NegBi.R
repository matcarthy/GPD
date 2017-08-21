#Analyses 
##OLS 2012

lm12.1 <- lm(gpd12nomiss$tot.aversion.idx ~ as.factor(gpd12nomiss$Q19_2) + as.factor(gpd12nomiss$Q19_3) + 
               gpd12nomiss$effective.idx + gpd12nomiss$know.law.idx + gpd12nomiss$know.idx + gpd12nomiss$agesex.f +
               gpd12nomiss$education_Level.f + gpd12nomiss$race.f + gpd12nomiss$protect.idx + gpd12nomiss$actual.exp.idx +
               gpd12nomiss$know.pp.idx, data = gpd12nomiss, weights = gpd12nomiss$wgt)

summary(lm12.1)

lm12.2 <- lm(gpd12nomiss$tot.aversion.idx ~ as.factor(gpd12nomiss$Q19_2) + gpd12nomiss$know.law.idx + 
               gpd12nomiss$know.idx + gpd12nomiss$agesex.f +  as.factor(gpd12nomiss$Q10_0) + 
               gpd12nomiss$actual.exp.idx, data = gpd12cooks, weights = gpd12cooks$wgt)

summary(lm12.2)

### Diagnostics 
lev <- hat(model.matrix(lm12.2))
plot(lev)
gpd12nolev <- gpd12nomiss[lev >0.055,]

plot(lm12.2$fitted, lm12.2$res)

cooks <- cooks.distance(lm12.2)
plot(cooks, ylab="Cooks distance")
gpd12cooks <- gpd12nomiss[cooks >0.06,]

qqnorm(lm12.2$res)
qqline(lm12.2$res)

hist(lm12.2$res)


##OLS 2014

lm14.1 <- lm(gpd14nomiss$tot.aversion.idx ~ gpd14nomiss$tot.trust.idx + gpd14nomiss$know.pp.idx + gpd14nomiss$know.tech.idx +
               as.factor(gpd14nomiss$S14_0) + as.factor(gpd14nomiss$S14_1) + as.factor(gpd14nomiss$S14_2) +
               as.factor(gpd14nomiss$S14_3) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + as.factor(gpd14nomiss$S17) +
               as.factor(gpd14nomiss$S6) + gpd14nomiss$race.f + gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$income.f +
               gpd14nomiss$ed.f + gpd14nomiss$age.f + gpd14nomiss$gender.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)


summary(lm14.1)

lm14.2 <- lm(gpd14nomiss$tot.aversion.idx ~ gpd14nomiss$tot.trust.idx + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S6) + 
               gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$ed.f + gpd14nomiss$age.f, data = gpd14cooks, weights = gpd14cooks$Weight)

summary(lm14.2)

### Diagnositcs

lev <- hat(model.matrix(lm14.2))
plot(lev)
gpd14nolev <- gpd14nomiss[lev >0.15,]

plot(lm14.2$fitted, lm14.2$res)

cooks <- cooks.distance(lm14.2)
plot(cooks, ylab="Cooks distance")
gpd14cooks <- gpd14nomiss[cooks >0.02,]

qqnorm(lm14.2$res)
qqline(lm14.2$res)

hist(lm14.2$res)

##neg binomial 2014

library(MASS)
lm14.3 <- glm.nb(gpd14nomiss$tot.trust.idx ~ gpd14nomiss$know.pp.idx + gpd14nomiss$know.tech.idx + gpd14nomiss$tot.aversion.idx +
                   as.factor(gpd14nomiss$S14_0) + as.factor(gpd14nomiss$S14_1) + as.factor(gpd14nomiss$S14_2) + 
                   as.factor(gpd14nomiss$S14_3) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + as.factor(gpd14nomiss$S17) +
                   as.factor(gpd14nomiss$S6) + gpd14nomiss$race.f + gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$income.f +
                   gpd14nomiss$ed.f + gpd14nomiss$age.f + gpd14nomiss$gender.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)


summary(lm14.3)

lm14.4 <- glm.nb(gpd14nomiss$tot.trust.idx ~  gpd14nomiss$tot.aversion.idx + as.factor(gpd14nomiss$S14_2) +  as.factor(gpd14nomiss$S1) + 
                   as.factor(gpd14nomiss$S5) + as.factor(gpd14nomiss$S17) + gpd14nomiss$ed.f + gpd14nomiss$age.f, data = gpd14nomiss, weights = gpd14cooks$Weight)

summary(lm14.4)



