library(tidyverse); library(EnvStats); library(psych); library(car)

#read indexed original data
occ.orig <- read.csv("data-occ-original-index.csv", stringsAsFactors = TRUE)
occ.combined <- read.csv("data-occ-combined-index.csv", stringsAsFactors = TRUE)

#Test for interaction Original
fm.lm <- lm(Index_FM ~ Subsistence + Sex + Subsistence:Sex, data=occ.orig)
car::Anova(fm.lm, type='III')
fm.combo.lm <- lm(Index_FM ~ Subsistence + Sex + Subsistence:Sex, data=occ.combined)
car::Anova(fm.combo.lm, type='III')

bcb.lm <- lm(BicondylarBreadth ~ Subsistence + Sex + Subsistence:Sex, data=occ.orig)
car::Anova(bcb.lm, type="III")
bcb.combo.lm <- lm(BicondylarBreadth ~ Subsistence + Sex + Subsistence:Sex, data=occ.combined)
car::Anova(bcb.combo.lm, type='III')

li.lm <- lm(LambdaInion ~ Subsistence + Sex + Subsistence:Sex, data=occ.orig)
car::Anova(li.lm, type="III")
form.lm <- lm(Index_Form ~ Subsistence + Sex + Subsistence:Sex, data=occ.orig)
car::Anova(form.lm, white.adjust=TRUE, Type="III")
nuchal.lm <- lm(Index_Nuchal ~ Subsistence + Sex + Subsistence:Sex, data=occ.orig)
car::Anova(nuchal.lm, type="III")
rm(fm.lm, fm.combo.lm, bcb.lm, bcb.combo.lm, li.lm,form.lm, nuchal.lm)

#No interaction, use Type ii
fm.lm2 <- lm(Index_FM ~ Subsistence + Sex, data=occ.orig)
fm.combo.lm2 <- lm(Index_FM ~ Subsistence + Sex, data=occ.combined)

bcb.lm2 <- lm(BicondylarBreadth ~ Subsistence + Sex, data=occ.orig)
bcb.combo.lm2 <- lm(BicondylarBreadth ~ Subsistence + Sex, data=occ.combined)

li.lm2 <- lm(LambdaInion ~ Subsistence + Sex, data=occ.orig)
form.lm2 <- lm(Index_Form ~ Subsistence + Sex, data=occ.orig)
nuchal.lm2 <- lm(Index_Nuchal ~ Subsistence + Sex, data=occ.orig)

car::Anova(fm.lm2, type='II')
car::Anova(fm.combo.lm2, white.adjust=TRUE, Type="II")

car::Anova(bcb.lm2, type="II")
car::Anova(bcb.combo.lm2, type='II')

car::Anova(li.lm2, type="II")
car::Anova(form.lm2, white.adjust=TRUE, Type="II")
car::Anova(nuchal.lm2, type="II")

summary(fm.lm2)
summary(fm.combo.lm2)

summary(bcb.lm2)
summary(bcb.combo.lm2)

summary(li.lm2)
summary(form.lm2)
summary(nuchal.lm2)

#aov for tukey
bcb.combo.aov <- aov(BicondylarBreadth ~ Subsistence + Sex, data=occ.combined)
fm.combo.aov<- aov(Index_FM ~ Subsistence + Sex, data=occ.combined)
TukeyHSD(bcb.combo.aov, conf.level=.95)
TukeyHSD(fm.combo.aov, conf.level=.95)

#tidy
rm(list = ls())
gc()

