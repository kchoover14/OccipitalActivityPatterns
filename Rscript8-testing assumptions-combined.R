library(tidyverse); library(EnvStats)
library(psych); library(car); library(MASS)

#load data and prepare for merge
occ.orig <- read.csv("data-occ-original-index.csv", stringsAsFactors = TRUE)
occ.orig <- dplyr::select(occ.orig, Collection, Subsistence, Sex, BicondylarBreadth, Index_FM)
occ.mined <- read.csv("data-occ-mined-index.csv", stringsAsFactors = TRUE)
occ.mined <- dplyr::select(occ.mined, Collection, Subsistence, Sex, BicondylarBreadth, Index_FM)
occ.combined <- rbind(occ.orig, occ.mined)
write.csv(occ.combined, "data-occ-combined-index.csv", row.names = F, quote = F)
rm(occ.orig, occ.mined, occ.combined)

#load data
occ.combined <- read.csv("data-occ-combined-index.csv", stringsAsFactors = TRUE)

#descriptives
substats <- describeBy(occ.combined[4:5], occ.combined$Subsistence)
substats.table <-do.call("rbind",substats)
write.csv(substats.table, file="occ.descstats-SUB-combined.csv", quote = FALSE,
          row.names = FALSE)
rm(substats, substats.table)
sexstats <- describeBy(occ.combined[4:5], occ.combined$Sex)
sexstats.table <-do.call("rbind",sexstats)
write.csv(sexstats.table, file="occ.descstats-SEX-combined.csv", quote = FALSE,
          row.names = FALSE)
rm(sexstats, sexstats.table)

#testing assumptions, normal distribution
png("SF8-normal plot-bcb-combined data.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
car::qqp(occ.combined$BicondylarBreadth, "norm", grid=FALSE, ylab=paste("Bicondylar Breadth Area"),
         xlab=paste("Normal Distribution Quantiles"))
dev.off()
png("SF9-normal plot-fm-combined data.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
car::qqp(occ.combined$Index_FM, "norm", grid=FALSE, ylab=paste("Foramen Magnum Area"),
         xlab=paste("Normal Distribution Quantiles"))
dev.off()

#testing assumptions, equality of variances
a <- leveneTest(occ.combined$BicondylarBreadth ~ occ.combined$Subsistence)
b <- leveneTest(occ.combined$BicondylarBreadth ~ occ.combined$Sex)

c <- leveneTest(occ.combined$Index_FM ~ occ.combined$Subsistence)
d <- leveneTest(occ.combined$Index_FM ~ occ.combined$Sex) # at 0.046

#check difference between min and max var
temp.fem <- filter(occ.combined, Sex == "Female", na.rm = TRUE)
temp.male <- filter(occ.combined, Sex == "Male", na.rm = TRUE)
var(temp.fem$Index_FM, na.rm = TRUE) #8868
var(temp.male$Index_FM, na.rm = TRUE) #12480
#Female variance for Index FM is 71% the size of male
#could use lm b/c small differences
#will use white.adjust in Anova
rm(temp.fem, temp.male)

levene.desc <- rbind(a, b, c, d)
levene.desc <- data.frame(levene.desc)
rm(a, b, c, d)
head(row.names(levene.desc))
tail(row.names(levene.desc))
row.names(levene.desc)[row.names(levene.desc) == "group"] <- "BicondylarBreadth_Sub"
row.names(levene.desc)[row.names(levene.desc) == ""] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group1"] <- "BicondylarBreadth_Sex"
row.names(levene.desc)[row.names(levene.desc) == "1"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group2"] <- "Index_FM_Sub"
row.names(levene.desc)[row.names(levene.desc) == "2"] <- ""
row.names(levene.desc)[row.names(levene.desc) == "group3"] <- "Index_FM_Sex"
row.names(levene.desc)[row.names(levene.desc) == "3"] <- ""
levene.desc <- round(levene.desc, 4)
write.csv(levene.desc, file="occ.levenes-combined.csv", quote = FALSE,
          row.names = TRUE)

#testing assumptions, outliers
rosnerTest(occ.combined$BicondylarBreadth, k=5, alpha=0.05, warn=TRUE)
rosnerTest(occ.combined$Index_FM, k=5, alpha=0.05, warn=TRUE)

#tidy
rm(list = ls())
gc()


